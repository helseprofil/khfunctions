#' @title LagKUBE
#' @description 
#' The main function of the production line, producing the files going to FHI Statistikk and public health profiles
#'
#' @param name Name of kube, corresponding to KUBE_NAVN in ACCESS
#' @param write should results be written to files, default = TRUE. Set to FALSE for testing (only save to global envir)
#' @param alarm if TRUE, plays a sound when done
#' @param geonaboprikk  should the file be secondary censored on geographical codes? default = TRUE
#' @param year year to get valid GEO codes and to produce correct FRISKVIK files, defaults to getOption("khfunctions.year")
#' @param dumps list of required dumps, in the format list(dumpname = "format")
#' @return complete data file, publication ready file, and quality control file.
LagKUBE <- function(name, write = FALSE, alarm = FALSE, geonaboprikk = TRUE, year = getOption("khfunctions.year"), dumps = list()) {
  on.exit(lagkube_cleanup(), add = TRUE)
  check_connection_folders()
  check_if_lagkube_available()
  user_args = as.list(environment())
  parameters <- get_cubeparameters(user_args = user_args)
  sink(file = file.path(getOption("khfunctions.root"), getOption("khfunctions.dumpdir"), paste0("KUBELOGG/", parameters$name, "_", parameters$batchdate, "_LOGG.txt")), split = TRUE)
  
  if(!parameters$geonaboprikk) message("OBS! GEO-naboprikking er deaktivert!")
  load_and_format_files(parameters = parameters)
  parameters[["filedesign"]] <- get_filedesign(parameters = parameters)
  parameters[["PredFilter"]] <- set_predictionfilter(parameters = parameters)
  save_kubespec_csv(spec = parameters$CUBEinformation)
  write_access_specs(parameters = parameters)
  
  TNF <- merge_teller_nevner(parameters = parameters)
  KUBE <- TNF$TNF
  if(parameters$TNPinformation$NEVNERKOL != "-") add_new_value_columns(dt = KUBE, formulas = "RATE={TELLER/NEVNER}", post_moving_average = FALSE)
  organize_file_for_moving_average(dt = KUBE)
  parameters[["MOVAVparameters"]] <- get_movav_information(dt = KUBE, parameters = parameters)
  KUBE <- aggregate_to_periods(dt = KUBE, reset_rate = TRUE, parameters = parameters)
  parameters[["CUBEdesign"]] <- update_cubedesign_after_moving_average(dt = KUBE, origdesign = TNF$KUBEd$MAIN, parameters = parameters)
  
  KUBE <- add_predteller(dt = KUBE, parameters = parameters)
  KUBE <- add_meisskala(dt = KUBE, parameters = parameters)
  remove_original_files_from_buffer()
  KUBE <- scale_rate_and_meisskala(dt = KUBE, parameters = parameters)
  KUBE <- fix_geo_special(dt = KUBE, parameters = parameters)

  KUBE <- do_special_handling(dt = KUBE, code = parameters$CUBEinformation$SLUTTREDIGER, parameters = parameters, dumpname = "SLUTTREDIGER")
  
  parameters[["MALTALL"]] <- get_maltall_column(parameters = parameters)
  KUBE <- do_format_cube_columns(dt = KUBE, parameters = parameters)
  KUBE <- add_smr_and_meis(dt = KUBE, parameters = parameters)
  KUBE <- adjust_smr_and_meis_to_country_normal(dt = KUBE, parameters = parameters)
  KUBE <- filter_invalid_geo_alder_kjonn(dt = KUBE, parameters = parameters)
  parameters[["etabs"]] <- get_etabs(columnnames = names(KUBE), parameters = parameters)
  KUBE <- set_etab_names(dt = KUBE, etablist = parameters$etabs)
  outvalues <- get_outvalues_allvis(parameters = parameters)
  outdimensions <- get_outdimensions(dt = KUBE, etabs = parameters$etabs$tabnames, parameters = parameters)
  
  KUBE <- do_censor_cube(dt = KUBE, parameters = parameters)
  KUBE <- do_special_handling(dt = KUBE, code = parameters$CUBEinformation$RSYNT_POSTPROSESS, parameters = parameters, dumpname = "RSYNT_POSTPROSESS")
  
  ALLVIS <- data.table::copy(KUBE)
  ALLVIS <- do_remove_censored_observations(dt = ALLVIS, outvalues = outvalues)
  generate_and_export_all_friskvik_indicators(dt = dt, parameters = parameters)
  ALLVIS <- ALLVIS[, c(..outdimensions, ..outvalues, "SPVFLAGG")]
  QC <- LagQCKube(allvis = ALLVIS, allvistabs = outdimensions, kube = KUBE)
  
  RESULTAT <<- list(KUBE = KUBE, ALLVIS = ALLVIS, QC = QC)
  write_cube_output(outputlist = RESULTAT, parameters = parameters)
  cat("-------------------------KUBE", parameters$name, "FERDIG--------------------------------------\n")
  cat("Se output med RESULTAT$KUBE (full), RESULTAT$ALLVIS (utfil) eller RESULTAT$QC (kvalkont)")
  if(alarm) try(beepr::beep(1))
}

#' @title check_if_lagkube_available
#' @description
#' Checks if guardfile exists, indicating that the system is already running.
#' If file doesn't exist, or if user overrides and force continue, the file
#' is generated and TRUE is returned indicating that the function may continue. 
#' If the file exists and the user does not override, FALSE is returned indicating 
#' that the system is busy and data processing stops.
#' 
#' An on.exit call must be included in the main function to delete
#' the file when the function finish or crash. This function checks if the file already exists, 
#' and generate the file if not (or overridden by user). 
#' @keywords internal
#' @noRd
check_if_lagkube_available <- function(){
  file <- get_lagkube_guardfile_path()
  continue <- TRUE
  if(file.exists(file)){
    force_continue <- utils::menu(choices = c("YES", "NO"),
                                  title = paste0("It appears that another cube is already being processed on this computer. ",
                                                 "To avoid errors due to parallell processing, this is not allowed.\n\n",
                                                 "If you are not running another file, you can continue.\n\n",
                                                 "Continue?"))
    if(force_continue == 2) stop("LagKUBE() stopped due to paralell processing. Wait until the other file is done and start again")
  }
  if(continue) fs::file_create(file)
}

#' @keywords internal
#' @noRd
get_lagkube_guardfile_path <- function(){
  file.path(fs::path_home(), getOption("khfunctions.lagkube_guardfile"))
}

#' @keywords internal
#' @noRd
lagkube_cleanup <- function(){
  fs::file_delete(get_lagkube_guardfile_path())
  RODBC::odbcCloseAll()
  sink()
}

#' @keywords internal
#' @noRd
remove_original_files_from_buffer <- function(){
  .GlobalEnv$BUFFER <- NULL
  gc()
}

#' LagKubeDatertCsv
#' Wrapper around LagKUBE, with default options to save output files
LagKubeDatertCsv <- function(name, write = TRUE, alarm = FALSE, geonaboprikk = TRUE, dumps = list()){ 
  invisible(LagKUBE(name = name, write = write, alarm = alarm, geonaboprikk = geonaboprikk, dumps = dumps))
}
