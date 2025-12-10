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
#' @param removebuffer should original files in the buffer be removed when no longer needed to free memory?
#' @param qualcontrol perform initial qualcontrol of data (default = FALSE for now)
#' @return complete data file, publication ready file, and quality control file.
#' @export 
LagKUBE <- function(name, write = TRUE, alarm = FALSE, geonaboprikk = TRUE, year = getOption("khfunctions.year"), dumps = list(), removebuffer = TRUE, qualcontrol = TRUE) {
  on.exit(lagkube_cleanup(parameters = parameters), add = TRUE)
  check_connection_folders()
  check_if_lagkube_available()
  user_args <- as.list(environment())
  parameters <- get_cubeparameters(user_args = user_args)
  parameters[["old_locale"]] <- ensure_utf8_encoding()
  if(parameters$write) sink(file = file.path(getOption("khfunctions.root"), getOption("khfunctions.kubedir"), getOption("khfunctions.kube.logg"), paste0(parameters$name, "_", parameters$batchdate, "_LOGG.txt")), split = TRUE)
  if(!parameters$geonaboprikk) message("OBS! GEO-naboprikking er deaktivert!")
  # For dev and debug: use SetKubeParameters("NAME") and run step by step below

  parameters[["duck"]] <- init_duckdb(dbname = "kubeduck")
  load_and_format_files(parameters = parameters)
  parameters[["filedesign"]] <- get_filedesign(parameters = parameters)
  parameters[["PredFilter"]] <- set_predictionfilter(parameters = parameters)
  save_kubespec_csv(spec = parameters$CUBEinformation)
  write_access_specs(parameters = parameters)
  
  TNF <- merge_teller_nevner(parameters = parameters)
  KUBE <- TNF$TNF
  organize_file_for_moving_average(dt = KUBE)
  parameters[["MOVAVparameters"]] <- get_movav_information(dt = KUBE, parameters = parameters)
  KUBE <- aggregate_to_periods(dt = KUBE, parameters = parameters)
  add_crude_rate(dt = KUBE, parameters = parameters)
  parameters[["CUBEdesign"]] <- update_cubedesign_after_moving_average(dt = KUBE, origdesign = TNF$KUBEd$MAIN, parameters = parameters)
  
  KUBE <- add_predteller(dt = KUBE, parameters = parameters)
  KUBE <- add_meisskala(dt = KUBE, parameters = parameters)
  if(parameters$removebuffer) remove_original_files_from_buffer()
  KUBE <- scale_rate_and_meisskala(dt = KUBE, parameters = parameters)
  KUBE <- fix_geo_special(dt = KUBE, parameters = parameters)

  parameters[["MALTALL"]] <- get_maltall_column(parameters = parameters)
  KUBE <- do_format_cube_columns(dt = KUBE, parameters = parameters)
  KUBE <- add_smr_and_meis(dt = KUBE, parameters = parameters)
  KUBE <- adjust_smr_and_meis_to_country_normal(dt = KUBE, parameters = parameters)
  KUBE <- filter_invalid_geo_alder_kjonn(dt = KUBE, parameters = parameters)
  parameters[["etabs"]] <- get_etabs(columnnames = names(KUBE), parameters = parameters)
  KUBE <- set_etab_names(dt = KUBE, etablist = parameters$etabs)
  parameters[["outvalues"]] <- get_outvalues_allvis(parameters = parameters)
  parameters[["outdimensions"]] <- get_outdimensions(dt = KUBE, etabs = parameters$etabs$tabnames, parameters = parameters)
  
  KUBE <- do_censor_cube(dt = KUBE, parameters = parameters)
  KUBE <- do_special_handling(name = "RSYNT_POSTPROSESS", dt = KUBE, code = parameters$CUBEinformation$RSYNT_POSTPROSESS, parameters = parameters)
  KUBE <- do_special_handling(name = "SLUTTREDIGER", dt = KUBE, code = parameters$CUBEinformation$SLUTTREDIGER, parameters = parameters)
  
  ALLVIS <- data.table::copy(KUBE)
  ALLVIS <- do_remove_censored_observations(dt = ALLVIS, outvalues = parameters$outvalues)
  generate_and_export_all_friskvik_indicators(dt = ALLVIS, parameters = parameters)
  ALLVIS <- ALLVIS[, .SD, .SDcols = c(parameters$outdimensions, parameters$outvalues, "SPVFLAGG")]
  QC <- LagQCKube(allvis = ALLVIS, allvistabs = parameters$outdimensions, kube = KUBE)

  ALLVIS <- do_special_handling(name = "ALLVISFILTER", dt = ALLVIS, code = parameters$CUBEinformation$ALLVISFILTER, parameters = parameters)
  RESULTAT <<- list(KUBE = KUBE, ALLVIS = ALLVIS, QC = QC)
  write_cube_output(outputlist = RESULTAT, parameters = parameters)
  if(parameters$qualcontrol) control_cube_output(outputlist = RESULTAT, parameters = parameters)
  cat("\n\n-------------------------KUBE", parameters$name, "FERDIG--------------------------------------")
  cat("\nSe output med RESULTAT$KUBE (full), RESULTAT$ALLVIS (utfil) eller RESULTAT$QC (kvalkont)")
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
lagkube_cleanup <- function(parameters){
  fs::file_delete(get_lagkube_guardfile_path())
  if(parameters$write) sink()
  if(parameters$old_locale != "nb-NO.UTF-8") Sys.setlocale("LC_ALL", parameters$old_locale)
  DBI::dbDisconnect(parameters$duck)
  fs::file_delete(DBI::dbGetInfo(parameters$duck)$dbname)
  RODBC::odbcCloseAll()
}

#' @keywords internal
#' @noRd
remove_original_files_from_buffer <- function(){
  .GlobalEnv$BUFFER <- NULL
  gc()
}

#' LagKubeDatertCsv
#' Wrapper around LagKUBE, with default options to save output files
#' @export
LagKubeDatertCsv <- function(name, write = TRUE, alarm = FALSE, geonaboprikk = TRUE, dumps = list()){ 
  invisible(LagKUBE(name = name, write = write, alarm = alarm, geonaboprikk = geonaboprikk, dumps = dumps))
}
