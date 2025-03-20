#' @title LagKUBE
#' @description 
#' The main function of the production line, producing the files going to FHI Statistikk and public health profiles
#'
#' @param KUBEid Name of kube, corresponding to KUBE_NAVN in ACCESS
#' @param versjonert 
#' @param csvcopy Save a CSV-copy?
#' @param globs global parameters, defaults to SettGlobs
#' @param dumps list of required dumps
#' @param write should results be written to files, default = TRUE. Set to FALSE for testing (only save to global envir)
#' @param alarm if TRUE, plays a sound when done
#' @param geonaboprikk  should the file be secondary censored on geographical codes? default = TRUE
#' @param ... 
#' @return complete data file, publication ready file, and quality control file.
LagKUBE <- function(KUBEid, versjonert = FALSE, csvcopy = FALSE, dumps = list(),write = FALSE, alarm = FALSE, geonaboprikk = TRUE, ...) {
  check_if_lagkube_available()
  on.exit(lagkube_cleanup(), add = TRUE)
  batchdate <- SettKHBatchDate()
  globs <- SettGlobs()
  sink(file = file.path(getOption("khfunctions.root"), getOption("khfunctions.dumpdir"), paste0("KUBELOGG/", KUBEid, "_", batchdate, "_LOGG.txt")), split = TRUE)
  if(!geonaboprikk) message("OBS! GEO-naboprikking er deaktivert!")
  
  parameters <- get_cubeparameters(KUBEid = KUBEid, batchdate = batchdate, globs = globs)
  load_and_format_files(parameters, batchdate = batchdate, versjonert = versjonert, globs = globs)
  parameters[["filedesign"]] <- get_filedesign(parameters = parameters, globs = globs)
  parameters[["PredFilter"]] <- set_predictionfilter(parameters = parameters, globs = globs)
  save_kubespec_csv(spec = parameters$CUBEinformation)
  if(write) save_access_specs(KUBEid = KUBEid, parameterlist = parameters, batchdate = batchdate, globs = globs)
  
  TNF <- merge_teller_nevner(parameters = parameters, globs = globs)
  KUBE <- TNF$TNF
  if(parameters$TNPinformation$NEVNERKOL != "-") KUBE <- LeggTilNyeVerdiKolonner(KUBE, NYEdscr = "RATE={TELLER/NEVNER}", postMA = FALSE)
  organize_file_for_moving_average(dt = KUBE)
  parameters[["MOVAVparameters"]] <- get_movav_information(dt = KUBE, parameters = parameters)
  KUBE <- aggregate_to_periods(dt = KUBE, setrate = TRUE, parameters = parameters, globs = globs)
  parameters[["CUBEdesign"]] <- update_cubedesign_after_moving_average(dt = KUBE, origdesign = TNF$KUBEd$MAIN, parameters = parameters)
  
  KUBE <- add_predteller(dt = KUBE, parameters = parameters, globs = globs)
  KUBE <- add_meisskala(dt = KUBE, parameters = parameters, globs = globs)
  if("raaKUBE1" %in% names(dumps)) DumpTabell(KUBE, paste0(KUBEid, "_raaKUBE1"), globs = globs, format = dumps[["raaKUBE1"]])
  remove_original_files_from_buffer()
  
  KUBE <- scale_rate_and_meisskala(dt = KUBE, parameters = parameters)
  fix_geo_special(d = KUBE, specs = parameters$fileinformation[[parameters$files$TELLER]], id = KUBEid)
  if ("maKUBE0" %in% names(dumps)) DumpTabell(KUBE, paste0(KUBEid, "_maKUBE0"), globs = globs, format = dumps[["maKUBE0"]])

  KUBE <- do_censor_kube_r(dt = KUBE, parameters = parameters, globs = globs)
  if ("KUBE_SLUTTREDIGERpre" %in% names(dumps)) DumpTabell(KUBE, paste0(KUBEid, "_KUBE_SLUTTREDIGERpre"), globs = globs, format = dumps[["KUBE_SLUTTREDIGERpre"]])
  KUBE <- do_special_handling(dt = KUBE, code = parameters$CUBEinformation$SLUTTREDIGER, batchdate = batchdate, stata_exe = globs$StataExe)
  if ("KUBE_SLUTTREDIGERpost" %in% names(dumps)) DumpTabell(KUBE, paste0(KUBEid, "_KUBE_SLUTTREDIGERpost"), globs = globs, format = dumps[["KUBE_SLUTTREDIGERpost"]])
  
  parameters[["MALTALL"]] <- get_maltall_column(parameters = parameters)
  KUBE <- do_format_cube_columns(dt = KUBE, parameters = parameters)
  KUBE <- add_smr_and_meis(dt = KUBE, parameters = parameters)
  KUBE <- adjust_smr_and_meis_to_country_normal(dt = KUBE, parameters = parameters, globs = globs)
  KUBE <- filter_invalid_geo_alder_kjonn(dt = KUBE, globs = globs)
  etabs <- get_etabs(columnnames = names(KUBE), parameters = parameters)
  KUBE <- set_etab_names(dt = KUBE, etablist = etabs)
  outvalues <- get_outvalues_allvis(parameters = parameters)
  outdimensions <- get_outdimensions(dt = KUBE, etabs = etabs$tabnames, parameters = parameters)
  
  if ("STATAPRIKKpre" %in% names(dumps)) DumpTabell(KUBE, paste0(KUBEid, "_STATAPRIKKpre"), globs = globs, format = dumps[["STATAPRIKKpre"]])
  dims <- find_dims_for_stataprikk(dt = KUBE, etabs = etabs)
  save_kubespec_csv(spec = parameters$CUBEinformation, dims = dims, geonaboprikk = geonaboprikk, geoprikktriangel = get_geonaboprikk_triangles())
  KUBE <- do_stata_censoring(dt = KUBE, spc = parameters$CUBEinformation, batchdate = batchdate, stata_exe = globs$StataExe)
  if ("STATAPRIKKpost" %in% names(dumps)) DumpTabell(KUBE, paste0(KUBEid, "_STATAPRIKKpost"), globs = globs, format = dumps[["STATAPRIKKpost"]])
  KUBE <- do_special_handling(dt = KUBE, code = parameters$CUBEinformation$RSYNT_POSTPROSESS, batchdate = batchdate, stata_exe = globs$StataExe)
  if ("RSYNT_POSTPROSESSpost" %in% names(dumps)) DumpTabell(KUBE, paste0(KUBEid, "_RSYNT_POSTPROSESSpost"), globs = globs, format = dumps[["RSYNT_POSTPROSESSpost"]])
  
  ALLVIS <- data.table::copy(KUBE)
  ALLVIS <- do_remove_censored_observations(dt = ALLVIS, outvalues = outvalues)
  if(isTRUE(write)) LagAlleFriskvikIndikatorerForKube(KUBEid = KUBEid, KUBE = ALLVIS, FGP = parameters$fileinformation[[parameters$files$TELLER]], modus = parameters$CUBEinformation$MODUS, batchdate = batchdate, globs = globs)
  ALLVIS <- ALLVIS[, c(..outdimensions, ..outvalues, "SPVFLAGG")]
  QC <- LagQCKube(allvis = ALLVIS, allvistabs = outdimensions, kube = KUBE)
  RESULTAT <<- list(KUBE = KUBE, ALLVIS = ALLVIS, QC = QC)
  if(isTRUE(write)) save_cube_output(outputlist = RESULTAT, KUBEid = KUBEid, batchdate = batchdate, versjonert = versjonert, geonaboprikk = geonaboprikk)
  cat("-------------------------KUBE", KUBEid, "FERDIG--------------------------------------\n")
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

#' @noRd
get_lagkube_guardfile_path <- function(){
  file.path(fs::path_home(), getOption("khfunctions.lagkube_guardfile"))
}

#' @noRd
lagkube_cleanup <- function(){
  fs::file_delete(get_lagkube_guardfile_path())
  RODBC::odbcCloseAll()
  sink()
}

#' @noRd
remove_original_files_from_buffer <- function(){
  .GlobalEnv$BUFFER <- NULL
  gc()
}

#' LagKubeDatertCsv
#' Wrapper around LagKUBE, with default options to save output files
LagKubeDatertCsv <- function(KUBEID, 
                             dumps = list(), 
                             versjonert = TRUE,
                             csvcopy = TRUE,
                             write = TRUE,
                             alarm = FALSE) {
  invisible(LagKUBE(KUBEid = KUBEID, versjonert = versjonert, csvcopy = csvcopy, dumps = dumps, write = write, alarm = alarm))
}

#' LagFlereKuber
#' Wrapper aroung LagKUBE, allowing for more than one KUBE to be made simultaneously
#' @param KUBEid_ALLE
#' @param ... Optional, can set versjonert, csvcopy, write arguments if TRUE not wanted
LagFlereKuber <- function(KUBEid_ALLE, 
                          dumps = list(), 
                          alarm = FALSE,
                          ...) {
  for (KUBEid in KUBEid_ALLE) {
    LagKubeDatertCsv(KUBEid, dumps = dumps, alarm = alarm, ...)
  }
  sink()
}
