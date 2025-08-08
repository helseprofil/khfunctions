#' @title LagFilgruppe 
#' @description
#' Loops over all original files, creates a table and append to the complete file group 
#'
#' @param gruppe name of filegroup
#' @param write save output files? default = TRUE
#' @param dumps list of intermediate files to save, used for debugging and development. 
#' @export
LagFilgruppe <- function(name, write = TRUE, dumps = list(), qualcontrol = TRUE) {
  on.exit(lagfilgruppe_cleanup(parameters = parameters), add = TRUE)
  check_connection_folders()
  user_args = as.list(environment())
  # For dev and debug: use SetFilgruppeParameters("NAME") and run step by step below
  parameters <- get_filegroup_parameters(user_args = user_args)
  if(parameters$write) sink(file = file.path(getOption("khfunctions.root"), getOption("khfunctions.fgdir"), getOption("khfunctions.fg.logg"), paste0(parameters$name, "_", parameters$batchdate, "_LOGG.txt")), split = TRUE)
  if(parameters$n_files == 0) stop("Ingen originalfiler funnet, filgruppe kan ikke genereres. Sjekk at staving matcher for alle relevante felter i ACCESS")
  filegroup_check_original_files_and_spec(parameters = parameters)
  
  Filgruppe <- data.table::data.table()
  codebooklog <- initiate_codebooklog(nrow = 0)
  cat("\n\n* Starter lesing, formattering og stabling av originalfiler\n-----")
  # For dev and debug: set file_number = the file you want to test
  for(file_number in 1:parameters$n_files){ # (For dev, can set file_number in e.g 1:3)
    new_file <- make_table_from_original_file(file_number = file_number, codebooklog = codebooklog, parameters = parameters)
    Filgruppe <- data.table::rbindlist(list(Filgruppe, new_file), fill = T)
    cat("\n* Fil stablet, antall rader nå: ", nrow(Filgruppe), "\n")
  }
  cat("-----\n* Alle originalfiler lest og stablet")
  if(parameters$write) write_codebooklog(log = codebooklog, parameters = parameters)
  
  cleanlog <- initiate_cleanlog(dt = Filgruppe, codebooklog = codebooklog, parameters = parameters)
  Filgruppe <- clean_filegroup_dimensions(dt = Filgruppe, parameters = parameters, cleanlog = cleanlog)
  Filgruppe <- clean_filegroup_values(dt = Filgruppe, parameters = parameters, cleanlog = cleanlog)
  if(parameters$write) write_cleanlog(log = cleanlog, parameters = parameters)
  cat("\n-----\n* Alle dimensjoner og verdikolonner vasket")
  
  do_set_fg_column_order(dt = Filgruppe)
  do_set_fg_value_names(dt = Filgruppe, parameters = parameters)
  remove_helper_columns(dt = Filgruppe)
  set_integer_columns(dt = Filgruppe)
  Filgruppe <- do_special_handling(name = "RSYNT_PRE_FGLAGRING", dt = Filgruppe, code = parameters$filegroup_information$RSYNT_PRE_FGLAGRING, parameters = parameters)
  
  # DEV: KAN GEOHARMONISERING SKJE HER?? Må I SåFALL OMKODE GEO OG AGGREGERE FILGRUPPEN
  RESULTAT <<- list(Filgruppe = Filgruppe, cleanlog = cleanlog, codebooklog = codebooklog)
  write_filegroup_output(dt = Filgruppe, parameters = parameters)
  if(parameters$qualcontrol) control_fg_output(outputlist = RESULTAT)

  cat("-------------------------FILGRUPPE", parameters$name, "FERDIG--------------------------------------\n")
  cat("Se output med RESULTAT$Filgruppe, RESULTAT$cleanlog (rensing av kolonner) eller RESULTAT$codebooklog (omkodingslogg)")
}

lagfilgruppe_cleanup <- function(parameters){
  if(parameters$write) sink()
  RODBC::odbcCloseAll()
  if(exists(.GlobalEnv$org_geo_codes)) rm(.GlobalEnv$org_geo_codes)
}

#' @title delete_old_filegroup_log
#' Bør implementeres i lagfilgruppe_cleanup for å rydde opp etter kjøring, men da må først all kodeboklogg med SV = 'S' fjernes først
#' @noRd
delete_old_filegroup_log <- function(filegroup, parameters){
  RODBC::sqlQuery(parameters$log, paste0("DELETE * FROM KODEBOK_LOGG WHERE FILGRUPPE='", filegroup, "' AND SV='S'"))
  RODBC::sqlQuery(parameters$log, paste0("DELETE * FROM INNLES_LOGG WHERE FILGRUPPE='", filegroup, "' AND SV='S'"))
  return(invisible(NULL))
}

#' @title initiate_cleanlog
#' @description
#' Initiates log for filegroup cleaning
#' @noRd
initiate_cleanlog <- function(dt, codebooklog, parameters){
  log <- parameters$read_parameters[KOBLID %in% unique(dt$KOBLID), .SD, .SDcols = c("KOBLID", "DELID")][, KOBLID := as.character(KOBLID)]
  n_rows <- dt[, .(N_rows = .N), by = KOBLID]
  log <- collapse::join(log, n_rows, on = "KOBLID", verbose = 0)
  n_recoded <- codebooklog[, .(N_values_recoded = sum(as.numeric(FREQ), na.rm = T)), by = KOBLID]
  log <- collapse::join(log, n_recoded, on = "KOBLID", verbose = 0)
  n_deleted <- codebooklog[OMK == "-", .(N_rows_deleted = sum(as.numeric(FREQ), na.rm = T)), by = KOBLID]
  log <- collapse::join(log, n_deleted, on = "KOBLID", verbose = 0)
  data.table::setnafill(log, fill = 0, cols = names(log)[sapply(log, is.numeric)])
  return(log)
}

#' @keywords internal
#' @noRd
do_set_fg_column_order <- function(dt){
  colorder <- "GEO"
  dims <- c(grep("GEO", getOption("khfunctions.standarddimensions"), value = T, invert = T))
  for(i in c(dims, "TAB", "VAL", "GEOniv", "FYLKE", "KOBLID")){
    colorder <- c(colorder, (names(dt)[startsWith(names(dt), i)]))
  }
  data.table::setcolorder(dt, colorder)
}

#' @title remove_helper_columns
#' @noRd
remove_helper_columns <- function(dt){
  helpers <- c("LEVEL")
  helpers <- helpers[helpers %in% names(dt)]
  dt[, (helpers) := NULL]
}
