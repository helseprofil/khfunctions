#' @title LagFilgruppe 
#' @description
#' Loops over all original files, creates a table and append to the complete file group 
#'
#' @param gruppe name of filegroup
#' @param write save output files? default = TRUE
#' @param dumps list of intermediate files to save, used for debugging and development. 
#' @export
LagFilgruppe <- function(name, write = TRUE, dumps = list()) {
  on.exit(lagfilgruppe_cleanup(), add = TRUE)
  check_connection_folders()
  user_args = as.list(environment())
  parameters <- get_filegroup_parameters(user_args = user_args)
  if(parameters$n_files == 0) stop("Ingen originalfiler funnet, filgruppe kan ikke genereres")
  filegroup_check_original_files_and_spec(parameters = parameters)
  
  Filgruppe <- data.table::data.table()
  codebooklog <- initiate_codebooklog(nrow = 0)
  cat("\n\n* Starter lesing, formattering og stabling av originalfiler\n-----")
  for(file_number in 1:parameters$n_files){
    new_file <- make_table_from_original_file(file_number = file_number, codebooklog = codebooklog, parameters = parameters)
    Filgruppe <- data.table::rbindlist(list(Filgruppe, new_file), fill = T)
    cat("\n* Fil stablet, antall rader nå: ", nrow(Filgruppe), "\n")
  }
  cat("-----\n* Alle originalfiler lest og stablet")
  
  cleanlog <- initiate_cleanlog(dt = Filgruppe, codebooklog = codebooklog, parameters = parameters)
  Filgruppe <- clean_filegroup_dimensions(dt = Filgruppe, parameters = parameters, cleanlog = cleanlog)
  Filgruppe <- clean_filegroup_values(dt = Filgruppe, parameters = parameters, cleanlog = cleanlog)
  analyze_cleanlog(log = cleanlog)
  
  cat("\n-----\n* Alle dimensjoner og verdikolonner vasket og ok")
  do_set_fg_column_order(dt = Filgruppe)
  do_set_fg_value_names(dt = Filgruppe, parameters = parameters)
  remove_helper_columns(dt = Filgruppe)
  Filgruppe <- do_special_handling(name = "RSYNT_PRE_FGLAGRING", dt = Filgruppe, code = parameters$filegroup_information$RSYNT_PRE_FGLAGRING, parameters = parameters)
  set_integer_columns(dt = Filgruppe)
  
  # DEV: KAN GEOHARMONISERING SKJE HER?? Må I SåFALL OMKODE GEO OG AGGREGERE FILGRUPPEN
  RESULTAT <- list(Filgruppe = Filgruppe, cleanlog = cleanlog, codebooklog = codebooklog)
  write_filegroup_output(outputs = RESULTAT, parameters = parameters)
  RESULTAT <<- RESULTAT
  cat("-------------------------FILGRUPPE", parameters$name, "FERDIG--------------------------------------\n")
  cat("Se output med RESULTAT$Filgruppe, RESULTAT$cleanlog (rensing av kolonner) eller RESULTAT$codebooklog (omkodingslogg)")
}

lagfilgruppe_cleanup <- function(){
  RODBC::odbcCloseAll()
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
  n_deleted <- codebooklog[KBOMK == "-", .(N_rows_deleted = sum(as.numeric(FREQ), na.rm = T)), by = KOBLID]
  log <- collapse::join(log, n_deleted, on = "KOBLID", verbose = 0)
  data.table::setnafill(log, fill = 0, cols = names(log)[sapply(log, is.numeric)])
  return(log)
}

#' @title analyze_cleanlog
#' @description
#' Analyzes log after cleaning dimensions and values, and stops data processing 
#' if any invalid values are found and prints a report. 
#' @noRd
analyze_cleanlog <- function(log){
  ok_cols <- grep("_ok$", names(log), value = T)
  any_not_ok <- log[rowSums(log[, .SD, .SDcols = ok_cols] == 0) > 0]
  if(nrow(any_not_ok) == 0) return(invisible(NULL))
  
  not_ok_cols <- ok_cols[sapply(any_not_ok[, .SD, .SDcols = ok_cols], function(col) any(col == 0))]
  out <- any_not_ok[, .SD, .SDcols = c("KOBLID", not_ok_cols)]
  cat("\n***** OBS! Feil funnet\n-----")
  cat("\nTabellen viser hvilke filer og hvilke kolonner det er funnet feil i\n-----\n")
  print(out)
  cat("\n-----\n")
  stop("Kolonnene vist i tabellen over med verdi = 0 må ordnes i kodebok")
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
