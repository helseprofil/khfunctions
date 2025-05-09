#' @title LagFilgruppe 
#' @description
#' Loops over all original files, creates a table and append to the complete file group 
#'
#' @param gruppe 
#' @param printR 
#' @param printSTATA 
#' @param versjonert 
#' @param dumps 
#' @param localDir 
LagFilgruppe <- function(gruppe, versjonert = TRUE, write = TRUE, dumps = list()) {
  on.exit(lagfilgruppe_cleanup(), add = TRUE)
  parameters <- get_filegroup_parameters(name = gruppe, versjonert = versjonert, dumps = dumps)
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
  write_codebooklog(log = codebooklog, parameters = parameters, write = write)
  
  # Clean dimension columns
  cleanlog <- initiate_cleanlog(dt = Filgruppe, codebooklog = codebooklog)
  Filgruppe <- clean_filegroup_dimensions(dt = Filgruppe, parameters = parameters, cleanlog = cleanlog)
  Filgruppe <- clean_filegroup_values(dt = Filgruppe, parameters = parameters, cleanlog = cleanlog)
  analyze_cleanlog(log = cleanlog)
  do_set_value_names(dt = Filgruppe, parameters = parameters)
  cat("-----\n* Alle dimensjoner og verdikolonner vasket og ok")
  remove_helper_columns(dt = Filgruppe)
  
  if ("RSYNT_PRE_FGLAGRINGpre" %in% names(dumps)) DumpTabell(Filgruppe, paste(filbesk$FILGRUPPE, "RSYNT_PRE_FGLAGRINGpre", sep = "_"), format = dumps[["RSYNT_PRE_FGLAGRINGpre"]])
  Filgruppe <- do_special_handling(dt = Filgruppe, code = parameters$filegroup_information$RSYNT_PRE_FGLAGRING, batchdate = parameters$batchdate, stata_exe = parameters$StataExe, DTout = T)
  if ("RSYNT_PRE_FGLAGRINGpost" %in% names(dumps)) DumpTabell(Filgruppe, paste(filbesk$FILGRUPPE, "RSYNT_PRE_FGLAGRINGpost", sep = "_"), format = dumps[["RSYNT_PRE_FGLAGRINGpost"]])
  
  # DEV: KAN GEOHARMONISERING SKJE HER?? Må I SåFALL OMKODE GEO OG AGGREGERE FILGRUPPEN
  
  if(write) write_filegroup_output(outfile = Filgruppe, name = gruppe, versjonert = versjonert, batchdate = parameters$batchdate)
  RESULTAT <<- list(Filgruppe = Filgruppe, cleanlog = cleanlog, codebooklog = codebooklog)
}

lagfilgruppe_cleanup <- function(){
  RODBC::odbcCloseAll()
}

#' Bør implementeres i lagfilgruppe_cleanup for å rydde opp etter kjøring, men da må først all kodeboklogg med SV = 'S' fjernes først
delete_old_filegroup_log <- function(filegroup, parameters){
  RODBC::sqlQuery(parameters$log, paste0("DELETE * FROM KODEBOK_LOGG WHERE FILGRUPPE='", filegroup, "' AND SV='S'"))
  RODBC::sqlQuery(parameters$log, paste0("DELETE * FROM INNLES_LOGG WHERE FILGRUPPE='", filegroup, "' AND SV='S'"))
  return(invisible(NULL))
}

#' @title initiate_cleanlog
#' @description
#' Initiates log for filegroup cleaning
#' @noRd
initiate_cleanlog <- function(dt, codebooklog){
  log <- dt[, .(N_rows = .N), by = KOBLID]
  n_recoded <- codebooklog[, .(N = sum(as.numeric(FREQ), na.rm = T)), by = KOBLID]
  n_deleted <- codebooklog[OMK == "-", .(N = sum(as.numeric(FREQ), na.rm = T)), by = KOBLID]
  log[n_recoded, on = "KOBLID", N_values_recoded := i.N]
  log[n_deleted, on = "KOBLID", N_rows_deleted := i.N]
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

#' @title write_codebooklog
#' @noRd
write_codebooklog <- function(log, parameters, write){
  log[, let(FILGRUPPE = parameters$filegroup_name, BATCHDATE = parameters$batchdate, SV = "S", OK = 1)]
  data.table::setcolorder(log, c("KOBLID", "FILGRUPPE", "DELID", "FELTTYPE", "ORG", "KBOMK", "OMK", "FREQ", "SV", "BATCHDATE", "OK")) # sett som options
  
  if(!write) return(invisible(NULL))
  cat("\n* Skriver kodebok-logg")
  # KODE FOR Å TØME GAMMEL LOGG
  # KODE FOR Å SKRIVE NY LOGG
}

#' @title remove_helper_columns
#' @noRd
remove_helper_columns <- function(dt){
  helpers <- c("LEVEL")
  helpers <- helpers[helpers %in% names(dt)]
  dt[, (helpers) := NULL]
}
