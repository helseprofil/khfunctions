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
  parameters <- get_filegroup_parameters(user_args = user_args)
  # For dev and debug: use SetFilgruppeParameters("NAME") and run step by step below
  if(parameters$write) sink(file = file.path(getOption("khfunctions.root"), getOption("khfunctions.fgdir"), getOption("khfunctions.fg.logg"), paste0(parameters$name, "_", parameters$batchdate, "_LOGG.txt")), split = TRUE)
  if(parameters$n_files == 0) stop("Ingen originalfiler funnet, filgruppe kan ikke genereres. Sjekk at staving matcher for alle relevante felter i ACCESS")
  filegroup_check_original_files_and_spec(parameters = parameters)
  
  codebooklog <- initiate_codebooklog(nrow = 0)
  print_console_message("\n\n* Starter lesing, formattering og stabling av originalfiler\n-----")
  if(parameters$n_files == 1){
    make_table_from_original_file(file_number = 1, codebooklog = codebooklog, parameters = parameters)
  } else {
    for(file_number in 1:parameters$n_files){ # (For dev, can set file_number in e.g 1:3)
      make_table_from_original_file(file_number = file_number, codebooklog = codebooklog, parameters = parameters)
    }
  }
  clean_tempfiles(con = parameters$duck)
  do_clean_duckdb(con = parameters$duck)
  print_console_message("-----\n* Alle originalfiler lest og stablet")
  if(parameters$write) write_codebooklog(log = codebooklog, parameters = parameters)
  cleanlog <- initiate_cleanlog_db(codebooklog = codebooklog, parameters = parameters)
  clean_filegroup_dimensions_duckdb(parameters = parameters, cleanlog = cleanlog)
  clean_filegroup_values_duckdb(parameters = parameters, cleanlog = cleanlog)
  
  if(parameters$write) write_cleanlog(log = cleanlog, parameters = parameters)
  print_console_message("\n-----\n* Alle dimensjoner og verdikolonner vasket")
  rename_fg_value_columns_duckdb(parameters = parameters)
  set_integer_columns_duckdb(con = parameters$duck)
  
  do_special_handling(name = "RSYNT_PRE_FGLAGRING", dt = NULL, dt_name = "Filgruppe", 
                      code = parameters$filegroup_information$RSYNT_PRE_FGLAGRING, 
                      parameters = parameters, duck = TRUE, tablename = "FILGRUPPE")
  
  write_filegroup_output(dt = Filgruppe, parameters = parameters)
  if(parameters$qualcontrol) control_fg_output(outputlist = RESULTAT)

  print_console_message("\n\n-------------------------FILGRUPPE", parameters$name, "FERDIG--------------------------------------")
  print_console_message("\nSe output med RESULTAT$Filgruppe, RESULTAT$cleanlog (rensing av kolonner) eller RESULTAT$codebooklog (omkodingslogg)")
}

lagfilgruppe_cleanup <- function(parameters){
  if(parameters$write) sink()
  if(parameters$old_locale != "nb-NO.UTF-8") Sys.setlocale("LC_ALL", parameters$old_locale)
  RODBC::odbcCloseAll()
  if(exists("org_geo_codes", envir = .GlobalEnv)) rm(org_geo_codes, envir = .GlobalEnv)
  if(!is.null(parameters$duck)){
    DBI::dbDisconnect(parameters$duck)
    fs::file_delete(DBI::dbGetInfo(parameters$duck)$dbname)
  }
  if(!is.null(parameters$threads)){
    data.table::setDTthreads(parameters$threads$dt)
    collapse::set_collapse(nthreads = parameters$threads$collapse)
  }
}

#' @title initiate_cleanlog
#' @description
#' Initiates log for filegroup cleaning
#' @noRd
initiate_cleanlog_db <- function(codebooklog, parameters){
  if(!"FILGRUPPE" %in% DBI::dbListTables(parameters$duck)) stop("FILGRUPPE finnes ikke i duckdb, kan ikke initiere cleanlog")
  koblids <- as.character(DBI::dbGetQuery(parameters$duck, "SELECT DISTINCT KOBLID FROM FILGRUPPE;"))
  log <- parameters$read_parameters[KOBLID %in% koblids, .SD, .SDcols = c("KOBLID", "DELID")][, KOBLID := as.character(KOBLID)]
  n_rows <- data.table::setDT(DBI::dbGetQuery(parameters$duck, "SELECT KOBLID, COUNT(*) AS n_rows FROM FILGRUPPE GROUP BY KOBLID;"))
  log <- collapse::join(log, n_rows, on = "KOBLID", verbose = 0)
  n_recoded <- codebooklog[, .(N_values_recoded = sum(as.numeric(FREQ), na.rm = T)), by = KOBLID]
  log <- collapse::join(log, n_recoded, on = "KOBLID", verbose = 0)
  n_deleted <- codebooklog[OMK == "-", .(N_rows_deleted = sum(as.numeric(FREQ), na.rm = T)), by = KOBLID]
  log <- collapse::join(log, n_deleted, on = "KOBLID", verbose = 0)
  data.table::setnafill(log, fill = 0, cols = names(log)[sapply(log, is.numeric)])
  return(log)
}

rename_fg_value_columns_duckdb <- function(parameters){
  con <- parameters$duck
  vals <- intersect(c("VAL1", "VAL2", "VAL3"), DBI::dbListFields(con, "FILGRUPPE"))
  valnames <- as.character(parameters$filegroup_information[paste0(vals, "navn")])
  
  rename_map <- data.table::data.table(
    old = c(vals, paste0(vals, ".a"), paste0(vals, ".f")),
    new = c(valnames, paste0(valnames, ".a"), paste0(valnames, ".f"))
  )
  
  sql <- paste0(sprintf(
    'ALTER TABLE FILGRUPPE RENAME COLUMN "%s" TO "%s"',
    rename_map$old,
    rename_map$new
  ),
  collapse = ";\n"
  )
  
  invisible(DBI::dbExecute(con, sql))
}
