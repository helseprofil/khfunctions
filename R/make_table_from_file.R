#' @title make_table_from_original_file
#' @description
#' Read and formats an original file before stacking
make_table_from_original_file <- function(file_number, codebooklog, parameters){
  dumps = parameters$dumps
  report_filegroup_progress(file_number = file_number, parameters = parameters)
  clean_tempfiles(con = parameters$duck)
  
  filedescription <- parameters$read_parameters[file_number]
  filecolumns <- identify_columns_in_file(filedescription = filedescription)
  read_original_file(filedescription = filedescription, parameters = parameters, dumps = dumps)
  convert_duckdb_cols_to_string(con = parameters$duck, table_name = "temp_orgfile")
  set_manheader_db(manheader = filedescription$MANHEADER, con = parameters$duck)
  
  if(is_not_empty(filedescription$RSYNT1)){
    do_special_handling(name = "RSYNT1", dt = NULL, dt_name = "DF", code = filedescription$RSYNT1, 
                        parameters = parameters, duck = TRUE, tablename = "temp_orgfile", 
                        koblid = filedescription$KOBLID, filedescription = filedescription)
  }
  
  give_columns_default_names(filedescription = filedescription, defcolumns = filecolumns$have, con = parameters$duck)
  do_handle_fylltab(filedescription = filedescription, con = parameters$duck)
  do_handle_kastkols_db(kastkols = filedescription$KASTKOLS, con = parameters$duck)  
  do_reshape_var_db(filedescription = filedescription, con = parameters$duck)
  do_split_multihead(dt = DF, filedescription = filedescription, con = parameters$duck)
  
  if(is_not_empty(filedescription$RSYNT2)){
    do_special_handling(name = "RSYNT2", dt = DF, dt_name = "DF", code = filedescription$RSYNT2, 
                        parameters = parameters, duck = TRUE, tablename = "temp_orgfile",
                        koblid = filedescription$KOBLID)
  }
  
  give_columns_default_names(filedescription = filedescription, defcolumns = filecolumns$have, con = parameters$duck)
  do_set_default_values_db(filedescription = filedescription, defaultcolumns = filecolumns$default, con = parameters$duck)
  check_if_all_columns_exist(filecolumns = filecolumns, con = parameters$duck)
  drop_unwanted_columns_db(con = parameters$duck)
  # do_aggregate_if_grunnkrets(dt = DF, filedescription = filedescription, parameters = parameters) # DEPRECATED?
  do_convert_na_to_empty_db(con = parameters$duck)
  invisible(DBI::dbExecute(parameters$duck, paste0("ALTER TABLE temp_orgfile ADD COLUMN KOBLID VARCHAR DEFAULT ", filedescription$KOBLID)))
  recode_columns_with_codebook(dt = DF, filedescription = filedescription, parameters = parameters, codebooklog = codebooklog, dumps = dumps)
  do_recode_tknr_db(tknr = filedescription$TKNR, parameters = parameters)
  do_recode_soner_4_db(filedescription = filedescription, con = parameters$duck)
  
  append_temp_orgfil_to_filgruppe(con = parameters$duck)
  # add_table_to_fgduck(dt = DF, con = parameters$duck)
}

# Process file DUCKDB ----
set_manheader_db <- function(manheader, con){
  if(is_empty(manheader)) return(invisible(NULL))
  origcols <- DBI::dbListFields(con, "temp_orgfile")
  manheader_split <- trimws(unlist(strsplit(manheader, "=")))
  old <- manheader_to_vector(manheader_split[[1]], "old", origcols)
  new <- manheader_to_vector(manheader_split[[2]], "new", origcols)
  
  if(length(old) != length(new)) stop("Feil i MANHEADER: Ulikt antall kolonner angitt på hver side av '='")
  
  for(i in seq_along(old)){
    invisible(DBI::dbExecute(con,sprintf("ALTER TABLE temp_orgfile RENAME COLUMN %s TO %s",
                                         DBI::dbQuoteIdentifier(con, old[i]),
                                         DBI::dbQuoteIdentifier(con, new[i]))))
  }
}

manheader_to_vector <- function(string, old_new = c("old", "new"), origcols){
  if(grepl("^\\[", string)) string <- gsub("^\\[|\\]$", "", string)
  if(grepl("^c\\(", string)) string <- gsub("^c\\(|\\)$", "", string)
  if(grepl("\"", string)) string <- gsub("\"", "", string)
  colnames <- trimws(strsplit(string, ",")[[1]])
  if(old_new == "new") return(colnames)
  
  numeric <- suppressWarnings(as.numeric(colnames))
  if(all(!is.na(numeric))){
    if(min(numeric) <= 0 && max(numeric) > length(origcols)) stop("Feil i MANHEADER: Angitt kolonnenummer eksisterer ikke i filen")
    return(origcols[numeric])
  }
  if(all(is.na(numeric))){
    if(!all(colnames %in% origcols)) stop("Feil i MANHEADER: Minst ett angitt gammelt kolonnenavn [", paste(colnames, collapse = ", "), "] eksisterer ikke i filen")
    return(colnames)
  }
  stop("Feil i MANHEADER: både kolonnenavn og posisjon angitt som gamle kolonnenavn")
}
  
#' @title give_columns_default_name
#' @description
#' Renames columns to GEO, AAR, KJONN, ALDER, UTDANN, INNVKAT, LANDBAK, TAB1:3, VAL1:3
#' Use information from INNLESING to check which columns should be renamed to standard names. 
#' @noRd
give_columns_default_names <- function(filedescription, defcolumns, con){
  cols <- DBI::dbListFields(con, "temp_orgfile")
  rename <- setNames(as.character(filedescription[, ..defcolumns]), defcolumns)
  rename <- rename[rename != names(rename)]
  idx <- which(rename %in% cols)
  rename <- rename[idx]
  
  # Må sjekke om noen ønskede kolonnenavn allerede eksisterer, 
  # f.eks. om filen har kolonnene aar og AAR, og forsøker å endre aar til AAR
  collision <- names(rename) %in% cols
  
  if(any(collision)) {
    stop("Kan ikke gi standardkolonnenavn. Følgende målkolonner finnes allerede i filen: ",
         paste(names(rename)[collision], collapse = ", "))
  }
  
  old <- as.character(rename)
  new <- names(rename)
  
  for(i in seq_along(old)){
    invisible(DBI::dbExecute(con, sprintf("ALTER TABLE temp_orgfile RENAME COLUMN %s TO %s",
                                          DBI::dbQuoteIdentifier(con, old[i]),
                                          DBI::dbQuoteIdentifier(con, new[i]))))
  }
}

do_handle_kastkols_db <- function(kastkols, con){
  if(is_empty(kastkols)) return(invisible(NULL))
  
  remove <- gsub("^c\\(|\\)$", "", kastkols)
  remove <- as.integer(trimws(unlist(strsplit(remove, ","))))
  
  cols <- DBI::dbListFields(con, "temp_orgfile")
  
  if(any(remove < 1L | remove > length(cols))) stop("Feil i KASTKOLS: Angitt kolonnenummer eksisterer ikke i filen")
  
  cols_remove <- cols[remove]
  
  for(col in cols_remove){
    invisible(DBI::dbExecute(con, sprintf("ALTER TABLE temp_orgfile DROP COLUMN %s",
                                          DBI::dbQuoteIdentifier(con, col))))
  }
  invisible(NULL)
}

do_reshape_var_db <- function(filedescription, con){
  invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS temp_orgfile_reshape"))
  
  if(is_empty(filedescription$RESHAPEvar)) return(invisible(NULL))
  
  allcols <- DBI::dbListFields(con, "temp_orgfile")
  cols <- get_reshape_parameters(filedescription = filedescription, allcolumns = allcols)
  if(length(intersect(cols$id, cols$measure)) > 0) stop("Kolonne kan ikke være både RESHAPEid og RESHAPEmeas")
  if(cols$var %in% allcols) stop(sprintf("RESHAPEvar '%s' finnes allerede i datasettet", cols$var))
  if(cols$val %in% allcols) stop(sprintf("RESHAPEval '%s' finnes allerede i datasettet", cols$val))

  if(!is.null(cols$id) && !all(cols$id %in% allcols)) stop("Feil i RESHAPE: Kolonner angitt i RESHAPEid ikke funnet")
  if(!is.null(cols$measure) && !all(cols$measure %in% allcols)) stop("Feil i RESHAPE: Kolonner angitt i RESHAPEmeas ikke funnet")
  if(length(cols$measure) == 0) stop("Feil i RESHAPE: Både RESHAPEid og RESHAPEmeas er tomme")
  
  measure_sql <- paste(DBI::dbQuoteIdentifier(con, cols$measure), collapse = ", ")
  
  out_cols <- c(cols$id, cols$var, cols$val)
  if(anyDuplicated(out_cols) > 0) stop("RESHAPE genererer dublerte kolonnenavn")
  
  select_sql <- paste(
    sprintf(
      "\nCAST(%s AS VARCHAR) AS %s",
      DBI::dbQuoteIdentifier(con, out_cols),
      DBI::dbQuoteIdentifier(con, out_cols)
    ),
    collapse = ","
  )
  
  sql <- sprintf("CREATE OR REPLACE TABLE temp_orgfile_reshape AS SELECT %s FROM (SELECT * FROM temp_orgfile UNPIVOT (%s FOR %s IN (%s)))",
                 select_sql,
                 DBI::dbQuoteIdentifier(con, cols$val),
                 DBI::dbQuoteIdentifier(con, cols$var),
                 measure_sql
  )
  
  DBI::dbExecute(con, sql)
  DBI::dbExecute(con, "DROP TABLE IF EXISTS temp_orgfile")
  DBI::dbExecute(con, "ALTER TABLE temp_orgfile_reshape RENAME TO temp_orgfile")
  invisible(NULL)
}

#' @title get_reshape_parameters
#' @description
#' Identifies a list of columns needed for reshape
#' @noRd
get_reshape_parameters <- function(filedescription, allcolumns){
  id <- measure <- NULL
  if(is_not_empty(filedescription$RESHAPEid)) id <- gsub("\"", "", strsplit(filedescription$RESHAPEid, ",")[[1]])
  if(is_not_empty(filedescription$RESHAPEmeas)) measure <- gsub("\"", "", strsplit(filedescription$RESHAPEmeas, ",")[[1]])
  if(length(measure) == 0) measure <- setdiff(allcolumns, id)
  val <- ifelse(is_not_empty(filedescription$RESHAPEval), as.character(filedescription$RESHAPEval), "value")
  var <- data.table::fcoalesce(ifelse(is_not_empty(filedescription$MULTIHEAD), LesMultiHead(filedescription$MULTIHEAD)$varname, NA_character_),
                               ifelse(is_not_empty(filedescription$RESHAPEvar), as.character(filedescription$RESHAPEvar), NA_character_),
                               "variable")
  out <- list(id = id, measure = measure, var = var, val = val)
  out <- lapply(out, trimws)
  return(out)
}

#' @title do_set_default_values_db
#' @description setter default verdier for kolonner som ikke finnes i originalfil
#' @noRd
do_set_default_values_db <- function(filedescription, defaultcolumns, con){
  if(length(defaultcolumns) == 0) return(invisible(NULL))
  
  default <- filedescription[, ..defaultcolumns]
  default[, names(.SD) := lapply(.SD, function(x) sub("^<(.*)>$", "\\1", x))]
  existing_cols <- DBI::dbListFields(con, "temp_orgfile")
  cols_to_add <- setdiff(defaultcolumns, existing_cols)
  cols_to_update <- intersect(defaultcolumns, existing_cols)
  if(length(cols_to_add) > 0){
    for(col in cols_to_add){
      DBI::dbExecute(con, sprintf("ALTER TABLE temp_orgfile ADD COLUMN %s VARCHAR DEFAULT %s",
                                  as.character(DBI::dbQuoteIdentifier(con, col)),
                                  as.character(DBI::dbQuoteString(con, default[[col]][1]))))
    }
  }
  
  if(length(cols_to_update) > 0){
    set_clause <- paste(vapply(cols_to_update, function(col) {
      sprintf("%s = %s", 
              as.character(DBI::dbQuoteIdentifier(con, col)),
              as.character(DBI::dbQuoteString(con, default[[col]][1]))
          )
        },
        character(1)
      ),
      collapse = ", "
    )
    DBI::dbExecute(con, sprintf("UPDATE temp_orgfile SET %s", set_clause))
  }
  invisible(NULL)
}

#' @title drop_unwanted_columns_db
#' @description Fjerner kolonner som ikke trengs videre
#' @noRd
drop_unwanted_columns_db <- function(con){
  keep_cols <- c(getOption("khfunctions.kolorgs"), "LEVEL")
  existing_cols <- DBI::dbListFields(con, "temp_orgfile")
  cols_to_drop <- setdiff(existing_cols, keep_cols)
  if(length(cols_to_drop) == 0) return(invisible(NULL))
  
  for (col in cols_to_drop) {
    DBI::dbExecute(con, sprintf("ALTER TABLE temp_orgfile DROP COLUMN %s", as.character(DBI::dbQuoteIdentifier(con, col))))
  }
  invisible(NULL)
}

#' @title do_convert_na_to_empty_db
#' @description Erstatter manglende celler med "" (alle kolonner er tekst)
#' @noRd
do_convert_na_to_empty_db <- function(con) {
  cols <- DBI::dbListFields(con, "temp_orgfile")
  
  set_clause <- paste(sprintf("%s = COALESCE(%s, '')",
                              DBI::dbQuoteIdentifier(con, cols),
                              DBI::dbQuoteIdentifier(con, cols)),
                      collapse = ", ")
  
  DBI::dbExecute(con, sprintf("UPDATE temp_orgfile SET %s", set_clause))
  
  invisible(NULL)
}

#' @title check_if_all_columns_exist
#' @description
#' Checks if obligatory columns GEO, AAR, Val1, and values defined in  exists
#' @param dt data
#' @param filecolumns list of columns present in file and columns to be given default value
#' @noRd
check_if_all_columns_exist <- function(filecolumns, con){
  oblig <- c("GEO", "AAR", "VAL1")
  have <- filecolumns$have
  default <- filecolumns$default
  allcols <- DBI::dbListFields(con, "temp_orgfile")
  if(!all(oblig %in% allcols)) stop("Feil i innlesing: Kolonnene <", oblig[!(oblig %in% allcols)], "> finnes ikke\n")
  if(!all(have %in% allcols)) stop("Feil i innlesing: Kolonnene <", have[!(have %in% allcols)], "> finnes ikke\n")
  if(!all(default %in% allcols)) stop("Feil i innlesing: Kolonnene <", default[!(default %in% allcols)], "> skulle fått default verdi, men finnes ikke\n")
  return(invisible(NULL))
}

# Process file data.table ----




#' @title update_codebooklog
#' @description updates codebooklog by reference
#' @noRd
update_codebooklog <- function(codebooklog, recodelog){
  updated_codebooklog <- data.table::rbindlist(list(codebooklog, recodelog))
  codebooklog[, names(codebooklog) := NULL][, names(updated_codebooklog) := updated_codebooklog]
}

# READ/WRITE DUCKDB ----

#' @noRd
append_temp_orgfil_to_filgruppe <- function(con){
  
  if(!DBI::dbExistsTable(con, "temp_orgfile")) stop("temp_orgfile finnes ikke i duckdb")
  
  cols_orgfile <- DBI::dbListFields(con, "temp_orgfile")
  
  if(!DBI::dbExistsTable(con, "FILGRUPPE")) {
    DBI::dbExecute(con, "CREATE TABLE FILGRUPPE AS SELECT * FROM temp_orgfile")
    return(invisible(NULL))
  } else {
    cols_filgruppe <- DBI::dbListFields(con, "FILGRUPPE")
    missing_cols <- setdiff(cols_orgfile, cols_filgruppe)
    if(length(missing_cols) > 0) {
      for(col in missing_cols) {
        invisible(DBI::dbExecute(con, sprintf("ALTER TABLE FILGRUPPE ADD COLUMN %s VARCHAR default ''", 
                                              as.character(DBI::dbQuoteIdentifier(con, col)))))
      }
    }
  }
  cols_filgruppe <- DBI::dbListFields(con, "FILGRUPPE")
  missing_in_temp_orgfile <- setdiff(cols_filgruppe, cols_orgfile)
  for(col in missing_in_temp_orgfile){
    invisible(DBI::dbExecute(con, sprintf("ALTER TABLE temp_orgfile ADD COLUMN %s VARCHAR default ''", 
                                          as.character(DBI::dbQuoteIdentifier(con, col)))))
  }
  
  invisible(DBI::dbExecute(con, "INSERT INTO FILGRUPPE BY NAME SELECT * FROM temp_orgfile"))
  clean_tempfiles(con)
  invisible(NULL)
}

#' @noRd
add_table_to_fgduck <- function(dt, con){
  DBI::dbWriteTable(con, "tmp_in", dt, temporary = TRUE, overwrite = TRUE)
  on.exit(invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_in")), add = TRUE)
  
  if(!DBI::dbExistsTable(con, "FILGRUPPE")) {
    cols <- names(dt)
    cols_esc <- gsub('"', '""', cols, fixed = TRUE)
    sql <- sprintf("CREATE TABLE FILGRUPPE (%s)", paste(sprintf('"%s" VARCHAR', cols_esc), collapse = ", "))
    invisible(DBI::dbExecute(con, sql))
    invisible(DBI::dbExecute(con, "INSERT INTO FILGRUPPE BY NAME SELECT * FROM tmp_in"))
  } else {
    cols_existing <- DBI::dbListFields(con, "FILGRUPPE")
    cols_new <- names(dt)
    missing_cols <- setdiff(cols_new, cols_existing)
    if(length(missing_cols) > 0){
      for(col in missing_cols){
        col_esc <- gsub('"', '""', col, fixed = TRUE)
        invisible(DBI::dbExecute(con, sprintf("ALTER TABLE FILGRUPPE ADD COLUMN \"%s\" VARCHAR", col_esc)))
      }
    }
    invisible(DBI::dbExecute(con, "INSERT INTO FILGRUPPE BY NAME SELECT * FROM tmp_in"))
  }
}

# HELPERS ----
#' @title report_filegroup_progress
#' @noRd
report_filegroup_progress <- function(file_number, parameters){
  n_files <- parameters$n_files
  filename <- parameters$read_parameters[file_number]$FILNAVN
  print_console_message("\n", file_number, "/", n_files, ": ", filename, sep = "")
}

#' @title identify_columns_in_file
#' @description
#' Reads filedescription and indentify default columns existing in the file and columns that 
#' should be filled with default values. 
#' In INNLESING, a column name indicates that an existing column should be given a default name, and
#' values within <..> shuld be set as the default value for columns not existing in the file.
#' @param filedescription filedescription
#' @returns list of columns existing and columns to be given default values
#' @noRd
identify_columns_in_file <- function(filedescription){
  cols <- getOption("khfunctions.kolorgs")
  cols_in_file <- cols[grepl("^[^-<]", filedescription[, ..cols])]
  cols_with_default_value <- cols[grep("^<.*>", filedescription[, ..cols])]
  return(list(have = cols_in_file, default = cols_with_default_value))
}

clean_tempfiles <- function(con){
  for(tab in c("temp_orgfile", "temp_orgfile_reshape", "temp_recode")){
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tab))
  }
}

# Process data.table deprecated ----

#' @title do_reshape_var
#' @description
#' Reshapes the data to collect columns representing the same variable into long format
#' @noRd
do_reshape_var <- function(dt, filedescription, parameters){
  save_filedump_if_requested(dumpname = "RESHAPEpre", dt = NULL, parameters = parameters, koblid = filedescription$KOBLID, duck = TRUE, tablename = "temp_orgfile")
  on.exit({save_filedump_if_requested(dumpname = "RESHAPEpost", dt = NULL, parameters = parameters, koblid = filedescription$KOBLID, duck = TRUE, tablename = "temp_orgfile")}, add = TRUE)
  if(is_empty(filedescription$RESHAPEvar)) return(invisible(NULL))
  
  cols <- get_reshape_parameters(filedescription = filedescription, allcolumns = names(dt))
  if(!is.null(cols$id) && !all(cols$id %in% names(dt))) stop("Feil i RESHAPE: Kolonner angitt i RESHAPEid ikke funnet")
  if(!is.null(cols$measure) && !all(cols$measure %in% names(dt))) stop("Feil i RESHAPE: Kolonner angitt i RESHAPEmeas ikke funnet")
  if(!is.null(cols$id) && is.null(cols$measure)) stop("Feil i RESHAPE: Både RESHAPEid og RESHAPEmeas er tomme")
  reshape <- data.table::melt(dt, id.vars = cols$id, measure.vars = cols$measure, variable.name = cols$var, value.name = cols$val)
  dt[, names(dt) := NULL]
  dt[, (names(reshape)) := reshape]
  convert_all_columns_to_character(dt = dt)
}

#' @title do_set_default_values
#' @description
#' Sets default values for columns where the default value are provided in ACCESS::INNLESING within <...>
#' @noRd
do_set_default_values <- function(dt, filedescription, defaultcolumns){
  default <- filedescription[, ..defaultcolumns]
  default[, names(.SD) := lapply(.SD, function(x) sub("^<(.*)>$", "\\1", x))]
  dt[, names(default) := default]
}

#' @title convert_all_columns_to_character
#' @description
#' Make sure all columns are of type character
#' @param dt data
#' @noRd
convert_all_columns_to_character <- function(dt){
  non_char_cols <- names(dt)[!vapply(dt, is.character, FUN.VALUE = logical(1))]
  for (j in non_char_cols) {
    data.table::set(dt, j = j, value = as.character(dt[[j]]))
  }
}

#' @noRd
do_convert_na_to_empty <- function(dt){
  dt[, names(.SD) := lapply(.SD, function(x) data.table::fifelse(is.na(x), "", x))]
}

# TO BE DELETED ---- 

#' @description
#' Sannsynligvis aldri i bruk (bare i gamle STATBANK/INNVAND-filer), kan kanskje pensjoneres
#' @keywords deprecate
#' @noRd
merge_geo_d2 <- function(dt, filedescription){
  # Merge GEO delt i to
  if (filbesk$GEOd2 != "-" & !is.na(filbesk$GEOd2)) {
    DF[, filbesk$GEOd2] <- gsub("^(\\d|\\d{3})$", "0\\1", DF[, filbesk$GEOd2]) # setter på ledende 0
    DF$GEO <- paste(DF$GEO, DF[, filbesk$GEOd2], sep = "") # limer sammen GEO og GEOd2-kolonnen
  }
}

#' @description
#' Sannsynligvis aldri i bruk, kan kanskje pensjoneres
#' @keywords deprecate
#' @noRd
do_split_multihead <- function(dt, filedescription, con){
  if(is_empty(filedescription$MULTIHEAD)) return(invisible(NULL))
  dt <- fetch_duckdb_table(con = con, tablename = "temp_orgfile")
  mhl <- LesMultiHead(filedescription$MULTIHEAD)
  dt[, (mhl$colnames) := data.table::tstrsplit(mhl$varname, mhl$sep)]
  write_duckdb_table(dt = dt, con = parameters$duck, tablename = "temp_orgfile")
  rm(dt)
  invisible(gc())
}

#' @title do_handle_fylltab
#' @description
#' Fills columns according to information provided in INNLESING::FYLLTAB
#' Currently not in use, can be removed
#' @noRd
do_handle_fylltab <- function(filedescription, con){
  if(is_empty(filedescription$FYLLTAB)) return(invisible(NULL))
  dt <- fetch_duckdb_table(con = con, tablename = "temp_orgfile")
  cols <- trimws(strsplit(filedescription$FYLLTAB, ",")[[1]])
  if(!all(cols %in% names(dt))) stop("Feil i FYLLTAB: ", paste0("Kolonner ", paste(cols[!cols %in% names(dt)], collapse = ","), " finnes ikke"))
  
  for(col in cols){
    dt[dt[[col]] == "", (col) := NA]
    dt[, names(.SD) := zoo::na.locf(.SD, na.rm = FALSE), .SDcols = col]
  }
  write_duckdb_table(dt = dt, con = con, tablename = "temp_orgfile")
  rm(dt)
  invisible(gc())
}

#' @title do_aggregate_if_grunnkrets
#' @description
#' If original data is provided on grunnkrets level, aggregate
#' Potentially deprecated, not needed if parameter INNLESING::GRUNNKRETS
#' is not active.
#' @noRd
do_aggregate_if_grunnkrets <- function(dt, filedescription, parameters){
  if(is_empty(filedescription$GRUNNKRETS) || filedescription$GRUNNKRETS != 1) return(invisible(NULL))
  print_console_message("\n* Aggregerer fra grunnkrets...")
  colorder <- names(dt)
  aggregate <- collapse::join(dt, parameters$GkBHarm, how = "l", on = c("GEO" = "GK"), verbose = 0)
  aggregate[is.na(Bydel2004), Bydel2004 := paste(substr(GEO, 1, 4), "00", sep = "")]
  aggregate[, let(GEO = Bydel2004, Bydel2004 = NULL, FRA = NULL, TIL = NULL)]
  valcols <- names(aggregate)[grepl("^VAL\\d$", names(aggregate))]
  tabcols <- names(aggregate)[!names(dt) %in% valcols]
  aggregate[, names(.SD) := lapply(.SD, as.numeric), .SDcols = valcols]
  g = collapse::GRP(dt, tabcols)
  aggregate <- collapse::add_vars(g[["groups"]], collapse::fsum(collapse::get_vars(aggregate, valcols), g = g))
  dt[, names(dt):= NULL]
  dt[, names(aggregate) := aggregate]
  convert_all_columns_to_character()
  data.table::setcolorder(dt, colorder)
}
