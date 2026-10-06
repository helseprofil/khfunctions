# Funksjoner som er flyttet til khtools eller avviklet etter innføring av khtools

#' @description wrapper rundt dbquoteidentifier, for renere kode da denne brukes mange steder
#' @noRd
sqlquote <- function(con, x){
  DBI::dbQuoteIdentifier(con, x)
}

#' DBI::dblistFields
#' @noRd
get_duckdb_cols <- function(con, tablename){
  DBI::dbListFields(con, tablename)
}

get_duckdb_tables <- function(con){
  DBI::dbListTables(con = con)
}

#' @title init_duckdb
#' @description Initiates a duckdb to be used during data processing
#' @keywords duckdb
#' @noRd
init_duckdb <- function(dbname){
  duckdir <- file.path(fs::path_home(), "helseprofil", "duck")
  fs::dir_create(duckdir)
  db <- file.path(duckdir, paste0(dbname, ".duckdb"))
  
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = db)
  DBI::dbExecute(con, "SET memory_limit = '8GB'")
  
  temp_dir <- file.path(tempdir(), "duckdb", "temp")
  fs::dir_create(temp_dir)
  DBI::dbExecute(con, sprintf("SET temp_directory='%s'", gsub("\\\\", "/", temp_dir)))
  
  tabs <- khtools::duckdb_get_tables(con = con)
  for(i in seq_along(tabs)){
    invisible(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabs[[i]], " CASCADE;")))
  }
  con
}

#' @title drop_tables_duckdb
#' @description drops tables if they exist
#' @keywords duckdb
#' @noRd
drop_tables_duckdb <- function(con, tables){
  tables <- khtools::sql_quote_I(con, tables)
  sql <- paste(
    sprintf("DROP TABLE IF EXISTS %s", tables),
    collapse = ";\n"
  )
  invisible(DBI::dbExecute(con, sql))
}

#' @title do_clean_duckdb
#' @description Free up space in duckdb, to be used after extensive write operations
#' @keywords duckdb
#' @noRd
do_clean_duckdb <- function(con){
  if (DBI::dbIsValid(con)) {
    invisible(try(DBI::dbExecute(con, "CHECKPOINT"), silent = TRUE))
    invisible(try(DBI::dbExecute(con, "VACUUM"), silent = TRUE))
  }
}

# Helpers ----

#' @title is_duckdb_table
#' @description Checks if table exists in duckdb
#' @keywords duckdb
#' @noRd
is_duckdb_table <- function(con, tablename){
  if(is_empty(tablename)) return(FALSE)
  DBI::dbIsValid(con) && DBI::dbExistsTable(con, tablename)
}

#' @title write_duckdb_table
#' @description (over)write table to duckdb
#' @keywords duckdb
#' @noRd
write_duckdb_table <- function(con, tablename, data, temp = TRUE, overwrite = TRUE, ...){
  DBI::dbWriteTable(
    conn = con,
    name = tablename,
    value = data,
    overwrite = overwrite,
    temporary = temp,
    ...
  )
  invisible(NULL)
}

#' @title write_to_tmp_and_replace_table
#' @description wrapper used when table has been processed in R and is written back to duckdb.
#' @family duckdb
#' @noRd
write_to_tmp_and_replace_table <- function(con, tablename, data){
  tmp_table <- prepare_tmp_result_table(con, tablename)
  khtools::duckdb_write_table(con,tablename = tmp_table,data = data)
  khtools::duckdb_replace_table(con,target = tablename, source = tmp_table)
  invisible(NULL)
}

#' @title replace_table_duckdb
#' @description
#' Erstatter en tabell med en annen i duckdb. Ved bearbeiding av en tabell kan resultatet
#' skrives til en tmp-tabell, og så kan hovedtabellen erstattes med denne etterpå. Da slipper
#' vi CREATE TABLE TABELL AS SELECT * FROM TABELL ..., altså å overskrive tabellen med seg selv som kan være ustabilt. 
#' Vi kan i stedet bruke CREATE TABLE tmp AS SELECT * FROM TABELL, og deretter bruke 
#' khtools::duckdb_replace_table(con, target = TABELL, source = tmp). Dette vil først generere ny tabell
#' tmp, og deretter erstatte originaltabellen med denne. 
#' @family duckdb
#' @noRd
replace_table_duckdb <- function(con, target, source){
  
  if(identical(target, source)) stop("'target' og 'source' kan ikke være samme tabell")
  stopifnot(DBI::dbExistsTable(con, target))
  stopifnot(DBI::dbExistsTable(con, source))
  
  backup <- paste0(target, "__replace__table__backup")
  on.exit(khtools::duckdb_drop_tables(con, backup), add =)
  backup_sql <- khtools::sql_quote_I(con, backup)
  target_sql <- khtools::sql_quote_I(con, target)
  source_sql <- khtools::sql_quote_I(con, source)
  
  khtools::duckdb_drop_tables(con, backup)
  
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, sprintf("ALTER TABLE %s RENAME TO %s", target_sql, backup_sql))
    DBI::dbExecute(con, sprintf("ALTER TABLE %s RENAME TO %s", source_sql, target_sql))
  })
  
  invisible(NULL)
}

#' @title fetch_duckdb_table
#' @description fetch table from duckdb
#' @keywords duckdb
#' @noRd
fetch_duckdb_table <- function(con, tablename, limit = NULL){
  exist <- khtools::duckdb_table_exists(con, tablename)
  if(!exist) stop(tablename, " finnes ikke i duckdb")
  
  sql <- if(is.null(limit)){
    sprintf("SELECT * FROM %s", khtools::sql_quote_I(con, tablename))
  } else {
    sprintf("SELECT * FROM %s LIMIT %s", khtools::sql_quote_I(con, tablename), as.integer(limit))
  }
  
  dt <- DBI::dbGetQuery(con, sql)
  data.table::setDT(dt)
  dt[]
}

#' Legge til nye kolonner
#' @param cols Må legges til som en navngitt vektor på formatet c(KOL = "TYPE", KOL2 = "TYPE")
#' for eksempel (RATE = "DOUBLE", spvtmp = "INTEGER", TAB = "VARCHAR")
#' @noRd
init_new_duckdb_cols <- function(con, table, cols){
  sql <- paste(
    sprintf(
      "ALTER TABLE %s ADD COLUMN IF NOT EXISTS %s %s",
      table, khtools::sql_quote_I(con, names(cols)), unname(cols)), 
    collapse = ";\n")
  
  sql <- paste0(sql, ";")
  invisible(DBI::dbExecute(con, sql))
}

#' @description genererer navn til tmp_tabell for midlertidige resultater
#' @family duckdb
#' @noRd
set_tmp_result_table_name <- function(table){
  sprintf("%s___tmp_result", table)
}

#' @description genererer navn til tmp_tabell for midlertidige resultater. Sletter tabellen om den finnes.
#' @family duckdb
#' @noRd
prepare_tmp_result_table <- function(con, table){
  tmp_table <- set_tmp_result_table_name(table)
  
  if(DBI::dbExistsTable(con, tmp_table)){
    warning(sprintf("Tmp-tabellen '%s' fantes allerede og ble slettet",tmp_table))
  }
  khtools::duckdb_drop_tables(con, tmp_table)
  
  tmp_table
}

#' @title merge_duckdb_table
#' @description
#' Merges 2 tables in duckdb into a third table. If result = mergeto, mergefrom is merged into mergeto. If result != mergto, a new table is generated. 
#' @keywords duckdb
#' @noRd
merge_duckdb_table <- function(con, mergeto, mergefrom, result = NULL){
  if(is.null(result)) result <- mergeto
  if(identical(result, mergefrom)) stop("Måltabellen kan ikke være == mergefrom")
  to_cols <- khtools::duckdb_get_columns(con, mergeto)
  from_cols <- khtools::duckdb_get_columns(con, mergefrom)
  newcols_names <- setdiff(from_cols, to_cols)
  commoncols <- intersect(to_cols, from_cols)
  
  join_cols <- get_dimension_columns(commoncols)
  
  if(length(newcols_names) == 0L) {
    khtools::msg(sprintf("- Ingen nye kolonner å merge fra %s til %s, kan det ha gått galt i innlesing?", mergefrom, mergeto))
    return(invisible(NULL))
  }
  
  if(length(join_cols) == 0L) {
    khtools::msg(sprintf("- Forsøker å merge %s på %s, men finner ingen felles dimensjoner", mergefrom, mergeto))
    return(invisible(NULL))
  }
  
  
  join_cond <- paste0("org.", khtools::sql_quote_I(con, join_cols), " = new.", khtools::sql_quote_I(con, join_cols), 
                      collapse = " AND ")
  
  add_cols <- paste0("new.", khtools::sql_quote_I(con, newcols_names), collapse = ", ")
  
  khtools::msg(sprintf("\n- Merger %s til %s\n-- Nye kolonner: %s\n-- Join-kolonner: %s\n--- Resultattabell: %s", 
                       mergefrom, mergeto, 
                       paste(newcols_names, collapse = ", "), 
                       paste(join_cols, collapse = ", "),
                       result))
  
  result_sql <- khtools::sql_quote_I(con, result)
  mergeto_sql <- khtools::sql_quote_I(con, mergeto)
  mergefrom_sql <- khtools::sql_quote_I(con, mergefrom)
  updatetab <- identical(result, mergeto)
  
  target_table_merge <- if(updatetab) {
    set_tmp_result_table_name(result)
  } else {
    result
  }
  
  khtools::duckdb_drop_tables(con, target_table_merge)
  
  query <- sprintf(
    "CREATE TABLE %s AS SELECT org.*, %s 
    FROM %s AS org LEFT JOIN %s AS new ON %s", 
    khtools::sql_quote_I(con, target_table_merge), 
    add_cols, mergeto_sql, mergefrom_sql, join_cond)
  
  invisible(DBI::dbExecute(con, query))
  
  if(updatetab){
    khtools::duckdb_replace_table(con, target = result, source = target_table_merge)
  }
  
  actual_cols <- khtools::duckdb_get_columns(con, result)
  missing_new_cols <- setdiff(newcols_names, actual_cols)
  if(length(missing_new_cols) > 0) stop(sprintf("Merge feilet. Mangler kolonner i %s: %s", result, paste(missing_new_cols, collapse = ", ")))
  invisible(NULL)
}

ensure_kh_options <- function(){
  optkhfunctions <- orgdata:::is_globs("khfunctions")
  missing <- !(names(optkhfunctions) %in% names(options()))
  if (any(missing)) options(optkhfunctions[missing])
  options("khfunctions.snutter" = get_snutt_list())
  corrglobs <- orgdata:::is_correct_globs(c(optkhfunctions, "khfunctions.snutter"))
  isTRUE(corrglobs)
}

#' @title connect_khelsa
#' @description connects to khelsa.mdb
#' @keywords internal
#' @noRd
connect_khelsa <- function(){
  path <- file.path(getOption("khfunctions.root"),
                    getOption("khfunctions.db"))
  
  if(!file.exists(path)) stop("Finner ikke databasefilen ", path)
  
  RODBC::odbcDriverConnect(
    paste0(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
      "DBQ=", path, ";"
    )
  )
}

#' @keywords internal
#' @noRd
get_dimension_columns <- function(columnnames) {
  nodim <- c(get_value_columns(columnnames, full = TRUE), "KOBLID", "ROW", "missyear", "spv_tmp")
  return(setdiff(columnnames, nodim))
  # dims <- c(getOption("khfunctions.alldimensions"), "TAB1", "TAB2", "TAB3")
  # return(intersect(columnnames, dims))
}

#' @keywords internal
48

#' @noRd
get_value_columns <- function(columnnames, full = FALSE) {
  valcols <- grep("^(.*?)\\.f$", columnnames, value = T)
  valcols <- gsub("\\.f$", "", valcols)
  if(full) valcols <- paste0(rep(valcols, each = 7), c("", ".f", ".a", ".n", ".fn1", ".fn3", ".fn9"))
  return(intersect(columnnames, valcols))
}


print_console_message <- function(...){
  base::cat(...)
  base::cat("\n")
  utils::flush.console()
}

new_section_header <- function(msg){
  khtools::msg("\n# --", msg, "-- #\n")
}