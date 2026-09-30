# Funksjoner som er flyttet til khtools

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