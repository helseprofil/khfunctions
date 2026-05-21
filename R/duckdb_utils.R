#' @title init_duckdb
#' @description Initiates a duckdb to be used during data processing
#' @keywords duckdb
#' @noRd
init_duckdb <- function(dbname){
  tmpdir <- file.path(fs::path_home(), "helseprofil", "duck")
  if(!fs::dir_exists(tmpdir)) fs::dir_create(tmpdir)
  db <- file.path(tmpdir, paste0(dbname, ".duckdb"))
  if(file.exists(db)){
    try({
      con_tmp <- DBI::dbConnect(duckdb::duckdb(), dbdir = db)
      DBI::dbDisconnect(con_tmp, shutdown = TRUE)
    }, silent = TRUE)
    
    fs::file_delete(
      c(db,paste0(db, ".wal"),paste0(db, ".tmp"))[fs::file_exists(c(
      db,paste0(db, ".wal"),paste0(db, ".tmp")
    ))])
  }
  DBI::dbConnect(duckdb::duckdb(), dbdir = db)
}

#' @title do_clean_duckdb
#' @description Free up space in duckdb, to be used after extensive write operations
#' @keywords duckdb
#' @noRd
do_clean_duckdb <- function(parameters){
  if (DBI::dbIsValid(parameters$duck)) {
    invisible(try(DBI::dbExecute(parameters$duck, "CHECKPOINT"), silent = TRUE))
    invisible(try(DBI::dbExecute(parameters$duck, "VACUUM"), silent = TRUE))
  }
}

#' @title is_duckdb_file
#' @description Checks if table exists in duckdb
#' @keywords duckdb
#' @noRd
is_duckdb_file <- function(con, filename){
  DBI::dbIsValid(con) && filename %in% DBI::dbListTables(con)
}

#' @title quote_col_duckdb
#' @description quotes column names to allow ".", e.g. "VAL1.a" in queries.
#' @keywords duckdb
#' @noRd
quote_col_duckdb <- function(x){
  paste0('"', x, '"')
}