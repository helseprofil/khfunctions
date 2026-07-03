#' @title init_duckdb
#' @description Initiates a duckdb to be used during data processing
#' @keywords duckdb
#' @noRd
init_duckdb <- function(dbname){
  duckdir <- file.path(fs::path_home(), "helseprofil", "duck")
  fs::dir_create(duckdir)
  db <- file.path(duckdir, paste0(dbname, ".duckdb"))
  
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db)
  
  tabs <- DBI::dbListTables(con)
  for(i in seq_along(tabs)){
    invisible(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabs[[i]], " CASCADE;")))
  }
  con
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