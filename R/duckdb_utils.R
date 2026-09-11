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
do_clean_duckdb <- function(con){
  if (DBI::dbIsValid(con)) {
    invisible(try(DBI::dbExecute(con, "CHECKPOINT"), silent = TRUE))
    invisible(try(DBI::dbExecute(con, "VACUUM"), silent = TRUE))
  }
}

#' @title is_duckdb_table
#' @description Checks if table exists in duckdb
#' @keywords duckdb
#' @noRd
is_duckdb_table <- function(con, tablename){
  DBI::dbIsValid(con) && tablename %in% DBI::dbListTables(con)
}

drop_tables_duckdb <- function(con, tables){
  tables <- DBI::dbQuoteIdentifier(con, tables)
  sql <- paste(
    sprintf("DROP TABLE IF EXISTS %s", tables),
    collapse = ";\n"
  )
  invisible(DBI::dbExecute(con, sql))
}

#' @title drop_tables_duckdb_prefix
#' @description helper function to remove all tables with a specific prefix
#' @keywords internal
#' @family duckdb
#' @noRd
drop_tables_duckdb_prefix <- function(con, prefix){
  tabs <- DBI::dbListTables(con)
  drop <- grep(sprintf("^%s", prefix), tabs, value = TRUE)
  if(length(drop) > 0){
    drop_tables_duckdb(con, drop)
  }
  invisible(NULL)
}

#' @title quote_col_duckdb
#' @description quotes column names to allow ".", e.g. "VAL1.a" in queries.
#' @keywords duckdb
#' @noRd
quote_col_duckdb <- function(x){
  paste0('"', x, '"')
}

#' @description
#' oversetter enkle filter fra R til sql
#' @noRd
r_filter_to_sql <- function(filter_expr){
  sql <- filter_expr
  sql <- gsub("==", "=", sql, fixed = TRUE)
  sql <- gsub("&", " AND ", sql, fixed = TRUE)
  sql <- gsub("\\|", " OR ", sql)
  sql
}

#' @title fetch_duckdb_table
#' @description fetch table from duckdb
#' @keywords duckdb
#' @noRd
fetch_duckdb_table <- function(con, tablename){
  exist <- tablename %in% DBI::dbListTables(con)
  if(!exist) stop(tablename, " finnes ikke i duckdb")
  dt <- DBI::dbGetQuery(con, 
                        sprintf("SELECT * FROM %s", 
                                DBI::dbQuoteIdentifier(con, tablename))
                        )
  data.table::setDT(dt)
}



#' @title write_duckdb_table
#' @description (over)write table to duckdb
#' @keywords duckdb
#' @noRd
write_duckdb_table <- function(con, tablename, data, temp = TRUE, overwrite = TRUE){
  DBI::dbWriteTable(
    conn = con,
    name = tablename,
    value = data,
    overwrite = overwrite,
    temporary = temp
  )
  invisible(NULL)
}

#' @title convert_duckdb_cols_to_string
#' @description converts non-character columns to character 
#' @keywords duckdb
#' @noRd
convert_duckdb_cols_to_string <- function(con, table_name) {
  schema <- DBI::dbGetQuery(
    con, sprintf("SELECT column_name, data_type FROM information_schema.columns WHERE table_name = '%s' ORDER BY ordinal_position", table_name)
  )
  text_types <- c("VARCHAR","TEXT","CHAR","BPCHAR")
  cols_to_convert <- schema$column_name[!toupper(schema$data_type) %in% text_types]
  
  if(length(cols_to_convert) == 0) return(invisible(NULL))
  
  # Konverter kolonner med bare heltall til integer
  check_sql <- paste0(
    "SELECT\n",
    paste(
      sprintf(
        "COUNT(*) FILTER (
           WHERE %1$s IS NOT NULL
             AND %1$s <> FLOOR(%1$s)
         ) = 0 AS c%2$s",
        DBI::dbQuoteIdentifier(con, cols_to_convert),
        seq_along(cols_to_convert)
      ),
      collapse = ",\n"
    ),
    "\nFROM ",
    DBI::dbQuoteIdentifier(con, table_name)
  )
  
  check_res <- DBI::dbGetQuery(con, check_sql)
  integer_like_cols <- cols_to_convert[unlist(check_res, use.names = FALSE)]
  
  for(col in integer_like_cols) {
    DBI::dbExecute(con, sprintf("ALTER TABLE %s ALTER COLUMN %s TYPE BIGINT",
                                DBI::dbQuoteIdentifier(con, table_name),
                                DBI::dbQuoteIdentifier(con, col)))
  }
  
  # Konverter ALLE cols_to_convert til varchar
  
  for(col in cols_to_convert) {
    DBI::dbExecute(con, sprintf("ALTER TABLE %s ALTER COLUMN %s TYPE VARCHAR",
                                DBI::dbQuoteIdentifier(con, table_name),
                                DBI::dbQuoteIdentifier(con, col)))
  }
 
  invisible(NULL)
}
