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
        khtools::sql_quote_I(con, cols_to_convert),
        seq_along(cols_to_convert)
      ),
      collapse = ",\n"
    ),
    "\nFROM ",
    khtools::sql_quote_I(con, table_name)
  )
  
  check_res <- DBI::dbGetQuery(con, check_sql)
  integer_like_cols <- cols_to_convert[unlist(check_res, use.names = FALSE)]
  
  for(col in integer_like_cols) {
    DBI::dbExecute(con, sprintf("ALTER TABLE %s ALTER COLUMN %s TYPE BIGINT",
                                khtools::sql_quote_I(con, table_name),
                                khtools::sql_quote_I(con, col)))
  }
  
  # Konverter ALLE cols_to_convert til varchar
  
  for(col in cols_to_convert) {
    DBI::dbExecute(con, sprintf("ALTER TABLE %s ALTER COLUMN %s TYPE VARCHAR",
                                khtools::sql_quote_I(con, table_name),
                                khtools::sql_quote_I(con, col)))
  }
 
  invisible(NULL)
}
