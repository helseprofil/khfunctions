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

#' @description
#' hjelpefunksjon for å finne fellesdimensjoner for tabeller før merge. 
#' Bruker alle felles ikke-verdikolonner for å inkludere f.eks. GEOniv og FYLKE i join-nøkler.
#' @noRd
get_duckdb_join_cols <- function(con, table1, table2){
  commoncols <- intersect(khtools::duckdb_get_columns(con, table1),
                          khtools::duckdb_get_columns(con, table2))
  identify_nonvalues(commoncols)
}

#' @title convert_duckdb_cols_to_string
#' @description converts non-character columns to character 
#' @keywords duckdb
#' @noRd
convert_duckdb_cols_to_string <- function(con, tablename) {
  khtools::msg("- konverterer alle kolonner til string")
  cols <- khtools::duckdb_get_column_types(con, tablename)
  dims <- identify_dimensions(names(cols))
  is_dim <- names(cols) %in% dims
  
  numeric_types <- c("DOUBLE", "FLOAT", "REAL", "DOUBLE PRECISION")
  integer_like <- setNames(rep(FALSE, length(cols)), names(cols))
  dbl_cols <- names(cols)[toupper(cols) %in% numeric_types]
  
  if(length(dbl_cols) > 0) {
    integer_checks <- vapply(
      dbl_cols,
      function(col) {
        qcol <- khtools::sql_quote_I(con, col)
        sql <- sprintf(
        "SELECT NOT EXISTS (
            SELECT 1
            FROM %s
            WHERE %s IS NOT NULL
              AND %s <> CAST(%s AS BIGINT)
          ) AS INTEGER_LIKE",
          khtools::sql_quote_I(con, tablename),
          qcol, qcol, qcol
        )
        DBI::dbGetQuery(con, sql)$INTEGER_LIKE[[1]]
      },
      logical(1)
    )
    integer_like[names(integer_checks)] <- integer_checks
  }
  
  convert_expr <- vapply(
    seq_along(cols),
    function(i) {
      col <- names(cols)[i]
      qcol <- khtools::sql_quote_I(con, col)
      type <- toupper(cols[[i]])
      if(type %in% c("VARCHAR", "TEXT", "CHAR", "BPCHAR")) return(qcol)
      convert_via_bigint <- is_dim[i] || integer_like[[col]]
      if(convert_via_bigint) return(sprintf("CAST(CAST(%s AS BIGINT) AS VARCHAR) AS %s", qcol, qcol))
      sprintf("CAST(%s AS VARCHAR) AS %s", qcol, qcol)
    },
    character(1)
  )
  
  select_sql <- sprintf("SELECT\n%s\nFROM %s", paste(convert_expr, collapse = ",\n"), khtools::sql_quote_I(con, tablename))
  khtools::duckdb_replace_existing_table(con = con, target = tablename, select_sql = select_sql)
}
    


  
#   schema <- DBI::dbGetQuery(
#     con, sprintf("SELECT column_name, data_type FROM information_schema.columns WHERE table_name = '%s' ORDER BY ordinal_position", table_name)
#   )
#   text_types <- c("VARCHAR","TEXT","CHAR","BPCHAR")
#   cols_to_convert <- schema$column_name[!toupper(schema$data_type) %in% text_types]
#   
#   if(length(cols_to_convert) == 0) return(invisible(NULL))
#   
#   # Konverter kolonner med bare heltall til integer
#   check_sql <- paste0(
#     "SELECT\n",
#     paste(
#       sprintf(
#         "COUNT(*) FILTER (
#            WHERE %1$s IS NOT NULL
#              AND %1$s <> FLOOR(%1$s)
#          ) = 0 AS c%2$s",
#         khtools::sql_quote_I(con, cols_to_convert),
#         seq_along(cols_to_convert)
#       ),
#       collapse = ",\n"
#     ),
#     "\nFROM ",
#     khtools::sql_quote_I(con, table_name)
#   )
#   
#   check_res <- DBI::dbGetQuery(con, check_sql)
#   integer_like_cols <- cols_to_convert[unlist(check_res, use.names = FALSE)]
#   
#   for(col in integer_like_cols) {
#     DBI::dbExecute(con, sprintf("ALTER TABLE %s ALTER COLUMN %s TYPE BIGINT",
#                                 khtools::sql_quote_I(con, table_name),
#                                 khtools::sql_quote_I(con, col)))
#   }
#   
#   # Konverter ALLE cols_to_convert til varchar
#   
#   for(col in cols_to_convert) {
#     DBI::dbExecute(con, sprintf("ALTER TABLE %s ALTER COLUMN %s TYPE VARCHAR",
#                                 khtools::sql_quote_I(con, table_name),
#                                 khtools::sql_quote_I(con, col)))
#   }
#  
#   invisible(NULL)
# }
