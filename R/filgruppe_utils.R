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

set_integer_columns_duckdb <- function(con){
  integers <- c("AARl", "AARh", "ALDERl", "ALDERh", "KJONN", "UTDANN", "LANDBAK", "INNVKAT")
  cols <- intersect(integers,DBI::dbListFields(con, "FILGRUPPE"))
  
  sql <- paste(sprintf(
    "ALTER TABLE FILGRUPPE ALTER COLUMN %s TYPE INTEGER USING TRY_CAST(%s AS INTEGER)",
    cols,cols),collapse = ";\n")
  invisible(DBI::dbExecute(con, sql))
  invisible(NULL)
}

#' @title sort_filegroup_duckdb
#' @description
#' Sorterer FILGRUPPE etter alle dimensjonskolonner.
#' Overskriver tabellen med sortert versjon.
#' @param con duckdb-connection
#' @noRd
sort_filegroup_duckdb <- function(con){
  
  dims <- khfunctions:::get_dimension_columns(DBI::dbListFields(con, "FILGRUPPE"))
  
  partitiondims <- c("alder", "AARl", "lks")
  compressiondims <- c("KJONN", "UTDANN", "INNVKAT", "LANDBAK", "GEO")
  sortdims <- union(dims, c(partitiondims, compressiondims))
  sortdims_sql <- paste(sortdims, collapse = ", ")
  
  sql <- sprintf(
    "CREATE OR REPLACE TABLE FILGRUPPE AS
    SELECT *
    FROM FILGRUPPE
    ORDER BY %s",
    sortdims_sql
  )
  
  invisible(DBI::dbExecute(con, sql))
}

#' @title add_partition_columns_befolkning_duckdb
#' @description Legger til partisjoneringskolonner til befolkningsfilen
#' @param parameters globale parametre
#' @noRd
add_partition_columns_befolkning_duckdb <- function(parameters) {
  if(!grepl("BEF_GKny", parameters$name, ignore.case = TRUE)) return(invisible(NULL))
  
  con <- parameters$duck
  invisible(DBI::dbExecute(con, 
                           "ALTER TABLE FILGRUPPE ADD COLUMN IF NOT EXISTS lks INTEGER;"))
  
  invisible(DBI::dbExecute(con,
                           "ALTER TABLE FILGRUPPE ADD COLUMN IF NOT EXISTS alder VARCHAR;"))
  
  invisible(DBI::dbExecute(con,
                          "UPDATE FILGRUPPE SET
                          lks = CASE
                            WHEN GEOniv = 'V' THEN 1
                            ELSE 0
                          END,
                          alder = CASE
                            WHEN ALDERh <= 17 THEN '0_17'
                            WHEN ALDERh <= 29 THEN '18_29'
                            WHEN ALDERh <= 44 THEN '30_44'
                            WHEN ALDERh <= 67 THEN '45_67'
                            WHEN ALDERh <= 79 THEN '68_79'
                            ELSE '80_120'
                          END"
  ))
  
  invisible(NULL)
}