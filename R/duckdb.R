#' @title set_implicit_null_after_merge_duckdb
#' @description
#' Inserts values for implicit rows generated during merging. 
#' Works directly on tables in duckdb
#' @keywords duckdb
#' @noRd
set_implicit_null_after_merge_duckdb <- function(table, implicitnull_defs = list(), con) {
  
  khtools::msg("\n- Håndterer implisitte nuller")
  cols <- khtools::duckdb_get_columns(con, table)
  vals <- get_value_columns(cols)
  tbl_sql <- khtools::sql_quote_I(con, table)
  
  if("BEF" %in% names(implicitnull_defs) && any(grepl("^BEF", vals))){
    correctval <- grep("^BEF", vals, value = T)
    names(implicitnull_defs)[names(implicitnull_defs) == "BEF"] <- correctval
  }
  
  for (val in vals) {
    replacemissing <- list(0,0,1)
    
    if(val %in% names(implicitnull_defs)) {
      VALmiss <- implicitnull_defs[[val]]$miss
      if(grepl("\\D", VALmiss) && !VALmiss %in% c("..", ".", ":")){
        stop(val, " har ugyldig VALmiss, må være '..', '.',':' eller numerisk") 
      }
      
      missval <- ifelse(!grepl("\\D", VALmiss), as.numeric(VALmiss), 0)
      flagval <- data.table::fcase(VALmiss == "..", 1, 
                                   VALmiss == ".", 2,
                                   VALmiss == ":", 3,
                                   default = 0)
      replacemissing <- list(missval, flagval, 1)
    }
    
    valF <- paste0(val, ".f")
    if (!(valF %in% cols)) next
    # hopp over hvis .f ikke finnes
    
    val_sql <- khtools::sql_quote_I(con, val)
    valF_sql <- khtools::sql_quote_I(con, valF)
    valA_sql <- khtools::sql_quote_I(con, paste0(val, ".a"))
    
    cond <- sprintf("(%s IS NULL AND %s = 0) OR %s IS NULL", val_sql, valF_sql, valF_sql)
    
    query <- sprintf(
      "UPDATE %s SET %s = %s, %s = %s, %s = %s WHERE %s",
      tbl_sql, 
      val_sql, replacemissing[1],
      valF_sql, replacemissing[2],
      valA_sql, replacemissing[3],
      cond
    )
    
    invisible(DBI::dbExecute(con, query))
  }
}

#' @title do_aggregate_file_duckdb
#' @description Aggregerer verdikolonner for alle strata av dimensjoner. 
#' For verdier som ikke kan summeres vil tall som er sum av flere rader bli satt til NA med flagg = 2. 
#' @keywords duckdb
#' @noRd
do_aggregate_file_duckdb <- function(con, tablename, vals = list()){
  
  cols <- khtools::duckdb_get_columns(con, tablename)
  dimcols <- get_dimension_columns(cols)
  valcols <- get_value_columns(cols)
  
  dims_sql <- khtools::sql_quote_I(con, dimcols)
  
  aggcols_sql <- unlist(lapply(valcols,
      function(val){
        valf <- khtools::sql_quote_I(con, paste0(val, ".f"))
        vala <- khtools::sql_quote_I(con, paste0(val, ".a"))
        val <- khtools::sql_quote_I(con, val)
        c(sprintf("SUM(%s) AS %s", val, val),
          sprintf("MAX(%s) AS %s", valf, valf),
          sprintf("SUM(CASE WHEN %s IS NULL OR %s = 0 THEN 0 ELSE %s END) AS %s",
            val, val, vala, vala))
        }))
  
  sql <- sprintf("SELECT %s FROM %s GROUP BY %s",
                 paste(c(dims_sql, aggcols_sql),collapse = ", "),
                 khtools::sql_quote_I(con, tablename),
                 paste(dims_sql,collapse = ", "))
  
  khtools::duckdb_replace_existing_table(con, target = tablename, select_sql = sql)
  
  nonsum <- intersect(
    valcols,
    names(vals)[
    vapply(
      vals,
      function(x) identical(as.character(x$sumbar), "0"),
      logical(1))
    ])
  
  if(length(nonsum) > 0){
    for(val in nonsum){
      valf <- khtools::sql_quote_I(con, paste0(val, ".f"))
      vala <- khtools::sql_quote_I(con, paste0(val, ".a"))
      val <- khtools::sql_quote_I(con, val)
      sql_nonsum <- sprintf("UPDATE %s SET %s = NULL, %s = 2 WHERE %s > 1",
                            khtools::sql_quote_I(con, tablename), val, valf, vala)
      invisible(DBI::dbExecute(con, sql_nonsum))
    }
  }
  
  invisible(NULL)
}

set_integer_columns_duckdb <- function(con){
  integers <- c("AARl", "AARh", "ALDERl", "ALDERh", "KJONN", "UTDANN", "LANDBAK", "INNVKAT")
  cols <- intersect(integers,khtools::duckdb_get_columns(con, "FILGRUPPE"))
  
  sql <- paste(sprintf(
    "ALTER TABLE FILGRUPPE ALTER COLUMN %s TYPE INTEGER USING TRY_CAST(%s AS INTEGER)",
    cols,cols),collapse = ";\n")
  invisible(DBI::dbExecute(con, sql))
  invisible(NULL)
}
