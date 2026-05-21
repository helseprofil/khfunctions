#' @title merge_duckdb_table
#' @description
#' Merges 2 tables in duckdb into a third table. If result = mergeto, mergefrom is merged into mergeto. If result != mergto, a new table is generated. 
#' @keywords duckdb
#' @noRd
merge_duckdb_table <- function(result, mergeto, mergefrom, con){
  to_cols <- DBI::dbListFields(con, mergeto)
  from_cols <- DBI::dbListFields(con, mergefrom)
  commoncols <- intersect(to_cols, from_cols)
  newcols_names <- setdiff(from_cols, commoncols)
  
  join_cond <- paste0(
    "org.", quote_col_duckdb(commoncols), " = new.", quote_col_duckdb(commoncols),
    collapse = " AND "
  )
  
  add_cols <- paste0(
    "new.", quote_col_duckdb(newcols_names),
    collapse = ", "
  )
  
  query <- paste0(
    "CREATE OR REPLACE TABLE ", result, " AS
   SELECT org.*, ", add_cols, "
   FROM ", mergeto, " AS org
   LEFT JOIN ", mergefrom, " AS new
   ON ", join_cond
  )
  
  invisible(DBI::dbExecute(con, query))
}

#' @title set_implicit_null_after_merge_duckdb
#' @description
#' Inserts values for implicit rows generated during merging. 
#' Works directly on tables in duckdb
#' @keywords duckdb
#' @noRd
set_implicit_null_after_merge_duckdb <- function(table, implicitnull_defs = list(), con) {
  
  print_console_message("\n*** Håndterer implisitte nuller\n")
  cols <- DBI::dbListFields(con, table)
  vals <- get_value_columns(cols)
  
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
    valA <- paste0(val, ".a")
    
    # hopp over hvis .f ikke finnes
    if (!(valF %in% cols)) next
    
    cond <- paste0("(", quote_col_duckdb(val), " IS NULL AND ", quote_col_duckdb(valF), " = 0) OR ", quote_col_duckdb(valF), " IS NULL" )
    
    query <- paste0("UPDATE ", table, " SET ",
      quote_col_duckdb(val), " = ", replacemissing[1], ",
       ", quote_col_duckdb(valF), " = ", replacemissing[2], ",
       ", quote_col_duckdb(valA), " = ", replacemissing[3], "
       WHERE ", cond
    )
    
    invisible(DBI::dbExecute(con, query))
  }
}
