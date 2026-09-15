#' @title merge_duckdb_table
#' @description
#' Merges 2 tables in duckdb into a third table. If result = mergeto, mergefrom is merged into mergeto. If result != mergto, a new table is generated. 
#' @keywords duckdb
#' @noRd
merge_duckdb_table <- function(con, mergeto, mergefrom, result = NULL){
  if(is.null(result)) result <- mergeto
  to_cols <- DBI::dbListFields(con, mergeto)
  from_cols <- DBI::dbListFields(con, mergefrom)
  newcols_names <- setdiff(from_cols, to_cols)
  commoncols <- intersect(to_cols, from_cols)
  
  join_cols <- get_dimension_columns(commoncols)
  
  if(length(newcols_names) == 0L) {
    print_console_message(sprintf("- Ingen nye kolonner å merge fra %s til %s, kan det ha gått galt i innlesing?", mergefrom, mergeto))
    return(invisible(NULL))
  }
  
  if(length(join_cols) == 0L) {
    print_console_message(sprintf("- Forsøker å merge %s på %s, men finner ingen felles dimensjoner", mergefrom, mergeto))
    return(invisible(NULL))
  }
  
  tmp <- paste0(result, "__tmp")
  drop_tables_duckdb(con, tmp)
  
  tmp_sql <- DBI::dbQuoteIdentifier(con, tmp)
  mergeto_sql <- DBI::dbQuoteIdentifier(con, mergeto)
  mergefrom_sql <- DBI::dbQuoteIdentifier(con, mergefrom)
  
  join_cond <- paste0("org.", DBI::dbQuoteIdentifier(con, join_cols), " = new.", DBI::dbQuoteIdentifier(con, join_cols), 
                      collapse = " AND ")
  
  add_cols <- paste0("new.", DBI::dbQuoteIdentifier(con, newcols_names), collapse = ", ")
  
  
  query <- sprintf(
  "CREATE OR REPLACE TABLE %s AS SELECT org.*, %s 
   FROM %s AS org LEFT JOIN %s AS new ON %s", 
  tmp_sql, add_cols, mergeto_sql, mergefrom_sql, join_cond)
  
  
  print_console_message(sprintf("\n- Merger %s til %s\n-- Nye kolonner: %s\n-- Join-kolonner: %s\n--- Resultattabell: %s", 
                                mergefrom, mergeto, 
                                paste(newcols_names, collapse = ", "), 
                                paste(join_cols, collapse = ", "),
                                result))
  
  invisible(DBI::dbExecute(con, query))
  replace_table_duckdb(con = con, target = result, source = tmp)
}

#' @title set_implicit_null_after_merge_duckdb
#' @description
#' Inserts values for implicit rows generated during merging. 
#' Works directly on tables in duckdb
#' @keywords duckdb
#' @noRd
set_implicit_null_after_merge_duckdb <- function(table, implicitnull_defs = list(), con) {
  
  print_console_message("*** Håndterer implisitte nuller")
  cols <- DBI::dbListFields(con, table)
  vals <- get_value_columns(cols)
  tbl_sql <- DBI::dbQuoteIdentifier(con, table)
  
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
    
    val_sql <- DBI::dbQuoteIdentifier(con, val)
    valF_sql <- DBI::dbQuoteIdentifier(con, valF)
    valA_sql <- DBI::dbQuoteIdentifier(con, paste0(val, ".a"))
    
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

do_aggregate_file_duckdb <- function(con, tablename, vals = list()){
  
  cols <- DBI::dbListFields(con, tablename)
  dimcols <- get_dimension_columns(cols)
  valcols <- get_value_columns(cols)
  
  dims_sql <- DBI::dbQuoteIdentifier(con, dimcols)
  table_sql <- DBI::dbQuoteIdentifier(con, tablename)
  
  aggcols_sql <- unlist(lapply(valcols,
      function(val){
        valf <- DBI::dbQuoteIdentifier(con, paste0(val, ".f"))
        vala <- DBI::dbQuoteIdentifier(con, paste0(val, ".a"))
        val <- DBI::dbQuoteIdentifier(con, val)
        c(sprintf("SUM(%s) AS %s", val, val),
          sprintf("MAX(%s) AS %s", valf, valf),
          sprintf("SUM(CASE WHEN %s IS NULL OR %s = 0 THEN 0 ELSE %s END) AS %s",
            val, val, vala, vala))
        }))
  
  result_tmp <- paste0(tablename, "_tmp")
  result_tmp_sql <- DBI::dbQuoteIdentifier(con, result_tmp)
  drop_tables_duckdb(con, result_tmp)
  
  sql <- sprintf(paste("CREATE OR REPLACE TABLE %s AS",
                       "SELECT %s FROM %s",
                       "GROUP BY %s"),
                 result_tmp_sql,
                 paste(c(dims_sql, aggcols_sql),collapse = ", "),
                 table_sql,
                 paste(dims_sql,collapse = ", "))
  
  
  invisible(DBI::dbExecute(con, sql))
  
  nonsum <- intersect(
    valcols,
    names(vals)[
    vapply(
      vals,
      function(x) identical(as.character(x$sumbar), "0"),
      logical(1))
    ])
  
  if(length(nonsum) > 0){
    replace_sql <- unlist(
      lapply(nonsum, function(val){
        valf <- DBI::dbQuoteIdentifier(con, paste0(val, ".f"))
        vala <- DBI::dbQuoteIdentifier(con, paste0(val, ".a"))
        val <- DBI::dbQuoteIdentifier(con, val)
        c(sprintf("CASE WHEN %s > 1 THEN NULL ELSE %s END AS %s", vala, val, val),
          sprintf("CASE WHEN %s > 1 THEN 2 ELSE %s END AS %s",vala, valf, valf))
      }))
    
    sql <- sprintf(
      paste("CREATE OR REPLACE TABLE %s AS",
            "SELECT * REPLACE(%s) FROM %s"),
      table_sql,
      paste(replace_sql, collapse = ", "),
      table_sql
    )
    
    invisible(DBI::dbExecute(con, sql))
  }
  
  invisible(NULL)
}
