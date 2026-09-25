#' @title merge_duckdb_table
#' @description
#' Merges 2 tables in duckdb into a third table. If result = mergeto, mergefrom is merged into mergeto. If result != mergto, a new table is generated. 
#' @keywords duckdb
#' @noRd
merge_duckdb_table <- function(con, mergeto, mergefrom, result = NULL){
  if(is.null(result)) result <- mergeto
  if(identical(result, mergefrom)) stop("Måltabellen kan ikke være == mergefrom")
  to_cols <- get_duckdb_cols(con, mergeto)
  from_cols <- get_duckdb_cols(con, mergefrom)
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
  
  
  join_cond <- paste0("org.", sqlquote(con, join_cols), " = new.", sqlquote(con, join_cols), 
                      collapse = " AND ")
  
  add_cols <- paste0("new.", sqlquote(con, newcols_names), collapse = ", ")
  
  print_console_message(sprintf("\n- Merger %s til %s\n-- Nye kolonner: %s\n-- Join-kolonner: %s\n--- Resultattabell: %s", 
                                mergefrom, mergeto, 
                                paste(newcols_names, collapse = ", "), 
                                paste(join_cols, collapse = ", "),
                                result))
  
  result_sql <- sqlquote(con, result)
  mergeto_sql <- sqlquote(con, mergeto)
  mergefrom_sql <- sqlquote(con, mergefrom)
  updatetab <- identical(result, mergeto)
  
  target_table_merge <- if(updatetab) {
    set_tmp_result_table_name(result)
  } else {
    result
  }
  
  drop_tables_duckdb(con, target_table_merge)
    
  query <- sprintf(
    "CREATE TABLE %s AS SELECT org.*, %s 
    FROM %s AS org LEFT JOIN %s AS new ON %s", 
    sqlquote(con, target_table_merge), 
    add_cols, mergeto_sql, mergefrom_sql, join_cond)
 
  invisible(DBI::dbExecute(con, query))
  
  if(updatetab){
    replace_table_duckdb(con, target = result, source = target_table_merge)
  }
  
  actual_cols <- get_duckdb_cols(con, result)
  missing_new_cols <- setdiff(newcols_names, actual_cols)
  if(length(missing_new_cols) > 0) stop(sprintf("Merge feilet. Mangler kolonner i %s: %s", result, paste(missing_new_cols, collapse = ", ")))
  invisible(NULL)
}

#' @title set_implicit_null_after_merge_duckdb
#' @description
#' Inserts values for implicit rows generated during merging. 
#' Works directly on tables in duckdb
#' @keywords duckdb
#' @noRd
set_implicit_null_after_merge_duckdb <- function(table, implicitnull_defs = list(), con) {
  
  print_console_message("\n- Håndterer implisitte nuller")
  cols <- get_duckdb_cols(con, table)
  vals <- get_value_columns(cols)
  tbl_sql <- sqlquote(con, table)
  
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
    
    val_sql <- sqlquote(con, val)
    valF_sql <- sqlquote(con, valF)
    valA_sql <- sqlquote(con, paste0(val, ".a"))
    
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
  
  cols <- get_duckdb_cols(con, tablename)
  dimcols <- get_dimension_columns(cols)
  valcols <- get_value_columns(cols)
  
  dims_sql <- sqlquote(con, dimcols)
  
  aggcols_sql <- unlist(lapply(valcols,
      function(val){
        valf <- sqlquote(con, paste0(val, ".f"))
        vala <- sqlquote(con, paste0(val, ".a"))
        val <- sqlquote(con, val)
        c(sprintf("SUM(%s) AS %s", val, val),
          sprintf("MAX(%s) AS %s", valf, valf),
          sprintf("SUM(CASE WHEN %s IS NULL OR %s = 0 THEN 0 ELSE %s END) AS %s",
            val, val, vala, vala))
        }))
  
  tmp_result <- prepare_tmp_result_table(con, tablename)
  
  sql <- sprintf("CREATE TABLE %s AS SELECT %s FROM %s GROUP BY %s",
                 sqlquote(con, tmp_result),
                 paste(c(dims_sql, aggcols_sql),collapse = ", "),
                 sqlquote(con, tablename),
                 paste(dims_sql,collapse = ", "))
  
  invisible(DBI::dbExecute(con, sql))
  replace_table_duckdb(con, source = tmp_result, target = tablename)
  
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
      valf <- sqlquote(con, paste0(val, ".f"))
      vala <- sqlquote(con, paste0(val, ".a"))
      val <- sqlquote(con, val)
      sql_nonsum <- sprintf("UPDATE %s SET %s = NULL, %s = 2 WHERE %s > 1",
                            sqlquote(con, tablename), val, valf, vala)
      invisible(DBI::dbExecute(con, sql_nonsum))
    }
  }
  
  invisible(NULL)
}

set_integer_columns_duckdb <- function(con){
  integers <- c("AARl", "AARh", "ALDERl", "ALDERh", "KJONN", "UTDANN", "LANDBAK", "INNVKAT")
  cols <- intersect(integers,get_duckdb_cols(con, "FILGRUPPE"))
  
  sql <- paste(sprintf(
    "ALTER TABLE FILGRUPPE ALTER COLUMN %s TYPE INTEGER USING TRY_CAST(%s AS INTEGER)",
    cols,cols),collapse = ";\n")
  invisible(DBI::dbExecute(con, sql))
  invisible(NULL)
}
