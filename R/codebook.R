#' @title recode_columns_with_codebook
#' @description
#' Initialiserer kodebokomkoding.
#'
#' Oppretter tom kodeboklogg i DuckDB og identifiserer hvilke
#' kolonner i temp_orgfile som faktisk skal omkodes for aktuell fil.
#'
#' @param filedescription filedescription
#' @param parameters parameterliste
#'
#' @returns
#' Invisibly NULL dersom ingen kolonner skal omkodes.
#' Ellers en liste med kodebokutvalg og kolonner som skal omkodes.
recode_columns_with_codebook <- function(filedescription, parameters){
  con <- parameters$duck
  
  save_filedump_if_requested(dumpname = "KODEBOKpre", dt = NULL, parameters = parameters, koblid = filedescription$KOBLID, duck = TRUE, tablename = "temp_orgfile")
  on.exit({
    save_filedump_if_requested(dumpname = "KODEBOKpost", dt = NULL, parameters = parameters, koblid = filedescription$KOBLID, duck = TRUE, tablename = "temp_orgfile")
    alltabs <- khtools::duckdb_get_tables(con) 
    temps <- alltabs[grepl("^temp_codebook_map_", alltabs)]
    khtools::duckdb_drop_tables(con, c("temp_codebooklog_append", temps))
  }, add = TRUE)
  
  codebook <- parameters$codebook[DELID %in% c(filedescription$DELID, "FELLES")]
  if(nrow(codebook) == 0) return(invisible(NULL))
  
  recodecols <- intersect(unique(codebook$FELTTYPE), khtools::duckdb_get_columns(con, "temp_orgfile"))
  if(length(recodecols) == 0) return(invisible(NULL))
  
  khtools::msg("- KODEBOK:")
  n_before <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS N FROM temp_orgfile")$N
  total_recoded <- 0
  mapped_cols <- character()
  
  for(col in recodecols){
    n_recoded <- build_codebook_mapping(con = con, col = col, codebook = codebook[FELTTYPE == col], koblid = filedescription$KOBLID)
    if(n_recoded > 0) mapped_cols <- c(mapped_cols, col)
    total_recoded <- total_recoded + n_recoded
  }
  
  apply_all_codebook_mappings(con = con, tablename = "temp_orgfile", mapped_cols = mapped_cols)
  
  n_after <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS N FROM temp_orgfile")$N
  n_removed <- n_before - n_after
  khtools::msg("-- Omkodet ", total_recoded, " verdier/celler", sep = "")
  if(n_removed > 0){
    khtools::msg("-- Kaster ", n_removed, " slettede rader", sep = "")
  }
  
  invisible(NULL)
}

apply_all_codebook_mappings <- function(con, tablename, mapped_cols){
  if(length(mapped_cols) == 0){ return(invisible(NULL))}
  select_expr <- c(
    sprintf("t.* EXCLUDE (%s)", paste(khtools::sql_quote_I(con, mapped_cols), collapse = ", " )),
    
    vapply(mapped_cols,
      function(col){ 
        alias <- sprintf("m_%s", tolower(col))
        sprintf("CASE WHEN %s.ORG IS NULL THEN t.%s ELSE %s.OMK END AS %s",
          alias,
          khtools::sql_quote_I(con, col),
          alias,
          khtools::sql_quote_I(con, col)
        )
      }, character(1))
    )
  
  join_expr <- vapply(
    mapped_cols,
    function(col){
      alias <- sprintf("m_%s", tolower(col))
      sprintf(
        "LEFT JOIN %s %s ON t.%s = %s.ORG",
        khtools::sql_quote_I(con, sprintf("temp_codebook_map_%s",tolower(col))),
        alias,
        khtools::sql_quote_I(con, col), 
        alias
      )}, character(1)
    )
  
  keep_expr <- paste(vapply(
      mapped_cols,
      function(col){
        alias <- sprintf("m_%s", tolower(col))
        sprintf("(%s.ORG IS NULL OR %s.OMK IS NULL OR %s.OMK <> '-')", alias, alias, alias)
      },
      character(1)
    ), collapse = "\nAND ")
  
  sql <- sprintf(
  "SELECT %s FROM %s t
  %s 
  WHERE %s",
  paste(select_expr, collapse = ",\n"),
  khtools::sql_quote_I(con, tablename),
  paste(join_expr, collapse = "\n"),
  keep_expr
  )
  
  khtools::duckdb_replace_existing_table(con = con, target = tablename, select_sql = sql)
  invisible(NULL)
}


create_codebooklog_tables <- function(con){
  
  khtools::duckdb_drop_tables(con, c("codebooklog", "codebooklog_append"))
  
  invisible(DBI::dbExecute(con,
    "CREATE TABLE codebooklog(
      KOBLID VARCHAR,
      DELID VARCHAR,
      FELTTYPE VARCHAR,
      TYPE VARCHAR,
      ORG VARCHAR,
      OMK VARCHAR,
      FREQ BIGINT
    )
    "
  ))
  
  invisible(DBI::dbExecute(con,
    "CREATE TABLE codebooklog_append 
    AS SELECT * FROM codebooklog WHERE 1 = 0"
  ))
  
  invisible(NULL)
}

build_codebook_mapping <- function(con, col, codebook, koblid){
  values <- data.table::setDT(
    DBI::dbGetQuery(con,
                    sprintf("SELECT %s AS ORG,
                            COUNT(*) AS FREQ
                            FROM temp_orgfile
                            GROUP BY 1", 
                            khtools::sql_quote_I(con, col))))
  
  values[, `:=`(OMK_KB = ORG, OMK_FINAL = ORG)]
  n_recoded <- 0L
  kb <- codebook[TYPE == "KB" & FELTTYPE == col]
  
  if(nrow(kb) > 0){
    values[kb, on = c(ORG = "ORGKODE"), `:=` (OMK_KB = i.NYKODE, DELID = i.DELID)]
    values[, OMK_FINAL := OMK_KB]
    
    kb_log <- values[is.na(OMK_KB) | ORG != OMK_KB
                     ][, .(KOBLID = koblid, 
                             DELID,
                             FELTTYPE = col,
                             TYPE = "KB",
                             ORG,
                             OMK = OMK_KB,
                             FREQ)]
    
    if(nrow(kb_log) > 0){
      n_recoded <- n_recoded + sum(kb_log$FREQ, na.rm = TRUE)
      khtools::duckdb_write_table(con, table = "codebooklog_append", data = kb_log, overwrite = TRUE)
      khtools::duckdb_append_table(con, target = "codebooklog", source = "codebooklog_append")
    }
  }
  
  sub_cb <- codebook[TYPE == "SUB" & FELTTYPE == col]
  if(nrow(sub_cb) > 0){
    sub_log <- data.table::rbindlist(lapply(
        seq_len(nrow(sub_cb)),
        function(i){
          idx <- grepl(sub_cb$ORGKODE[i], values$OMK_FINAL, perl = TRUE)
          
          if(grepl("^VAL\\d+$", col)){
            
            freq_i <- if(any(idx)){
              values[idx, sum(FREQ, na.rm = TRUE)]
            } else {
              0L
            }
            
            log_i <- data.table::data.table(KOBLID = koblid,
                                            DELID = sub_cb$DELID[i],
                                            FELTTYPE = col,
                                            TYPE = "SUB",
                                            ORG = sub_cb$ORGKODE[i],
                                            OMK = sub_cb$NYKODE[i],
                                            FREQ = freq_i)
          } else {
            log_i <- values[idx, .(KOBLID = koblid, 
                                   DELID = sub_cb$DELID[i],
                                   FELTTYPE = col,
                                    TYPE = "SUB",
                                    ORG = OMK_FINAL,
                                    OMK = sub(sub_cb$ORGKODE[i], sub_cb$NYKODE[i], OMK_FINAL, perl = TRUE),
                                    FREQ
                                  )
            ]
          }
          
          values[
            idx,
            OMK_FINAL := sub(
              sub_cb$ORGKODE[i],
              sub_cb$NYKODE[i],
              OMK_FINAL,
              perl = TRUE
            )
          ]
          
          log_i
        }
      ),
      fill = TRUE
    )
    
    if(nrow(sub_log)){
      n_recoded <- n_recoded + sum(sub_log$FREQ, na.rm = TRUE)
      khtools::duckdb_write_table(con = con, table = "codebooklog_append", data = sub_log, overwrite = TRUE)
      khtools::duckdb_append_table(con, target = "codebooklog", source = "codebooklog_append")
    }
  }
  
  if(col != "GEO" && !grepl("^VAL\\d+$", col)){
    unchanged_log <- values[
      !is.na(OMK_FINAL) & ORG == OMK_FINAL,
      .(
        KOBLID = koblid,
        DELID = NA_character_,
        FELTTYPE = col,
        TYPE = "IKKE_OMKODET",
        ORG,
        OMK = NA_character_,
        FREQ = NA_integer_
      )
    ]
    
    if(nrow(unchanged_log) > 0){
      khtools::duckdb_write_table(con = con, table = "codebooklog_append", data = unchanged_log, overwrite = TRUE)
      khtools::duckdb_append_table(con, target = "codebooklog", source = "codebooklog_append")
    }
  }
  
  has_changes <- any(is.na(values$OMK_FINAL) | values$ORG != values$OMK_FINAL)
  if(!has_changes) return(0L)

  khtools::duckdb_write_table(
    con = con, table = sprintf("temp_codebook_map_%s", tolower(col)),
    data = values[, .(ORG, OMK = OMK_FINAL)], overwrite = TRUE)
  
  return(n_recoded)
}







