#' @title recode_columns_with_codebook
#' @description
#' Recode data according to criteria set in KODEBOK. 
#' Recodings with TYPE = "KB" are directly recoded using do_recode_kb, 
#' and recodings with TYPE = "SUB" are applied as regex using do_recode_regex. 
#' Codebooklog summarises all recodings, and are updated by reference after 
#' each file has been recoded. 
#'
#' @param dt data
#' @param filedescription filedescription
#' @param codebook codebook subset for file
#' @param recode_cols columns to recode
#' @param codebooklog overal recode log
#' @param dumps in case file dumps before or after kodebok are requested.
#' @returns recoded data file (updated by reference)
recode_columns_with_codebook <- function(dt, filedescription, parameters, codebooklog, dumps){
  save_filedump_if_requested(dumpname = "KODEBOKpre", dt = NULL, parameters = parameters, koblid = filedescription$KOBLID, duck = TRUE, tablename = "temp_orgfile")
  on.exit({
    save_filedump_if_requested(dumpname = "KODEBOKpost", dt = NULL, parameters = parameters, koblid = filedescription$KOBLID, duck = TRUE, tablename = "temp_orgfile")
    invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS temp_recode"))
    if("ROWID_KH" %in% DBI::dbListFields(con, "temp_orgfile")) {
      invisible(DBI::dbExecute(con, "ALTER TABLE temp_orgfile DROP COLUMN ROWID_KH"))
    }
  }, add = TRUE)
  
  con <- parameters$duck
  
  codebook <- parameters$codebook[DELID %in% c(filedescription$DELID, "FELLES")]
  recodecols <- intersect(unique(codebook$FELTTYPE), 
                          DBI::dbListFields(con, "temp_orgfile"))
  if(nrow(codebook) == 0) return(invisible(NULL))
  
  invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS temp_recode"))
  
  if("ROWID_KH" %in% DBI::dbListFields(con, "temp_orgfile")) invisible(DBI::dbExecute(con,"ALTER TABLE temp_orgfile DROP COLUMN ROWID_KH"))
  
  invisible(DBI::dbExecute(con, "ALTER TABLE temp_orgfile ADD COLUMN ROWID_KH BIGINT"))
  invisible(DBI::dbExecute(con, "UPDATE temp_orgfile SET ROWID_KH = rowid"))
  
  recode_dt <- data.table::setDT(
    DBI::dbGetQuery(con, sprintf("SELECT ROWID_KH, %s FROM temp_orgfile",
        paste(DBI::dbQuoteIdentifier(con, recodecols),collapse = ", "))))
  
  print_console_message("\n* KODEBOK:")
  recodelog <- initiate_codebooklog(nrow = 0)
  for(col in recodecols){
    orgvalues <- unique(recode_dt[[col]])
    cb_subset <- codebook[FELTTYPE == col]
    recodelog <- do_recode_kb(dt = recode_dt, cb = cb_subset, col = col, log = recodelog)
    recodelog <- do_recode_regex(dt = recode_dt, cb = cb_subset, col = col, log = recodelog)
    recodelog <- do_list_unchanged_values(col = col, orgvalues = orgvalues, log = recodelog)
  }
  recodelog[, KOBLID := filedescription$KOBLID]
  n_recoded <- sum(as.numeric(recodelog$FREQ), na.rm = T)
  print_console_message("\n** Omkodet ", n_recoded, " verdier/celler", sep = "")
  update_codebooklog(codebooklog = codebooklog, recodelog = recodelog)
  
  if(n_recoded == 0) return(invisible(NULL))
  
  recode_dt[, kast := as.integer(rowSums(.SD == "-", na.rm = TRUE) > 0), .SDcols = recodecols]
  DBI::dbWriteTable(con, "temp_recode", recode_dt, overwrite = TRUE)
  update_recoded_cols_db(con = con, recodecols = recodecols)
  
  n_remove <- recode_dt[, sum(kast, na.rm = T)]
  if(n_remove > 0){
    print_console_message("\n** Kaster", n_remove, "slettede rader")
    DBI::dbExecute(con, "DELETE FROM temp_orgfile WHERE ROWID_KH IN (SELECT ROWID_KH FROM temp_recode WHERE kast = 1)")
  }
  
  invisible(NULL)
}

update_recoded_cols_db <- function(con, recodecols){
  set_clause <- paste(sprintf("%1$s = r.%1$s", as.character(DBI::dbQuoteIdentifier(con, recodecols))), collapse = ", ")
  sql <- sprintf("UPDATE temp_orgfile t SET %s FROM temp_recode r WHERE t.ROWID_KH = r.ROWID_KH",
                 set_clause)
  DBI::dbExecute(con, sql)
}

do_list_unchanged_values <- function(col, orgvalues, log){
  if(grepl("^VAL\\d{1}", col) | col == "GEO") return(log)
  unchanged <- setdiff(orgvalues, log$ORG)
  newlog <- initiate_codebooklog(nrow = length(unchanged))
  newlog[, let(FELTTYPE = col, TYPE = "IKKE_OMKODET", ORG = unchanged)]
  log <- data.table::rbindlist(list(log, newlog))
  return(log)
}

#' @title initiate_codebook_log
#' @description initiates an empty codebooklog
#' @noRd
initiate_codebooklog <- function(nrow = 0){
  columns <- c("KOBLID", "DELID",  "FELTTYPE", "TYPE", "ORG", "OMK", "FREQ")
  log <- data.table::setDT(as.list(setNames(rep(NA_character_, length(columns)), columns)))
  if(nrow == 0) return(log[0])
  return(log[1:nrow])
}

#' @title do_recode_kb
#' @description 
#' Recode data directly for KODEBOK entries of type "KB"
#' Codebook is subset to only use data actually existing in the data. 
#' @returns recoded data (by reference) and updated recode_log to be appended to 
#' complete recode log. 
#' @noRd
do_recode_kb <- function(dt, cb, col, log){
  cb <- cb[TYPE == "KB" & ORGKODE %in% unique(dt[[col]])]
  if(nrow(cb) == 0) return(log)
  newlog <- initiate_codebooklog(nrow = nrow(cb))
  newlog[, let(DELID = cb$DELID, FELTTYPE = col, TYPE = "KB", ORG = cb$ORGKODE, OMK = cb$NYKODE)]
  freq <- data.table::setnames(dt[dt[[col]] %in% cb$ORGKODE, .N, by = col], c("ORG", "FREQ"))
  newlog <- newlog[freq, on = "ORG", FREQ := i.FREQ]
  dt[cb, on = setNames("ORGKODE", col), (col) := i.NYKODE]
  log <- data.table::rbindlist(list(log, newlog))
  return(log)
}

#' @title do_recode_regex
#' @description 
#' Recode data using regex with sub() function to allow for regex also in the replacement value. 
#' For VALx-columns, only one line per regex is recorded in the log. 
#' For other columns, one line is recored per unique value that is recoded
#' @returns recoded data (by reference) and updated recode_log to be appended to 
#' complete recode log. 
#' @noRd
do_recode_regex <- function(dt, cb, col, log){
  cb <- cb[TYPE == "SUB"]
  if(nrow(cb) == 0) return(log)
  
  if(!grepl("^VAL\\d{1}", col)){
    newlog <- initiate_codebooklog()
    for(i in seq_len(nrow(cb))){
      cb_i <- cb[i]
      orgcodes_i <- unique(dt[grepl(cb_i$ORGKODE, dt[[col]])][[col]])
      newlog_i <- initiate_codebooklog(nrow = length(orgcodes_i))
      newlog_i[, let(DELID = cb_i$DELID, FELTTYPE = col, TYPE = "SUB", ORG = orgcodes_i)]
      newlog_i[, OMK := sub(cb_i$ORGKODE, cb_i$NYKODE, ORG, perl = TRUE)]
      newlog <- data.table::rbindlist(list(newlog, newlog_i))
    }
    
    freq <- data.table::setnames(dt[dt[[col]] %in% newlog$ORG, .N, by = col], c("ORG", "FREQ"))
    newlog <- newlog[freq, on = "ORG", FREQ := i.FREQ]
    translated_cb <- newlog[, .SD, .SDcols = c("ORG", "OMK")]
    dt[translated_cb, on = setNames("ORG", col), (col) := i.OMK]
  }
  
  if(grepl("^VAL\\d{1}", col)){
    newlog <- initiate_codebooklog(nrow = nrow(cb))
    newlog[, let(DELID = cb$DELID, FELTTYPE = col, TYPE = "SUB", ORG = cb$ORGKODE, OMK = cb$NYKODE)]
    for(i in seq_len(nrow(cb))){
      idx <- which(!is.na(dt[[col]]) & grepl(cb[i, ORGKODE], dt[[col]]))
      newlog[i, FREQ := dt[idx, .N]]
      data.table::set(dt, i = idx, j = col, value = sub(cb[i, ORGKODE], cb[i, NYKODE], dt[[col]][idx], perl = TRUE))
    }
  }
  log <- data.table::rbindlist(list(log, newlog))
  return(log)
}

#' @title do_remove_deleted_rows
#' @description
#' Delete rows recoded to "-" via codebook
#' @param dt data
#' @param cols cols affected by codebook
#' @noRd
do_remove_deleted_rows <- function(dt, cols){
  dt[, let(kast = 0)]
  dt[rowSums(dt[, ..cols] == "-", na.rm = T) > 0, let(kast = 1)]
  n_remove <- sum(dt$kast, na.rm = T)
  if(n_remove > 0) print_console_message("\n** Kaster", n_remove, "slettede rader")
  dt <- dt[kast == 0][, let(kast = NULL)]
  return(dt)
}

#' @title do_recode_tknr_db
#' @description Koder om TKNR-koder til kommunekoder via TKNR-tabellen i access
#' @noRd
do_recode_tknr_db <- function(tknr, parameters){
  if (is_empty(tknr) || tknr != "1") return(invisible(NULL))
  
  on.exit({
    invisible(DBI::dbExecute(parameters$duck, "DROP TABLE IF EXISTS temp_tknr"))
  }, add = TRUE)
  print_console_message("\n* Omkoder fra TKNR")
  DBI::dbWriteTable(parameters$duck, "temp_tknr", parameters$TKNR,overwrite = TRUE)
  
  DBI::dbExecute(parameters$duck,
    "UPDATE temp_orgfile AS t SET GEO = x.NYKODE FROM temp_tknr AS x WHERE t.GEO = x.ORGKODE AND x.NYKODE IS NOT NULL"
  )
  invisible(NULL)
}

#' @title do_recode_soner_4_db
#' @description Gjør 4-sifrede sonekoder om til 6-sifret ved å legge til 00
#' @noRd
do_recode_soner_4_db <- function(filedescription, con) {
  if(is_empty(filedescription$SONER) || !grepl("4", filedescription$SONER)) return(invisible(NULL))
  print_console_message("\n* Omkoder 4-sifrede GEO-koder til 6-sifret sonekode")
  DBI::dbExecute(con, "UPDATE temp_orgfile SET GEO = GEO || '00' WHERE length(GEO) = 4")
  invisible(NULL)
}





#' @title do_recode_tknr
#' @description
#' Recode data provided with TKNR using the TKNR table from ACCESS
#' @noRd
do_recode_tknr <- function(dt, tknr, parameters){
  if(is_empty(tknr) || tknr != "1") return(invisible(NULL))
  print_console_message("\n* Omkoder fra TKNR")
  dt[parameters$TKNR, on = c(GEO = "ORGKODE"), GEO := data.table::fifelse(!is.na(i.NYKODE), i.NYKODE, GEO)]
}

#' @title do_recode_soner_4
#' @description
#' Recode GEO-codes with length 4 to 6-digit codes by adding 00. 
#' @noRd
do_recode_soner_4 <- function(dt, filedescription){
  if(!grepl("4", filedescription$SONER)) return(invisible(NULL))
  print_console_message("\n* Omkoder 4-sifrede GEO-koder til 6-sifret sonekode")
  dt[nchar(GEO) == 4, let(GEO = paste0(GEO, "00"))]
}
