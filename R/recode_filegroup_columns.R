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
  dt <- data.table::copy(dt)
  if("KODEBOKpre" %in% names(dumps)) DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "KODEBOKpre", sep = "_"), format = dumps[["KODEBOKpre"]])
  on.exit({if("KODEBOKpost" %in% names(dumps)) DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "KODEBOKpost", sep = "_"), format = dumps[["KODEBOKpost"]])}, add = TRUE)
  
  codebook <- parameters$codebook[DELID %in% c(filedescription$DELID, "FELLES")]
  recodecols <- unique(codebook$FELTTYPE)[unique(codebook$FELTTYPE) %in% names(dt)]
  if(nrow(codebook) == 0) return(dt)
  
  cat("\n* KODEBOK:")
  recodelog <- initiate_codebooklog(nrow = 0)
  for(col in recodecols){
    cb_subset <- codebook[FELTTYPE == col]
    recodelog <- do_recode_kb(dt = dt, cb = cb_subset, col = col, log = recodelog)
    recodelog <- do_recode_regex(dt = dt, cb = cb_subset, col = col, log = recodelog)
  }
  recodelog[, KOBLID := filedescription$KOBLID]
  n_recoded <- sum(as.numeric(recodelog$FREQ))
  cat("\n** Omkodet ", n_recoded, " verdier", sep = "")
  update_codebooklog(codebooklog = codebooklog, recodelog = recodelog)
  
  if(length(recodecols) > 0) dt <- do_remove_deleted_rows(dt = dt, cols = recodecols)
  return(dt)
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
  newlog[, let(DELID = cb$DELID, FELTTYPE = col, ORG = cb$ORGKODE, KBOMK = cb$NYKODE, OMK = cb$NYKODE)]
  freq <- data.table::setnames(dt[get(col) %in% cb$ORGKODE, .N, by = col], c("ORG", "FREQ"))
  newlog <- newlog[freq, on = "ORG", FREQ := i.FREQ]
  dt[cb, on = setNames("ORGKODE", col), (col) := data.table::fifelse(!is.na(i.NYKODE), i.NYKODE, get(col))]
  log <- rbindlist(list(log, newlog))
  return(log)
}

#' @title do_recode_regex
#' @description 
#' Recode data using regex with sub() function to allow for regex also in the replacement value. 
#' @returns recoded data (by reference) and updated recode_log to be appended to 
#' complete recode log. 
#' @noRd
do_recode_regex <- function(dt, cb, col, log){
  cb <- cb[TYPE == "SUB"]
  if(nrow(cb) == 0) return(log)
  newlog <- initiate_codebooklog(nrow = nrow(cb))
  newlog[, let(DELID = cb$DELID, FELTTYPE = col, ORG = cb$ORGKODE, KBOMK = cb$NYKODE, OMK = cb$NYKODE)]
  for(i in 1:nrow(cb)){
    newlog[i, FREQ := dt[grepl(cb[i, ORGKODE], get(col)), .N]]
    dt[grepl(cb[i, ORGKODE], get(col)), (col) := sub(cb[i, ORGKODE], cb[i, NYKODE], get(col), perl = TRUE)]
  }
  log <- rbindlist(list(log, newlog))
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
  if(n_remove > 0) cat("\n** Kaster", n_remove, "slettede rader")
  dt <- dt[kast == 0][, let(kast = NULL)]
  return(dt)
}

#' @title do_recode_tknr
#' @description
#' Recode data provided with TKNR using the TKNR table from ACCESS
#' @noRd
do_recode_tknr <- function(dt, tknr, parameters){
  if(is_empty(tknr) || tknr != "1") return(invisible(NULL))
  cat("\n* Omkoder fra TKNR")
  dt[parameters$TKNR, on = c(GEO = "ORGKODE"), GEO := data.table::fifelse(!is.na(i.NYKODE), i.NYKODE, GEO)]
}

#' @title do_recode_soner_4
#' @description
#' Recode GEO-codes with length 4 to 6-digit codes by adding 00. 
#' @noRd
do_recode_soner_4 <- function(dt, filedescription){
  if(!grepl("4", filedescription$SONER)) return(invisible(NULL))
  cat("\n* Omkoder 4-sifrede GEO-koder til 6-sifret sonekode")
  dt[nchar(GEO) == 4, let(GEO = paste0(GEO, "00"))]
}
