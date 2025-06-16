# Helper functions used in the data processing, both in LagKUBE and LagFilgruppe

#' @title expand.grid.dt
#' @description Takes a list of data.tables and generate a data.table with all combinations
#' @keywords internal
#' @noRd
expand.grid.dt <- function(...){
  DTs <- list(...)
  if(length(DTs) == 0) stop("No tables (empty list) passed to expand.grid.dt")
  for(i in seq_along(DTs)){
    if(!is(DTs[[i]], "data.table")) DTs[[i]] <- data.table::setDT(DTs[[i]])
  }
  
  rows <- do.call(data.table::CJ, lapply(DTs, function(x) seq(nrow(x))))
  
  for(i in seq_along(DTs)) DTs[[i]] <- DTs[[i]][rows[[i]]]
  
  res <- DTs[[1L]]
  if(length(DTs) > 1){
    for(i in 2:length(DTs)){
      res[, names(DTs[[i]]) := DTs[[i]]]
    }
  }
  
  rm(DTs, rows)
  invisible(gc())
  return(res)
}

#' @title set_implicit_null_after_merge (kb)
#' @description
#' Fixing implicit 0 occurring after merging, using information from VALXmiss in access. 
#' Previous name SettMergeNAs
#' @keywords internal
#' @noRd
set_implicit_null_after_merge <- function(file, implicitnull_defs = list()) {
  cat("\n*** Håndterer implisitte nuller")
  vals <- get_value_columns(names(file))
  
  for (val in vals) {
    if (val %in% names(implicitnull_defs)) {
      VALmiss <- implicitnull_defs[[val]]$miss
      replacemissing <- list()
      if(VALmiss == "..") replacemissing <- list(0, 1, 1)
      if(VALmiss == ".") replacemissing <- list(0, 2, 1)
      if(VALmiss == ":") replacemissing <- list(0, 3, 1)
      if(!grepl("\\D", VALmiss)) replacemissing <- list(as.numeric(VALmiss), 0, 1)
      if(length(replacemissing) == 0) stop(val, " listed in VALXnavn, but VALXmiss is not '..', '.', ':', or numeric")
    } else {
      replacemissing <- list(0, 0, 1)
    }
    
    valF <- paste0(val, ".f")
    valA <- paste0(val, ".a")
    missingrows <- which((is.na(file[[val]]) & file[[valF]] == 0) | is.na(file[[valF]]))
    n_missing <- length(missingrows)
    if(n_missing > 0) cat("\n - Setter", val, "=", replacemissing[[1]], "and", valF, "=", replacemissing[[2]], "for",  n_missing, "rader")
    file[missingrows, names(.SD) := replacemissing, .SDcols = c(val, valF, valA)]
  }
  return(file)
}


## OLD, to be replaced or killed ----

#' KHerr (kb)
KHerr <- function(error) {
  cat(
    "***************************************************************************\n",
    "*KHFEIL!! ", error, "\n***************************************************************************\n"
  )
}

#' FinnFilGruppeFraKoblid (kb)
#'
#' @param koblid 
#' @param globs global parameters, defaults to SettGlobs
FinnFilGruppeFraKoblid <- function(koblid, globs = get_global_parameters()) {
  return(as.character(sqlQuery(globs$dbh, paste("SELECT FILGRUPPE FROM ORGINNLESkobl WHERE KOBLID=", koblid, sep = ""), stringsAsFactors = FALSE)))
}

#' SkrivKBLogg (kb)
#'
#' @param KB 
#' @param type 
#' @param filbesk 
#' @param gruppe 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
SkrivKBLogg <- function(KB, type, filbesk, gruppe, batchdate = SettKHBatchDate(), globs = get_global_parameters()) {
  sqlQuery(globs$log, paste("DELETE * FROM KODEBOK_LOGG WHERE KOBLID=", filbesk$KOBLID, " AND TYPE='", type, "' AND SV='S'", sep = ""))
  sqlSave(globs$log, cbind(KOBLID = filbesk$KOBLID, FILGRUPPE = gruppe, FELTTYPE = type, SV = "S", KB[, c("ORG", "KBOMK", "OMK", "FREQ", "OK")], BATCHDATE = batchdate), "KODEBOK_LOGG", rownames = FALSE, append = TRUE)
}

#' SVcloneRecord (kb)
#'
#' @param dbh 
#' @param table 
#' @param koblid 
SVcloneRecord <- function(dbh, table, koblid) {
  design <- names(sqlQuery(dbh, paste("SELECT * FROM ", table, " WHERE KOBLID=-1", sep = "")))
  felt <- paste(design, collapse = ",")
  feltm <- sub("SV", "'V' AS SV", felt)
  sql <- paste(
    "INSERT INTO ", table, "(", felt, ")",
    "SELECT ", feltm, "FROM ", table,
    "WHERE KOBLID=", koblid, "AND SV='S'"
  )
  sqlQuery(dbh, sql)
}

#' TilFilLogg (kb)
#'
#' @param koblid 
#' @param felt 
#' @param verdi 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
TilFilLogg <- function(koblid, felt, verdi, batchdate = SettKHBatchDate(), globs = get_global_parameters()) {
  # Sjekk om finnes rad for filid, eller lag ny
  if (nrow(sqlQuery(globs$log, paste("SELECT * FROM INNLES_LOGG WHERE KOBLID=", koblid, " AND SV='S' AND BATCH='", batchdate, "'", sep = ""))) == 0) {
    print("**************Hvorfor er jeg egentlig her?*********************'")
    sqlQuery(globs$log, paste("DELETE * FROM INNLES_LOGG WHERE KOBLID=", koblid, "AND SV='S'", sep = ""))
    upd <- paste("INSERT INTO INNLES_LOGG ( KOBLID, BATCH, SV, FILGRUPPE ) SELECT=", koblid, ",'", batchdate, "', 'S',", FinnFilGruppeFraKoblid(koblid, globs = globs), sep = "")
    sqlQuery(globs$log, upd)
  }
  if (is.character(verdi)) {
    verdi <- paste("'", verdi, "'", sep = "")
    verdi <- gsub("\\n", "' & Chr(13) & Chr(10) & '", verdi) # Veldig saer \n i Access!
  }
  upd <- paste("UPDATE INNLES_LOGG SET ", felt, "=", verdi, " WHERE KOBLID=", koblid, " AND SV='S' AND BATCH='", batchdate, "'", sep = "")
  tmp <- sqlQuery(globs$log, upd)
  # cat("********\n",tmp,"__________\n")
}

## Try to handle problem with "memory exhausted (limit reached?)" the solution above
#' expand.grid.df (ybk)
#'
#' @param ... 
expand.grid.df <- function(...) {
  DFs <- list(...)
  
  ddt <- lapply(DFs, function(x) is(x, "data.table"))
  dx <- which(ddt == 0)
  
  if (length(dx) > 0){
    for (i in dx){
      DFs[[i]] <- data.table::as.data.table(DFs[[i]])
    }
  }
  
  rows <- do.call(data.table::CJ, lapply(DFs, function(x) seq(nrow(x))))
  
  for (i in seq_along(DFs))
    names(DFs)[i] <- paste0("data", i)
  
  DFlength <- length(DFs)
  DFnames <- names(DFs)
  
  res <- DFs[[1L]][rows[[1L]]]
  DFs[[1L]] <- NULL
  for (i in seq_len(DFlength)[-1L]){
    x <- DFnames[i]
    res <- res[, c(.SD, DFs[[x]][rows[[i]]])]
    DFs[[x]] <- NULL
  }
  
  rm(DFs, rows)
  gc()
  data.table::setDT(res)
}
