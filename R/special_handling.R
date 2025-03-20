do_special_handling <- function(dt, code, batchdate, globs){
  is_code <- !is.null(code) && !is.na(code) && code != ""
  if(!is_code) return(dt)
  code <- gsub("\\\r", "\\\n", code)
  is_stata <- grepl("<STATA>", code)
  
  if(is_stata){
    code <- gsub("<STATA>[ \n]*(.*)", "\\1", code)
    RES <- KjorStataSkript(TABLE = dt, script = code, tableTYP = "DT", batchdate = batchdate, globs = globs)
    if (RES$feil != "") stop("Something went wrong in STATA", RES$feil, sep = "\n")
    return(RES$TABLE)
  }
  
  code_env <- new.env()
  dtname <- as.character(substitute(dt))
  assign(dtname, dt, envir = code_env)
  newdt <- get(dtname, envir = code_env)
  rsynterr <- try(eval(parse(text = code), envir = code_env), silent = TRUE)
  if ("try-error" %in% class(rsynterr)) {
    print(rsynterr)
    stop("Something went wrong in R")
  }
  assign(dtname, get(dtname, envir = code_env), envir = code_env)
  dt <- get(dtname, envir = code_env)
  return(dt)
}

#' KjorStataScript (kb, ybk?)
#'
#' @param TABLE 
#' @param script 
#' @param tableTYP 
#' @param batchdate 
#' @param globs
KjorStataSkript <- function(TABLE, script, tableTYP = "DF", batchdate = SettKHBatchDate(), globs = SettGlobs()) {
  is_kh_debug()
  
  tmpdir <- file.path(fs::path_home(), "helseprofil", "STATAtmp")
  if(!fs::dir_exists(tmpdir)) fs::dir_create(tmpdir)
  wdOrg <- getwd()
  setwd(tmpdir)
  tmpdo <- paste("STATAtmp_", batchdate, ".do", sep = "")
  tmpdta <- paste("STATAtmp_", batchdate, ".dta", sep = "")
  tmplog <- paste("STATAtmp_", batchdate, ".log", sep = "")
  TABLE[TABLE == ""] <- " " # STATA stÃƒÂ¸tter ikke "empty-string"
  names(TABLE) <- gsub("^(\\d.*)$", "S_\\1", names(TABLE)) # replace numeric column names
  names(TABLE) <- gsub("^(.*)\\.([afn].*)$", "\\1_\\2", names(TABLE)) # Endre .a, .f, .n og .fn1/3/9 til _
  # haven::write_dta(TABLE, tmpdta)
  foreign::write.dta(TABLE, tmpdta)
  
  sink(tmpdo)
  cat("use ", tmpdta, "\n", sep = "")
  cat(script, "\n")
  
  if (globs$StataVers < 12) {
    cat("save ", tmpdta, ",  replace\n", sep = "")
  } else if (globs$StataVers %in% 12:13) {
    cat("saveold ", tmpdta, ", replace\n", sep = "")
  } else {
    cat("saveold ", tmpdta, ", version(11) replace\n", sep = "")
  }
  sink()
  statacall <- paste("\"", globs$StataExe, "\" /e do ", tmpdo, " \n", sep = "")
  system(statacall, intern = TRUE)
  log <- readLines(tmplog)
  feil <- ""
  if (log[length(log)] != "end of do-file") {
    log_start <- which(grepl(paste("do", tmpdo), log))
    feil <- paste(log[log_start:length(log)], collapse = "\n")
  } else {
    # Byttet fra foreign::read.dta fordi den ikke klarte å lese inn norske tegn
    # Satte opprinnelig encoding = "UTF-8", men dette krasjet for en SYSVAK-originalfil. 
    # Usikker på om det er nødvendig å spesifisere encoding, det ser ut til å fungere fint uten. 
    TABLE <- haven::read_dta(tmpdta)
  }
  # Reverserer omforminger for aa kunne skrive til STATA
  TABLE[TABLE == " "] <- ""
  names(TABLE) <- gsub("^S_(\\d.*)$", "\\1", names(TABLE))
  names(TABLE) <- gsub("^(.*)_([afn].*)$", "\\1.\\2", names(TABLE)) # Endre _a, _f, _n og _fn1/3/9 til .
  
  # delete data file
  file.remove(tmpdta)
  setwd(wdOrg)
  if (tableTYP == "DT") {
    data.table::setDT(TABLE)
  }
  return(list(TABLE = TABLE, feil = feil))
}
