#' @title do_special_handling
#' @description
#' Do special handling of data files, either in R or STATA. 
#' code must be provided in specific files and specified in STATA to be used at certain points in data processing. 
#' 
#' @param name name of RSYNT point, refer to the column, to be used for filedump names
#' @param dt data to be processed 
#' @param code code to be performed, either R or STATA
#' @param parameters global parameters
#' @param koblid for RSYNT points applied to individual original files, koblid is needed for filedump names
do_special_handling <- function(name, dt, code, parameters, koblid = NULL){
  save_filedump_if_requested(dumpname = paste0(name, "pre"), dt = dt, parameters = parameters, koblid = koblid)
  on.exit({save_filedump_if_requested(dumpname = paste0(name, "post"), dt = dt, parameters = parameters, koblid = koblid)}, add = TRUE)
  
  is_code <- !is.null(code) && !is.na(code) && code != ""
  if(!is_code) return(dt)
  code <- gsub("\\\r", "\\\n", code)
  is_stata <- grepl("<STATA>", code)
  
  if(is_stata){
    cat("\n**Starter STATA-snutt:", name)
    code <- gsub("<STATA>[ \n]*(.*)", "\\1", code)
    dt <- do_stata_processing(dt = dt, script = code, parameters = parameters)
    cat("\n**Ferdig i STATA")
    return(dt)
  }
  
  cat("\n**Starter R-snutt:", name)
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
  cat("\n**R-snutt ferdig")
  return(dt)
}

#' @title do_stata_processing
#' @description
#' Fix column names not allowed in STATA. 
#' Save data to ..helseprofil/STATAtmp/
#' Generate .do-file based on STATA syntax passed to script
#' Start STATA and run .do-file with the .dta, overwrites .dta file.
#' Reads .log file to check for errors, function stops if any error occured.
#' Read, revert column name changes, and return processed file as data.table. 
#' @param TABLE data to be processed
#' @param script stata script
#' @param batchdate used to generate file names
#' @param stata_exe path to STATA program
do_stata_processing <- function(dt, script, parameters){
  on.exit(stata_processing_cleanup(statafiles = statafiles, orgwd = orgwd), add = T)
  tmpdir <- file.path(fs::path_home(), "helseprofil", "STATAtmp")
  if(!fs::dir_exists(tmpdir)) fs::dir_create(tmpdir)
  orgwd <- getwd()
  setwd(tmpdir)
  batchdate <- parameters$batchdate
  statafiles <- set_stata_filenames(batchdate = batchdate, tmpdir = tmpdir)
  charactercols <- names(dt)[sapply(dt, is.character)]
  cat("\n***Fikser kolonnenavn før stata")
  dt[, (charactercols) := lapply(.SD, function(x) ifelse(x == "", " ", x)), .SDcols = charactercols] 
  statanames <- fix_column_names_pre_stata(oldnames = names(dt))
  data.table::setnames(dt, statanames)
  cat("\n***Skriver STATA-fil")
  do_write_parquet(dt = dt, filepath = statafiles$parquet_out)
  cat("\n***Kjører STATA-script")
  generate_stata_do_file(script = script, statafiles = statafiles)
  run_stata_script(dofile = statafiles$do, stata_exe = parameters$StataExe)
  check_stata_log_for_error(statafiles = statafiles)
  cat("\n***Leser filen inn igjen og fikser kolonnenavn")
  dt <- data.table::copy(data.table::as.data.table(arrow::read_parquet(statafiles$parquet_in)))
  dt[, (charactercols) := lapply(.SD, function(x) ifelse(x == " ", "", x)), .SDcols = charactercols] 
  rnames <- fix_column_names_post_stata(oldnames = names(dt))
  data.table::setnames(dt, rnames)
  return(dt)
}



#' @descriptio Delete temporary data files and reset working directory
#' @keywords internal
#' @noRd
stata_processing_cleanup <- function(statafiles, orgwd){
  cat("\n***Sletter midlertidige datafiler")
  gc()
  file.remove(statafiles$parquet_in)
  file.remove(statafiles$parquet_out)
  setwd(orgwd)
}

#' @description 
#' Fix column names to comply with STATA rules. 
#' Some specific conversions are later reversed when data is read back into R.
#' Some general conversions are not reversed. 
#' @noRd
fix_column_names_pre_stata <- function(oldnames){
  fixednames <- oldnames
  fixednames <- gsub("^(\\d.*)$", "S_\\1", fixednames, perl = TRUE)
  fixednames <- gsub("^(.*)\\.(f|a|n|fn1|fn3|fn9)$", "\\1_\\2", fixednames)
  fixednames <- gsub("[^[:alnum:]_æÆøØåÅ]", "_", fixednames, perl = TRUE)
  fixednames <- gsub("^(?![a-zA-Z_])(.*)", "_\\1", fixednames, perl = TRUE)
  fixednames <- gsub("^_+$", "var", fixednames)
  fixednames <- substr(fixednames, 1, 32)
  return(fixednames)
}

#' @noRd
fix_column_names_post_stata <- function(oldnames){
  fixednames <- oldnames
  fixednames <- gsub("^S_(\\d.*)$", "\\1", fixednames)
  fixednames <- gsub("^(.*)_(f|a|n|fn1|fn3|fn9)$", "\\1.\\2", fixednames)
  return(fixednames)
}

#' @noRd
set_stata_filenames <- function(batchdate, tmpdir){
  statafiles <- list()
  statafiles[["do"]] <-  file.path(tmpdir, paste0("STATAtmp_", batchdate, ".do"))
  statafiles[["parquet_out"]] <- file.path(tmpdir, paste0("STATAtmp_out", batchdate, ".parquet"))
  statafiles[["parquet_in"]] <- file.path(tmpdir, paste0("STATAtmp_in", batchdate, ".parquet"))
  statafiles[["log"]] <- file.path(tmpdir, paste0("STATAtmp_", batchdate, ".log"))
  return(statafiles)
}

#' @noRd
generate_stata_do_file <- function(script, statafiles){
  sink(statafiles$do)
  cat("net install pq, from(http://fmwww.bc.edu/RePEc/bocode/p)\n") 
  cat("pq use using ", statafiles$parquet_out, "\n", sep = "")
  cat(script, "\n")
  cat("pq save using ", statafiles$parquet_in, ", replace\n", sep = "")
  sink()
}

#' @noRd
run_stata_script <- function(dofile, stata_exe){
  call <- paste("\"", stata_exe, "\" /e do ", dofile, " \n", sep = "")
  system(call, intern = TRUE)
}

#' @noRd
check_stata_log_for_error <- function(statafiles){
  log <- readLines(statafiles$log)
  if(log[length(log)] != "end of do-file") {
    log_start <- which(grepl(paste("do", statafiles$do), log))
    feil <- paste(log[log_start:length(log)], collapse = "\n")
    stop("Something went wrong in STATA", feil, sep = "\n")
  }
  return(invisible(NULL))
}


