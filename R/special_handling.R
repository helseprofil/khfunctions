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
    code <- gsub("<STATA>[ \n]*(.*)", "\\1", code)
    dt <- do_stata_processing(dt = dt, script = code, parameters = parameters)
    return(dt)
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
  tmpdir <- file.path(fs::path_home(), "helseprofil", "STATAtmp")
  if(!fs::dir_exists(tmpdir)) fs::dir_create(tmpdir)
  batchdate <- parameters$batchdate
  orgwd <- getwd()
  setwd(tmpdir)
  statafiles <- set_stata_filenames(batchdate = batchdate, tmpdir = tmpdir)
  charactercols <- names(dt)[sapply(dt, is.character)]
  dt[, (charactercols) := lapply(.SD, function(x) ifelse(x == "", " ", x)), .SDcols = charactercols] 
  fix_column_names_pre_stata(dt = dt)
  haven::write_dta(dt, statafiles$dta)
  on.exit(file.remove(statafiles$dta), add = T)
  generate_stata_do_file(script = script, statafiles = statafiles)
  run_stata_script(dofile = statafiles$do, stata_exe = parameters$StataExe)
  check_stata_log_for_error(statafiles = statafiles)
  dt <- data.table::setDT(haven::read_dta(statafiles$dta))
  dt[, (charactercols) := lapply(.SD, function(x) ifelse(x == " ", "", x)), .SDcols = charactercols] 
  fix_column_names_post_stata(dt = dt)
  # if(!is(dt, "data.table")) data.table::setDT(dt)
  setwd(orgwd)
  return(dt)
}

#' @description 
#' Fix column names to comply with STATA rules. 
#' Some specific conversions are later reversed when data is read back into R.
#' Some general conversions are not reversed. 
#' @noRd
fix_column_names_pre_stata <- function(dt){
  fixednames <- names(dt)
  fixednames <- gsub("^(\\d.*)$", "S_\\1", fixednames)
  fixednames <- gsub("^(.*)\\.(f|a|n|fn1|fn3|fn9)$", "\\1_\\2", fixednames)
  
  fixednames <- gsub("[^a-zA-Z0-9_æÆøØåÅ]", "_", fixednames)
  fixednames <- gsub("^(?![a-zA-Z_])(.*)", "_\\1", fixednames, perl = TRUE)
  fixednames <- gsub("^_+$", "var", fixednames)
  fixednames <- substr(fixednames, 1, 32)
  data.table::setnames(dt, old = names(dt), new = fixednames)
}

#' @noRd
fix_column_names_post_stata <- function(dt){
  fixednames <- names(dt)
  fixednames <- gsub("^S_(\\d.*)$", "\\1", fixednames)
  fixednames <- gsub("^(.*)_(f|a|n|fn1|fn3|fn9)$", "\\1.\\2", fixednames)
  data.table::setnames(dt, fixednames)
}

#' @noRd
set_stata_filenames <- function(batchdate, tmpdir){
  statafiles <- list()
  statafiles[["do"]] <-  file.path(tmpdir, paste0("STATAtmp_", batchdate, ".do"))
  statafiles[["dta"]] <- file.path(tmpdir, paste0("STATAtmp_", batchdate, ".dta"))
  statafiles[["log"]] <- file.path(tmpdir, paste0("STATAtmp_", batchdate, ".log"))
  return(statafiles)
}

#' @noRd
generate_stata_do_file <- function(script, statafiles){
  sink(statafiles$do)
  cat("use ", statafiles$dta, "\n", sep = "")
  cat(script, "\n")
  cat("save ", statafiles$dta, ", replace\n", sep = "")
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


