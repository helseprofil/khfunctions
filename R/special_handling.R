#' @title do_special_handling
#' @description
#' Do special handling of data files, either in R or STATA. 
#' code must be provided in specific files and specified in STATA to be used at certain points in data processing. 
#' 
#' @param dt data to be processed 
#' @param code code to be performed, either R or STATA
#' @param batchdate batchdate, used to create filepaths for stata processing
#' @param globs global parameters
do_special_handling <- function(dt, code, batchdate, stata_exe, DTout = TRUE){
  is_code <- !is.null(code) && !is.na(code) && code != ""
  if(!is_code) return(dt)
  code <- gsub("\\\r", "\\\n", code)
  is_stata <- grepl("<STATA>", code)
  
  if(is_stata){
    code <- gsub("<STATA>[ \n]*(.*)", "\\1", code)
    dt <- do_stata_processing(TABLE = dt, script = code, batchdate = batchdate, stata_exe = stata_exe, DTout = DTout)
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
do_stata_processing <- function(TABLE, script, batchdate = SettKHBatchDate(), stata_exe, DTout = TRUE) {
  tmpdir <- file.path(fs::path_home(), "helseprofil", "STATAtmp")
  if(!fs::dir_exists(tmpdir)) fs::dir_create(tmpdir)
  orgwd <- getwd()
  setwd(tmpdir)
  statafiles <- set_stata_filenames(batchdate = batchdate, tmpdir = tmpdir)
  TABLE <- fix_column_names_pre_stata(TABLE = TABLE)
  haven::write_dta(TABLE, statafiles$dta)
  on.exit(file.remove(statafiles$dta), add = T)
  generate_stata_do_file(script = script, statafiles = statafiles)
  run_stata_script(dofile = statafiles$do, stata_exe = stata_exe)
  check_stata_log_for_error(statafiles = statafiles)
  TABLE <- haven::read_dta(statafiles$dta)
  TABLE <- fix_column_names_post_stata(TABLE = TABLE)
  if(DTout) data.table::setDT(TABLE)
  setwd(orgwd)
  return(TABLE)
}

#' @description 
#' Fix column names to comply with STATA rules. 
#' Some specific conversions are later reversed when data is read back into R.
#' Some general conversions are not reversed. 
#' @noRd
fix_column_names_pre_stata <- function(TABLE){
  TABLE[TABLE == ""] <- " " 
  names(TABLE) <- gsub("^(\\d.*)$", "S_\\1", names(TABLE))
  names(TABLE) <- gsub("^(.*)\\.(f|a|n|fn1|fn3|fn9)$", "\\1_\\2", names(TABLE))
  
  names(TABLE) <- gsub("[^a-zA-Z0-9_æÆøØåÅ]", "_", names(TABLE))
  names(TABLE) <- gsub("^(?![a-zA-Z_])(.*)", "_\\1", names(TABLE), perl = TRUE)
  names(TABLE) <- gsub("^_+$", "var", names(TABLE))
  names(TABLE) <- substr(names(TABLE), 1, 32)
  return(TABLE)
}

#' @noRd
fix_column_names_post_stata <- function(TABLE){
  TABLE[TABLE == " "] <- ""
  names(TABLE) <- gsub("^S_(\\d.*)$", "\\1", names(TABLE))
  names(TABLE) <- gsub("^(.*)_(f|a|n|fn1|fn3|fn9)$", "\\1.\\2", names(TABLE))
  return(TABLE)
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


