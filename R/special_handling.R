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
    cat("\n** Starter STATA-snutt:", name)
    code <- gsub("<STATA>[ \n]*(.*)", "\\1", code)
    dt <- do_stata_processing(dt = dt, script = code, parameters = parameters)
    cat("\n** Ferdig i STATA")
    return(dt)
  }
  
  cat("\n** Starter R-snutt:", name)
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
  cat("\n** R-snutt ferdig")
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
  use_parquet <- can_use_parquet(dt = dt)
  statafiles <- set_stata_filenames(batchdate = batchdate, tmpdir = tmpdir)
  charactercols <- names(dt)[sapply(dt, is.character)]
  dt[, (charactercols) := lapply(.SD, function(x) ifelse(x == "", " ", x)), .SDcols = charactercols] 
  statanames <- fix_column_names_pre_stata(oldnames = names(dt))
  data.table::setnames(dt, statanames)
  
  do_write_stata_file(dt = dt, statafiles = statafiles, use_parquet = use_parquet)
  generate_stata_do_file(script = script, statafiles = statafiles, use_parquet = use_parquet)
  run_stata_script(dofile = statafiles$do, stata_exe = parameters$StataExe)
  check_stata_log_for_error(statafiles = statafiles)
  dt <- do_read_stata_file(statafiles = statafiles, use_parquet = use_parquet)
  
  charactercols <- names(dt)[sapply(dt, is.character)]
  dt[, (charactercols) := lapply(.SD, function(x) ifelse(x == " ", "", x)), .SDcols = charactercols] 
  rnames <- fix_column_names_post_stata(oldnames = names(dt))
  data.table::setnames(dt, rnames)
  return(dt)
}

#' @descriptio Delete temporary data files and reset working directory
#' @keywords internal
#' @noRd
stata_processing_cleanup <- function(statafiles, orgwd){
  cat("\n*** Sletter midlertidige datafiler")
  gc()
  for(file in c("parquet_in", "parquet_out", "dta")){
    path <- statafiles[[file]]
    if(file.exists(path)) file.remove(path)
  }
  setwd(orgwd)
}

#' @description 
#' Fix column names to comply with STATA rules. 
#' Some specific conversions are later reversed when data is read back into R.
#' Some general conversions are not reversed. 
#' @noRd
fix_column_names_pre_stata <- function(oldnames){
  cat("\n*** Fikser kolonnenavn før stata")
  fixednames <- oldnames
  fixednames <- gsub("^(\\d.*)$", "S_\\1", fixednames, perl = TRUE)
  fixednames <- gsub("^(.*)\\.(f|a|n|fn1|fn3|fn9)$", "\\1_\\2", fixednames)
  fixednames <- gsub("[^[:alnum:]_\u00c6\u00d8\u00c5\u00e6\u00f8\u00e5]", "_", fixednames, perl = TRUE)
  fixednames <- gsub("^(?![a-zA-Z_])(.*)", "_\\1", fixednames, perl = TRUE)
  fixednames <- gsub("^_+$", "var", fixednames)
  fixednames <- substr(fixednames, 1, 32)
  return(fixednames)
}

#' @noRd
fix_column_names_post_stata <- function(oldnames){
  cat("\n*** Leser filen inn igjen og fikser kolonnenavn")
  fixednames <- oldnames
  fixednames <- gsub("^S_(\\d.*)$", "\\1", fixednames)
  fixednames <- gsub("^(.*)_(f|a|n|fn1|fn3|fn9)$", "\\1.\\2", fixednames)
  return(fixednames)
}

#' @title can_use_parquet
#' @description
#' If the longest value of a character column contain [æøå], represented by unicode sequences.
#' STATA will crop the value when reading a .parquet file, as these characters are multibyte. 
#' This function returns TRUE if this is not the case, indicating that .parquet can be used.
#' If the longest value of any column contain [æøå], FALSE is returned, indicating that .dta-format should be used. 
#' @param dt data
#' @keywords internal
#' @noRd
can_use_parquet <- function(dt){
  out <- TRUE
  charcols <- names(dt)[sapply(dt, is.character)]
  i <- 1
  while(out && i <= length(charcols)){
    vals <- collapse::funique(dt[[charcols[i]]])
    vals <- vals[nchar(vals) == collapse::fmax(nchar(vals))]
    if(any(grepl("[\u00c6\u00d8\u00c5\u00e6\u00f8\u00e5]", vals))){
      out <- FALSE
    }
    i <- i + 1
  }
  return(out)
}

#' @noRd
set_stata_filenames <- function(batchdate, tmpdir){
  statafiles <- list()
  statafiles[["do"]] <-  file.path(tmpdir, paste0("STATAtmp_", batchdate, ".do"))
  statafiles[["dta"]] <- file.path(tmpdir, paste0("STATAtmp_", batchdate, ".dta"))
  statafiles[["log"]] <- file.path(tmpdir, paste0("STATAtmp_", batchdate, ".log"))
  statafiles[["parquet_out"]] <- file.path(tmpdir, paste0("STATAtmp_out", batchdate, ".parquet"))
  statafiles[["parquet_in"]] <- file.path(tmpdir, paste0("STATAtmp_in", batchdate, ".parquet"))
  return(statafiles)
}

#' @title do_write_stata_file
#' @description
#' Write file to be processed by stata.
#' If use_parquet, the statafiles$parquet_out file is written. If not, the statafiles$dta file is used.
#' @param dt data
#' @param statafiles list of paths
#' @param use_parquet TRUE/FALSE
#' @keywords internal
#' @noRd
do_write_stata_file <- function(dt, statafiles, use_parquet){
  cat("\n*** Skriver STATA-fil")
  if(use_parquet){
    do_write_parquet(dt = dt, filepath = statafiles$parquet_out)
  } else {
    haven::write_dta(dt, statafiles$dta)
  }
}

#' @title do_read_stata_file
#' @description
#' Reads in file after stata processing. If use_parquet, the statafiles$parquet_in file is read, or the statafiles$dta file is read.
#' @param statafiles list of paths
#' @param use_parquet TRUE/FALSE
#' @keywords internal
#' @noRd
do_read_stata_file <- function(statafiles, use_parquet){
  if(use_parquet){
    dt <- data.table::copy(data.table::as.data.table(arrow::read_parquet(statafiles$parquet_in)))
  } else {
    dt <- data.table::setDT(haven::read_dta(statafiles$dta))
  }
  return(dt)
}

#' @noRd
generate_stata_do_file <- function(script, statafiles, use_parquet){
  if(use_parquet){
    sink(statafiles$do)
    cat("net install pq, from(http://fmwww.bc.edu/RePEc/bocode/p)\n") 
    cat("pq use using ", statafiles$parquet_out, "\n", sep = "")
    cat(script, "\n")
    cat("pq save using ", statafiles$parquet_in, ", replace\n", sep = "")
    sink()
  } else {
    sink(statafiles$do)
    cat("use ", statafiles$dta, "\n", sep = "")
    cat(script, "\n")
    cat("save ", statafiles$dta, ", replace\n", sep = "")
    sink()
  }
}

#' @noRd
run_stata_script <- function(dofile, stata_exe){
  cat("\n*** Kjører STATA-script...")
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

