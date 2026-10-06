# small functions used to check values, fetch names etc

#' @keywords internal
#' @noRd
is_not_empty <- function(value){
  !is.null(value) && !is.na(value) && value != ""
}

#' @keywords internal
#' @noRd
is_empty <- function(value){
  is.null(value) || is.na(value) || value == "" 
}

#' @title classify_columns
#' @description 
#' Klassifiserer kolonner som dimensjoner, verdier, hjelpekolonner og annet. 
#' For gruppering trengs ofte bare dimensjoner (ikke misc som GEOniv og FYLKE)
#' For aggregering må misc-kolonnene også bevares
#' @noRd
classify_columns <- function(columnnames){
  alldims <- c(
    getOption("khtools.dim.standard"),
    getOption("khtools.dim.interval"),
    getOption("khtools.dim.tab")
  )
  
  dims <- intersect(columnnames, alldims)
  f_cols <- grep("^(.*?)\\.f$", columnnames, value = T)
  vals <- gsub("\\.f$", "", f_cols)
  vals_helpers <- grep("^(.*?)\\.(f|a|n|fn1|fn3|fn9)$", columnnames, value = T)
  misc <- setdiff(columnnames, c(dims, vals, vals_helpers))
  
  list(dims = dims, vals = vals, vals_helpers = vals_helpers, misc = misc)
}

identify_dimensions <- function(columnnames){
  classify_columns(columnnames)$dims
}

identify_values <- function(columnnames, helpers = FALSE){
  x <- classify_columns(columnnames)
  if(!helpers) return(x$vals)
  c(x$vals, x$vals_helpers)
}

identify_nonvalues <- function(columnnames){
  x <- classify_columns(columnnames)
  c(x$dims, x$misc)
}

#' @title fix_befgk_spelling
#' @description Make sure BEF_GK is always read in with the same case (in access the spelling differs)
#' @keywords internal
#' @noRd
fix_befgk_spelling <- function(x){
  return(sub("BEF_GK", "BEF_GK", x, ignore.case = T))
}

ensure_utf8_encoding <- function(){
  old_ctype <- Sys.getlocale("LC_CTYPE")
  if(old_ctype != "nb-NO.UTF-8") Sys.setlocale("LC_ALL", "nb-NO.UTF-8")
  return(old_ctype)
}

set_threads <- function(){
  old_dt <- data.table::getDTthreads()
  old_collapse <- collapse::get_collapse("nthreads")
  use <- max(1L, min(6L, parallel::detectCores() %/% 2L))
  use_dt <- pmax(old_dt, use)
  data.table::setDTthreads(use_dt)
  collapse::set_collapse(nthreads = use)
  khtools::msg("* Antall kjerner brukt\n- data.table:", use_dt, "\n- collapse: ", use)
  
  return(list(dt = old_dt,
              collapse = old_collapse))
}
