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

#' @keywords internal
#' @noRd
get_value_columns <- function(columnnames, full = FALSE) {
  valcols <- grep("^(.*?)\\.f$", columnnames, value = T)
  valcols <- gsub("\\.f$", "", valcols)
  if(full) valcols <- paste0(rep(valcols, each = 7), c("", ".f", ".a", ".n", ".fn1", ".fn3", ".fn9"))
  return(intersect(columnnames, valcols))
}

#' @keywords internal
#' @noRd
get_dimension_columns <- function(columnnames) {
  nodim <- c(get_value_columns(columnnames, full = TRUE), "KOBLID", "ROW", "missyear", "spv_tmp")
  return(setdiff(columnnames, nodim))
  # dims <- c(getOption("khfunctions.alldimensions"), "TAB1", "TAB2", "TAB3")
  # return(intersect(columnnames, dims))
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
  print_console_message("\n* Antall kjerner brukt\n** data.table:", use_dt, "\n** collapse: ", use)
  
  return(list(dt = old_dt,
              collapse = old_collapse))
}

print_console_message <- function(...) {
  base::cat(..., "\n")
  utils::flush.console()
}
