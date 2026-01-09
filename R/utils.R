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