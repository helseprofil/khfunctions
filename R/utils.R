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
  data.table::setDTthreads(use)
  collapse::set_collapse(nthreads = use)
  cat("\n* Antall kjerner brukt:", use)
  
  return(list(dt = old_dt,
              collapse = old_collapse))
}

#' @title merge_cols_by_reference
#' @description
#' Adds columns from newdata to orgdata by reference
#' @keywords internal
#' @noRd
merge_cols_by_reference <- function(orgdata, newdata){
  commoncols <- intersect(
      get_dimension_columns(names(orgdata)),
      get_dimension_columns(names(newdata))
  )
  newcols_names <- setdiff(names(newdata), commoncols)
  
  dup_check <- newdata[, .N, by = commoncols][N > 1]
  
  if (nrow(dup_check) > 0) {
    stop(
      sprintf(
        "merge_cols_by_reference(): newdata har duplikate nøkler i commoncols (%s). Eksempel:\n%s",
        paste(commoncols, collapse = ", "),
        paste(utils::capture.output(print(head(dup_check))), collapse = "\n")
      ),
      call. = FALSE
    )
  }
  
  newcols_vals <- newdata[orgdata, on = commoncols, ..newcols_names]
  data.table::set(orgdata, j = newcols_names, value = newcols_vals)
}