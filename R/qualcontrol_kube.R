#' @keywords internal
#' @noRd
control_cube_output <- function(dt, parameters){
  cat("\n\n---\n* Kvalitetskontroll:")
  censoring <- control_censoring(dt = dt, parameters = parameters)
}

#' @keywords internal
#' @noRd
control_censoring <- function(dt, parameters){
  cat("\n** Sjekker prikking (verdier under grensen):")
  if(is_empty(parameters$Censor_type)){
    cat("\n*** Kube ikke prikket, sjekk skippet")
    return(0)
  }
  teller_ok <- teller_nevner_ok <- nevner_ok <- TRUE
  tellerlim <- parameters$CUBEinformation[[ifelse(parameters$Censor_type == "STATA", "Stata_PRIKK_T", "PRIKK_T")]]
  nevnerlim <- parameters$CUBEinformation[[ifelse(parameters$Censor_type == "STATA", "Stata_PRIKK_N", "PRIKK_N")]]
  is_tellerlim <- is_not_empty(tellerlim)
  is_nevnerlim <- is_not_empty(nevnerlim)
  
  if(is_tellerlim) teller_ok <- unique(rowSums(dt[sumTELLER_uprikk <= tellerlim, .SD, .SDcols = parameters$outvalues], na.rm = T)) == 0 
  if(is_tellerlim & "sumTELLER_uprikk" %in% names(dt)) teller_nevner_ok <- unique(rowSums(dt[sumNEVNER_uprikk - sumTELLER_uprikk <= tellerlim, .SD, .SDcols = parameters$outvalues], na.rm = T)) == 0
  if(is_nevnerlim) nevner_ok <- unique(rowSums(dt[sumNEVNER_uprikk <= nevnerlim, .SD, .SDcols = parameters$outvalues], na.rm = T)) == 0
  checks <- setNames(c(teller_ok, teller_nevner_ok, nevner_ok), c("TELLER", "TELLER-NEVNER", "NEVNER"))
  
  if(all(checks)){
    cat("\n*** Prikking ok, alle verdier under satte grenser!")
    return(0)
  }
  if(any(!checks)){
    notok <- paste(names(checks)[which(!checks)], collapse = ", ")
    cat("\n*** OBS! prikking ikke ok for: ", notok)
    return(1)
  }
}
