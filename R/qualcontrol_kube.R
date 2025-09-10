#' @keywords internal
#' @noRd
control_cube_output <- function(outputlist, parameters){
  cat("\n\n---\n* Kvalitetskontroll:\n---")
  all_checks <- numeric()
  all_checks <- c(all_checks, control_censoring(dt = outputlist$QC, parameters = parameters))
  all_checks <- c(all_checks, control_standardization(dt = outputlist$ALLVIS, parameters = parameters))
  all_checks <- c(all_checks, control_aggregation(dt = outputlist$KUBE))
  control_meis_rate(dt = outputlist$KUBE, parameters = parameters)
  if(sum(all_checks) == 0){
    cat("\n\n---\n* Alle sjekker passert!\n---\n")
  } else {
    cat("\n\n---\n* Noen av sjekkene feilet, manuell kontroll nødvendig!\n---\n")
  }
}

#' @keywords internal
#' @noRd
control_censoring <- function(dt, parameters){
  cat("\n\n* Sjekker prikking (verdier under grensen):")
  if(is_empty(parameters$Censor_type)){ 
    cat("\n** Kube ikke prikket")
    return(0)
  }
  teller_ok <- teller_nevner_ok <- nevner_ok <- TRUE
  tellerlim <- parameters$CUBEinformation[[ifelse(parameters$Censor_type == "STATA", "Stata_PRIKK_T", "PRIKK_T")]]
  nevnerlim <- parameters$CUBEinformation[[ifelse(parameters$Censor_type == "STATA", "Stata_PRIKK_N", "PRIKK_N")]]
  is_tellerlim <- is_not_empty(tellerlim)
  is_nevnerlim <- is_not_empty(nevnerlim)
  
  if(is_tellerlim){
    teller_ok <- dt[sumTELLER_uprikk <= tellerlim, .SD, .SDcols = parameters$outvalues]
    teller_ok <- nrow(teller_ok) == 0 || all(is.na(teller_ok))
  }
  if(is_tellerlim & "sumTELLER_uprikk" %in% names(dt)){
    teller_nevner_ok <- dt[sumNEVNER_uprikk - sumTELLER_uprikk <= tellerlim, .SD, .SDcols = parameters$outvalues]
    teller_nevner_ok <- nrow(teller_nevner_ok) == 0 || all(is.na(teller_nevner_ok))
  } 
  if(is_nevnerlim){
    nevner_ok <- dt[sumNEVNER_uprikk <= nevnerlim, .SD, .SDcols = parameters$outvalues]
    nevner_ok <- nrow(nevner_ok) == 0 || all(is.na(nevner_ok))
  }
  checks <- setNames(c(teller_ok, teller_nevner_ok, nevner_ok), c("TELLER", "TELLER-NEVNER", "NEVNER"))
  
  if(all(checks)){
    cat("\n** Alle verdier under satte grenser, prikking OK!")
    return(0)
  }
  if(any(!checks)){
    notok <- paste(names(checks)[which(!checks)], collapse = ", ")
    cat("\n** OBS! noe er galt med prikkingen av: ", notok, ", IKKE OK", sep = "")
    return(1)
  }
}

control_standardization <- function(dt, parameters){
  cat("\n\n* Sjekker standardiseringsår")
  if(parameters$CUBEinformation$REFVERDI_VP != "P"){
    cat("\n** Kube ikke standardisert")
    return(0)
  }
  refverdi <- parameters$CUBEinformation$REFVERDI
  aarh <- as.numeric(sub("\\d{4}_(\\d{4})", "\\1", max(dt$AAR)))
  aarl <- as.numeric(sub("(\\d{4})_\\d{4}", "\\1", max(dt$AAR)))
  if(grepl("SISTE", refverdi, ignore.case = TRUE) || grepl(aarh, refverdi)){
    cat("\n** Standardisert mot siste år/periode, OK!")
    return(0)
  }
  
  cat("\n*** Kube ikke standardisert mot siste år, IKKE OK!")
  return(1)
}

control_aggregation <- function(dt){
  cat("\n\n* Sjekker aggregering mellom geonivå")
  n_teller <- dt[!is.na(sumTELLER), .N]
  n_nevner <- dt[!is.na(sumNEVNER), .N]
  if(n_teller == 0 && n_nevner == 0){
    cat("\n** Ingen ikke-missing teller/nevner i filen, skippet")
    return(0)
  }
  LF <- compare_geolevels(dt = dt, overcat = "L", undercat = "F")
  FK <- compare_geolevels(dt = dt, overcat = "F", undercat = "K")
  BK <- compare_kommune_bydel(dt = dt)
  comparisons <- LF + FK + BK
  if(comparisons > 0){
    cat("\n** Summen av lavere nivå er større enn høyere nivå, noe er feil med aggregeringen, IKKE OK!")
    return(1)
  }
  return(0)
}

compare_geolevels <- function(dt, overcat, undercat){
  if(!all(c(overcat, undercat) %in% unique(dt$GEOniv))) return(0)
  cat("\n** Sjekker", overcat, "vs", undercat)
  check_teller <- dt[GEOniv %in% c(overcat, undercat) & !is.na(sumTELLER), .N] > 0
  check_nevner <- dt[GEOniv %in% c(overcat, undercat) & !is.na(sumNEVNER), .N] > 0
  
  if(!check_teller && !check_nevner){
    cat("\n*** Ingen ikke-missing teller/nevner, sjekk skippet")
    return(0)
  }
  
  t_ok <- n_ok <- TRUE
  
  if(check_teller){
    sumt_overcat <- collapse::fsum(dt[GEOniv == overcat, sumTELLER])
    sumt_undercat <- collapse::fsum(dt[GEOniv == undercat, sumTELLER])
    t_ok <- sumt_overcat >= sumt_undercat
    if(t_ok) cat("\n*** Totalsummen (teller) av ", overcat, " er større eller lik ", undercat, ", OK!", sep = "")
    if(!t_ok) cat("\n*** Totalsummen (teller) av ", overcat, " er MINDRE enn ", undercat, ", IKKE OK!", sep = "")
  }
  
  if(check_nevner){
    sumn_overcat <- collapse::fsum(dt[GEOniv == overcat, sumNEVNER])
    sumn_undercat <- collapse::fsum(dt[GEOniv == undercat, sumNEVNER])
    n_ok <- sumn_overcat >= sumn_undercat
    if(t_ok) cat("\n*** Totalsummen (nevner) av ", overcat, " er større eller lik ", undercat, ", OK!", sep = "")
    if(!t_ok) cat("\n*** Totalsummen (nevner) av ", overcat, " er MINDRE enn ", undercat, ", IKKE OK!", sep = "")
  }

  if(t_ok && n_ok) return(0)
  return(1)
}

#' @keywords internal
#' @noRd
compare_kommune_bydel <- function(dt){
  if(!all(c("B", "K") %in% unique(dt$GEOniv))) return(0)
  byer <- c("0301", "1103", "4601", "5001")
  cols <- c("GEOniv", "GEO", "sumTELLER", "sumNEVNER")
  d <- dt[GEOniv %in% c("B", "K") & grepl(paste0("^", byer, collapse = "|"), GEO), .SD, .SDcols = cols]
  out <- compare_geolevels(dt = d, overcat = "K", undercat = "B")
  return(out)
}

#' @keywords internal
#' @noRd
control_meis_rate <- function(dt, parameters){
  if(parameters$CUBEinformation$REFVERDI_VP != "P") return(invisible(NULL))
  cat("\n\n* Sjekker forholdet mellom MEIS og RATE")
  cols <- c(parameters$outdimensions, "MEIS", "RATE")
  d <- dt[!is.na(MEIS), .SD, .SDcols = cols]
  
  for(dim in setdiff(parameters$outdimensions, c("GEO", "AAR"))){
    tot <- qualcontrol:::find_total(cube = d, dim = dim)
    if(!is.na(tot)) d <- d[get(dim) == tot]
    if(!is.na(tot) | length(unique(d[[dim]])) == 1) d[, names(.SD) := NULL, .SDcols = dim]
  }

  d[, let(diff = round(MEIS - RATE, 2), `ratio, %` = round(100*MEIS/RATE, 2))]
  cat("\n\n** Diff og ratio (%) på landsnivå per år:\n\n")
  d_country <- d[GEO == 0]
  print(d_country, nrows = nrow(d_country))
  
  cat("\n\n** 5 største (begge veier) diff og ratio (%) (ekskludert landstall):\n\n")
  d <- d[GEO != 0]
  print(d[order(`ratio, %`, decreasing = T)][c(1:5, seq(nrow(d)-4, nrow(d)))])
}
