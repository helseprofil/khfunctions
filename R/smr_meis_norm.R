#' @keywords internal
#' @noRd
add_smr_and_meis <- function(dt, parameters){
  if(parameters$PredFilter$ref_year_type == "Specific") calculate_smr_and_meis(dt = dt, parameters = parameters)
  if(parameters$PredFilter$ref_year_type == "Moving") calculate_smrtmp(dt = dt, parameters = parameters)
}

#' @keywords internal
#' @noRd
calculate_smr_and_meis <- function(dt, parameters){
  if(parameters$CUBEinformation$REFVERDI_VP == "P"){
    dt[, let(SMR = sumTELLER / sumPREDTELLER * 100,
             MEIS = sumTELLER / sumPREDTELLER * MEISskala)]
    dt[sumPREDTELLER == 0, let(SMR = NA, MEIS = NA)]
  }
}

#' @keywords internal
#' @noRd
calculate_smrtmp <- function(dt, parameters){
  dt[, let(SMRtmp = NA_real_)]
  if(parameters$CUBEinformation$REFVERDI_VP == "P") dt[, let(SMRtmp = sumTELLER / sumPREDTELLER * 100)]
  if(parameters$CUBEinformation$REFVERDI_VP == "V") dt[, let(SMRtmp = 100)]
}

#' @title adjust_smr_and_meis_to_country_normal
#' @description Find the subset to estimate country-level normals, 
#' and adjusts SMR and MEIS to make sure country always have SMR = 100
#' @keywords internal
#' @noRd
adjust_smr_and_meis_to_country_normal <- function(dt, parameters){
  normsubset <- get_normsubset(dt = dt, parameters = parameters)
  commondims <- intersect(names(normsubset), names(dt))
  dt[normsubset, on = commondims, lopendeMEISref := i.lopendeMEISref]
  do_adjust_smr_and_meis(dt = dt, parameters = parameters)
}

#' @keywords internal
#' @noRd
get_normsubset <- function(dt, parameters){
  if(parameters$PredFilter$ref_year_type == "Specific") normsubset <- dt[GEOniv == "L"]
  if(parameters$PredFilter$ref_year_type == "Moving") normsubset <- dt[x, env = list(x = str2lang(parameters$PredFilter$meisskalafilter))]
  if(nrow(normsubset) == 0) stop("Noe er feil i ACCESS::KUBER::REFVERDI, klarer ikke hente subset for landsnormal")
  normsubset <- format_normsubset(dt = normsubset, parameters = parameters)
  return(normsubset)  
}

#' @keywords internal
#' @noRd
format_normsubset <- function(dt, parameters){
  if(parameters$PredFilter$ref_year_type == "Specific"){
    dt[, lopendeMEISref := MEIS]
    outcols <- setdiff(intersect(names(dt), parameters$DefDesign$DesignKolsFA), c("GEOniv", "GEO", "FYLKE"))
    dt[, c(..outcols, "lopendeMEISref")]
    return(dt)
  }
  
  outcols <- setdiff(intersect(names(dt), parameters$DefDesign$DesignKolsFA), parameters$PredFilter$Predfiltercolumns)
  maltall <- parameters$MALTALL
  data.table::setnames(dt, c(maltall, "SMRtmp"), c("NORM", "NORMSMR"))
  dt <- dt[, c(..outcols, "NORM", "NORMSMR")]
  return(dt)
}

#' @title do_adjust_smr_and_meis
#' @description
#' Juster SMR proporsjonalt slik at NORM (landet) alltid har SMR=100
#' SMR>100 kan oppstaa dersom det f.eks. er noen med ukjent alder/kjoenn.
#' Ratene for ukjent alder/kjoenn vil ikke matche nevner fra BEF, derfor vil det predikeres for faa doede relativt til observert
#' @keywords internal
#' @noRd
do_adjust_smr_and_meis <- function(dt, parameters){
  if(parameters$PredFilter$ref_year_type == "Specific"){
    dt[, lopendeFORHOLDSVERDI := MEIS / lopendeMEISref * 100]
    dt[, let(SMR = lopendeFORHOLDSVERDI, NORM = lopendeMEISref)]
    return(invisible(NULL))
  }
  
  dt[, SMR := NA_real_]
  if(parameters$CUBEinformation$REFVERDI_VP == "P") dt[, SMR := sumTELLER / sumPREDTELLER * 100]
  if(parameters$CUBEinformation$REFVERDI_VP == "V") dt[, SMR := MALTALL / NORM * 100]
  dt[, let(SMR = 100 * (SMR / NORMSMR), MEIS = SMR * NORM / 100)]
  return(invisible(NULL))
}
