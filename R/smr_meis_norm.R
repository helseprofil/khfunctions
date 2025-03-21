add_smr_and_meis <- function(dt, parameters){
  if(parameters$PredFilter$ref_year_type == "Specific") dt <- calculate_smr_and_meis(dt = dt, parameters = parameters)
  if(parameters$PredFilter$ref_year_type == "Moving") dt <- calculate_smrtmp(dt = dt, parameters = parameters)
  return(dt)
}

calculate_smr_and_meis <- function(dt, parameters){
  dt[, let(SMR = NA_real_, MEIS = MALTALL)]
  if(parameters$CUBEinformation$REFVERDI_VP == "P"){
    dt[, let(SMR = sumTELLER / sumPREDTELLER * 100,
             MEIS = sumTELLER / sumPREDTELLER * MEISskala)]
  }
  return(dt)
}

calculate_smrtmp <- function(dt, parameters){
  dt[, let(SMRtmp = NA_real_)]
  if(parameters$CUBEinformation$REFVERDI_VP == "P") dt[, let(SMRtmp = sumTELLER / sumPREDTELLER * 100)]
  if(parameters$CUBEinformation$REFVERDI_VP == "V") dt[, let(SMRtmp = 100)]
  return(dt)
}

#' @title adjust_smr_and_meis_to_country_normal
#' @description Find the subset to estimate country-level normals, 
#' and adjusts SMR and MEIS to make sure country always have SMR = 100
#' @noRd
adjust_smr_and_meis_to_country_normal <- function(dt, parameters, globs){
  normsubset <- get_normsubset(dt = dt, parameters = parameters, globs = globs)
  dt <- collapse::join(dt, normsubset, how = "l", overid = 2, verbose = 0)
  dt <- do_adjust_smr_and_meis(dt = dt, parameters = parameters)
  return(dt)
}

get_normsubset <- function(dt, parameters, globs){
  if(parameters$PredFilter$ref_year_type == "Specific") normsubset <- dt[GEOniv == "L"]
  if(parameters$PredFilter$ref_year_type == "Moving") normsubset <- dt[eval(rlang::parse_expr(parameters$PredFilter$meisskalafilter))]
  if(nrow(normsubset) == 0) stop("Noe er feil i ACCESS::KUBER::REFVERDI, klarer ikke hente subset for landsnormal")
  normsubset <- format_normsubset(dt = normsubset, parameters = parameters, globs = globs)
  return(normsubset)  
}

format_normsubset <- function(dt, parameters, globs){
  if(parameters$PredFilter$ref_year_type == "Specific"){
    dt[, lopendeMEISref := MEIS]
    outcols <- setdiff(intersect(names(dt), globs$DefDesign$DesignKolsFA), c("GEOniv", "GEO", "FYLKE"))
    dt <- dt[, c(..outcols, "lopendeMEISref")]
    return(dt)
  }
  
  outcols <- setdiff(intersect(names(dt), globs$DefDesign$DesignKolsFA), parameters$PredFilter$Predfiltercolumns)
  maltall <- parameters$MALTALL
  if (maltall %in% c("TELLER", "RATE")) {
    data.table::setnames(dt, c(paste0(maltall, c("", ".f", ".a", ".n")), "SMRtmp"), c(paste0("NORM", c("", ".f", ".a", ".n")), "NORMSMR"))
    dt <- dt[, c(..outcols, paste0("NORM", c("", ".f", ".a", ".n")), "NORMSMR")]
  } else {
    data.table::setnames(VF, c(maltall, "SMRtmp"), c("NORM", "NORMSMR"))
    dt <- dt[, c(..outcols, "NORM", "NORMSMR")]
  }
  return(dt)
}

#' @title do_adjust_smr_and_meis
#' @description
#' Juster SMR proporsjonalt slik at NORM (landet) alltid har SMR=100
#' SMR>100 kan oppstaa dersom det f.eks. er noen med ukjent alder/kjoenn.
#' Ratene for ukjent alder/kjoenn vil ikke matche nevner fra BEF, derfor vil det predikeres for faa doede relativt til observert
#' @noRd
do_adjust_smr_and_meis <- function(dt, parameters){
  if(parameters$PredFilter$ref_year_type == "Specific"){
    dt[, lopendeFORHOLDSVERDI := MEIS / lopendeMEISref * 100]
    dt[, let(SMR = lopendeFORHOLDSVERDI, NORM = lopendeMEISref)]
    return(dt)
  }
  
  dt[, SMR := NA_real_]
  if(parameters$CUBEinformation$REFVERDI_VP == "P") dt[, SMR := sumTELLER / sumPREDTELLER * 100]
  if(parameters$CUBEinformation$REFVERDI_VP == "V") dt[, SMR := MALTALL / NORM * 100]
  dt[, let(SMR = 100 * (SMR / NORMSMR), MEIS = SMR * NORM / 100)]
  return(dt)
}
