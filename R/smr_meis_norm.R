#' @keywords internal
#' @noRd
add_smr_and_meis <- function(dt, parameters){
  if(parameters$PredFilter$ref_year_type == "Specific") calculate_smr_and_meis(dt = dt, parameters = parameters)
  if(parameters$PredFilter$ref_year_type == "Moving") calculate_smrtmp(dt = dt, parameters = parameters)
  adjust_smr_and_meis_to_country_normal(dt = dt, parameters = parameters)
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
  merge_cols_by_reference(orgdata = dt, newdata = normsubset)
  # commondims <- intersect(names(normsubset), names(dt))
  # newcols <- setdiff(names(normsubset), commondims)
  # newvals <- normsubset[dt, on = commondims, ..newcols]
  # data.table::set(dt, j = newcols, value = newvals)
  do_adjust_smr_and_meis(dt = dt, parameters = parameters)
}

#' @keywords internal
#' @noRd
get_normsubset <- function(dt, parameters){
  if(parameters$PredFilter$ref_year_type == "Specific"){
    normsubset <- dt[GEOniv == "L"]
  } else if(parameters$PredFilter$ref_year_type == "Moving"){
    normsubset <- dt[x, env = list(x = str2lang(parameters$PredFilter$meisskalafilter))]
  }
  normsubset <- data.table::copy(normsubset)
  if(nrow(normsubset) == 0) stop("Noe er feil i ACCESS::KUBER::REFVERDI, klarer ikke hente subset for landsnormal")
  format_normsubset(dt = normsubset, parameters = parameters)
  return(normsubset)  
}

#' @keywords internal
#' @noRd
format_normsubset <- function(dt, parameters){
  if(parameters$PredFilter$ref_year_type == "Specific"){
    dt[, lopendeMEISref := MEIS]
    keep <- c(setdiff(intersect(names(dt), parameters$DefDesign$DesignKolsFA), c("GEOniv", "GEO", "FYLKE")), 
              "lopendeMEISref")
  } else {
    maltall <- parameters$MALTALL
    data.table::setnames(dt, c(maltall, "SMRtmp"), c("NORM", "NORMSMR"))
    keep <- c(setdiff(intersect(names(dt), parameters$DefDesign$DesignKolsFA), parameters$PredFilter$Predfiltercolumns),
                 "NORM", "NORMSMR")
  }
  
  data.table::set(dt, j = setdiff(names(dt), keep), value = NULL)
  return(invisible(NULL))
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
    ref <- dt[["lopendeMEISref"]]
    val <- dt[["MEIS"]] / ref * 100
    data.table::set(dt, j = c("lopendeFORHOLDSVERDI", "SMR", "NORM"), value = list(val, val, ref))
  } else if(parameters$PredFilter$ref_year_type == "Moving"){
    normsmr <- dt[["NORMSMR"]]
    norm <- dt[["NORM"]]
    if(parameters$CUBEinformation$REFVERDI_VP == "P"){
      smr0 <- dt[["sumTELLER"]] / dt[["sumPREDTELLER"]] * 100
    } else if (parameters$CUBEinformation$REFVERDI_VP == "V") {
      smr0 <- dt[["MALTALL"]] / norm * 100
    }
    smr <- 100 * (smr0 / normsmr)
    meis <- smr * norm / 100

    data.table::set(dt, j = c("SMR", "MEIS"), value = list(smr, meis))
  }
  return(invisible(NULL))
}
