#' @keywords internal
#' @noRd
control_cube_output <- function(outputlist, parameters){
  print_console_message("\n\n---\n* Kvalitetskontroll:\n---")
  all_checks <- numeric()
  all_checks <- c(all_checks, control_censoring(dt = outputlist$QC, parameters = parameters))
  all_checks <- c(all_checks, control_standardization(dt = outputlist$ALLVIS, parameters = parameters))
  all_checks <- c(all_checks, control_aggregation(dt = outputlist$KUBE, parameters = parameters))
  if(sum(all_checks) == 0){
    print_console_message("\n\n---\n* Alle sjekker passert!\n---\n")
  } else {
    print_console_message("\n\n---\n* Noen av sjekkene feilet, manuell kontroll nødvendig!\n---\n")
  }
  control_meis_rate(dt = outputlist$KUBE, parameters = parameters)
  control_rate_lks(dt = outputlist$KUBE, parameters = parameters)
}

#' @keywords internal
#' @noRd
control_censoring <- function(dt, parameters){
  print_console_message("\n\n* Sjekker prikking (verdier under grensen):")
  if(is_empty(parameters$Censor_type)){ 
    print_console_message("\n** Kube ikke prikket")
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
    print_console_message("\n** Alle verdier under satte grenser, prikking OK!")
    return(0)
  }
  if(any(!checks)){
    notok <- paste(names(checks)[which(!checks)], collapse = ", ")
    print_console_message("\n** OBS! noe er galt med prikkingen av: ", notok, ", IKKE OK", sep = "")
    return(1)
  }
}

control_standardization <- function(dt, parameters){
  print_console_message("\n\n* Sjekker standardiseringsår")
  if(parameters$CUBEinformation$REFVERDI_VP != "P"){
    print_console_message("\n** Kube ikke standardisert")
    return(0)
  }
  if(parameters$CUBEinformation$REFVERDI_VP == "P" & grepl("UNGDATA", parameters$name)){
    print_console_message("\n** UNGDATA-kube, standardiseres ikke mot siste år, OK!")
    return(0)
  }
  refverdi <- parameters$CUBEinformation$REFVERDI
  aarh <- as.numeric(sub("\\d{4}_(\\d{4})", "\\1", max(dt$AAR)))
  if(grepl("SISTE", refverdi, ignore.case = TRUE) || grepl(aarh, refverdi)){
    print_console_message("\n** Standardisert mot siste år/periode, OK!")
    return(0)
  }
  
  print_console_message("\n*** Kube ikke standardisert mot siste år, IKKE OK!")
  return(1)
}

control_aggregation <- function(dt, parameters){
  print_console_message("\n\n* Sjekker aggregering mellom geonivå")
  n_teller <- dt[!is.na(sumTELLER), .N]
  n_nevner <- dt[!is.na(sumNEVNER), .N]
  if(n_teller == 0 && n_nevner == 0){
    print_console_message("\n** Ingen ikke-missing teller/nevner i filen, skippet")
    return(0)
  }
  LF <- compare_geolevels(dt = dt, level = "F", parameters = parameters)
  FK <- compare_geolevels(dt = dt, level = "K", parameters = parameters)
  BK <- compare_geolevels(dt = dt, level = "B", parameters = parameters)
  VKB <- compare_geolevels(dt = dt, level = "V", parameters = parameters)
  
  comparisons <- LF + FK + BK + VKB
  if(comparisons > 0){
    print_console_message("\n** Summen av lavere nivå er større enn høyere nivå, noe er feil med aggregeringen, IKKE OK!")
    return(1)
  }
  return(0)
}

compare_geolevels <- function(dt, level = c("F", "K", "B", "V"), parameters){
  level <- match.arg(level)
  if(!level %in% unique(dt$GEOniv)) return(0)
  parents <- switch(level,
                    "F" = collapse::funique(dt[GEOniv == "L", GEO]),
                    "K" = collapse::funique(dt[GEOniv == "F", GEO]),
                    "B" = collapse::funique(substr(dt[GEOniv == "B", GEO], 1, 4)),
                    "V" = sub("00$", "", collapse::funique(substr(dt[GEOniv == "V", GEO], 1, 6))))
  if(length(parents) == 0){
    print_console_message("\n*** Ingen rader funnet for overnivå, sjekk ikke utført")
    return(0)
  } 
  overniv_name <- switch(level,
                         "F"="L", "K"="F", "B"="K", "V"="K/B")
  dims <- c("GEOniv", setdiff(parameters$outdimensions, "GEO"))
  d <- data.table::copy(dt[GEOniv == level | GEO %in% parents, .SD, .SDcols = c(dims, c("GEO", "sumTELLER", "sumNEVNER"))])
  d[GEOniv != level, GEOniv := overniv_name]
  if(level == "F"){
    d[, overniv_kode := "0"]
  } else {
    cropgeo <- switch(level, "K" = 2, "B" = 4, "V" = 6)
    d[, overniv_kode := sub("00$", "", substr(GEO, 1, cropgeo))]
  }
  
  print_console_message("\n** Sjekker", switch(level, "F"="Fylke", "K"="Kommune", "B"="Bydel", "V"="Levekårsoner"))
  check_teller <- d[!is.na(sumTELLER), .N] > 0
  check_nevner <- d[!is.na(sumNEVNER), .N] > 0
  
  if(!check_teller && !check_nevner){
    print_console_message("\n*** Ingen ikke-missing teller/nevner, sjekk skippet")
    return(0)
  }
  
  t_ok <- n_ok <- TRUE
  
  g <- collapse::GRP(d, c(dims, "overniv_kode"))
  if(check_teller){
    sumT_check <- check_value(d = d, value = "sumTELLER", g = g, overniv = overniv_name, underniv = level)
    t_ok <- sumT_check[, .N] == 0
    if(t_ok) print_console_message("\n*** sumTELLER for GEOniv='", level, "' er mindre eller lik '", overniv_name, "' i alle strata, OK!", sep = "")
    if(!t_ok){
      print_console_message("\n*** sumTELLER for GEOniv='", level, "' er STØRRE enn overkategorien, IKKE OK, se utslag i egen tabell!", sep = "")
      View(sumT_check, title = paste0("sumTELLER_check_", level, "_vs_", overniv_name))
    }
  }
  
  if(check_nevner){
    sumN_check <- check_value(d = d, value = "sumNEVNER", g = g, overniv = overniv_name, underniv = level)
    n_ok <- sumN_check[, .N] == 0
    if(n_ok) print_console_message("\n*** sumNEVNER for GEOniv='", level, "' er mindre eller lik '", overniv_name, "' i alle strata, OK!", sep = "")
    if(!n_ok){
      print_console_message("\n*** sumNEVNER for GEOniv='", level, "' er STØRRE enn '", overniv_name, "', IKKE OK!, se utslag i egen tabell!", sep = "")
      View(sumN_check, title = paste0("sumNEVNER_check_", level, "_vs_", overniv_name))
    }
  }

  if(t_ok && n_ok) return(0)
  return(1)
}

#' @title check_value
#' @description helper function for compare_geolevels
#' @param d data
#' @param value value column name
#' @param g GRP object to make comparisons in all strata
#' @keywords internal
#' @noRd
check_value <- function(d, value, g, overniv, underniv){
  out <- data.table::copy(g[["groups"]])
  data.table::set(out, j = value, value = collapse::fsum(d[[value]], g = g))
  out <- data.table::dcast(out, formula = ... ~ GEOniv, value.var = value)
  diff <- out[[overniv]] - out[[underniv]]
  data.table::set(out, j = "diff", value = diff)
  return(out[diff < 0])
}

#' @keywords internal
#' @noRd
control_meis_rate <- function(dt, parameters){
  if(parameters$CUBEinformation$REFVERDI_VP != "P") return(invisible(NULL))
  print_console_message("\n\n* Sjekker forholdet mellom MEIS og RATE")
  cols <- c(parameters$outdimensions, "MEIS", "RATE")
  d <- dt[!is.na(MEIS), .SD, .SDcols = cols]
  
  for(dim in setdiff(parameters$outdimensions, c("GEO", "AAR"))){
    tot <- qualcontrol:::find_total(cube = d, dim = dim)
    if(!is.na(tot)) d <- d[d[[dim]] == tot]
    if(!is.na(tot) | length(unique(d[[dim]])) == 1) d[, names(.SD) := NULL, .SDcols = dim]
  }

  d[, let(diff = round(MEIS - RATE, 2), `ratio, %` = round(100*MEIS/RATE, 2))]
  print_console_message("\n\n** Diff og ratio (%) på landsnivå per år:\n\n")
  d_country <- d[GEO == 0]
  print(d_country, nrows = nrow(d_country))
  
  print_console_message("\n\n** 5 største (begge veier) diff og ratio (%) (ekskludert landstall):\n\n")
  d <- d[GEO != 0 & !is.nan(`ratio, %`)][order(`ratio, %`, decreasing = T)]
  if(nrow(d) <= 10){
    print(d)
  } else {
    print(d[c(1:5, seq(.N-4, .N))])
  }
}

#' @title control_rate_lks
#' @description check if MEIS/RATE for parent geo is between min/max LKS-value.
#' @param dt data
#' @param parameters parameters from lagKUBE
#' @keywords internal
#' @noRd
control_rate_lks <- function(dt, parameters){
  if(!grepl("V", parameters$CUBEinformation$GEOniv)) return(invisible(NULL))
  
  overcat <- sub("00$", "", collapse::funique(substr(dt[GEOniv == "V", GEO], 1, 6)))
  val <- data.table::fcase("MEIS" %in% parameters$outvalues, "MEIS",
                           "RATE" %in% parameters$outvalues, "RATE",
                           default = NA_character_)
  if(is.na(val)){
    print_console_message("\n* Kuben inneholder ikke MEIS eller RATE, sjekk av levekårssoner skippet")
    return(invisible(NULL))
  } else {
    print_console_message(paste0("\n\n* Sjekker ", val, "-nivå for levekårssoner mot sitt overnivå"))
  }
  
  dims <- parameters$outdimensions
  tncols <- intersect(names(dt), c("sumTELLER", "sumNEVNER"))
  d <- data.table::copy(dt)[(GEOniv == "V" | GEO %in% overcat), .SD, .SDcols = c("GEOniv", dims, val, tncols, "spv_tmp")]
  d[, kommune := sub("00$", "", substr(GEO, 1, 6))]
  data.table::set(d, j = val, value = round(d[[val]], 1))
  bycols <- c("kommune", setdiff(dims, "GEO"))
  
  d <- d[, N := .N, by = bycols][N > 2]
  
  g <- collapse::GRP(d, bycols)
  res <- collapse::add_vars(g[["groups"]],
                            N_lks = collapse::fndistinct(data.table::fifelse(d[["GEOniv"]] == "V", d[["GEO"]], NA_character_), g = g),
                            overniv = collapse::fmax(data.table::fifelse(d[["GEOniv"]] == "V", NA_real_, d[[val]]), g = g),
                            lks_min = collapse::fmin(data.table::fifelse(d[["GEOniv"]] == "V", d[[val]], NA_real_), g = g),
                            lks_max = collapse::fmax(data.table::fifelse(d[["GEOniv"]] == "V", d[[val]], NA_real_), g = g),
                            overniv_sumteller = collapse::fmax(data.table::fifelse(d[["GEOniv"]] == "V", NA_real_, d[["sumTELLER"]]), g = g),
                            lks_sumteller = collapse::fsum(data.table::fifelse(d[["GEOniv"]] == "V", d[["sumTELLER"]], NA_real_), g = g))
  res[, let(teller_diff = overniv_sumteller - lks_sumteller)][, names(.SD) := NULL, .SDcols = c("overniv_sumteller", "lks_sumteller")]
  if("sumNEVNER" %in% tncols){
    res <- collapse::add_vars(res,
                              overniv_sumnevner = collapse::fmax(data.table::fifelse(d[["GEOniv"]] == "V", NA_real_, d[["sumNEVNER"]]), g = g),
                              lks_sumnevner = collapse::fsum(data.table::fifelse(d[["GEOniv"]] == "V", d[["sumNEVNER"]], NA_real_), g = g))
    
    res[, let(nevner_diff = overniv_sumnevner - lks_sumnevner)][, names(.SD) := NULL, .SDcols = c("overniv_sumnevner", "lks_sumnevner")]
  } else {
    res[, let(nevner_diff = NA_real_)]
  }
 
  out <- res[overniv < lks_min | overniv > lks_max]
  out[, retning := data.table::fifelse(overniv < lks_min, "under", "over")]
  out[retning == "under", diff := overniv - lks_min]
  out[retning == "over", diff := overniv - lks_max]
  colsrename <- c("overniv", "lks_min", "lks_max")
  data.table::setnames(out, colsrename, paste0(colsrename, "_", val))
  data.table::setcolorder(out, c("kommune", "N_lks"))
  
  n <- collapse::join(out[, .SD, .SDcols = bycols], d[GEOniv == "V", .SD, .SDcols = c(bycols, "spv_tmp")], multiple = T, on = bycols, , verbose = 0, overid = 2)
  n <- unique(n[, n_lks_med_tall := sum(spv_tmp == 0), by = bycols])[, .SD, .SDcols = c(bycols, "n_lks_med_tall")]
  out <- collapse::join(out, n, on = bycols, how = "l", verbose = 0, overid = 2)[n_lks_med_tall > 0]
  
  if(nrow(out) > 0){
    print_console_message("\n** Se tabell for radene som har gitt utslag")
    View(out, title = "lks_rateutslag")
  } else {
    print_console_message("\n** Ingen utslag, kommunetallet ligger mellom minste og største LKS")
  }
}
