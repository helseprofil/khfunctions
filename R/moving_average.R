get_movav_information <- function(dt, parameters){
  mapar <- list()
  mapar[["aar"]] <- unique(dt[, mget(c("AARl", "AARh"))])
  mapar[["int_lengde"]] <- unique(mapar$aar[, AARh - AARl + 1])
  if(length(mapar$int_lengde) > 1) stop("Inndata har ulike årsintervaller!")
  mapar[["is_movav"]] <- parameters$CUBEinformation$MOVAV > 1
  mapar[["movav"]] <- parameters$CUBEinformation$MOVAV
  mapar[["snitt"]] <- parameters$fileinformation[[parameters$files$TELLER]]$ValErAarsSnitt
  mapar[["is_orig_snitt"]] <- !is.na(mapar$snitt) && mapar$snitt != 0
  snitt_orgintmult <- ifelse(mapar$is_orig_snitt, mapar$int_lengde, 1)
  mapar[["orgintMult"]] <- ifelse(mapar$is_movav, 1, snitt_orgintmult)
  return(mapar)
}

#' @title aggregate_to_moving_average
#' @description
#' Finn "snitt" for ma-aar.
#' DVs, egentlig lages forloepig bare summer, snitt settes etter prikking under
#' Snitt tolerer missing av type .f=1 ("random"), men bare noen faa anonyme .f>1, se KHaggreger
#' Rapporterer variabelspesifikk VAL.n som angir antall aar brukt i summen naar NA holdt utenom
#' 
#' Dersom is_movav = FALSE, legges val.n til for alle verdikolonner, satt til 1 dersom originale snitt
#' og satt til antall år dersom originale summer. 
#'
#' @param dt KUBE to be aggregated 
#' @param reset_rate Reset RATE after aggregating to periods?
#' @param parameters cube parameters
aggregate_to_periods <- function(dt, reset_rate = TRUE, parameters){
  dt <- do_balance_missing_teller_nevner(dt = dt)
  
  if(parameters$MOVAVparameters$is_movav){
    dt <- do_aggregate_periods(dt = dt, parameters = parameters)
    dt <- do_filter_periods_with_missing_original(dt)
    if(reset_rate && parameters$TNPinformation$NEVNERKOL != "-") dt <- LeggTilNyeVerdiKolonner(TNF = dt, NYEdscr = "RATE={TELLER/NEVNER}", postMA = TRUE)
  } else {
    dt <- do_handle_indata_periods(dt = dt, parameters = parameters)
  }
  return(dt)
}

#' @title do_balance_missing_teller_nevner
#' @description
#' Maa "balansere" NA i teller og nevner slik sumrate og sumnevner balanserer.
#' Kunne med god grunn satt SPVFLAGG her og saa bare operert med denne som en egenskap for hele linja i det som kommer
#' Men for aa ha muligheten for aa haandtere de forskjellige variablene ulikt og i full detalj lar jeg det staa mer generelt
#' Slik at dataflyten stoetter en slik endring
#' 
#' Om enkeltobservasjoner ikke skal brukes, men samtidig tas ut av alle summeringer
#' kan man ha satt VAL=0,VAL.f=-1
#' Dette vil ikke oedelegge summer der tallet inngaar. Tallet selv, eller sumemr av kun slike tall, settes naa til NA
#' Dette brukes f.eks naar SVANGERROYK ekskluderer Oslo, Akershus. Dette er skjuling, saa VAL.f=3
#' 
#' @param dt data
do_balance_missing_teller_nevner <- function(dt){
  vals <- intersect(c("TELLER", "NEVNER", "RATE"), names(dt))
  valsF <- paste0(vals, ".f")
  if(length(vals) == 0) return(dt)
  dt[, maxF := do.call(pmax, .SD), .SDcols = valsF]
  if (length(vals) > 0) {
    dt[maxF > 0, (vals) := NA]
    dt[maxF > 0, (valsF) := maxF]
    dt[maxF == -1, (vals) := NA]
    dt[maxF == -1, (valsF) := maxF]
  }
  dt[, maxF := NULL]
  return(dt)
}

#' @title do_aggregate_periods
#' @description
#' Aggregate from original data to moving averages
#' 
#' @param dt data
#' @param parameters cube parameters
do_aggregate_periods <- function(dt, parameters){
  period = parameters$MOVAVparameters$movav
  if(any(dt$AARl != dt$AARh)) stop(paste0("Aggregering til ", movav, "-årige tall er ønsket, men originaldata inneholder allerede flerårige tall og kan derfor ikke aggregeres!"))
  n_missing_year <- find_missing_year(aarl = parameters$MOVAVparameters$aar$AARl)
  aggregated_dt <- calculate_period_sums(dt = dt, period = period, n_missing_year = n_missing_year)
  return(aggregated_dt)
}

#' @title find_missing_year
#' @noRd
find_missing_year <- function(aarl){
  aarl_min_max <- min(aarl):max(aarl)
  aarl_missing <- aarl_min_max[!aarl_min_max %in% aarl]
  if(length(aarl_missing) > 0) cat("Setter rullende gjennomsnitt, mangler data for:", paste0(aarl_missing, collapse = ", "), "\n")
  return(length(aarl_missing))
}

#' @title calculate_period_sums
#' @description
#' Aggregates value columns to period sums for periods defined in ACCESS::KUBER::MOVAV
#' @noRd
calculate_period_sums <- function(dt, period, n_missing_year){
  cat("* Aggregerer til ", period, "-årige tall\n", sep = "")
  allperiods <- find_periods(aarh = unique(dt$AARh), period = period)
  dt <- extend_to_periods(dt = dt, periods = allperiods)
  values <- get_value_columns(names(dt))
  dt[, paste0(rep(values, each = 4), c(".fn1", ".fn3", ".fn9", ".n")) := NA_integer_]
  dims <- get_dimension_columns(names(dt))
  colorder <- dims
  for(val in values){
    dt[is.na(get(val)) | get(val) == 0, paste0(val, ".a") := 0]
    dt[get(paste0(val, ".f")) %in% c(1,2), paste0(val, ".fn1") := 1]
    dt[get(paste0(val, ".f")) == 3, paste0(val, ".fn3") := 1]
    dt[get(paste0(val, ".f")) == 9, paste0(val, ".fn9") := 1]
    dt[get(paste0(val, ".f")) == 0, paste0(val, ".n") := 1]
    colorder <- c(colorder, paste0(val, c("", ".f", ".a", ".fn1",".fn3", ".fn9", ".n")))
  }
  g <- collapse::GRP(dt, dims)
  aggdt <- collapse::add_vars(g[["groups"]],
                              collapse::fsum(collapse::get_vars(dt, values), g = g, fill = T),
                              collapse::fsum(collapse::get_vars(dt, paste0(values, ".a")), g = g, fill = T),
                              collapse::fsum(collapse::get_vars(dt, paste0(values, ".fn1")), g = g, fill = T),
                              collapse::fsum(collapse::get_vars(dt, paste0(values, ".fn3")), g = g, fill = T),
                              collapse::fsum(collapse::get_vars(dt, paste0(values, ".fn9")), g = g, fill = T),
                              collapse::fsum(collapse::get_vars(dt, paste0(values, ".n")), g = g, fill = T))
  aggdt[, (paste0(values, ".f")) := 0]
  data.table::setcolorder(aggdt, colorder)
  
  if(n_missing_year <= period){
    for(val in values) aggdt[aggdt[[paste0(val, ".fn9")]] > n_missing_year, (c(val, paste0(val, ".f"))) := list(NA, 9)]
  }
  
  f9s <- names(aggdt)[grepl(".f9$", names(aggdt))]
  if (length(f9s) > 0) aggdt[, (f9s) := NULL]

  return(aggdt)
}

find_periods <- function(aarh, period){
  max_aar <- (min(aarh) + period - 1):max(aarh)
  min_aar <- max_aar - period + 1
  return(data.table::data.table(aarl = min_aar, aarh = max_aar))
}

extend_to_periods <- function(dt, periods){
  out <- data.table::copy(dt)[0, ]
  for(i in 1:nrow(periods)){
    aarl <- periods[i, aarl]
    aarh <- periods[i, aarh]
    newperiod <- dt[AARl >= aarl & AARh <= aarh][, let(AARl = aarl, AARh = aarh)]
    out <- data.table::rbindlist(list(out, newperiod))
  }
  return(out)
}

#' @title handle_indata_periods
#' @description
#' If indata contains periods, val.n must be adjusted accordingly
#' @keywords internal
#' @noRd
do_handle_indata_periods <- function(dt, parameters){
  # USIKKER PÅ OM DETTE HÅNDTERES KORREKT, SJEKK MED EN FIL SOM INNEHOLDER FLERÅRIGE SNITT ELLER SUMMER
  # Må legge til VAL.n når originale summer, VAL.n = 1 om originale snit
  n <- ifelse(parameters$MOVAVparameters$is_orig_snitt, 1, parameters$MOVAVparameters$int_lengde) 
  dt[, paste0(names(.SD), ".n") := n, .SDcols = get_value_columns(names(dt))]
  return(dt)
}

#' @title filter_periods_with_missing_original
#' @description
#' Filters out averages with too much missing from original data. The 
#' tolerance is set by the option anon_tot_tol in the config file, and
#' for rows where the proportion of original val.f == 3 (as indicated 
#' by the helper column val.fn3) > the tolerance, the average is set to 
#' NA and val.f = 3
#' @keywords internal
#' @noRd
do_filter_periods_with_missing_original <- function(dt){
  values <- get_value_columns(names(dt))
  anonymous_tolerance <- getOption("khfunctions.anon_tot_tol") 
  for(val in values){
      val.f <- paste0(val, ".f")
      val.n <- paste0(val, ".n")
      val.fn3 <- paste0(val, ".fn3")
      if(val.n %in% names(dt)) dt[get(val.n) > 0 & get(val.fn3)/get(val.n) >= anonymous_tolerance, c(val, val.f) := list(NA, 3)]
  }
  return(dt)
}

#' @title organize_file_for_moving_average
#' @description
#' Make sure dt is arranged according to AARl and AARh last
organize_file_for_moving_average <- function(dt){
  tabcols_minus_aar <- grep("^AARl$|^AARh$", get_dimension_columns(names(dt)), value = T, invert = T)
  key <- c(tabcols_minus_aar, "AARl", "AARh")
  if(!identical(data.table::key(dt), key)) data.table::setkeyv(dt, key)
}
