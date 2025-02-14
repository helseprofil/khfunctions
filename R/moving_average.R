get_movav_information <- function(dt, parameters){
  out <- list()
  data.table::setkeyv(dt, c("AARl", "AARh"))
  out[["aar"]] <- unique(dt[, mget(c("AARl", "AARh"))])
  out[["int_lengde"]] <- unique(out$aar[, AARh - AARl + 1])
  if (length(out$int_lengde) > 1) stop("Inndata har ulike årsintervaller!")
  out[["is_movav"]] <- parameters$CUBEinformation$MOVAV > 1
  out[["movav"]] <- parameters$CUBEinformation$MOVAV
  out[["snitt"]] <- parameters$fileinformation[[parameters$files$TELLER]]$ValErAarsSnitt
  out[["is_orig_snitt"]] <- !is.na(out$snitt) && out$snitt != 0
  snitt_orgintmult <- ifelse(out$is_orig_snitt, out$int_lengde, 1)
  out[["orgintMult"]] <- ifelse(out$is_movav, 1, snitt_orgintmult)
  return(out)
}

#' @title aggregate_to_moving_average
#' @description
#' Finn "snitt" for ma-aar.
#' DVs, egentlig lages forloepig bare summer, snitt settes etter prikking under
#' Snitt tolerer missing av type .f=1 ("random"), men bare noen faa anonyme .f>1, se KHaggreger
#' Rapporterer variabelspesifikk VAL.n som angir antall aar brukt i summen naar NA holdt utenom
#'
#' @param dt KUBE to be aggregated 
#' @param parameters cube parameters
#' @param globs global parameters
aggregate_to_moving_average <- function(dt, parameters, globs){
  
  dt <- do_balance_missing_teller_nevner(dt = dt)
  
  if(parameters$MOVAVparameters$is_movav){
    dt <- do_aggregate_periods(dt = dt, parameters = parameters, globs = globs)
    return(dt)
  }
  
  # USIKKER PÅ OM DETTE HÅNDTERES KORREKT
  # SJEKK MED EN FIL SOM INNEHOLDER FLERÅRIGE SNITT ELLER SUMMER
  # Må legge til VAL.n når originale summer, VAL.n = 1 om originale snit
  n <- ifelse(parameters$MOVAVparameters$is_orig_snitt, 1, parameters$MOVAVparameters$int_lengde) 
  dt[, paste0(names(.SD), ".n") := n, .SDcols = get_value_columns(names(dt))]
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
  n = length(vals)
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
#' @param globs global parameters
do_aggregate_periods <- function(dt, parameters, globs){
  movav = parameters$MOVAVparameters$movav
  if(any(dt$AARl != dt$AARh)) stop(paste0("Aggregering til ", movav, "-årige tall er ønsket, men originaldata inneholder allerede flerårige tall og kan derfor ikke aggregeres!"))
  n_missing_year <- find_missing_year(aarl = parameters$MOVAVparameters$aar$AARl)
  aggregated_dt <- FinnSumOverAar(KUBE = dt, per = movav, FyllMiss = TRUE, AntYMiss = n_missing_year, globs = globs)
  
  if(parameters$TNPinformation$NEVNERKOL != "-") {
    aggregated_dt <- LeggTilNyeVerdiKolonner(aggregated_dt, "RATE={TELLER/NEVNER}")
    israteskala <- !is.na(parameters$CUBEinformation$RATESKALA) && parameters$CUBEinformation$RATESKALA != ""
    if(parameters$standardmethod == "DIR" & israteskala) aggregated_dt[, RATE := RATE * as.numeric(parameters$CUBEinformation$RATESKALA)]
  }
  return(aggregated_dt)
}

find_missing_year <- function(aarl){
  aarl_min_max <- min(aarl):max(aarl)
  aarl_missing <- aarl_min_max[!aarl_min_max %in% aarl]
  if(length(aarl_missing) > 0) cat("Setter rullende gjennomsnitt, mangler data for:", paste0(aarl_missing, collapse = ", "), "\n")
  return(length(aarl_missing))
}

#' FinnSumOverAar (kb)
#'
#' @param KUBE 
#' @param per 
#' @param FyllMiss 
#' @param AntYMiss 
#' @param globs global parameters, defaults to SettGlobs
FinnSumOverAar <- function(KUBE, per = 0, FyllMiss = FALSE, AntYMiss = 0, globs = SettGlobs()) {
  is_kh_debug()
  UT <- KUBE[0, ]
  tabs <- setdiff(get_dimension_columns(names(KUBE)), c("AARl", "AARh"))
  valkols <- get_value_columns(names(KUBE))
  # Utrykk for KH-aggregering (med hjelpestoerrelses for snitt)
  lpv <- paste0(valkols, "=sum(", valkols, ",na.rm=TRUE),",
                valkols, ".f=0,",
                valkols, ".a=sum(", valkols, ".a*(!is.na(", valkols, ") & ", valkols, "!=0)),",
                valkols, ".fn1=sum(", valkols, ".f %in% 1:2),",
                valkols, ".fn3=sum(", valkols, ".f==3),",
                valkols, ".fn9=sum(", valkols, ".f==9),",
                valkols, ".n=sum(", valkols, ".f==0)",
                # valkols,".n=sum(as.numeric(!is.na(",valkols,")))",
                collapse = ","
  )
  lpsvars <- unlist(lapply(valkols, function(x) {
    paste0(x, c(".fn1", ".fn3", ".fn9", ".n"))
  }))
  UT[, (lpsvars) := NA_integer_]
  
  aara <- unique(KUBE$AARh)
  if (FyllMiss == TRUE) {
    aara <- (min(aara) + per - 1):max(aara)
  } else {
    aara <- intersect((min(aara) + per - 1):max(aara), aara)
  }
  cat("Finner", per, "-aars sum for ")
  for (aar in aara) {
    cat(aar, " ")
    lp <- paste0("list(AARl=", aar - per + 1, ",AARh=", aar, ",", lpv, ")")
    UT <- rbind(UT, KUBE[AARh %in% c((aar - per + 1):aar), eval(parse(text = lp)), by = tabs][, names(UT), with = FALSE])
  }
  cat("\n")
  for (valkol in valkols) {
    eval(parse(text = paste0("UT[", valkol, ".f>0,", valkol, ":=list(NA)]")))
  }
  
  if (AntYMiss <= per) {
    for (valkol in valkols) {
      eval(parse(text = paste0("UT[", valkol, ".fn9>", AntYMiss, ",c(\"", valkol, "\",\"", valkol, ".f\"):=list(NA,9)]")))
    }
  }
  
  f9s <- names(UT)[grepl(".f9$", names(UT))]
  if (length(f9s) > 0) {
    UT[, (f9s) := NULL]
  }
  
  return(UT)
}

#' @title handle_indata_periods
#' @description
#' If indata contains periods, val.n must be adjusted accordingly
#' 
#' @returns
#' @export
#'
#' @examples
do_handle_indata_periods <- function(){
  if (parameters$CUBEinformation$MOVAV <= 1){}
}

get_orgintmult <- function(dt, parameters){
  
}