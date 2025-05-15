do_censor_kube_r <- function(dt, parameters, globs){
  if(is_not_empty(parameters$CUBEinformation$PRIKK_T)) dt <- do_censor_teller(dt = dt, limit = parameters$CUBEinformation$PRIKK_T)
  if(is_not_empty(parameters$CUBEinformation$PRIKK_N)) dt <- do_censor_nevner(dt = dt, limit = parameters$CUBEinformation$PRIKK_N)
  if(is_not_empty(parameters$CUBEinformation$OVERKAT_ANO)) dt <- do_censor_secondary(dt = dt, parameters = parameters, globs = globs)
  if(is_not_empty(parameters$CUBEinformation$STATTOL_T)) dt <- do_censor_statistical_tolerance(dt = dt, limit = parameters$CUBEinformation$STATTOL_T)
  return(dt)
}

do_censor_teller <- function(dt, limit){
  cat("T-PRIKKER", dt[TELLER <= limit, .N], "rader\n")
  cat("N-T-PRIKKER", dt[TELLER > limit & NEVNER - TELLER <= limit, .N], "rader\n")
  dt[(TELLER <= limit & TELLER.f >= 0) | (NEVNER - TELLER <= limit & TELLER.f >= 0 & NEVNER.f >= 0), 
     let(TELLER = NA, TELLER.f = 3, RATE = NA, RATE.f = 3)]
  return(dt)
}

do_censor_nevner <- function(dt, limit){
  cat("N-PRIKKER", dt[NEVNER <= limit, .N], "rader\n")
  dt[NEVNER <=limit & NEVNER.f >= 0, let(TELLER = NA, TELLER.f = 3, RATE = NA, RATE.f = 3)]
  return(dt)
}

do_censor_secondary <- function(dt, parameters, globs){
  # Naboprikking gir .f=4  slik at ikke slaar ut i HULL under
  dt <- AnonymiserNaboer(FG = dt, 
                         ovkatstr = parameters$CUBEinformation$OVERKAT_ANO, 
                         FGP = parameters$fileinformation[[parameters$files$TELLER]], 
                         ref_year_type = parameters$PredFilter$ref_year_type, 
                         globs = globs)  
  return(dt)
}

do_censor_statistical_tolerance <- function(dt, limit){
  dims <- setdiff(get_dimension_columns(names(dt)), c("AARl", "AARh")) 
  weak_limit <- getOption("khfunctions.anon_svakandel")
  missing_limit <- getOption("khfunctions.anon_hullandel")
  helper_columns <- c("n_year", "n_weak", "n_missing", "censor")
  
  dt[, (helper_columns) := NA_real_]
  dt[TELLER.f < 9, let(n_year = .N, n_weak = sum(is.na(TELLER) | TELLER <= limit), n_missing = sum(TELLER.f == 3)), by = dims]
  dt[TELLER.f < 9, censor := ifelse(n_weak / n_year > weak_limit | n_missing / n_year > missing_limit, 1, 0)]
  
  cat("Skjuler", dt[censor == 1, .N], "rader (serieprikking og svake tidsserier)\n")
  dt[censor == 1, let(TELLER = NA, TELLER.f = 3, RATE = NA, RATE.f = 3)]
  dt[, (helper_columns) := NULL]
  return(dt)
}

#' AnonymiserNaboer (kb)
#' 
#'
#' @param FG 
#' @param ovkatstr 
#' @param FGP 
#' @param ref_year_type 
#' @param globs
AnonymiserNaboer <- function(FG, ovkatstr, FGP = list(amin = 0, amax = 120), ref_year_type = "Moving", globs = SettGlobs()) {
  # is_kh_debug()
  FG <- data.table::copy(FG)
  AoverkSpecs <- SettNaboAnoSpec(ovkatstr, FGP = FGP, globs = globs)
  
  vals <- get_value_columns(names(FG))
  # FinnValKolsF funker ikke riktig!!!! Baade pga nye flag slik som fn9 og pga verdikolonner uten .f (MEISskala) etc
  # Maa utbedres gjennomgripende, men kan ikke gjoere dette naa derfor bare denne ad hoc loesninga
  if (ref_year_type == "Moving") {
    alletabs <- setdiff(names(FG), get_value_columns(names(FG), full = TRUE))
  } else {
    alletabs <- intersect(c("GEO", "GEOniv", "FYLKE", "AARl", "AARh", "ALDERl", "ALDERh", "KJONN", "TAB1", "TAB2", "UTDANN", "INNVKAT", "LANDBAK"), names(FG))
  }
  for (ovkSpec in AoverkSpecs) {
    FGt <- FG[eval(parse(text = ovkSpec$subcond)), ]
    FGr <- FG[!eval(parse(text = ovkSpec$subcond)), ]
    overkats <- ovkSpec$overkat
    for (val in vals) {
      eval(parse(text = paste0(
        "FGt[,", val, ".na:=0]"
      )))
    }
    for (i in 1:length(overkats)) {
      kombs <- utils::combn(names(overkats), i)
      for (j in 1:ncol(kombs)) {
        substrs <- character(0)
        overtabs <- character(0)
        for (del in kombs[, j]) {
          substrs <- c(substrs, overkats[[del]]$over)
          overtabs <- c(overtabs, overkats[[del]]$kols)
        }
        substr <- paste0("(", substrs, ")", collapse = " | ")
        for (val in vals) {
          bycols <- setdiff(alletabs, overtabs)
          eval(parse(text = paste0(
            "FGt[!(", substr, "),", val, ".na:=ifelse((", val, ".na==1 | any(", val, ".f %in% 3:4)),1,0),by=bycols]"
          )))
        }
      }
    }
    
    for (val in vals) {
      eval(parse(text = paste0(
        "FGt[", val, ".na==1,", val, ".f:=4]"
      )))
      eval(parse(text = paste0(
        "FGt[", val, ".na==1,", val, ":=NA]"
      )))
      eval(parse(text = paste0(
        "FGt[,", val, ".na:=NULL]"
      )))
    }
    
    FG <- rbind(FGt, FGr)
  }
  return(FG)
}


#' @title do_stata_censoring
#' @description
#' Reads censoring information from ACCESS. 
#' If Stata_PRIKK parameters are set, the STATA censoring script (by Jørgen Meisfjord) 
#' is run using the do_stata_processing function
#' @param dt data file to be censored
#' @param spc KUBE spec
#' @param batchdate batchdate, to be used for temporary files
#' @param stata_exe path to STATA program
do_stata_censoring <- function(dt, spc, batchdate, stata_exe){
  stataVar <- c("Stata_PRIKK_T", "Stata_PRIKK_N", "Stata_STATTOL_T")
  RprikkVar <- c("PRIKK_T", "PRIKK_N", "STATTOL_T")
  spc <- data.table::as.data.table(spc)[, mget(c(stataVar, RprikkVar))]
  s_prikk <- sum(sapply(spc[, ..stataVar], get_col), na.rm = TRUE)
  r_prikk <- sum(sapply(spc[, ..RprikkVar], get_col), na.rm = TRUE)
  check_if_only_r_or_stata_prikk(r = r_prikk, s = s_prikk)
  if(s_prikk == 0) return(dt)
  
  sfile <- file.path(getOption("khfunctions.root"), getOption("khfunctions.stataprikkfile"))
  synt <- paste0('include "', sfile, '"')
  dt <- do_stata_processing(TABLE = dt, script = synt, batchdate = batchdate, stata_exe = stata_exe)
  return(dt)
}

check_if_only_r_or_stata_prikk <- function(r, s){
  if (r > 0 & s > 0) stop("You can't prikk for both R and Stata way. Choose either one!")
  invisible()
}

get_col <- function(var, num = TRUE){
  if(is.na(var) || var == "") var <- NA
  if(num) var <- var_num(var)
  if(!is.na(var) && num) var <- 1
  return(var)
}

var_num <- function(x){
  if(!is.numeric(x)) x <- NA
  return(x)
}

#' @title do_remove_censored_observations
#' @description
#' Removes censored observations and set SPVFLAGG
#' 
#' Først beregnes max .f-variabel for rader der ingen av .f-variablene == 2. (tSPV_uten2) og for alle (tSPV_alle)
#' Dette er unoedvendig kronglete. Men dersom f.eks RATE.f=2 pga TELLER.f=1, oenskes SPVFLAGG=1. 
#' tSPV_uten2 prioriteres, og dersom denne == 0 vil tSPV_alle brukes for å sette SPVFLAGG.
#' @noRd
do_remove_censored_observations <- function(dt, outvalues){
  valF <- paste0(union(getOption("khfunctions.valcols"), outvalues), ".f")
  valF <- intersect(names(dt), valF)
  dt[, let(SPVFLAGG = 0, tSPV_alle = 0, tSPV_uten2 = 0)]
  if(length(valF) > 0){
    dt[, tSPV_alle := do.call(pmax, c(.SD, list(na.rm = T))), .SDcols = valF]
    
    dt[rowSums(dt[, ..valF] == 2) == 0, tSPV_uten2 := do.call(pmax, c(.SD, list(na.rm = T))), .SDcols = valF]
    dt[, SPVFLAGG := ifelse(tSPV_uten2 == 0, tSPV_alle, tSPV_uten2)]
    dt[, c("tSPV_uten2", "tSPV_alle") := NULL]
    dt[SPVFLAGG > 0, (outvalues) := NA]
  }
  dt[is.na(SPVFLAGG), SPVFLAGG := 0]
  dt[SPVFLAGG %in% c(-1, 4), SPVFLAGG := 3]
  dt[SPVFLAGG == 9, SPVFLAGG := 1]
  return(dt)
}
