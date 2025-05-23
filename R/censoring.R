#' @title do_censor_kube_r
#' @description
#' old censoring routine, most censoring is now done in STATA
#' @keywords internal
#' @noRd
do_censor_kube_r <- function(dt, parameters){
  if(is_not_empty(parameters$CUBEinformation$PRIKK_T)) dt <- do_censor_teller(dt = dt, limit = parameters$CUBEinformation$PRIKK_T)
  if(is_not_empty(parameters$CUBEinformation$PRIKK_N)) dt <- do_censor_nevner(dt = dt, limit = parameters$CUBEinformation$PRIKK_N)
  if(is_not_empty(parameters$CUBEinformation$OVERKAT_ANO)) dt <- do_censor_secondary(dt = dt, parameters = parameters)
  if(is_not_empty(parameters$CUBEinformation$STATTOL_T)) dt <- do_censor_statistical_tolerance(dt = dt, limit = parameters$CUBEinformation$STATTOL_T)
  return(dt)
}

#' @keywords internal
#' @noRd
do_censor_teller <- function(dt, limit){
  cat("T-PRIKKER", dt[TELLER <= limit, .N], "rader\n")
  cat("N-T-PRIKKER", dt[TELLER > limit & NEVNER - TELLER <= limit, .N], "rader\n")
  dt[(TELLER <= limit & TELLER.f >= 0) | (NEVNER - TELLER <= limit & TELLER.f >= 0 & NEVNER.f >= 0), 
     let(TELLER.f = 3, RATE.f = 3)]
  return(dt)
}

#' @keywords internal
#' @noRd
do_censor_nevner <- function(dt, limit){
  cat("N-PRIKKER", dt[NEVNER <= limit, .N], "rader\n")
  dt[NEVNER <= limit & NEVNER.f >= 0, let(TELLER.f = 3, RATE.f = 3)]
  return(dt)
}

#' @keywords internal
#' @noRd
# do_censor_secondary <- function(dt, parameters){
#   # Naboprikking gir .f=4  slik at ikke slaar ut i HULL under
#   dt <- AnonymiserNaboer(FG = dt, 
#                          ovkatstr = parameters$CUBEinformation$OVERKAT_ANO, 
#                          FGP = parameters$fileinformation[[parameters$files$TELLER]], 
#                          ref_year_type = parameters$PredFilter$ref_year_type, 
#                          parameters = parameters)  
#   return(dt)
# }

#' @keywords internal
#' @noRd
do_censor_statistical_tolerance <- function(dt, limit){
  dims <- setdiff(get_dimension_columns(names(dt)), c("AARl", "AARh")) 
  weak_limit <- getOption("khfunctions.anon_svakandel")
  missing_limit <- getOption("khfunctions.anon_hullandel")
  helper_columns <- c("n_year", "n_weak", "n_missing", "censor")
  
  dt[, (helper_columns) := NA_real_]
  dt[TELLER.f < 9, let(n_year = .N, n_weak = sum(is.na(TELLER) | TELLER <= limit), n_missing = sum(TELLER.f == 3)), by = dims]
  dt[TELLER.f < 9, censor := ifelse(n_weak / n_year > weak_limit | n_missing / n_year > missing_limit, 1, 0)]
  
  cat("Skjuler", dt[censor == 1, .N], "rader (serieprikking og svake tidsserier)\n")
  dt[censor == 1, let(TELLER.f = 3, RATE.f = 3)]
  dt[, (helper_columns) := NULL]
  return(dt)
}

#' @title do_censor_secondary
#' @description Naboprikking gir .f=4  slik at ikke slaar ut i HULL under
#' @param dt data
#' @param parameters global parameters
#' @keywords internal
#' @noRd
do_censor_secondary <- function(dt, parameters) {
  FG <- data.table::copy(dt)
  AoverkSpecs <- SettNaboAnoSpec(parameters = parameters)
  
  vals <- get_value_columns(names(FG))
  # FinnValKolsF funker ikke riktig!!!! Baade pga nye flag slik som fn9 og pga verdikolonner uten .f (MEISskala) etc
  # Maa utbedres gjennomgripende, men kan ikke gjoere dette naa derfor bare denne ad hoc loesninga
  if (parameters$PredFilter$ref_year_type == "Moving") {
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

#' SettNaboAnoSpec (kb)
#' @param parameters global parameters
#' @keywords internal
#' @noRd
SettNaboAnoSpec <- function(parameters) {
  
  ovkatspec <- parameters$CUBEinformation$OVERKAT_ANO 
  FGP <- parameters$fileinformation[[parameters$files$TELLER]]
  
  Foverkat <- list()
  if (is_not_empty(ovkatspec)) {
    specs <- unlist(stringr::str_split(ovkatspec, ";"))
    i <- 1
    for (spec in specs) {
      if (grepl("\\[(.*?)\\]=\\[.*\\]", spec)) {
        subcond <- gsub("^\\[(.*?)\\]=\\[.*\\]", "\\1", spec)
        subcond <- paste0("(", subcond, ")")
        ovkatstr <- gsub("^\\[(.*?)\\]=\\[(.*)\\]", "\\2", spec)
      } else {
        subcond <- "TRUE"
        ovkatstr <- spec
      }
      
      overkat <- list()
      ovkatstr <- gsub("([^=]+)=([^=]+)", "\\1==\\2", ovkatstr)
      ovkatstr <- gsub("(.*)ALDER=='*ALLE'*(.*)", paste0("\\1", "ALDER==", FGP$amin, "_", FGP$amax, "\\2"), ovkatstr)
      ovkatstr <- gsub("(.*)ALDER=='*(\\d+)_('| )(.*)", paste0("\\1", "ALDER==\\2_", FGP$amax, "\\3\\4"), ovkatstr)
      for (del in names(parameters$DefDesign$DelKolN)) {
        delN <- parameters$DefDesign$DelKolN[del]
        if (parameters$DefDesign$DelType[del] == "COL") {
          if (grepl(paste0("(^|\\&) *", delN, " *== *'*(.*?)'* *(\\&|$)"), ovkatstr)) {
            over <- gsub(paste0(".*(^|\\&) *(", delN, " *== *'*.*?'*) *(\\&|$).*"), "\\2", ovkatstr)
            overkat[[del]] <- list(over = over, kols = delN)
          }
        } else if (parameters$DefDesign$DelType[del] == "INT") {
          if (grepl(paste0("(^|\\&) *", delN, "l *== *'*(.*?)'* *($|\\&)"), ovkatstr) &&
              grepl(paste0("(^|\\&) *", delN, "h *== *'*(.*?)'* *($|\\&)"), ovkatstr)) {
            overl <- gsub(paste0(".*(^|\\&) *(", delN, "l *== *'*.*?)'* *($|\\&).*"), "\\2", ovkatstr)
            overh <- gsub(paste0(".*(^|\\&) *(", delN, "h *== *'*.*?)'* *($|\\&).*"), "\\2", ovkatstr)
            overkat[[del]] <- list(over = paste(overl, overh, sep = " & "), kols = paste0(delN, c("l", "h")))
          } else if (grepl(paste0("(^|\\&) *", delN, " *== *'*(.*?)'* *($|\\&)"), ovkatstr)) {
            intval <- unlist(stringr::str_split(gsub(paste0("(^|.*\\&) *", delN, " *== *'*(.*?)'* *($|\\&.*)"), "\\2", ovkatstr), "_"))
            if (length(intval) == 1) {
              intval <- c(intval, intval)
            }
            over <- paste0(paste0(delN, "l"), "==", intval[1], " & ", paste0(delN, "h"), "==", intval[2])
            overkat[[del]] <- list(over = over, kols = paste0(delN, c("l", "h")))
          }
        }
      }
      Foverkat[[i]] <- list(subcond = subcond, overkat = overkat)
      i <- i + 1
    }
  }
  return(Foverkat)
}

#' @title do_stata_censoring
#' @description
#' Reads censoring information from ACCESS. 
#' If Stata_PRIKK parameters are set, the STATA censoring script (by Jørgen Meisfjord) 
#' is run using the do_stata_processing function
#' @param dt data file to be censored
#' @param parameters cube parameters
#' @keywords internal
#' @noRd
do_stata_censoring <- function(dt, parameters){
  stataVar <- c("Stata_PRIKK_T", "Stata_PRIKK_N", "Stata_STATTOL_T")
  RprikkVar <- c("PRIKK_T", "PRIKK_N", "STATTOL_T")
  spc <- data.table::as.data.table(parameters$CUBEinformation)[, .SD, .SDcols = c(stataVar, RprikkVar)]
  s_prikk <- sum(sapply(spc[, ..stataVar], get_col), na.rm = TRUE)
  r_prikk <- sum(sapply(spc[, ..RprikkVar], get_col), na.rm = TRUE)
  check_if_only_r_or_stata_prikk(r = r_prikk, s = s_prikk)
  if(s_prikk == 0) return(dt)
  
  sfile <- file.path(getOption("khfunctions.root"), getOption("khfunctions.stataprikkfile"))
  synt <- paste0('include "', sfile, '"')
  dt <- do_stata_processing(TABLE = dt, script = synt, batchdate = parameters$batchdate, stata_exe = parameters$StataExe)
  return(dt)
}

#' @keywords internal
#' @noRd
check_if_only_r_or_stata_prikk <- function(r, s){
  if (r > 0 & s > 0) stop("You can't prikk for both R and Stata way. Choose either one!")
  invisible()
}

#' @keywords internal
#' @noRd
get_col <- function(var, num = TRUE){
  if(is.na(var) || var == "") var <- NA
  if(num) var <- var_num(var)
  if(!is.na(var) && num) var <- 1
  return(var)
}

#' @keywords internal
#' @noRd
var_num <- function(x){
  if(!is.numeric(x)) x <- NA
  return(x)
}

#' @keywords internal
#' @noRd
get_geonaboprikk_triangles <- function(){
  data.table::data.table("Stata_naboprGeo_LF" = paste0("niva1", getOption("khfunctions.geoprikk")$LF),
                         "Stata_naboprGeo_FK" = paste0("niva2", getOption("khfunctions.geoprikk")$FK),
                         "Stata_naboprGeo_KB" = paste0("niva3", getOption("khfunctions.geoprikk")$KB))
}

#' @title save_kubespec_csv (ybk)
#' @description Saves ACCESS specs + list of dimensions to be used in STATA censoring
#' @keywords internal
#' @noRd
save_kubespec_csv <- function(spec, dims = NULL, geonaboprikk = NULL, geoprikktriangel = NULL){
  rootDir <- file.path(fs::path_home(), "helseprofil")
  if (!fs::dir_exists(rootDir))
    fs::dir_create(rootDir)
  
  specDF <- data.table::as.data.table(spec)
  varStata <- grep("^Stata", names(specDF), value = TRUE)
  varSpec <- c("KUBE_NAVN", varStata)
  varDF <- specDF[, .SD, .SDcols = varSpec]
  if(!is.null(dims)) varDF[, DIMS := list(dims)]
  if(!is.null(geonaboprikk)) varDF[, GEOnaboprikk := as.character(geonaboprikk)]
  if(!is.null(geoprikktriangel)) varDF[, names(geoprikktriangel) := geoprikktriangel]
  fileSpec <- file.path(rootDir, "kubespec.csv")
  data.table::fwrite(varDF, fileSpec, sep = ";", sep2 = c("", " ", ""))
  return(invisible(specDF))
}

#' find_dims (vl)
#' 
#' Helper function for kube_spec, finding dimensions in KUBE
#' @keywords internal
#' @noRd
find_dims_for_stataprikk <- function(dt, etabs){
  alldims <- c(getOption("khfunctions.khtabs"), etabs$tabnames)
  alldims[alldims %in% names(dt)]
}

#' @title do_remove_censored_observations
#' @description
#' Removes censored observations and set SPVFLAGG
#' 
#' Først beregnes max .f-variabel for rader der ingen av .f-variablene == 2. (tSPV_uten2) og for alle (tSPV_alle)
#' Dette er unoedvendig kronglete. Men dersom f.eks RATE.f=2 pga TELLER.f=1, oenskes SPVFLAGG=1. 
#' tSPV_uten2 prioriteres, og dersom denne == 0 vil tSPV_alle brukes for å sette SPVFLAGG.
#' @keywords internal
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