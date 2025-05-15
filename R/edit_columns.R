#' @title scale_rate_and_meisskala
#' @description
#' scales RATE and MEISskala according to ACCESS::KUBER::RATESKALA
#' @noRd
scale_rate_and_meisskala <- function(dt, parameters){
  is_rateskala <- is_not_empty(parameters$CUBEinformation$RATESKALA)
  scale <- as.numeric(parameters$CUBEinformation$RATESKALA)
  if(!is_rateskala) return(dt)
  cat("* Skalerer RATE til per", scale, "\n")
  
  if("RATE" %in% names(dt)) dt[, RATE := RATE * scale]
  if("MEISskala" %in% names(dt)) dt[, MEISskala := MEISskala * scale]
  return(dt)
}

#' @title do_format_cube_columns
#' @description
#' Adds missing columns, creates sumvalues and nonsumvalues, sets alder and aar columns
#' Creates new columns post moving average, as defined in ACCESS::TNP_PROD::NYEKOL_RAD_postMA
#' @noRd
do_format_cube_columns <- function(dt, parameters){
  dt <- add_missing_columns(dt = dt)
  dt <- add_sumvalues(dt = dt, factor = parameters$MOVAVparameters$orgintMult)
  dt <- set_nonsumvalues(dt = dt)
  dt <- set_alder_aar(dt = dt)
  if(is_not_empty(parameters$TNPinformation$NYEKOL_RAD_postMA)) dt <- LeggTilNyeVerdiKolonner(dt, parameters$TNPinformation$NYEKOL_RAD_postMA, slettInf = TRUE, postMA = TRUE)
  dt <- add_maltall(dt = dt, maltallcolumn = parameters$MALTALL)
  return(dt)
}

#' @title add_missing_columns
#' @description sets obligatory columns = NA if they do not exist. 
#' @param dt dataset
#' @noRd
add_missing_columns <- function(dt){
  obligcolumns <- c("TELLER","NEVNER","RATE","PREDTELLER")
  obligcolumns <- paste0(rep(obligcolumns, each = 4), c("", ".f", ".a", ".n"))
  missingcolumns <- setdiff(obligcolumns, names(dt))
  if(length(missingcolumns) > 0) dt[, (missingcolumns) := NA_real_]
  return(dt)
}

#' @title get_outvalues_allvis
#' @description finds value columns to be included in output
#' @param parameters cube parameters   
#' @noRd
get_outvalues_allvis <- function(parameters){
  cols <- character(0)
  if(parameters$CUBEinformation$REFVERDI_VP == "P") cols <- c("T", "RATE", "SMR", "MEIS")
  if(is_not_empty(parameters$CUBEinformation$NESSTARTUPPEL)){
    cols <- gsub("\\s", "", parameters$CUBEinformation$NESSTARTUPPEL)
    cols <- unlist(strsplit(cols, ","))
    if(any(!cols %in% names(getOption("khfunctions.valcols")))){
      stop("Feil i ACCESS::KUBER::NESSTARTUPPEL, aksepterte verdier (kommaseparert): ",
           paste0(names(getOption("khfunctions.valcols")), collapse = ","))
    } 
  }
  cols <- as.character(getOption("khfunctions.valcols")[cols])
  
  if(is_not_empty(parameters$CUBEinformation$EKSTRAVARIABLE)){
    extravalue <- unlist(stringr::str_split(parameters$CUBEinformation$EKSTRAVARIABLE, ","))
    cols <- c(cols, extravalue)
  }
  return(cols)
}

#' @title filter_invalid_outcodes
#' @description
#' @param dt dataset
#' @param globs global parameters
#' @noRd
filter_invalid_geo_alder_kjonn <- function(dt, globs = SettGlobs()){
  dt <- dt[GEO %in% globs$UtGeoKoder]
  if("ALDER" %in% names(dt)) dt <- dt[!ALDER %in% c(getOption("khfunctions.alder_illegal"), getOption("khfunctions.alder_illegal"))]
  if("KJONN" %in% names(dt)) dt <- dt[!KJONN %in% c(getOption("khfunctions.illegal"), getOption("khfunctions.ukjent"))]
  return(dt)
}

#' @title add_sumvalues
#' @description adds sumTELLER, sumNEVNER, sumPREDTELLER to data
#' @param dt dataset
#' @param factor factor to multiply orignial variables by, generated as part of MOVAV-parameters
#' @noRd
add_sumvalues <- function(dt, factor){
  dt[, let(sumTELLER = factor * TELLER,
           sumNEVNER = factor * NEVNER,
           sumPREDTELLER = factor * PREDTELLER)]
  return(dt)
}

#' @title set_nonsumvalues
#' @description sets original values (exept RATE/SMR) by dividing by the corresponding .n column. 
#' @param dt dataset
#' @noRd
set_nonsumvalues <- function(dt){
  values <- setdiff(get_value_columns(names(dt)), c("RATE", "SMR"))
  if(length(values) == 0) return(dt)
  for(val in values){
      valN = paste0(val, ".n")
      dt[, (val) := get(val)/get(valN)]
  }
  return(dt)
}

set_alder_aar <- function(dt){
  dt[, AAR := paste0(AARl, "_", AARh)]
  if (all(c("ALDERl", "ALDERh") %in% names(dt))) dt[, ALDER := paste0(ALDERl, "_", ALDERh)]
  return(dt)
}

#' @title get_maltall_column
#' @description gets the column containing maltall
#' @param parameters cube parameters
#' @noRd
get_maltall_column <- function(parameters){
  if(is_not_empty(parameters$CUBEinformation$MTKOL)) return(parameters$CUBEinformation$MTKOL)
  if(parameters$TNPinformation$NEVNERKOL == "-") return("TELLER")
  return("RATE")
}

#' @title add_maltall
#' @description sets maltall column
#' @param dt dataset
#' @param maltallcolumn the column containing maltall, identified with get_maltall_column()
#' @noRd
add_maltall <- function(dt, maltallcolumn){
  dt[, MALTALL := get(maltallcolumn)]
  return(dt)
}

get_etabs <- function(columnnames, parameters){
  spec <- parameters$fileinformation[[parameters$files$TELLER]]
  tabcols <- get_tab_columns(columnnames)
  tabnames <- character(0)
  for(tab in tabcols){
    tabnames <- c(tabnames, spec[[tab]])
  }
  return(list(tabcols = tabcols, tabnames = tabnames))
}
  
set_etab_names <- function(dt, etablist){
  data.table::setnames(dt, old = etablist$tabcols, new = etablist$tabnames)
  return(dt)
}

get_outdimensions <- function(dt, etabs, parameters){
  dims <- c(getOption("khfunctions.khtabs"), etabs)
  if(is_not_empty(parameters$CUBEinformation$DIMDROPP)){
    dimdropp <- unlist(strsplit(parameters$CUBEinformation$DIMDROPP, ","))
    dims <- setdiff(dims, dimdropp)
  }
  if("ALDER" %notin% names(dt)) dims <- setdiff(dims, "ALDER")
  if("KJONN" %notin% names(dt)) dims <- setdiff(dims, "KJONN")
  return(dims)
}

#' @title fix_geo_special
#' @description Manually handle bydel startaar, DK2020 and AALESUND/HARAM
#' @noRd
fix_geo_special <- function(d, specs, id = KUBEid){
  
  valK <- get_value_columns(names(d))
  bydelstart <- specs[["B_STARTAAR"]]
  dk2020 <- as.character(c(5055, 5056, 5059, 1806, 1875))
  dk2020start <- specs[["DK2020_STARTAAR"]]
  isbydelstart <- !is.na(bydelstart) && bydelstart > 0
  isdk2020 <- !is.na(dk2020start) && dk2020start > 0
  
  if(!isbydelstart && !isdk2020) return(invisible(d))
  
  cat("* Håndterer bydelsstartår og delingskommuner\n")
  
  if (isbydelstart) {
    cat(" - Sletter bydelstall for år før ", bydelstart, "\n", sep = "")
    d[GEOniv %in% c("B", "V") & AARl < bydelstart, (valK) := NA]
    d[GEOniv %in% c("B", "V") & AARl < bydelstart, (paste0(valK, ".f")) := 9]
  }
  
  if (isdk2020) {
    cat(" - Sletter kommunetall for delingskommuner for år før ", dk2020start, "\n", sep = "")
    d[GEOniv == "K" & GEO %chin% dk2020 & AARl < dk2020start, (valK) := NA]
    d[GEOniv == "K" & GEO %chin% dk2020 & AARl < dk2020start, (paste0(valK, ".f")) := 9]
    
    # Add fix for AAlesund/Haram split, which should not get data in 2020-2023, except for VALGDELTAKELSE
    cat(" - Håndterer Ålesund/Haram for årene 2020-2023\n")
    .years <- 2020:2023
    if(id == "VALGDELTAKELSE"){
      .years <- 2019:2022
    } 
    .geos <- c("1508", "1580")
    d[GEOniv == "K" & GEO %in% .geos &  (AARl %in% .years | AARh %in% .years | (AARl < min(.years) & AARh > max(.years))), (valK) := NA]
    d[GEOniv == "K" & GEO %in% .geos &  (AARl %in% .years | AARh %in% .years | (AARl < min(.years) & AARh > max(.years))), (paste0(valK, ".f")) := 9]
  }
  return(invisible(d))
}
