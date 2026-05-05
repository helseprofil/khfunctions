#' @title scale_rate_and_meisskala
#' @description
#' scales RATE and MEISskala according to ACCESS::KUBER::RATESKALA
#' @noRd
scale_rate_and_meisskala <- function(dt, parameters){
  is_rateskala <- is_not_empty(parameters$CUBEinformation$RATESKALA)
  scalevalue <- as.numeric(parameters$CUBEinformation$RATESKALA)
  if(!is_rateskala) return(invisible(NULL))
  cat("\n* Skalerer RATE til per", scalevalue, "\n")
  
  if("RATE" %in% names(dt)) dt[, RATE := RATE * scalevalue]
  if("MEISskala" %in% names(dt)) dt[, MEISskala := MEISskala * scalevalue]
}

#' @title get_maltall_column
#' @description gets the column containing maltall
#' @param parameters cube parameters
#' @keywords internal
#' @noRd
get_maltall_column <- function(parameters){
  if(is_not_empty(parameters$CUBEinformation$MTKOL)) return(parameters$CUBEinformation$MTKOL)
  if(parameters$TNPinformation$NEVNERKOL == "-") return("TELLER")
  return("RATE")
}

#' @title do_format_cube_columns
#' @description
#' Adds missing columns, creates sumvalues and nonsumvalues, sets alder and aar columns
#' Creates new columns post moving average, as defined in ACCESS::TNP_PROD::NYEKOL_RAD_postMA
#' @noRd
do_format_cube_columns <- function(dt, parameters){
  add_missing_columns(dt = dt)
  add_sumvalues(dt = dt, factor = parameters$MOVAVparameters$orgintMult)
  set_nonsumvalues(dt = dt)
  set_alder_aar(dt = dt)
  if(is_not_empty(parameters$TNPinformation$NYEKOL_RAD_postMA)) compute_new_value_from_formula(dt = dt, formulas = parameters$TNPinformation$NYEKOL_RAD_postMA, post_moving_average = TRUE)
  data.table::set(dt, j = "MALTALL", value = dt[[parameters$MALTALL]])
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
  if(length(missingcolumns) > 0) data.table::set(dt, j = missingcolumns, value = NA_real_)
}

#' @title add_sumvalues
#' @description adds sumTELLER, sumNEVNER, sumPREDTELLER to data
#' @param dt dataset
#' @param factor factor to multiply orignial variables by, generated as part of MOVAV-parameters
#' @keywords internal
#' @noRd
add_sumvalues <- function(dt, factor){
  dt[, let(sumTELLER = factor * TELLER,
           sumNEVNER = factor * NEVNER,
           sumPREDTELLER = factor * PREDTELLER)]
}

#' @title set_nonsumvalues
#' @description sets original values (exept RATE/SMR) by dividing by the corresponding .n column. 
#' @param dt dataset
#' @keywords internal
#' @noRd
set_nonsumvalues <- function(dt){
  values <- setdiff(get_value_columns(names(dt)), c("RATE", "SMR"))
  if(length(values) == 0) return(dt)
  for(val in values){
    valN = paste0(val, ".n")
    data.table::set(dt, j = val, value = dt[[val]] / dt[[valN]])
  }
}

#' @keywords internal
#' @noRd
set_alder_aar <- function(dt){
  data.table::set(dt, j = "AAR", value = paste0(dt[["AARl"]], "_", dt[["AARh"]]))
  if(all(c("ALDERl", "ALDERh") %in% names(dt))){
    data.table::set(dt, j = "ALDER", value = paste0(dt[["ALDERl"]], "_", dt[["ALDERh"]]))
  } 
}

#' @title filter_invalid_outcodes
#' @description remove GEO codes not listed in ACCESS:GEOkoder
#' @param dt dataset
#' @param parameters global parameters
#' @keywords internal
#' @noRd
filter_invalid_geo_alder_kjonn <- function(dt, parameters){
  valid_geo <- parameters[["GeoKoder"]][TYP == "O" & TIL == 9999, GEO]
  dt <- dt[GEO %in% valid_geo]
  if("ALDER" %in% names(dt)) dt <- dt[!ALDER %in% c(getOption("khfunctions.alder_illegal"), getOption("khfunctions.alder_illegal"))]
  if("KJONN" %in% names(dt)) dt <- dt[!KJONN %in% c(getOption("khfunctions.illegal"), getOption("khfunctions.ukjent"))]
  return(dt)
}

#' @keywords internal
#' @noRd
get_etabs <- function(columnnames, parameters){
  spec <- parameters$fileinformation[[parameters$files$TELLER]]
  tabcols <- grep("^TAB\\d+$", columnnames, value = T)
  tabnames <- character(0)
  for(tab in tabcols){
    tabnames <- c(tabnames, spec[[tab]])
  }
  return(list(tabcols = tabcols, tabnames = tabnames))
}
  
#' @keywords internal
#' @noRd
set_etab_names <- function(dt, etablist){
  data.table::setnames(dt, old = etablist$tabcols, new = etablist$tabnames)
  return(dt)
}

#' @keywords internal
#' @noRd
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
