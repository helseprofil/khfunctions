#' @title generate_and_export_all_friskvik_indicators
#' @description
#' Loops over requested friskvik files and export the file as csv
#' 
#' @param dt ALLVIS cube
#' @param parameters global parameters
generate_and_export_all_friskvik_indicators <- function(dt, parameters) {
  if(!parameters$write) return(invisible(NULL))
  indikatorer <- parameters$friskvik[, .SD, .SDcols = c("INDIKATOR", "ID")]
  if (nrow(indikatorer) == 0){
    cat("\n** INGEN FRISKVIKFILER SATT OPP")
    return(invisible(NULL))
  }
  
  for(i in 1:nrow(indikatorer)){ 
    cat("\n** Lager Friskvikfil for", indikatorer[i, INDIKATOR])
    generate_friskvik_indicator(dt = dt, id = indikatorer[i, ID], parameters = parameters)
  }
}

#' @title generate_friskvik_indicator
#' @param dt ALLVIS cube
#' @param parameters parameters
#' @param id friskvik ID
generate_friskvik_indicator <- function(dt, id, parameters) {
  FVdscr <- parameters$friskvik[ID == id]
  if(!FVdscr$MODUS %in% c("K", "F", "B")){
    cat("ADVARSEL!!!!!!!! modus ", FVdscr$MODUS, "kan ikke brukes i FRISKVIK ikke\nFriskvikfil for ID =", id, "kan ikke genereres")
    return(invisible(NULL))
  }
  
  switch(FVdscr$MODUS, 
         "K" = {
           FriskVDir <- ifelse(FVdscr$PROFILTYPE == "FHP", getOption("khfunctions.fhpK"), getOption("khfunctions.ovpK"))
           GEOfilter <- c("K", "F", "L")
         },
         "B" = {
           FriskVDir <- ifelse(FVdscr$PROFILTYPE == "FHP", getOption("khfunctions.fhpB"), getOption("khfunctions.ovpB"))
           GEOfilter <- c("B", "K", "F", "L")
         },
         "F" = {
           FriskVDir <- ifelse(FVdscr$PROFILTYPE == "FHP", getOption("khfunctions.fhpF"), getOption("khfunctions.ovpF"))
           GEOfilter <- c("F", "L")
         })
  
  d <- dt[GEOniv %in% GEOfilter]
  d <- do_filter_friskvik_age(dt = d, age_filter = FVdscr$ALDER, parameters = parameters)
  d <- do_filter_friskvik_tabs(dt = d, dscr = FVdscr)
  exprows <- parameters$GeoKoder[TYP == "O" & GEOniv %in% GEOfilter & FRA <= parameters$year & TIL > parameters$year, .N]
  if(nrow(d) != exprows && !grepl("UNGDATA", parameters$cube_name)){
    stop("FEIL I FRISKVIKFILTER som gir ", nrow(d), " / ", exprows, " forventede rader!")
  }
  
  d[, (setdiff(getOption("khfunctions.profiltabs"), names(d))) := NA_character_]
  missing <- setdiff(c(getOption("khfunctions.profiltabs"), getOption("khfunctions.profilvals")), names(d))
  if (length(missing) > 0) {
    cat("\n!!OBS, Kolonnene", missing, "mangler i friskvikfilen, settes til NA!")
    d[, (missing) := NA]
  }
  
  if(is_not_empty(FVdscr$ALTERNATIV_MALTALL)){
    d[, MALTALL := get(FVdscr$ALTERNATIV_MALTALL)]
    d[, setdiff(getOption("khfunctions.profilvals"), "MALTALL") := NA]
  }
  
  d[SPVFLAGG > 0, getOption("khfunctions.profilvals") := NA]
  d <- d[, mget(c(getOption("khfunctions.profiltabs"), getOption("khfunctions.profilvals")))]
  
  setPath <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kubedir"), FriskVDir, parameters$year, "csv")
  
  if (!fs::dir_exists(setPath)) fs::dir_create(setPath)
  
  utfiln <- file.path(setPath, paste0(FVdscr$INDIKATOR, "_", parameters$batchdate, ".csv"))
  cat("-->> FRISKVIK EKSPORT:", utfiln, "\n")
  data.table::fwrite(FRISKVIK, utfiln, sep = ";", row.names = FALSE)
} 

#' @keywords internal
#' @noRd
do_filter_friskvik_age <- function(dt, age_filter, parameters){
  if(is_empty(age_filter) || age_filter == "-") return(dt)
  amin <- parameters$fileinformation[[parameters$files$TELLER]]$amin
  amax <- parameters$fileinformation[[parameters$files$TELLER]]$amax
  age_filter <- FVdscr$ALDER
  age_filter <- gsub("^(\\d+)$", "\\1_\\1", age_filter)
  age_filter <- gsub("^(\\d+)_$", paste0("\\1_", amax), age_filter)
  age_filter <- gsub("^_(\\d+)$", paste0(amin, "_\\1"), age_filter)
  age_filter <- gsub("^ALLE$", paste0(amin, "_", amax), age_filter)
  if(!grepl("^\\d+_\\d+$", age_filter)) stop("FRISKVIK::ALDER har feil format, må være X_X, X_, _X eller ALLE")
  return(d[ALDER == age_filter])
}

#' @keywords internal
#' @noRd
do_filter_friskvik_tabs <- function(dt, dscr){
  for (tab in c("AARh", "KJONN", "INNVKAT", "UTDANN", "LANDBAK")) {
    if(is_not_empty(dscr[[tab]]) && dscr[[tab]] != "-") dt <- dt[get(tab) == dscr[[tab]]]
  }
  
  if(is_not_empty(dscr$EKSTRA_TAB) && dscr$EKSTRA_TAB != "-"){
    dt <- dt[eval(rlang::parse_expr(dscr$EKSTRA_TAB))]
    dt[, ETAB := dscr$EKSTRA_TAB]
  }
  return(dt)
}


