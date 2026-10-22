extend_to_periods <- function(dt, periods){
  out <- data.table::copy(dt)[0, ]
  for(i in 1:nrow(periods)){
    aarl <- periods[i, AARl]
    aarh <- periods[i, AARh]
    newperiod <- dt[AARl >= aarl & AARh <= aarh][, let(AARl = aarl, AARh = aarh)]
    out <- data.table::rbindlist(list(out, newperiod))
  }
  return(out)
}

#' @title do_harmonize_geo
#' @description
#' Harmonizes geographical codes, used in `LagFilgruppe()`. 
#' Uses table KnrHarm in ACCESS, read into parameters, for recoding to current geographical codes.
#' @noRd 
do_harmonize_geo <- function(file, vals = list(), rectangularize = TRUE, parameters) {
  geoomk <- parameters$KnrHarm
  georecode <- sum(collapse::funique(file$GEO) %in% geoomk$GEO)
  if(georecode > 0){
    khtools::msg("\n*** Recoding", georecode, "geo-codes")
    file <- collapse::join(file, geoomk, on = "GEO", how = "left", overid = 0, verbose = 0)
    file[!is.na(GEO_omk), let(GEO = GEO_omk)]
    file[, GEO_omk := NULL]
    if("HARMstd" %in% names(file)) file[, HARMstd := NULL]
  }
  if("FYLKE" %in% names(file)) file[, FYLKE := NULL]
  file <- do_aggregate_file(file = file, valsumbardef = vals)
  
  if(rectangularize){
    rectangularized <- data.table::data.table()
    design <- find_filedesign(file, parameters = parameters)
    year <- ifelse(is_empty(parameters$year), getOption("khfunctions.profileyear"), parameters$year)
    for (Gn in design$Part[["Gn"]][["GEOniv"]]) {
      validgeo <- data.table::data.table(GEO = parameters$GeoKoder[GEOniv == Gn & FRA <= year & TIL > year]$GEO)
      designgeo <- design$Design[HAR == 1 & GEOniv == Gn, .SD, .SDcols = intersect(names(file), names(design$Design))]
      rectangularized <- data.table::rbindlist(list(expand.grid.dt(designgeo, validgeo), rectangularized))
    }
    file <- collapse::join(rectangularized, file, how = "l", overid = 0, verbose = 0)
    set_implicit_null_after_merge(dt = file, implicitnull_defs = vals)
  }
  
  file[, FYLKE := ifelse(GEOniv %in% c("H", "L"), "00", substr(GEO, 1, 2))]
  return(file)
}


#' @title ungdata_reshape_questions
#' @description
#' For å lage kombinasjonsindikatorer av ungdata må dataene reshapes før den generelle RSYNT1-snutten slår inn
#' Denne funksjonen reshaper data og lager et langt datasett hvor disse spørsmålene er lagt i TAB1 og TAB3
#' @noRd
ungdata_reshape_questions <- function(data, questions, filedescription){
  cols <- intersect(names(data), questions)
  khtools::msg("- reshaper følgende spørsmål til TAB1 (svar) + TAB3 (spørsmål): ", paste(cols, collapse = ", "))
  data <- data.table::melt(data, 
                           variable.name = filedescription[["TAB3"]], 
                           value.name = filedescription[["TAB1"]], 
                           measure.vars = cols, 
                           variable.factor = FALSE)
  data <- data[!is.na(x), env = list(x = filedescription[["TAB1"]])]
  return(data)
}
