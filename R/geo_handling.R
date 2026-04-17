#' @title do_harmonize_geo
#' @description
#' Harmonizes geographical codes, used in `LagFilgruppe()`. 
#' Uses table KnrHarm in ACCESS, read into parameters, for recoding to current geographical codes.
#' @noRd 
do_harmonize_geo <- function(file, vals = list(), rectangularize = TRUE, parameters) {
  geoomk <- parameters$KnrHarm
  georecode <- sum(collapse::funique(file$GEO) %in% geoomk$GEO)
  if(georecode > 0){
    cat("\n*** Recoding", georecode, "geo-codes")
    file <- collapse::join(file, geoomk, on = "GEO", how = "left", overid = 0, verbose = 0)
    file[!is.na(GEO_omk), let(GEO = GEO_omk)]
    file[, let(GEO_omk = NULL, HARMstd = NULL)]
  }
  if("FYLKE" %in% names(file)) file[, FYLKE := NULL]
  file <- do_aggregate_file(file = file, valsumbardef = vals)
  
  if(rectangularize){
    rectangularized <- data.table::data.table()
    design <- find_filedesign(file, parameters = parameters)
    year <- ifelse(is_empty(parameters$year), getOption("khfunctions.year"), parameters$year)
    for (Gn in design$Part[["Gn"]][["GEOniv"]]) {
      validgeo <- data.table::data.table(GEO = parameters$GeoKoder[GEOniv == Gn & FRA <= year & TIL > year]$GEO)
      designgeo <- design$Design[HAR == 1 & GEOniv == Gn, mget(intersect(names(file), names(design$Design)))]
      rectangularized <- data.table::rbindlist(list(expand.grid.dt(designgeo, validgeo), rectangularized))
    }
    file <- collapse::join(rectangularized, file, how = "l", overid = 0, verbose = 0)
    file <- set_implicit_null_after_merge(file, implicitnull_defs = vals)
  }
  
  file[, FYLKE := ifelse(GEOniv %in% c("H", "L"), "00", substr(GEO, 1, 2))]
  return(file)
}

#' @title fix_geo_special
#' @description Manually handle bydel startaar, DK2020 and Aalesund/Haram
#' @keywords internal
#' @noRd
fix_geo_special <- function(dt, parameters){
  specs = parameters$fileinformation[[parameters$files$TELLER]] 
  valK <- get_value_columns(names(dt))
  bydelstart <- specs[["B_STARTAAR"]]
  dk2020 <- as.character(c(5055, 5056, 5059, 1806, 1875))
  dk2020start <- specs[["DK2020_STARTAAR"]]
  isbydelstart <- !is.na(bydelstart) && bydelstart > 0
  isdk2020 <- !is.na(dk2020start) && dk2020start > 0
  
  if(!isbydelstart && !isdk2020) return(invisible(dt))
  
  cat("\n* Håndterer bydelsstartår og delingskommuner\n")
  
  if (isbydelstart) {
    cat(" - Sletter bydelstall for år før ", bydelstart, "\n", sep = "")
    dt[GEOniv %in% c("B", "V") & AARl < bydelstart, (valK) := NA]
    dt[GEOniv %in% c("B", "V") & AARl < bydelstart, (c(paste0(valK, ".f"), "spv_tmp")) := 9]
  }
  
  if (isdk2020) {
    cat(" - Sletter kommunetall for delingskommuner for år før ", dk2020start, "\n", sep = "")
    dt[GEOniv == "K" & GEO %chin% dk2020 & AARl < dk2020start, (valK) := NA]
    dt[GEOniv == "K" & GEO %chin% dk2020 & AARl < dk2020start, (c(paste0(valK, ".f"), "spv_tmp")) := 9]
    
    # Add fix for AAlesund/Haram split, which should not get data in 2020-2023, except for VALGDELTAKELSE
    cat(" - Håndterer Ålesund/Haram for årene 2020-2023\n")
    ystart <- ifelse(parameters$name == "VALGDELTAKELSE", 2019, 2020)
    .years <- seq(ystart, ystart+3)
    .geos <- c("1508", "1580")
    dt[GEOniv == "K" & GEO %in% .geos &  (AARl %in% .years | AARh %in% .years | (AARl < min(.years) & AARh > max(.years))), (valK) := NA]
    dt[GEOniv == "K" & GEO %in% .geos &  (AARl %in% .years | AARh %in% .years | (AARl < min(.years) & AARh > max(.years))), (c(paste0(valK, ".f"), "spv_tmp")) := 9]
  }
  return(invisible(dt))
}