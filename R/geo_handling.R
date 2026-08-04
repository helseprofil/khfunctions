#' @title do_harmonize_geo
#' @description
#' Harmonizes geographical codes, used in `LagFilgruppe()`. 
#' Uses table KnrHarm in ACCESS, read into parameters, for recoding to current geographical codes.
#' @noRd 
do_harmonize_geo <- function(file, vals = list(), rectangularize = TRUE, parameters) {
  geoomk <- parameters$KnrHarm
  georecode <- sum(collapse::funique(file$GEO) %in% geoomk$GEO)
  if(georecode > 0){
    print_console_message("\n*** Recoding", georecode, "geo-codes")
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
      designgeo <- design$Design[HAR == 1 & GEOniv == Gn, .SD, .SDcols = intersect(names(file), names(design$Design))]
      rectangularized <- data.table::rbindlist(list(expand.grid.dt(designgeo, validgeo), rectangularized))
    }
    file <- collapse::join(rectangularized, file, how = "l", overid = 0, verbose = 0)
    set_implicit_null_after_merge(dt = file, implicitnull_defs = vals)
  }
  
  file[, FYLKE := ifelse(GEOniv %in% c("H", "L"), "00", substr(GEO, 1, 2))]
  return(file)
}

#' @title fix_geo_special
#' @description Manually handle bydel startaar, DK2020 and Aalesund/Haram
#' @keywords internal
#' @noRd
fix_geo_special <- function(dt, parameters){
  geonivs <- unique(dt[["GEOniv"]])
  specs <- parameters$fileinformation[[parameters$files$TELLER]] 
  vals <- get_value_columns(names(dt))
  flags <- intersect(c("spv_tmp", grep("\\.f$", names(dt), value = T)), names(dt))
  bydelstart <- specs[["B_STARTAAR"]]
  dk2020 <- as.character(c(5055, 5056, 5059, 1806, 1875))
  dk2020start <- specs[["DK2020_STARTAAR"]]
  isbydelstart <- !is.na(bydelstart) && bydelstart > 0 & any(geonivs %in% c("B", "V"))
  isdk2020 <- !is.na(dk2020start) && dk2020start > 0 & "K" %in% geonivs
  
  if(!isbydelstart && !isdk2020) return(invisible(NULL))
  
  # STATA-prikkingen er avhengig av at alle måltall settes til NA
  # Dersom vi går over til R-prikking, kan måltallene bevares
  if (isbydelstart) {
    print_console_message("\n* Håndterer bydelsstartår (bydeler og levekårssoner)\n")
    print_console_message(" - Sletter tall for år før ", bydelstart, " dersom de finnes\n", sep = "")
    idx <- which(dt[["GEOniv"]] %in% c("B", "V") & dt[["AARl"]] < bydelstart)
    data.table::set(dt, i = idx, j = flags, value = 9L)
    # data.table::set(dt, i = idx, j = vals, value = NA)
  }
  
  # Fjerner tall før startår for LKS, som definert i tabell ACCESS::LKS_STARTAAR
  # Dette gjøres uansett om bydelstart er satt i access eller ikke, dersom kuben har levekårssonedata. 
  if("V" %in% unique(dt[["GEOniv"]])){
    print_console_message("\n* Håndterer startår for levekårssoner\n")
    dt[parameters$LKS_STARTAAR, lks_startaar := i.lks_startaar, on = "GEO"]
    idx <- which(dt[["AARl"]] < dt[["lks_startaar"]])
    data.table::set(dt, i = idx, j = flags, value = 9L)
    # data.table::set(dt, i = idx, j = vals, value = NA)
    data.table::set(dt, j = "lks_startaar", value = NULL)
  }
  
  if (isdk2020) {
    print_console_message("\n* Håndterer delingskommuner 2020 (DK2020) \n")
    print_console_message(" - Sletter kommunetall for delingskommuner for år før ", dk2020start, "\n", sep = "")
    idx <- which(dt[["GEOniv"]] == "K" & dt[["GEO"]] %chin% dk2020 & dt[["AARl"]] < dk2020start)
    data.table::set(dt, i = idx, j = flags, value = 9L)
    # data.table::set(dt, i = idx, j = vals, value = NA)
    
    # Add fix for AAlesund/Haram split, which should not get data in 2020-2023, except for VALGDELTAKELSE
    print_console_message(" - Håndterer Ålesund/Haram for årene 2020-2023\n")
    ystart <- ifelse(parameters$name == "VALGDELTAKELSE", 2019, 2020)
    ystop <- ystart + 3
    idx <- which(dt[["GEO"]] %in% c("1508", "1580") & (dt[["AARl"]] <= ystop & dt[["AARh"]] >= ystart))
    data.table::set(dt, i = idx, j = flags, value = 9L)
    # data.table::set(dt, i = idx, j = vals, value = NA)
  }
  
  idx <- which(dt[["spv_tmp"]] == 9)
  data.table::set(dt, i = idx, j = "geoprikket", value = 1L)
  return(invisible(NULL))
}

#' @title do_handle_coverage
#' @keywords internal
#' @noRd
do_handle_coverage <- function(dt, geolevel = c("B", "V"), parameters){
  if("dekningprikket" %notin% names(dt)) dt[, dekningprikket := 0L]
  geolevel <- match.arg(geolevel)
  if(geolevel %notin% collapse::funique(dt[["GEOniv"]])) return(invisible(NULL))
  print_console_message(paste0("\n** Skjuler tall med dårlig dekning for GEOniv == '", geolevel, "'"))
  print_console_message("\n*** Originalt", dt[GEOniv == geolevel, .N], "rader")
  # Sette inn kommentar om kriteriene?
  dims <- parameters$outdimensions
  flags <- c(grep("\\.f$", names(dt), value = T))
  skjul <- get_deletestrata(dt, dims, geolevel)
  if(skjul[, .N] > 0){
    dt[skjul, dekningprikket := 1L, on = dims]
    n_new <- dt[spv_tmp == 0 & dekningprikket == 1L, .N]
    dt[spv_tmp == 0 & dekningprikket == 1L, (c(flags, "spv_tmp")) := 1L]
    print_console_message("\n***", n_new, "rader skjules")
  } else {
    print_console_message("\n*** Ingen rader skjules")
  }
}


#' @title get_deletestrata
#' @keywords internal
#' @noRd
get_deletestrata <- function(dt, dims, level){
  overcat <- switch(level,
                    "B" = collapse::funique(substr(dt[GEOniv == "B", GEO], 1, 4)),
                    "V" = sub("00$", "", collapse::funique(substr(dt[GEOniv == "V", GEO], 1, 6))))
  deletestrata <- data.table::copy(dt)[(GEOniv == level | GEO %in% overcat), .SD, .SDcols = c("GEOniv", dims, "sumTELLER", "sumNEVNER")]
  overlevels <- switch(level, 
                       "B" = "K", 
                       "V" = c("K", "B"))
  length_overniv <- ifelse(level == "B", 4, 6)
  deletestrata[, let(GEOniv = data.table::fifelse(GEOniv %in% overlevels, "Over", "Under"),
                     overniv = sub("00$", "", substr(GEO, 1, length_overniv)))]

  deletecodes <- unique(deletestrata[GEOniv == "Under", .SD, .SDcols = c("overniv", "GEO")])
  
  bycols <- c("overniv", "GEOniv", setdiff(dims, "GEO"))
  g <- collapse::GRP(deletestrata, bycols)
  deletestrata <- collapse::add_vars(g[["groups"]],
                                     keep = collapse::fsum(is.na(deletestrata[["sumTELLER"]]), g = g) == 0,
                                     collapse::fsum(collapse::get_vars(deletestrata, c("sumTELLER", "sumNEVNER")), g = g))[keep == TRUE][, keep := NULL]
  deletestrata <- data.table::melt(deletestrata, measure.vars = c("sumTELLER", "sumNEVNER"), variable.name = "col", value.name = "val")
  deletestrata <- data.table::dcast(deletestrata, ... ~ GEOniv, value.var = "val")
  deletestrata[, ukjent := 1 - (Under/Over)] 
  deletestrata[is.nan(ukjent) & Over == 0 & Under == 0, ukjent := 0]
  bycols <- setdiff(bycols, "GEOniv")
  f <- as.formula(paste(paste(bycols, collapse = " + "), "~ col"))
  deletestrata <- data.table::dcast(deletestrata, formula = f, value.var = "ukjent")
  deletestrata[, diff := sumTELLER - sumNEVNER]
  deletestrata <- deletestrata[sumTELLER > getOption("khfunctions.dekning_sumteller") | abs(diff) > getOption("khfunctions.dekning_diff"), .SD, .SDcols = bycols]
  
  delete <- collapse::join(deletestrata, deletecodes, on = "overniv", multiple = TRUE, verbose = FALSE, overid = 2)[, .SD, .SDcols = dims]
  return(delete)
}

#' @title add_missing_lks
#' @param dt 
#' @param parameters 
#' @noRd
add_missing_lks <- function(dt, parameters){
  if(!"V" %in% unlist(strsplit(parameters$CUBEinformation$GEOniv, ","))) return(invisible(NULL))
  
  # Legg til soner for kommuner med bare en sone
  single <- data.table::data.table(lks = parameters$GeoKoder[GEOniv == "V", unique(GEO)])
  single[, overniv := sub("00$", "", substr(lks, 1, 6))]
  single[, N := .N, by = overniv]
  single <- single[N == 1]
  if(nrow(single) > 0){
    add_single <- dt[GEO %in% single$overniv]
    data.table::set(add_single, j = "GEOniv", value = "V")
    add_single[single, on = setNames("overniv", "GEO"), GEO := lks]
    } else {
      add_single <- dt[0]
    }
  
  # Legg til ugyldige soner, som skal eksistere men være prikket
  invalid <- data.table::data.table(lks = parameters$GeoKoder[GEOniv == "V" & TYP == "U" & !GEO %in% unique(add_single[["GEO"]]), unique(GEO)])
  if(nrow(invalid) > 0){
    invalid[, overniv := sub("00$", "", substr(lks, 1, 6))]
    add_invalid <- dt[GEO %in% invalid$overniv]
    data.table::set(add_invalid, j = "GEOniv", value = "V")
    add_invalid[invalid, on = setNames("overniv", "GEO"), GEO := lks]
    add_invalid[, let(spv_tmp = 2, geoprikket = 1)]
    vals <- union(get_value_columns(names(dt)), c("sumTELLER", "sumNEVNER", "MEIS", "RATE", "SMR"))
    data.table::set(add_invalid, j = vals, value = NA)
    } else {
      add_invalid <- dt[0]
    }
  
  dt <- data.table::rbindlist(list(dt, add_single, add_invalid))
  return(dt)
}
