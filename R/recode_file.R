#' @title do_recode_file
#' @description filters and recode dimensions according to redesign
#' @keywords internal
#' @noRd
do_filter_and_recode_to_redesign <- function(dt, redesign, parameters){
  cols <- list(orgcols = names(dt), dims = get_dimension_columns(names(dt)), values = get_value_columns(names(dt)))
  
  if(nrow(redesign$FULL) > 0){
    dt <- do_filter_dimensions(dt = dt, filters = redesign$Filters)
    dt <- do_recode_and_aggregate_dimensions(dt = dt, recode = redesign$KBs, cols = cols, parameters = parameters)
  }
  
  if (nrow(redesign$Udekk) > 0) {
    dt <- add_udekk(dt = dt, udekk = data.table::copy(redesign$Udekk), cols = cols)
  }
  
  data.table::setkeyv(dt, cols$dims)
  return(dt)
}

#' @keywords internal|
#' @noRd
do_filter_dimensions <- function(dt, filters){
  filterparts <- names(filters)
  if(length(filterparts) == 0) return(dt)
  filtered <- do.call(expand.grid.dt, filters)
  orgrow <- dt[, .N]
  dt <- collapse::join(dt, filtered, how = "inner", overid = 2, verbose = 0)
  filtrow <- dt[, .N]
  if(filtrow != orgrow) cat("\n** Filtrerer på", names(filtered), "\n** rader før:", orgrow, ", og etter: ", filtrow)
  return(dt)
}

#' @keywords internal|
#' @noRd
do_recode_and_aggregate_dimensions <- function(dt, recode, cols, parameters){
  recodeparts <- get_partcols_and_set_aggpri(names(recode), parameters = parameters)
  if(length(recodeparts) == 0) return(dt)
  
  for(part in recodeparts){
    partinfo <- get_part_info(part = part, parameters = parameters)
    recodebook <- recode[[part]]
    # replicate_4 <- recodebook[, .(N=.N), by = c(partinfo$cols)][, mean(N)] < 4
    dt <- collapse::join(dt, recodebook, how = "inner", multiple = TRUE, overid = 2, verbose = 0)
    if(part == "Gn") dt <- fix_recode_geo(dt = dt, parameters = parameters)
    dt[, (partinfo$cols) := mget(partinfo$colsomk)]
    dt[, names(.SD) := NULL, .SDcols = partinfo$colsomk]
    dt <- do_aggregate_file(file = dt)
    cat(paste0("\n** Omkoder og aggregerer ", partinfo$name, ", rader nå: ", nrow(dt)))
  }
  return(dt)
}

#' @keywords internal
#' @noRd
fix_recode_geo <- function(dt, parameters){
  dt[GEOniv_omk == "L", let(GEO = "0", FYLKE = "00")]
  dt[GEOniv_omk == "F", let(GEO = substr(GEO, 0, 2))]
  dt[GEOniv_omk == "K", let(GEO = substr(GEO, 0, 4))]
  dt[GEOniv_omk == "B" & !GEO %in% parameters$GeoKoder[GEOniv == "B", GEO], let(GEO = "999999", FYLKE = "99")]
  if("H" %in% unique(dt$GEOniv)){
    dt <- collapse::join(dt, parameters$HELSEREG, on = setNames("FYLKE", "GEO"), how = "l", overid = 2, verbose = 0)
    dt[GEOniv_omk == "H" & !is.na(HELSEREG), let(GEO = HELSEREG)][, let(HELSEREG = NULL)]
  }
  return(dt)
}

#' @keywords internal
#' @noRd
add_udekk <- function(dt, udekk, cols){
  extracols <- setdiff(cols$dims, names(udekk))
  dt <- collapse::join(dt, udekk, how = "anti", on = intersect(cols$dims, names(udekk)), overid = 2, verbose = 0)
  udekk[, (cols$values) := NA_real_]
  udekk[, (paste0(cols$values, ".f")) := 9]
  udekk[, (paste0(cols$values, ".a")) := 0]
  if(length(extracols) > 0){
    extra <- unique(dt[, .SD, .SDcols = extracols])
    udekk <- expand.grid.dt(udekk, extra)
  }
  
  dt <- data.table::rbindlist(list(dt[, .SD, .SDcols = cols$orgcols],
                                   udekk[, .SD, .SDcols = cols$orgcols]))
  return(dt)
}
