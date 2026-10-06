#' @title do_harmonize_geo_duckdb
#' @description
#' Harmonizes geographical codes, used in `LagFilgruppe()`. 
#' Uses table KnrHarm in duckdb for recoding to current geographical codes.
#' @family duckdb
#' @noRd 
do_harmonize_geo_duckdb <- function(con, tablename, valdefs = list(), add_fylke = TRUE){
  khtools::msg("\n** Geo-harmonisering")
  invisible(DBI::dbExecute(con,sprintf("ALTER TABLE %s DROP COLUMN IF EXISTS FYLKE",
                                       khtools::sql_quote_I(con, tablename))))
  cols <- khtools::duckdb_get_columns(con, tablename)
  table_sql <- khtools::sql_quote_I(con, tablename)
  
  nharm <- DBI::dbGetQuery(con,sprintf(
      paste("SELECT COUNT(DISTINCT f.GEO) AS n",
            "FROM %s f",
            "INNER JOIN KnrHarm k",
            "ON f.GEO = k.GEO"),
      table_sql))$n
  
  if(nharm > 0){
    khtools::msg("- Rekoder", nharm, "geo-koder")
    cols_sql <- khtools::sql_quote_I(con, setdiff(cols, "GEO"))

    select_sql <- paste(c("COALESCE(k.GEO_omk, f.GEO) AS GEO",
                          paste0("f.",cols_sql)), collapse = ", ")
  
    sql <- sprintf(
      "SELECT %s FROM %s f 
      LEFT JOIN KnrHarm k ON f.GEO = k.GEO",
      select_sql, table_sql)
    khtools::duckdb_replace_existing_table(con, target = tablename, select_sql = sql)
  } else {
    khtools::msg(paste0("- Alle GEO-koder var gyldige, ingen omkoding nødvendig"))
  }
    
  do_aggregate_file_duckdb(con = con, tablename = tablename, valdefs = valdefs)
  if(add_fylke) add_fylke_duckdb(con = con, tablename = tablename)
  
  invisible(NULL)
}

#' @title add_fylke_duckdb
#' @description
#' Helper function to add FYLKE column as function of GEO and GEOniv
#' @family duckdb
#' @noRd 
add_fylke_duckdb <- function(con, tablename){
  table_sql <- khtools::sql_quote_I(con, tablename)
  
  DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN FYLKE VARCHAR",table_sql))
  
  DBI::dbExecute(con,sprintf(
    "UPDATE %s 
    SET FYLKE = CASE WHEN GEOniv IN ('H', 'L') THEN '00' ELSE SUBSTR(GEO, 1, 2)
    END",
    table_sql
    )
  )
  
  invisible(NULL)
}

#' @title fix_geo_special
#' @description Manually handle bydel startaar, lks startaar, DK2020 and Aalesund/Haram
#' @keywords internal
#' @noRd
fix_geo_special <- function(parameters){
  con <- parameters$duck
  specs <- parameters$fileinformation[[parameters$files$TELLER]]
  bydelstart <- specs[["B_STARTAAR"]]
  dk2020start <- specs[["DK2020_STARTAAR"]]
  geonivs <- DBI::dbGetQuery(con, "SELECT DISTINCT GEOniv FROM KUBE")[[1]]
  
  flags <- intersect(
    c("spv_tmp", grep("\\.f$", khtools::duckdb_get_columns(con, "KUBE"), value = TRUE)),
    khtools::duckdb_get_columns(con, "KUBE")
  )
  
  where <- character()
  
  khtools::msg("\n* Spesialprikking for GEO")
  if(!is.na(bydelstart) && bydelstart > 0 && any(geonivs %in% c("B", "V"))){
    khtools::msg("- Sletter tall for bydel og levekårssoner før ", bydelstart, " dersom de finnes", sep = "")
    where <- c(
      where, sprintf("(GEOniv IN ('B','V') AND AARl < %s)", bydelstart))
  }
  
  if("V" %in% geonivs){
    khtools::msg("- Håndterer individuelle startår for levekårssoner")
    where <- c(where, "(GEOniv = 'V' AND EXISTS (SELECT 1 FROM LKS_STARTAAR l WHERE l.GEO = KUBE.GEO AND KUBE.AARl < l.lks_startaar))")
  }
  
  if(!is.na(dk2020start) && dk2020start > 0 && "K" %in% geonivs){
    khtools::msg("- Håndterer delingskommuner (DK2020) og Aalesund/Haram i perioden 2020-2023")
    where <- c(where, sprintf("(GEOniv = 'K' AND GEO IN ('5055','5056','5059','1806','1875') AND AARl < %s)", dk2020start))
    
    ystart <- if(parameters$name == "VALGDELTAKELSE") 2019L else 2020L
    ystop <- ystart + 3L
    
    where <- c(where, sprintf("(GEO IN ('1508','1580') AND AARl <= %s AND AARh >= %s)", ystop, ystart))
  }
  
  if(length(where) == 0) return(invisible(NULL)) 
  
  where_sql <- paste(where, collapse = "\nOR\n")
  
  set_sql <- c(
    sprintf("%s = 9", khtools::sql_quote_I(con, flags)),
    sprintf("%s = 1", khtools::sql_quote_I(con, "geoprikket"))
  )
  
  sql <- sprintf("UPDATE KUBE SET %s WHERE %s", paste(set_sql, collapse = ", "), where_sql)
  invisible(DBI::dbExecute(con, sql))
}


#' @title do_handle_coverage
#' @keywords internal
#' @noRd
do_handle_coverage <- function(dt, geolevel = c("B", "V"), parameters){
  if("dekningprikket" %notin% names(dt)) dt[, dekningprikket := 0L]
  geolevel <- match.arg(geolevel)
  if(geolevel %notin% collapse::funique(dt[["GEOniv"]])) return(invisible(NULL))
  khtools::msg(paste0("\n** Skjuler tall med dårlig dekning for GEOniv == '", geolevel, "'"))
  khtools::msg("- Originalt", dt[GEOniv == geolevel, .N], "rader")
  # Sette inn kommentar om kriteriene?
  dims <- parameters$outdimensions
  flags <- c(grep("\\.f$", names(dt), value = T))
  skjul <- get_deletestrata(dt, dims, geolevel)
  if(skjul[, .N] > 0){
    dt[skjul, dekningprikket := 1L, on = dims]
    n_new <- dt[spv_tmp == 0 & dekningprikket == 1L, .N]
    dt[spv_tmp == 0 & dekningprikket == 1L, (c(flags, "spv_tmp")) := 1L]
    khtools::msg("-", n_new, "rader skjules")
  } else {
    khtools::msg("- Ingen rader skjules")
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
#' @description
#' Legger til rader for lks for kommuner med bare en lks (kopi av kommunetallet) 
#' og for ugyldige lks (tallene prikket).
#' Dette er pga kartvisning. 
#' @noRd
add_missing_lks <- function(parameters){
  if(!"V" %in% unlist(strsplit(parameters$CUBEinformation$GEOniv, ","))) return(invisible(NULL))
  con <- parameters$duck
  cols <- khtools::duckdb_get_columns(con, "KUBE")
  vals <- intersect(union(identify_values(cols), c("sumTELLER", "sumNEVNER", "MEIS", "RATE", "SMR")), cols)
  
  khtools::duckdb_drop_tables(con, c("tmp_single_lks", "tmp_invalid_lks"))
  on.exit(khtools::duckdb_drop_tables(con, c("tmp_single_lks", "tmp_invalid_lks")), add = TRUE)
  
  # Kommuner med bare én levekårssone
  sql_single <- "
    WITH single AS (
      SELECT lks, overniv,
      COUNT(*) OVER (PARTITION BY overniv) AS n FROM 
      (
        SELECT DISTINCT GEO AS lks,
        regexp_replace(substr(GEO,1,6), '00$', '') AS overniv
        FROM GEOKODER
        WHERE GEOniv = 'V'
      ) x
    )
    SELECT
      k.* EXCLUDE (GEO, GEOniv),
      s.lks AS GEO,
      'V' AS GEOniv
    FROM KUBE k
    INNER JOIN single s ON k.GEO = s.overniv WHERE s.n = 1"
  
  khtools::duckdb_create_new_table(con, target = "tmp_single_lks",
                                   select_sql = sql_single)
  
  # Ugyldige levekårssoner 
  invalid_cols <- c(
    sprintf("%s = NULL", khtools::sql_quote_I(con, vals)),
    sprintf("%s = 2", khtools::sql_quote_I(con, "spv_tmp")),
    sprintf("%s = 1", khtools::sql_quote_I(con, "geoprikket"))
  )
  
  sql_invalid <- "
  WITH invalid AS (
    SELECT DISTINCT
      GEO AS lks,
      regexp_replace(substr(GEO, 1, 6), '00$', '') AS overniv
    FROM GEOKODER
    WHERE GEOniv = 'V'
      AND TYP = 'U'
      AND GEO NOT IN (SELECT DISTINCT GEO FROM tmp_single_lks)
  )
  SELECT
    k.* EXCLUDE (GEO, GEOniv),
    i.lks AS GEO,
    'V' AS GEOniv
  FROM KUBE k INNER JOIN invalid i ON k.GEO = i.overniv"
  
  khtools::duckdb_create_new_table(con, target = "tmp_invalid_lks",
                                   select_sql = sql_invalid)
  
  if(nrow(DBI::dbGetQuery(con, "SELECT 1 FROM tmp_invalid_lks LIMIT 1")) > 0){
    sql <- sprintf(
      "UPDATE tmp_invalid_lks
       SET %s",
      paste(invalid_cols, collapse = ", ")
    )
    
    invisible(DBI::dbExecute(con, sql))
  }
  
  # Legg til radene
  khtools::duckdb_append_table(con, target = "KUBE", source = "tmp_single_lks")
  khtools::duckdb_append_table(con, target = "KUBE", source = "tmp_invalid_lks")
  invisible(NULL)
}

# old version ----

#  Keep this as it is used in rsynt ungdata

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
    file[, let(GEO_omk = NULL, HARMstd = NULL)]
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



