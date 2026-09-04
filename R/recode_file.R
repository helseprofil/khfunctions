#' @title filter_and_recode_table_duckdb
#' @description filters and recode dimensions according to redesign
#' @keywords internal
#' @family duckdb
#' @noRd
filter_and_recode_table_duckdb <- function(con, tablename, redesign, parameters){
  drop_tmp_tables(con, "tmp_filter_")
  drop_tmp_tables(con, "tmp_recode_")
  on.exit({
    drop_tmp_tables(con, "tmp_filter_")
    drop_tmp_tables(con, "tmp_recode_")
  }, add = TRUE)
  
  
  do_filter_dimensions_duckdb(con = con, tablename = tablename, 
                              filters = redesign$Filters)
  do_recode_dimensions_duckdb(con = con, tablename = tablename, 
                              recode = redesign$KBs[intersect(redesign$order, names(redesign$KBs))],
                              parameters = parameters)
  add_udekk_duckdb(con = con, tablename = tablename, udekk = redesign$Udekk)
  invisible(NULL)
}

#' @title drop_tmp_tables
#' @description helper function to remove tmp_filter and tmp_recode tables
#' @keywords internal
#' @family duckdb
#' @noRd
drop_tmp_tables <- function(con, prefix){
  tabs <- DBI::dbListTables(con)
  drop <- grep(sprintf("^%s", prefix), tabs, value = TRUE)
  if(length(drop)){
    sql <- sprintf("DROP TABLE IF EXISTS %s", drop)
    for(s in sql){ 
      invisible(DBI::dbExecute(con, s)) 
    }
  }
  invisible(NULL)
}

#' @title do_filter_dimensions_duckdb
#' @description Filters table based on filters, generated with find_redesign
#' @family duckdb
#' @noRd
do_filter_dimensions_duckdb <- function(con, tablename, filters){
  if(length(filters) == 0) return(invisible(NULL))
  
  filter_tables <- character()
  for(part in names(filters)){
    filter_cols <- names(filters[[part]])
    filter_unique <- unique(filters[[part]])
    sql <- sprintf("SELECT DISTINCT %s FROM %s",
                   paste(filter_cols, collapse = ", "),
                   tablename
    )
    table_values <- data.table::setDT(DBI::dbGetQuery(con, sql))
    fullmatch <- collapse::join(table_values, filter_unique, how = "anti", verbose = 0)[, .N] == 0
    if(fullmatch) next
    
    filter_table <- sprintf("tmp_filter_%s", part)
    DBI::dbWriteTable(conn = con, name = filter_table, value = filter_unique, 
                      temporary = TRUE,overwrite = TRUE)
    filter_tables <- c(filter_tables, filter_table)
  }
  
  if(length(filter_tables) > 0){
    filterparts <-  sub(".*_(.*)", "\\1", filter_tables)
    filter_all_sql <- sprintf("CREATE TEMP TABLE tmp_filter_all AS SELECT * FROM %s",
                              paste(filter_tables, collapse = "\nCROSS JOIN "))
    
    invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_filter_all"))
    invisible(DBI::dbExecute(con, filter_all_sql))
    filter_cols <- DBI::dbListFields(con, "tmp_filter_all")
    join_condition <- paste(sprintf("t.%s = f.%s",filter_cols,filter_cols), collapse = "\n  AND ")
    
    filter_sql <- sprintf(
      "CREATE OR REPLACE TABLE %s AS
    SELECT t.* FROM %s t
    SEMI JOIN tmp_filter_all f
    ON %s",
      tablename, tablename, join_condition)
    
    n_before <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS N FROM %s", tablename))$N
    invisible(DBI::dbExecute(con, filter_sql))
    n_after <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS N FROM %s", tablename))$N
    filterpartsname <- as.character(parameters$DefDesign$DelKolN[filterparts])
    print_console_message(sprintf("Filtrering på %s: %s -> %s rader", paste(filterpartsname, collapse = ", "), n_before, n_after))
  }
}

#' @title do_recode_and_aggregate_dimensions_duckdb
#' @description Recode dimensions based on codebook generated from find_redesign
#' @family duckdb
#' @noRd
do_recode_dimensions_duckdb <- function(con, tablename, recode, parameters){
  
  recodeparts <- names(recode)
  if(length(recodeparts) == 0) return(invisible(NULL)) 
  
  for(part in recodeparts){
    partinfo <- get_part_info(part = part,parameters = parameters)
    recodebook <- recode[[part]]
    
    if(is.null(recodebook) || nrow(recodebook) == 0){
      next
    }
    
    recode_table <- sprintf("tmp_recode_%s",part)
    DBI::dbWriteTable(conn = con, name = recode_table, value = recodebook,
                      temporary = TRUE, overwrite = TRUE)
    
    table_cols <- DBI::dbListFields(con, tablename)
    
    join_condition <- paste(sprintf("t.%s = r.%s", partinfo$cols, partinfo$cols), collapse = "\n  AND ")
    
    select_cols <- character()
    
    for(col in table_cols){
      idx <- match(col, partinfo$cols)
      if(is.na(idx)){
        select_cols <- c(select_cols, sprintf("t.%s", DBI::dbQuoteIdentifier(con, col)))
      } else {
        select_cols <- c(select_cols,
                         sprintf("r.%s AS %s", 
                                 DBI::dbQuoteIdentifier(con, partinfo$colsomk[idx]), 
                                 DBI::dbQuoteIdentifier(con, partinfo$cols[idx])))
      }
    }
    
    recode_sql <- sprintf(
    "CREATE OR REPLACE TABLE %s AS 
    SELECT %s FROM %s t INNER JOIN %s r ON %s",
      tablename,
      paste(select_cols, collapse = ",\n"),
      tablename,
      recode_table,
      join_condition
    )
    
    n_before <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS N FROM %s", tablename))$N
    invisible(DBI::dbExecute(con, recode_sql))
    if(part == "Gn") fix_recode_geo_duckdb(con = con, tablename = tablename, parameters = parameters)
    do_aggregate_file_duckdb(con = con, tablename = tablename)
    n_after <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS N FROM %s", tablename))$N
    partname <- as.character(parameters$DefDesign$DelKolN[part])
    print_console_message(sprintf("\nOmkoding av %s: %s -> %s rader", partname, n_before, n_after))
  }
  
  invisible(NULL)
}

#' @title fix_recode_geo_duckdb
#' @description If GEOniv is recoded, this function fix columns GEO and FYLKE
#' @family duckdb
#' @noRd
fix_recode_geo_duckdb <- function(con, tablename, parameters){
  on.exit({
    invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_helsereg"))
    invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_geokoder_b"))
    }, add = TRUE)
  
  DBI::dbWriteTable(con, "tmp_helsereg", parameters$HELSEREG, temporary = TRUE, overwrite = TRUE)
  DBI::dbWriteTable(con,"tmp_geokoder_b", unique(parameters$GeoKoder[GEOniv == "B", .(GEO)]),temporary = TRUE,overwrite = TRUE)
  
  has_fylke <- "FYLKE" %in% DBI::dbListFields(con, tablename)
  fylke_sql <- if(has_fylke){
    "CASE
      WHEN t.GEOniv_omk = 'L' THEN '00'
      WHEN t.GEOniv_omk = 'B' AND b.GEO IS NULL THEN '99'
      ELSE t.FYLKE
    END AS FYLKE,"
  } else {
    ""
  }
  
  exclude_sql <- if(has_fylke){
    "t.* EXCLUDE (GEO, FYLKE)"
  } else {
    "t.* EXCLUDE (GEO)"
  }
  
  sql <- sprintf(
    "CREATE OR REPLACE TABLE %s AS SELECT
    CASE
      WHEN t.GEOniv_omk = 'L' THEN '0'
      WHEN t.GEOniv_omk = 'F' THEN SUBSTR(t.GEO, 1, 2)
      WHEN t.GEOniv_omk = 'K' THEN SUBSTR(t.GEO, 1, 4)
      WHEN t.GEOniv_omk = 'B' AND b.GEO IS NULL THEN '999999'
      WHEN t.GEOniv_omk = 'H' AND h.HELSEREG IS NOT NULL THEN h.HELSEREG
      ELSE t.GEO 
    END AS GEO,
    %s
    %s FROM %s t
    LEFT JOIN tmp_helsereg h ON t.GEO = h.FYLKE
    LEFT JOIN tmp_geokoder_b b ON t.GEO = b.GEO",
    tablename,
    fylke_sql,
    exclude_sql,
    tablename
  )
  
  invisible(DBI::dbExecute(con, sql))
}

#' @title add_udekk_duckdb
#' @description Add uncovered combinations to table
#' @family duckdb
#' @noRd
add_udekk_duckdb <- function(con, tablename, udekk){
  on.exit(invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_udekk")), add = TRUE)
  if(is.null(udekk) || nrow(udekk) == 0) return(invisible(NULL))
  
  DBI::dbWriteTable(conn = con, name = "tmp_udekk", value = udekk,
                    temporary = TRUE, overwrite = TRUE)
  table_cols <- DBI::dbListFields(con, tablename)
  dims <- get_dimension_columns(table_cols)
  vals <- get_value_columns(table_cols)
  
  udekk_cols <- names(udekk)
  join_cols <- intersect(dims, udekk_cols)
  extracols <- setdiff(dims, udekk_cols)
  
  anti_join_sql <- paste(sprintf("t.%s = u.%s", join_cols, join_cols),
                         collapse = "\n  AND ")
  
  if(length(extracols) > 0){
    extra_sql <- sprintf("(SELECT DISTINCT %s FROM %s)", paste(extracols, collapse = ", "), tablename)
    newrow_from_sql <- sprintf("tmp_udekk u CROSS JOIN %s e", extra_sql)
  } else {
    newrow_from_sql <- "tmp_udekk u"
  }
  
  value_sql <- c(sprintf("CAST(NULL AS DOUBLE) AS %s", vals),
                 sprintf("9 AS %s", DBI::dbQuoteIdentifier(con, paste0(vals, ".f"))),
                 sprintf("0 AS %s", DBI::dbQuoteIdentifier(con, paste0(vals, ".a"))))
  
  newrow_select <- c(sprintf("u.%s", udekk_cols),
                     if(length(extracols) > 0) sprintf("e.%s", extracols),
                     value_sql)
  
  keep_cols <- paste(sprintf("t.%s", 
                             DBI::dbQuoteIdentifier(con, table_cols)), 
                     collapse = ",\n ")
  
  sql <- sprintf(
    "CREATE OR REPLACE TABLE %s AS 
  SELECT %s FROM %s t
  ANTI JOIN tmp_udekk u ON %s
  UNION ALL BY NAME
  SELECT %s FROM %s",
    tablename, keep_cols, tablename, anti_join_sql, 
    paste(newrow_select, collapse = ",\n "), newrow_from_sql)
  
  invisible(DBI::dbExecute(con, sql))
  
  invisible(NULL)
}

# OLD ----

# KEEP until completely deprecated

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
  if(filtrow != orgrow) print_console_message("\n** Filtrerer på", names(filtered), "\n** rader før:", orgrow, ", og etter: ", filtrow)
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
    dt <- collapse::join(dt, recodebook, how = "inner", multiple = TRUE, overid = 2, verbose = 0)
    if(part == "Gn") dt <- fix_recode_geo(dt = dt, parameters = parameters)
    data.table::set(dt, j = partinfo$cols, value = dt[, .SD, .SDcols = partinfo$colsomk])
    data.table::set(dt, j = partinfo$colsomk, value = NULL)
    dt <- do_aggregate_file(file = dt)
    print_console_message(paste0("\n** Omkoder og aggregerer ", partinfo$name, ", rader nå: ", nrow(dt)))
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


  
