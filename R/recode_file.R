#' @title filter_and_recode_table_duckdb
#' @description filters and recode dimensions according to redesign
#' @keywords internal
#' @family duckdb
#' @noRd
filter_and_recode_table_duckdb <- function(con, tablename, redesign, parameters){
  drop_tables_duckdb_prefix(con, prefix = "tmp_filter_")
  drop_tables_duckdb_prefix(con, prefix = "tmp_recode_")
  on.exit({
    drop_tables_duckdb_prefix(con, prefix = "tmp_filter_")
    drop_tables_duckdb_prefix(con, prefix = "tmp_recode_")
  }, add = TRUE)
  
  print_console_message("\n- Filtrerer og omkoder", tablename)
  do_filter_dimensions_duckdb(con = con, tablename = tablename, 
                              filters = redesign$Filters)
  do_recode_dimensions_duckdb(con = con, tablename = tablename, 
                              recode = redesign$KBs[intersect(redesign$order, names(redesign$KBs))],
                              parameters = parameters)
  add_udekk_duckdb(con = con, tablename = tablename, udekk = redesign$Udekk)
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
                   sqlquote(con, tablename))
    table_values <- data.table::setDT(DBI::dbGetQuery(con, sql))
    fullmatch <- collapse::join(table_values, filter_unique, how = "anti", verbose = 0)[, .N] == 0
    if(fullmatch) next
    
    filter_table <- sprintf("tmp_filter_%s", part)
    write_duckdb_table(con, tablename = filter_table, data = filter_unique)
    filter_tables <- c(filter_tables, filter_table)
  }
  
  if(length(filter_tables) > 0){
    filterparts <-  sub(".*_(.*)", "\\1", filter_tables)
    filter_all_sql <- sprintf("CREATE TEMP TABLE tmp_filter_all AS SELECT * FROM %s",
                              paste(filter_tables, collapse = "\nCROSS JOIN "))
    
    drop_tables_duckdb(con, "tmp_filter_all")
    invisible(DBI::dbExecute(con, filter_all_sql))
    filter_cols <- get_duckdb_cols(con, "tmp_filter_all")
    join_condition <- paste(sprintf("t.%s = f.%s",filter_cols,filter_cols), collapse = "\n  AND ")
    
    tmp_result <- prepare_tmp_result_table(con, tablename)
    
    filter_sql <- sprintf(
      "CREATE TABLE %s AS SELECT t.* FROM %s t
      SEMI JOIN tmp_filter_all f ON %s",
      sqlquote(con, tmp_result),
      sqlquote(con, tablename), 
      join_condition)
    
    n_before <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS N FROM %s",  sqlquote(con, tablename)))$N
    invisible(DBI::dbExecute(con, filter_sql))
    replace_table_duckdb(con, target = tablename, source = tmp_result)
    n_after <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS N FROM %s",  sqlquote(con, tablename)))$N
    filterpartsname <- as.character(parameters$DefDesign$DelKolN[filterparts])
    print_console_message(sprintf("-- Filtrering på %s: %s -> %s rader", paste(filterpartsname, collapse = ", "), n_before, n_after))
  }
}

#' @title do_recode_and_aggregate_dimensions_duckdb
#' @description Recode dimensions based on codebook generated from find_redesign
#' @family duckdb
#' @noRd
do_recode_dimensions_duckdb <- function(con, tablename, recode, parameters){
  
  recodeparts <- names(recode)
  if(length(recodeparts) == 0) return(invisible(NULL)) 
  
  tbl_sql <- sqlquote(con, tablename)
  
  for(part in recodeparts){
    partinfo <- get_part_info(part = part,parameters = parameters)
    recodebook <- recode[[part]]
    
    if(is.null(recodebook) || nrow(recodebook) == 0){
      next
    }
    
    recode_table <- sprintf("tmp_recode_%s", part)
    write_duckdb_table(con, tablename = recode_table, data = recodebook)
    table_cols <- get_duckdb_cols(con, tablename)
    join_condition <- paste(sprintf("t.%s = r.%s", partinfo$cols, partinfo$cols), collapse = "\n  AND ")
    select_cols <- character()
    
    for(col in table_cols){
      idx <- match(col, partinfo$cols)
      if(is.na(idx)){
        select_cols <- c(select_cols, sprintf("t.%s", sqlquote(con, col)))
      } else {
        select_cols <- c(select_cols,
                         sprintf("r.%s AS %s", 
                                 sqlquote(con, partinfo$colsomk[idx]), 
                                 sqlquote(con, partinfo$cols[idx])))
      }
    }
    
    tmp_recode <- prepare_tmp_result_table(con, tablename)
    
    recode_sql <- sprintf(
    "CREATE TABLE %s AS SELECT %s FROM %s t INNER JOIN %s r ON %s",
      sqlquote(con, tmp_recode),
      paste(select_cols, collapse = ",\n"),
      tbl_sql,
      recode_table,
      join_condition
    )
    
    n_before <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS N FROM %s", tbl_sql))$N
    invisible(DBI::dbExecute(con, recode_sql))
    replace_table_duckdb(con, target = tablename, source = tmp_recode)
    if(part == "Gn") fix_recode_geo_duckdb(con = con, tablename = tablename, parameters = parameters)
    do_aggregate_file_duckdb(con = con, tablename = tablename)
    n_after <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS N FROM %s", tbl_sql))$N
    partname <- as.character(parameters$DefDesign$DelKolN[part])
    print_console_message(sprintf("-- Omkoding av %s: %s -> %s rader", partname, n_before, n_after))
  }
  
  invisible(NULL)
}

#' @title fix_recode_geo_duckdb
#' @description If GEOniv is recoded, this function fix columns GEO and FYLKE
#' @family duckdb
#' @noRd
fix_recode_geo_duckdb <- function(con, tablename, parameters){
  on.exit({
    drop_tables_duckdb(con, c("tmp_helsereg", "tmp_geokoder_b"))
    }, add = TRUE)
  
  write_duckdb_table(con, "tmp_helsereg", data = parameters$HELSEREG)
  write_duckdb_table(con, "tmp_geokoder_b", data = unique(parameters$GeoKoder[GEOniv == "B", .(GEO)]))
  
  has_fylke <- "FYLKE" %in% get_duckdb_cols(con, tablename)
  fylke_sql <- if(has_fylke){
    "CASE
      WHEN t.GEOniv = 'L' THEN '00'
      WHEN t.GEOniv = 'B' AND b.GEO IS NULL THEN '99'
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
  
  tmp_geofix <- prepare_tmp_result_table(con, tablename)
  
  sql <- sprintf(
    "CREATE TABLE %s AS SELECT
    CASE
      WHEN t.GEOniv = 'L' THEN '0'
      WHEN t.GEOniv = 'F' THEN SUBSTR(t.GEO, 1, 2)
      WHEN t.GEOniv = 'K' THEN SUBSTR(t.GEO, 1, 4)
      WHEN t.GEOniv = 'B' AND b.GEO IS NULL THEN '999999'
      WHEN t.GEOniv = 'H' AND h.HELSEREG IS NOT NULL THEN h.HELSEREG
      ELSE t.GEO 
    END AS GEO,
    %s
    %s FROM %s t
    LEFT JOIN tmp_helsereg h ON t.GEO = h.FYLKE
    LEFT JOIN tmp_geokoder_b b ON t.GEO = b.GEO",
    sqlquote(con, tmp_geofix),
    fylke_sql,
    exclude_sql,
    sqlquote(con, tablename)
  )
  
  invisible(DBI::dbExecute(con, sql))
  replace_table_duckdb(con, target = tablename, source = tmp_geofix)
  invisible(NULL)
}

#' @title add_udekk_duckdb
#' @description Add uncovered combinations to table
#' @family duckdb
#' @noRd
add_udekk_duckdb <- function(con, tablename, udekk){
  if(is.null(udekk) || nrow(udekk) == 0) return(invisible(NULL))
  write_duckdb_table(con, "tmp_udekk", data = udekk)
  on.exit(drop_tables_duckdb(con, "tmp_udekk"), add = TRUE)

  table_cols <- get_duckdb_cols(con, tablename)
  dims <- get_dimension_columns(table_cols)
  vals <- get_value_columns(table_cols)
  
  udekk_cols <- names(udekk)
  join_cols <- intersect(dims, udekk_cols)
  extracols <- setdiff(dims, udekk_cols)
  
  anti_join_sql <- paste(sprintf("t.%s = u.%s", join_cols, join_cols),
                         collapse = "\n  AND ")
  
  if(length(extracols) > 0){
    extra_sql <- sprintf("(SELECT DISTINCT %s FROM %s)", paste(extracols, collapse = ", "), sqlquote(con, tablename))
    newrow_from_sql <- sprintf("tmp_udekk u CROSS JOIN %s e", extra_sql)
  } else {
    newrow_from_sql <- "tmp_udekk u"
  }
  
  value_sql <- c(sprintf("CAST(NULL AS DOUBLE) AS %s", vals),
                 sprintf("9 AS %s", sqlquote(con, paste0(vals, ".f"))),
                 sprintf("0 AS %s", sqlquote(con, paste0(vals, ".a"))))
  
  newrow_select <- c(sprintf("u.%s", udekk_cols),
                     if(length(extracols) > 0) sprintf("e.%s", extracols),
                     value_sql)
  
  keep_cols <- paste(sprintf("t.%s", 
                             sqlquote(con, table_cols)), 
                     collapse = ",\n ")
  
  tmp_result <- prepare_tmp_result_table(con, tablename)
  
  sql <- sprintf(
    "CREATE TABLE %s AS 
    SELECT %s FROM %s t 
    ANTI JOIN tmp_udekk u ON %s
    UNION ALL BY NAME 
    SELECT %s FROM %s",
    sqlquote(con, tmp_result), 
    keep_cols, sqlquote(con, tablename), anti_join_sql, 
    paste(newrow_select, collapse = ",\n "), newrow_from_sql)
  
  invisible(DBI::dbExecute(con, sql))
  replace_table_duckdb(con, target = tablename, source = tmp_result)
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


  
