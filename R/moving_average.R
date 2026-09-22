# Get info ----

#' @title get_movav_information
#' @description
#' Extracts information needed for aggregation to moving averages
#' @noRd
get_movav_information <- function(parameters) {
  con <- parameters$duck
  mapar <- list()
  aar <- DBI::dbGetQuery(con, "SELECT DISTINCT AARl, AARh FROM KUBE")
  mapar[["int_lengde"]] <- unique(aar$AARh - aar$AARl + 1)
  if (length(mapar$int_lengde) > 1) stop("Inndata har ulike årsintervaller, kan ikke aggregeres!")
  mapar[["is_movav"]] <- parameters$CUBEinformation$MOVAV > 1
  mapar[["movav"]] <- parameters$CUBEinformation$MOVAV
  mapar[["snitt"]] <- parameters$fileinformation[[parameters$files$TELLER]]$ValErAarsSnitt
  mapar[["is_orig_snitt"]] <- !is.na(mapar$snitt) && mapar$snitt != 0
  snitt_orgintmult <- ifelse(mapar$is_orig_snitt, mapar$int_lengde, 1)
  mapar[["orgintMult"]] <- ifelse(mapar$is_movav, 1, snitt_orgintmult)
  mapar[["missyears"]] <- find_missing_year(unique(aar$AARl))
  
  parameters[["MOVAV"]] <- mapar
  return(invisible(parameters))
}

#' @title find_missing_year
#' @noRd
find_missing_year <- function(aarl){
  aarl_min_max <- min(aarl):max(aarl)
  aarl_missing <- aarl_min_max[!aarl_min_max %in% aarl]
  if(length(aarl_missing) > 0) print_console_message("\n*** Mangler data for:", paste0(aarl_missing, collapse = ", "), "\n")
  return(list(n = length(aarl_missing), years = aarl_missing))
}

# Aggregate ----

#' @title aggregate_to_periods
#' @description
#' Aggregerer tabellen til flerårige tall
#' Flerårige tall tolerer missing av type .f=1 ("random")
#' For anonymiserte tall tolereres missing så lenge andelen skjulte tall er under grensen definert i getOption("khfunctions.anon_tot_tol")
#' Rapporterer variabelspesifikk VAL.n som angir antall aar brukt i summen naar NA holdt utenom
#' Dersom is_movav = FALSE, legges val.n til for alle verdikolonner, satt til 1 dersom originale snitt
#' eller intervall-lengden dersom originale summer.
#' @family duckdb
#' @param con duckdb-connection
#' @param tablename Tabellnavn (KUBE, eller predrate om standardisering)
#' @param parameters Globale parametre
#' @param standard TRUE hvis standardfiler, hindrer uønsket fildump
#' @noRd
aggregate_to_periods <- function(tablename, parameters, standard = FALSE){
  if(!standard){
    save_filedump_if_requested(dumpname = "MOVAVpre", dt = NULL, parameters = parameters, duck = TRUE, tablename = tablename)
    on.exit({save_filedump_if_requested(dumpname = "MOVAVpost", dt = NULL, parameters = parameters, , duck = TRUE, tablename = tablename)}, add = TRUE)
  }
  con <- parameters$duck
  do_balance_missing_teller_nevner(con = con, tablename = tablename)
  
  if(parameters$MOVAV$is_movav){
    print_console_message("\n* Aggregering til flerårige tall")
    period <- parameters$MOVAV$movav
    print_console_message("- Aggregerer ", tablename, " til ", period, "-årige tall", sep = "")
    do_aggregate_periods(con = con, tablename = tablename, parameters = parameters)
    do_filter_periods_with_missing_original(con = con, tablename = tablename)
  } else {
    do_handle_indata_periods(con = con, tablename = tablename,parameters = parameters)
  }
  
  do_clean_duckdb(con = con)
  invisible(NULL)
}

#' @title do_balance_missing_teller_nevner
#' @description
#' Balanserer missing i teller og nevner slik at sumteller og sumnevner er balansert. 
#' @noRd
do_balance_missing_teller_nevner <- function(con, tablename){
  cols <- get_duckdb_cols(con, tablename)
  if (!all(c("TELLER", "NEVNER") %in% cols)) return(invisible(NULL))
  
  print_console_message("- Balanserer missing teller og nevner slik at sumNEVNER og sumTELLER er basert på likt antall år")
  table_sql <- sqlquote(con, tablename)
  maxf <- 'GREATEST("TELLER.f", "NEVNER.f")'
  
  sql <- sprintf(
    'UPDATE %s 
    SET
      "TELLER.f" = %s,
      "NEVNER.f" = %s,
      TELLER = NULL,
      NEVNER = NULL
    WHERE %s <> 0',
    table_sql, maxf, maxf, maxf
  )
  invisible(DBI::dbExecute(con, sql))
  invisible(NULL)
}

#' @title do_aggregate_periods
#' @description
#' Aggregate from original data to moving averages
#' 
#' @param dt data
#' @param parameters cube parameters
do_aggregate_periods <- function(con, tablename, parameters){
  tmp_periods <- "tmp_movav_periods"
  on.exit(drop_tables_duckdb(con = con, tables = tmp_periods), add = TRUE)
          
  period <- parameters$MOVAV$movav
  n_multi <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s WHERE AARl <> AARh", sqlquote(con, tablename)))$n
  if(n_multi > 0) stop(sprintf("Aggregering til %s-årige tall er ønsket, men originaldata inneholder allerede flerårige tall.", period))
  aarh <- DBI::dbGetQuery(con, sprintf("SELECT DISTINCT AARh FROM %s ORDER BY AARh", sqlquote(con, tablename)))$AARh
  allperiods <- find_periods(aarh = aarh, period = period)
  
  # DBI::dbWriteTable(con, name = tmp_periods, value = allperiods, overwrite = TRUE)
  write_duckdb_table(con, tablename = tmp_periods, data = allperiods)
  cols <- get_duckdb_cols(con, tablename)
  values <- get_value_columns(cols)
  dims <- get_dimension_columns(cols)
  dims_no_year <- sqlquote(con, setdiff(dims,c("AARl", "AARh")))

  # Bygge sql som velger og aggregerer kolonner, og setter år til periods$AARl/AARh
  select_parts <- c(
    "p.AARl AS AARl",
    "p.AARh AS AARh",
    sprintf("d.%s", dims_no_year)
  )
  
  for(val in values){
    val_sql <- sqlquote(con, val)
    val_f <- sqlquote(con, paste0(val, ".f"))
    val_a <- sqlquote(con, paste0(val, ".a"))
    select_parts <- c(
      select_parts,
      sprintf('SUM(d.%s) AS %s', val_sql, val_sql),
      sprintf('0 AS %s', val_f),
      sprintf('SUM(d.%s) AS %s', val_a, val_a),
      sprintf('SUM(CASE WHEN d.%s IN (1,2) THEN 1 ELSE 0 END) AS %s',
              val_f, sqlquote(con, paste0(val, ".fn1"))),
      sprintf('SUM(CASE WHEN d.%s = 3 THEN 1 ELSE 0 END) AS %s',
              val_f, sqlquote(con, paste0(val, ".fn3"))),
      sprintf('SUM(CASE WHEN d.%s = 9 THEN 1 ELSE 0 END) AS %s',
              val_f, sqlquote(con, paste0(val, ".fn9"))),
      sprintf('SUM(CASE WHEN d.%s = 0 THEN 1 ELSE 0 END) AS %s',
              val_f, sqlquote(con, paste0(val, ".n")))
    )
  }
  
  group_by <- c("p.AARl", "p.AARh", sprintf("d.%s", dims_no_year))

  tmp_result <- prepare_tmp_result_table(con, tablename)
  
  sql <- sprintf(
    "CREATE TABLE %s AS
    SELECT %s FROM %s d
    INNER JOIN %s p ON d.AARl >= p.AARl AND d.AARh <= p.AARh
    GROUP BY 
    %s",
    sqlquote(con, tmp_result),
    paste(select_parts, collapse = ",\n"),
    sqlquote(con, tablename),
    tmp_periods,
    paste(group_by, collapse = ",\n")
  )
  
  invisible(DBI::dbExecute(con, sql))
  replace_table_duckdb(con, target = tablename, source = tmp_result)
  
  # Denne er sketchy, for om hele år mangler så gir ikke dette .fn9 = 1. 
  # I en 5-årsperiode med 2 manglende år, må altså de tre andre årene ha f = 9 for at val.fn9 > antall manglende år
  # Denne bør raffineres i fremtiden. Vi bør også vurdere hvorvidt vi skal ha andre
  # kriterier for å ikke lage en sum, kanskje basert på .n-kolonnene (må ha minst x % av årene for å lage sum)
  # For nå er denne beholdt som den var opprinnelig.
  missing_year <- parameters$MOVAV$missyears
  if (missing_year$n <= period) {
    for (val in values) {
      val_sql <- sqlquote(con, val)
      val_f <- sqlquote(con, paste0(val, ".f"))
      val_fn9 <- sqlquote(con, paste0(val, ".fn9"))
      sql <- sprintf(
        "UPDATE %s
      SET
        %s = NULL,
        %s = 9
      WHERE %s > %s",
        sqlquote(con, tablename),
        val_sql,
        val_f,
        val_fn9,
        missing_year$n)
      invisible(DBI::dbExecute(con, sql))
    }
  }
  
  invisible(NULL)
} 
 
find_periods <- function(aarh, period){
  start_max <- min(aarh) + period - 1
  end_max <- max(aarh)
  if(start_max > end_max) stop("Aggregering til ", period, "-årige tall feilet pga utilstrekkelig antall årganger")
  max_aar <- as.integer(start_max:end_max)
  min_aar <- as.integer(max_aar - period + 1)
  return(data.table::data.table(AARl = min_aar, AARh = max_aar))
}

#' @title filter_periods_with_missing_original
#' @description
#' Filters out averages with too much missing from original data. The 
#' tolerance is set by the option anon_tot_tol in the config file, and
#' for rows where the proportion of original val.f == 3 (as indicated 
#' by the helper column val.fn3) > the tolerance, the average is set to 
#' NA and val.f = 3
#' @keywords internal
#' @noRd
do_filter_periods_with_missing_original <- function(con, tablename){
  
  cols <- get_duckdb_cols(con, tablename)
  values <- get_value_columns(cols)
  anonymous_tolerance <- getOption("khfunctions.anon_tot_tol")
  
  tbl_sql <- sqlquote(con, tablename)
  
  for(val in values){
    val_sql <- sqlquote(con, val)
    val_f   <- sqlquote(con, paste0(val, ".f"))
    val_n   <- sqlquote(con, paste0(val, ".n"))
    val_fn3 <- sqlquote(con, paste0(val, ".fn3"))
    
    if (!paste0(val, ".n") %in% cols) next
    
    sql <- sprintf(
      "UPDATE %s
      SET %s = NULL, %s = 3 
      WHERE %s > 0 AND CAST(%s AS DOUBLE) / %s >= %s",
      tbl_sql, val_sql, val_f,
      val_n, val_fn3, val_n,
      anonymous_tolerance
    )
    
    invisible(DBI::dbExecute(con, sql))
  }
  
  invisible(NULL)
}


#' @title handle_indata_periods
#' @description
#' If indata contains periods, val.n must be adjusted accordingly
#' @keywords internal
#' @noRd
do_handle_indata_periods <- function(con,tablename,parameters){
  n <- as.integer(ifelse(parameters$MOVAV$is_orig_snitt, 1L, parameters$MOVAV$int_lengde))
  cols <- get_duckdb_cols(con, tablename)
  values <- get_value_columns(cols)
  tbl_sql <- sqlquote(con, tablename)
  
  val_n_cols <- vector("character", length(values))
    
  for(i in seq_along(values)){
    val_n <- sqlquote(con, paste0(values[i], ".n"))
    val_n_cols[i] <- sprintf("%s = %s", val_n, n)
    if(!paste0(values[i], ".n") %in% cols) {
        DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s INTEGER",
            tbl_sql,val_n))
    }}
  
  update_sql <- sprintf("UPDATE %s SET %s", tbl_sql, paste(val_n_cols, collapse = ", "))
  
  invisible(DBI::dbExecute(con, update_sql))
  invisible(NULL)
}
