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

#' @title aggregate_to_moving_average_duckdb
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
aggregate_to_periods_duckdb <- function(tablename, parameters, standard = FALSE){
  if(!standard){
    save_filedump_if_requested(dumpname = "MOVAVpre", dt = NULL, parameters = parameters, duck = TRUE, tablename = tablename)
    on.exit({save_filedump_if_requested(dumpname = "MOVAVpost", dt = NULL, parameters = parameters, , duck = TRUE, tablename = tablename)}, add = TRUE)
  }
  con <- parameters$duck
  do_balance_missing_teller_nevner(con = con, tablename = tablename)
  
  if(parameters$MOVAV$is_movav){
    print_console_message("\n* Aggregering til flerårige tall")
    period <- parameters$MOVAV$movav
    print_console_message("- Aggregerer til ", period, "-årige tall", sep = "")
    do_aggregate_periods(con = con, tablename = tablename, parameters = parameters)
    do_filter_periods_with_missing_original(con = con, tablename = tablename)
  } else {
    do_handle_indata_periods(con = con, tablename = tablename,parameters = parameters)
  }
  
  invisible(NULL)
}

#' @title do_balance_missing_teller_nevner
#' @description
#' Balanserer missing i teller og nevner slik at sumteller og sumnevner er balansert. 
#' @noRd
do_balance_missing_teller_nevner <- function(con, tablename){
  cols <- DBI::dbListFields(con, tablename)
  if (!all(c("TELLER", "NEVNER") %in% cols)) return(invisible(NULL))
  
  print_console_message("- Balanserer missing teller og nevner slik at sumNEVNER og sumTELLER er basert på likt antall år")
  table_sql <- DBI::dbQuoteIdentifier(con, tablename)
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
  tbl_sql <- DBI::dbQuoteIdentifier(con, tablename)
  tmp_tbl <- DBI::dbQuoteIdentifier(con, paste0(tablename, "_MOVAV"))
  tmp_periods <- "tmp_movav_periods"
  
  on.exit(drop_tables_duckdb(con = con, tables = c(tmp_tbl, tmp_periods)), add = TRUE)
          
  period <- parameters$MOVAV$movav
  n_multi <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s WHERE AARl <> AARh", tbl_sql))$n
  if(n_multi > 0) stop(sprintf("Aggregering til %s-årige tall er ønsket, men originaldata inneholder allerede flerårige tall.", period))
  aarh <- DBI::dbGetQuery(con, sprintf("SELECT DISTINCT AARh FROM %s ORDER BY AARh", tbl_sql))$AARh
  allperiods <- find_periods(aarh = aarh, period = period)
  
  DBI::dbWriteTable(con, name = tmp_periods, value = allperiods, overwrite = TRUE)
  
  cols <- DBI::dbListFields(con, tablename)
  values <- get_value_columns(cols)
  dims <- get_dimension_columns(cols)
  dims_no_year <- DBI::dbQuoteIdentifier(con, setdiff(dims,c("AARl", "AARh")))

  # Bygge sql som velger og aggregerer kolonner, og setter år til periods$AARl/AARh
  select_parts <- c(
    "p.AARl AS AARl",
    "p.AARh AS AARh",
    sprintf("d.%s", dims_no_year)
  )
  
  for(val in values){
    val_sql <- DBI::dbQuoteIdentifier(con, val)
    val_f <- DBI::dbQuoteIdentifier(con, paste0(val, ".f"))
    val_a <- DBI::dbQuoteIdentifier(con, paste0(val, ".a"))
    select_parts <- c(
      select_parts,
      sprintf('SUM(d.%s) AS %s', val_sql, val_sql),
      sprintf('0 AS %s', val_f),
      sprintf('SUM(d.%s) AS %s', val_a, val_a),
      sprintf('SUM(CASE WHEN d.%s IN (1,2) THEN 1 ELSE 0 END) AS %s',
              val_f, DBI::dbQuoteIdentifier(con, paste0(val, ".fn1"))),
      sprintf('SUM(CASE WHEN d.%s = 3 THEN 1 ELSE 0 END) AS %s',
              val_f, DBI::dbQuoteIdentifier(con, paste0(val, ".fn3"))),
      sprintf('SUM(CASE WHEN d.%s = 9 THEN 1 ELSE 0 END) AS %s',
              val_f, DBI::dbQuoteIdentifier(con, paste0(val, ".fn9"))),
      sprintf('SUM(CASE WHEN d.%s = 0 THEN 1 ELSE 0 END) AS %s',
              val_f, DBI::dbQuoteIdentifier(con, paste0(val, ".n")))
    )
  }
  
  group_by <- c("p.AARl", "p.AARh", sprintf("d.%s", dims_no_year))

  sql <- sprintf(
    "CREATE OR REPLACE TABLE %s AS
    SELECT %s FROM %s d
    INNER JOIN %s p ON d.AARl >= p.AARl AND d.AARh <= p.AARh
    GROUP BY 
    %s",
    tmp_tbl,
    paste(select_parts, collapse = ",\n"),
    tbl_sql,
    tmp_periods,
    paste(group_by, collapse = ",\n")
  )
  
  invisible(DBI::dbExecute(con, sql))
  
  # Denne er sketchy, for om hele år mangler så gir ikke dette .fn9 = 1.
  # I en 5-årsperiode med 2 manglende år, må altså de tre andre årene ha f = 9 for at val.fn9 > antall manglende år
  # Denne bør raffineres i fremtiden. Vi bør også vurdere hvorvidt vi skal ha andre
  # kriterier for å ikke lage en sum, kanskje basert på .n-kolonnene (må ha minst x % av årene for å lage sum)
  # For nå er denne beholdt som den var opprinnelig.
  missing_year <- parameters$MOVAV$missyears
  if (missing_year$n <= period) {
    for (val in values) {
      val_sql <- DBI::dbQuoteIdentifier(con, val)
      val_f <- DBI::dbQuoteIdentifier(con, paste0(val, ".f"))
      val_fn9 <- DBI::dbQuoteIdentifier(con, paste0(val, ".fn9"))
      sql <- sprintf(
        "UPDATE %s
      SET
        %s = NULL,
        %s = 9
      WHERE %s > %s",
        tmp_tbl,
        val_sql,
        val_f,
        val_fn9,
        missing_year$n)
      invisible(DBI::dbExecute(con, sql))
    }
  }
  
  
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, sprintf("DROP TABLE %s", tbl_sql))
    DBI::dbExecute(con, sprintf("ALTER TABLE %s RENAME TO %s", tmp_tbl, tbl_sql))
  })
  
  invisible(NULL)
} 
 



find_periods <- function(aarh, period){
  start_max <- min(aarh) + period - 1
  end_max <- max(aarh)
  if(start_max > end_max) stop("Aggregering til ", period, "-årige tall feilet pga utilstrekkelig antall årganger")
  max_aar <- start_max:end_max
  min_aar <- max_aar - period + 1
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
  
  cols <- DBI::dbListFields(con, tablename)
  values <- get_value_columns(cols)
  anonymous_tolerance <- getOption("khfunctions.anon_tot_tol")
  
  tbl_sql <- DBI::dbQuoteIdentifier(con, tablename)
  
  for(val in values){
    val_sql <- DBI::dbQuoteIdentifier(con, val)
    val_f   <- DBI::dbQuoteIdentifier(con, paste0(val, ".f"))
    val_n   <- DBI::dbQuoteIdentifier(con, paste0(val, ".n"))
    val_fn3 <- DBI::dbQuoteIdentifier(con, paste0(val, ".fn3"))
    
    if (!paste0(val, ".n") %in% cols) next
    
    sql <- sprintf(
      "UPDATE %s
      SET 
      %s = NULL, 
      %s = 3
      WHERE
        %s > 0
        AND CAST(%s AS DOUBLE) / %s >= %s",
      tbl_sql,
      val_sql,
      val_f,
      val_n,
      val_fn3,
      val_n,
      anonymous_tolerance
    )
    
    DBI::dbExecute(con, sql)
  }
  
  invisible(NULL)
}




# add_n_missing_year <- function(dt, periods, missing_year){
#   periods[, missyear := 0]
#   for(i in 1:nrow(periods)){
#     periods[i, missyear := sum(missing_year$years %in% aarl:aarh)]
#   }
#   dt[periods, on = c("AARl" = "aarl"), missyear := i.missyear]
# }

#' @title handle_indata_periods
#' @description
#' If indata contains periods, val.n must be adjusted accordingly
#' @keywords internal
#' @noRd
do_handle_indata_periods <- function(con,tablename,parameters){
  n <- as.integer(ifelse(parameters$MOVAV$is_orig_snitt, 1L, parameters$MOVAV$int_lengde))
  cols <- DBI::dbListFields(con, tablename)
  values <- get_value_columns(cols)
  tbl_sql <- DBI::dbQuoteIdentifier(con, tablename)
  
  val_n_cols <- vector("character", length(values))
    
  for(i in seq_along(values)){
    val_n <- DBI::dbQuoteIdentifier(con, paste0(values[i], ".n"))
    val_n_cols[i] <- sprintf("%s = %s", val_n, n)
    if(!paste0(values[i], ".n") %in% cols) {
        DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s INTEGER",
            tbl_sql,val_n))
    }}
  
  update_sql <- sprintf("UPDATE %s SET %s", tbl_sql, paste(val_n_cols, collapse = ", "))
  
  invisible(DBI::dbExecute(con, update_sql))
  invisible(NULL)
}

# Deprecated ----

#' @title organize_file_for_moving_average
#' @description
#' Make sure dt is arranged according to AARl and AARh last
organize_file_for_moving_average <- function(dt){
  tabcols_minus_aar <- grep("^AARl$|^AARh$", get_dimension_columns(names(dt)), value = T, invert = T)
  key <- c(tabcols_minus_aar, "AARl", "AARh")
  if(!identical(data.table::key(dt), key)) data.table::setkeyv(dt, key)
}

get_movav_information_old <- function(dt, parameters){
  mapar <- list()
  mapar[["aar"]] <- unique(dt[, .SD, .SDcols = c("AARl", "AARh")])
  mapar[["int_lengde"]] <- unique(mapar$aar[, AARh - AARl + 1])
  if(length(mapar$int_lengde) > 1) stop("Inndata har ulike årsintervaller!")
  mapar[["is_movav"]] <- parameters$CUBEinformation$MOVAV > 1
  mapar[["movav"]] <- parameters$CUBEinformation$MOVAV
  mapar[["snitt"]] <- parameters$fileinformation[[parameters$files$TELLER]]$ValErAarsSnitt
  mapar[["is_orig_snitt"]] <- !is.na(mapar$snitt) && mapar$snitt != 0
  snitt_orgintmult <- ifelse(mapar$is_orig_snitt, mapar$int_lengde, 1)
  mapar[["orgintMult"]] <- ifelse(mapar$is_movav, 1, snitt_orgintmult)
  mapar[["missyears"]] <- find_missing_year(unique(dt$AARl))
  return(mapar)
}

#' @title aggregate_to_moving_average
#' @description
#' Finn "snitt" for ma-aar.
#' DVs, egentlig lages forloepig bare summer, snitt settes etter prikking under
#' Snitt tolerer missing av type .f=1 ("random"), men bare noen faa anonyme .f>1, se KHaggreger
#' Rapporterer variabelspesifikk VAL.n som angir antall aar brukt i summen naar NA holdt utenom
#' 
#' Dersom is_movav = FALSE, legges val.n til for alle verdikolonner, satt til 1 dersom originale snitt
#' og satt til antall år dersom originale summer. 
#'
#' @param dt KUBE to be aggregated 
#' @param reset_rate Reset RATE after aggregating to periods?
#' @param parameters cube parameters
aggregate_to_periods_old <- function(dt, parameters){
  save_filedump_if_requested(dumpname = "MOVAVpre", dt = dt, parameters = parameters)
  on.exit({save_filedump_if_requested(dumpname = "MOVAVpost", dt = dt, parameters = parameters)}, add = TRUE)
  do_balance_missing_teller_nevner_old(dt = dt)
  
  if(parameters$MOVAV$is_movav){
    dt <- do_aggregate_periods_old(dt = dt, parameters = parameters)
    dt <- do_filter_periods_with_missing_original_old(dt)
  } else {
    dt <- do_handle_indata_periods_old(dt = dt, parameters = parameters)
  }
  return(dt)
}

do_aggregate_periods_old <- function(dt, parameters){
  period <- parameters$MOVAV$movav
  if(any(dt$AARl != dt$AARh)) stop(paste0("Aggregering til ", movav, "-årige tall er ønsket, men originaldata inneholder allerede flerårige tall og kan derfor ikke aggregeres!"))
  
  aggregated_dt <- calculate_period_sums(dt = dt, period = period, missing_year = parameters$MOVAV$missyears)
  return(aggregated_dt)
}

#' @title calculate_period_sums
#' @description
#' Aggregates value columns to period sums for periods defined in ACCESS::KUBER::MOVAV
#' @noRd
calculate_period_sums <- function(dt, period, missing_year){
  # print_console_message("\n* Aggregerer til ", period, "-årige tall\n", sep = "")
  allperiods <- find_periods(aarh = unique(dt$AARh), period = period)
  dt <- extend_to_periods(dt = dt, periods = allperiods)
  values <- get_value_columns(names(dt))
  dt[, paste0(rep(values, each = 4), c(".fn1", ".fn3", ".fn9", ".n")) := NA_integer_]
  dims <- get_dimension_columns(names(dt))
  colorder <- dims
  for(val in values){
    dt[is.na(dt[[val]]) | dt[[val]] == 0, paste0(val, ".a") := 0]
    dt[dt[[paste0(val, ".f")]] %in% c(1,2), paste0(val, ".fn1") := 1]
    dt[dt[[paste0(val, ".f")]] == 3, paste0(val, ".fn3") := 1]
    dt[dt[[paste0(val, ".f")]] == 9, paste0(val, ".fn9") := 1]
    dt[dt[[paste0(val, ".f")]] == 0, paste0(val, ".n") := 1]
    colorder <- c(colorder, paste0(val, c("", ".f", ".a", ".fn1",".fn3", ".fn9", ".n")))
  }
  g <- collapse::GRP(dt, dims)
  aggdt <- collapse::add_vars(g[["groups"]],
                              collapse::fsum(collapse::get_vars(dt, values), g = g, fill = T),
                              collapse::fsum(collapse::get_vars(dt, paste0(values, ".a")), g = g, fill = T),
                              collapse::fsum(collapse::get_vars(dt, paste0(values, ".fn1")), g = g, fill = T),
                              collapse::fsum(collapse::get_vars(dt, paste0(values, ".fn3")), g = g, fill = T),
                              collapse::fsum(collapse::get_vars(dt, paste0(values, ".fn9")), g = g, fill = T),
                              collapse::fsum(collapse::get_vars(dt, paste0(values, ".n")), g = g, fill = T))
  aggdt[, (paste0(values, ".f")) := 0]
  data.table::setcolorder(aggdt, colorder)
  
  # add_n_missing_year(dt = aggdt, periods = allperiods, missing_year = missing_year)
  # Denne er sketchy, for om hele år mangler så gir ikke dette .fn9 = 1.
  # I en 5-årsperiode med 2 manglende år, må altså de tre andre årene ha f = 9 for at val.fn9 > antall manglende år
  if(missing_year$n <= period){
    for(val in values) aggdt[aggdt[[paste0(val, ".fn9")]] > missing_year$n, (c(val, paste0(val, ".f"))) := list(NA, 9)]
  }
  
  f9s <- names(aggdt)[grepl(".f9$", names(aggdt))]
  if (length(f9s) > 0) aggdt[, (f9s) := NULL]
  
  return(aggdt)
}

do_filter_periods_with_missing_original_old <- function(dt){
  values <- get_value_columns(names(dt))
  anonymous_tolerance <- getOption("khfunctions.anon_tot_tol") 
  for(val in values){
    val.f <- paste0(val, ".f")
    val.n <- paste0(val, ".n")
    val.fn3 <- paste0(val, ".fn3")
    if(val.n %in% names(dt)) dt[dt[[val.n]] > 0 & dt[[val.fn3]]/dt[[val.n]] >= anonymous_tolerance, c(val, val.f) := list(NA, 3)]
  }
  return(dt)
}

do_handle_indata_periods_old <- function(dt, parameters){
  # USIKKER PÅ OM DETTE HÅNDTERES KORREKT, SJEKK MED EN FIL SOM INNEHOLDER FLERÅRIGE SNITT ELLER SUMMER
  # Må legge til VAL.n når originale summer, VAL.n = 1 om originale snit
  n <- ifelse(parameters$MOVAV$is_orig_snitt, 1, parameters$MOVAV$int_lengde) 
  dt[, paste0(names(.SD), ".n") := n, .SDcols = get_value_columns(names(dt))]
  return(dt)
}

#' @title do_balance_missing_teller_nevner
#' @description
#' Maa "balansere" NA i teller og nevner slik sumrate og sumnevner balanserer.
#' Kunne med god grunn satt SPVFLAGG her og saa bare operert med denne som en egenskap for hele linja i det som kommer
#' Men for aa ha muligheten for aa haandtere de forskjellige variablene ulikt og i full detalj lar jeg det staa mer generelt
#' Slik at dataflyten stoetter en slik endring
#' 
#' Om enkeltobservasjoner ikke skal brukes, men samtidig tas ut av alle summeringer
#' kan man ha satt VAL=0,VAL.f=-1
#' Dette vil ikke oedelegge summer der tallet inngaar. Tallet selv, eller sumemr av kun slike tall, settes naa til NA
#' Dette brukes f.eks naar SVANGERROYK ekskluderer Oslo, Akershus. Dette er skjuling, saa VAL.f=3
#' 
#' @param dt data
do_balance_missing_teller_nevner_old <- function(dt){
  vals <- intersect(c("TELLER", "NEVNER"), names(dt))
  valsF <- paste0(vals, ".f")
  if(length(vals) == 0) return(dt)
  dt[, maxF := do.call(pmax, .SD), .SDcols = valsF]
  dt[maxF != 0, (valsF) := maxF]
  dt[maxF != 0, (vals) := NA]
  dt[, maxF := NULL]
}

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

