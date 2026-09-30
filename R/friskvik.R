#' @title generate_and_export_all_friskvik_indicators
#' @description
#' Looper over oppsatte friskvikfiler og skriver csv
#' @param parameters global parameters
#' @noRd
generate_and_export_all_friskvik_indicators <- function(parameters) {
  if(!parameters$write) return(invisible(NULL))
  indikatorer <- parameters$friskvik[, .SD, .SDcols = c("INDIKATOR", "ID")]
  if (nrow(indikatorer) == 0){
    khtools::msg("\n** INGEN FRISKVIKFILER SATT OPP")
    return(invisible(NULL))
  }
  
  khtools::msg("* Lager Friskvikfil(er):")
  for(i in seq_len(nrow(indikatorer))){ 
    generate_and_export_friskvik_indicator(id = indikatorer[i, ID], parameters = parameters)
  }
  invisible(NULL)
}


#' @title generate_and_export_friskvik_indicator
#' @description
#' Genererer og skriver hver enkelt friskvikfil 
#' @noRd
generate_and_export_friskvik_indicator <- function(id, parameters){
  con <- parameters$duck
  FVdscr <- parameters$friskvik[ID == id]
  if(nrow(FVdscr) != 1) stop("Fant ikke unik FRISKVIK-definisjon for ID = ", id)
  
  if(!FVdscr$MODUS %in% c("K", "F", "B")){
    khtools::msg("\n*** ADVARSEL: modus ", FVdscr$MODUS, " støttes ikke for FRISKVIK")
    return(invisible(NULL))
  }
  FriskVDir <- switch(FVdscr$MODUS,
                      "K" = ifelse(FVdscr$PROFILTYPE == "FHP",getOption("khfunctions.fhpK"),getOption("khfunctions.ovpK")),
                      "B" = ifelse(FVdscr$PROFILTYPE == "FHP",getOption("khfunctions.fhpB"),getOption("khfunctions.ovpB")),
                      "F" = ifelse(FVdscr$PROFILTYPE == "FHP",getOption("khfunctions.fhpF"),getOption("khfunctions.ovpF")))
  
  # GEO-filter
  geo_filter <- switch(FVdscr$MODUS,
                       "K" = c("K", "F", "L"),
                       "B" = c("B", "K", "F", "L"),
                       "F" = c("F", "L"))
  geovals <- paste(khtools::sql_quote_S(con, geo_filter),collapse = ", ")
  
  where <- sprintf("GEOniv IN (%s)", geovals)
  
  # ALDER-filter
  if(is_not_empty(FVdscr$ALDER) && FVdscr$ALDER != "-"){
    amin <- parameters$fileinformation[[parameters$files$TELLER]]$amin
    amax <- parameters$fileinformation[[parameters$files$TELLER]]$amax
    age_filter <- FVdscr$ALDER
    age_filter <- gsub("^(\\d+)$", "\\1_\\1", age_filter)
    age_filter <- gsub("^(\\d+)_$", paste0("\\1_", amax), age_filter)
    age_filter <- gsub("^_(\\d+)$", paste0(amin, "_\\1"), age_filter)
    age_filter <- gsub("^ALLE$", paste0(amin, "_", amax), age_filter)
    
    where <- c(where, sprintf("ALDER = '%s'", age_filter))
  }
  
  # STANDARD-dimensjon-filter
  for(tab in c("AARh", "KJONN", "INNVKAT", "UTDANN", "LANDBAK")){
    val <- FVdscr[[tab]]
    if(is_not_empty(val) && val != "-"){
      where <- c(where, sprintf("%s = '%s'", tab, val))}
  }
  
  # EKSTRA-dimensjon-filter
  if(is_not_empty(FVdscr$EKSTRA_TAB) && FVdscr$EKSTRA_TAB != "-"){
    where <- c(where, paste0("(",r_filter_to_sql(FVdscr$EKSTRA_TAB),")"))
  }
  
  where_sql <- paste(where, collapse = " AND ")
  
  select_cols <- c(getOption("khfunctions.profiltabs"), getOption("khfunctions.profilvals"))
  available_cols <- DBI::dbListFields(con, "ALLVIS_base")
  missing_cols <- setdiff(select_cols, available_cols)
  
  select_expr <- vapply(select_cols,
    function(x){ 
      if(x %in% available_cols){ 
        x
      } else if(x %in% getOption("khfunctions.profiltabs")){
        sprintf("CAST(NULL AS VARCHAR) AS %s", x)
      } else {
        sprintf("CAST(NULL AS DOUBLE) AS %s", x)
      }
      
    },
    character(1)
  )
  
  if(is_not_empty(FVdscr$ALTERNATIV_MALTALL)){
    altcol <- FVdscr$ALTERNATIV_MALTALL
    select_expr <- c(
      getOption("khfunctions.profiltabs"),
      sprintf("%s AS MALTALL", altcol),
      sprintf(
        "CAST(NULL AS DOUBLE) AS %s",
        setdiff(getOption("khfunctions.profilvals"), "MALTALL"))
    )
  }
  
  # Sjekke antall rader
  nrows_sql <- sprintf("SELECT COUNT(*) AS N FROM ALLVIS_base WHERE %s", where_sql)
  nrows <- DBI::dbGetQuery(con, nrows_sql)$N
  if(nrows == 0){
    khtools::msg("\n!!-->> INGEN RADER I FRISKVIKFIL, IKKE GENERERT:",msgpath)
    return(invisible(NULL))
  }
  
  n_expected_rows_sql <- sprintf("SELECT COUNT(*) AS N FROM GeoKoder WHERE TYP = 'O' AND GEOniv IN (%s) AND FRA <= '%s' AND TIL > '%s'",
                                 geovals, parameters$year, parameters$year)
  n_expected <- DBI::dbGetQuery(con, n_expected_rows_sql)$N
  
  if(nrows != n_expected && !grepl("UNGDATA", parameters$name)){
    warning("\nFEIL I FRISKVIKFILTER som gir ", nrows, " / ", n_expected, " forventede rader, er dette riktig eller er det noe feil i friskvikfilteret?")
  }
  
  # Skriv fil
  setPath <- file.path(getOption("khfunctions.root"),
                       getOption("khfunctions.kubedir"),
                       FriskVDir, parameters$year, "csv")
  
  if(!fs::dir_exists(setPath)) fs::dir_create(setPath)
  filename <- file.path(setPath, paste0(FVdscr$INDIKATOR, "_", parameters$batchdate,".csv"))
  msgpath <- paste0("- ", FriskVDir, "/", parameters$year, "/csv/", basename(filename))
  khtools::msg(msgpath)
  
  export_sql <- sprintf(
    "COPY (
    SELECT %s FROM ALLVIS_base WHERE %s
    )
    TO '%s'
    (HEADER, DELIMITER ';')",
    paste(select_expr, collapse = ",\n        "),
    where_sql,
    gsub("\\\\", "/", filename, fixed = TRUE)
  )
  
  invisible(DBI::dbExecute(con, export_sql))
  invisible(NULL)
}

#' @title generate_specific_friskvik_indicators
#' @description
#' Lager friskvikfiler fra allerede godkjent kube
#' @param cubename navn
#' @param friskvik_id Kan evt spesifisere FRISVIK_ID. Dersom denne er NULL lages alle
#' @param year friskvikår, default = getOption("khfunctions.year")
#' @export
generate_specific_friskvik_indicators <- function(cubename = NULL, friskvik_id = NULL, year = getOption("khfunctions.year")){
  if(is.null(cubename)) stop("cubename kan ikke være NULL")
  overwritewarning <- paste(
    "\n** Filer overskrives om de eksisterer",
    "\n** Bare csv-mappen endres, eksisterende godkjentmapper endres IKKE!"
  )
  
  user_args <- list(name = cubename, year = year)
  parameters <- get_cubeparameters(user_args = user_args)
  on.exit({
    duckfile <- DBI::dbGetInfo(parameters$duck)$dbname
    RODBC::odbcCloseAll()
    DBI::dbDisconnect(parameters$duck)
    if(fs::file_exists(duckfile)) fs::file_delete(duckfile)
  }, add = TRUE)
  
  valid_cube <- read_kubestatus(parameters$dbh, year)[KUBE_NAVN == cubename]
  
  if(nrow(valid_cube) == 0){
    khtools::msg("*** Ingen godkjent kube funnet i KUBESTATUS, kan ikke lage friskvikfiler")
    return(invisible(NULL))
  }
  
  if(nrow(valid_cube) > 1){
    khtools::msg("*** > 1 godkjent kube funnet i KUBESTATUS som matchet cubename, kan ikke lage friskvikfiler")
    return(invisible(NULL))
  }
  
  parameters[["batchdate"]] <- valid_cube$DATOTAG_KUBE
  indicators <- parameters$friskvik[, .SD, .SDcols = c("INDIKATOR", "ID")]
  
  if(is.null(friskvik_id)){
    ids <- indicators$ID
    names <- indicators$INDIKATOR
  } else if(any(!friskvik_id %in% indicators$ID)){
    stop("Minst en av ønskede FRISKVIK-IDer (",
         paste(friskvik_id, collapse = ", "),
         ") finnes ikke i FRISKVIK-tabellen!"
      )
  } else {
    ids <- indicators[ID %in% friskvik_id]$ID
    names <- indicators[ID %in% friskvik_id]$INDIKATOR
  }
 
  khtools::msg("* Lager følgende friskvikfiler, FRISKVIK-ID(er):",
                        paste0("\n- INDIKATOR: ",names, ", ID: ", ids), overwritewarning)
  
  cube_name <- sprintf("%s_%s.parquet", valid_cube$KUBE_NAVN, valid_cube$DATOTAG_KUBE)
  kubepath <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kubedir"), getOption("khfunctions.kube.dat"), "R", cube_name)
  
  if(!file.exists(kubepath)) stop("Finner ikke godkjent kube ", kubepath, "\nSjekk om datotag i kubestatus er korrekt")
  
  con <- parameters$duck
  DBI::dbExecute(con, sprintf("CREATE TABLE KUBE AS SELECT * FROM read_parquet(%s)", khtools::sql_quote_S(con, kubepath)))
  generate_allvis_base(parameters = parameters)
  
  khtools::msg("\n* Skriver filer: ")
  for(i in ids){
    generate_and_export_friskvik_indicator(id = i, parameters = parameters)
  }
  
  invisible(NULL)
}
