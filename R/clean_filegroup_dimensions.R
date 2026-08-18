clean_filegroup_dimensions_duckdb <- function(parameters, cleanlog){
  con <- parameters$duck
  print_console_message("\n\n* Starter rensing av dimensjoner...")
  do_clean_GEO_duckdb(con = con, parameters = parameters, cleanlog = cleanlog)
  do_clean_AAR_duckdb(con = con, cleanlog = cleanlog)
  do_clean_ALDER_duckdb(con = con, parameters = parameters, cleanlog = cleanlog)
  do_clean_dimension_duckdb(con = con, col = "KJONN", cleanlog = cleanlog, illegal = getOption("khfunctions.illegal"))
  do_clean_dimension_duckdb(con = con, col = "UTDANN", cleanlog = cleanlog, illegal = getOption("khfunctions.illegal"))
  do_clean_dimension_duckdb(con = con, col = "INNVKAT", cleanlog = cleanlog, illegal = getOption("khfunctions.innvkat_illegal"))
  do_clean_dimension_duckdb(con = con, col = "LANDBAK", cleanlog = cleanlog, illegal = getOption("khfunctions.landbak_illegal"))
  print_console_message("\n* Dimensjoner ferdig renset")
}

check_if_dimension_ok_duckdb <- function(con, cleanlog, col, illegal){
  dim_ok <- DBI::dbGetQuery(con, 
  sprintf("SELECT KOBLID, MIN(CASE WHEN %s IN ('%s') THEN 0 ELSE 1 END) AS ok
      FROM FILGRUPPE GROUP BY KOBLID",
      col,
      paste(illegal, collapse = "','")))
  
  data.table::setDT(dim_ok)
  cleanlog[dim_ok, on = "KOBLID", paste0(col, "_ok") := i.ok]
  rawfiles_not_ok <- dim_ok[ok == 0, unique(KOBLID)]
  n_not_ok <- length(rawfiles_not_ok)
  
  if(n_not_ok > 0) print_console_message("\n*** Fant ", n_not_ok, " ugyldige verdier for ", col, 
                                         "\n - Råfiler med ugyldige verdier (KOBLID): ", paste0(rawfiles_not_ok, collapse = ", "), sep = "")
  if(n_not_ok == 0) print_console_message("\n*** Alle ", col, " ok", sep = "")
  
  invisible(NULL)
}

# GEO ----
#' @Title do_clean_GEO_duckdb
#' @description
#' Renser GEO-verdier. Bygger en geo_map-tabell som skrives til duckdb, som brukes til å overskrive GEO-kolonnen
#' Legger til GEOniv og FYLKE
#' @noRd
do_clean_GEO_duckdb <- function(con, parameters, cleanlog){
  print_console_message("\n** Renser GEO og legger til GEOniv og FYLKE")
  on.exit({
    invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS sone6"))
    invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS geo_map"))
    },add = TRUE)
  
  build_geo_map(con = con, parameters = parameters)
  update <- DBI::dbGetQuery(con, "SELECT EXISTS(SELECT 1 FROM geo_map WHERE GEO_ORG != GEO_CLEAN) AS update")[["update"]]
  if(update){
  invisible(DBI::dbExecute(con, "UPDATE FILGRUPPE AS t SET
                                 GEO = m.GEO_CLEAN
                                 FROM geo_map AS m 
                                 WHERE t.GEO = m.GEO_ORG"))
  } else {
    print_console_message("\n*** Alle GEO-koder var gyldige")
  }
  
  check_if_dimension_ok_duckdb(con = con, cleanlog = cleanlog, 
                               col = "GEO", illegal = getOption("khfunctions.geo_illegal"))
  
  add_geoniv_fylke(con = con, parameters = parameters)
  invisible(NULL)
}

#' Legge til GEOniv og FYLKE
add_geoniv_fylke <- function(con, parameters){
  invisible(DBI::dbExecute(con, "ALTER TABLE FILGRUPPE ADD COLUMN IF NOT EXISTS GEOniv VARCHAR"))
  invisible(DBI::dbExecute(con, "ALTER TABLE FILGRUPPE ADD COLUMN IF NOT EXISTS FYLKE VARCHAR"))
  
  invisible(DBI::dbExecute(con,
    "UPDATE FILGRUPPE SET 
    GEOniv = CASE
      WHEN GEO = '0' THEN 'L'
      WHEN GEO IN ('81','82','83','84') THEN 'H'
      WHEN LENGTH(GEO) = 10 THEN 'V'
      WHEN LENGTH(GEO) = 6 THEN 'B'
      WHEN LENGTH(GEO) = 4 THEN 'K'
      WHEN LENGTH(GEO) = 2 THEN 'F'
      ELSE 'U'
    END,
    FYLKE = CASE
      WHEN GEO IN ('0','81','82','83','84') THEN '00'
      WHEN LENGTH(GEO) IN (2,4,6,10) THEN SUBSTRING(GEO, 1, 2)
      ELSE NULL
    END"))
  
  # SETT GEOniv S dersom nødvendig, gjelder for spesifikke KOBLID - sjekk om nødvendig
  sone6 <- parameters$read_parameters[grepl("6", SONER), unique(KOBLID)]
  if(length(sone6) > 0){
    DBI::dbWriteTable(con, "sone6", 
                      data.table::data.table(KOBLID = sone6),
                      temporary = TRUE, overwrite = TRUE)
    
    invisible(DBI::dbExecute(con,
                   "UPDATE FILGRUPPE AS f SET GEOniv = 'S' FROM sone6 AS s
                    WHERE f.KOBLID = s.KOBLID AND length(f.GEO) = 6"))
    
    invisible(DBI::dbExecute(con,
                             "UPDATE FILGRUPPE SET GEOniv = 'S' WHERE GEOniv = 'B' AND RIGHT(GEO, 2) = '00'"))
  }
}

#' @Title build_geo_map
#' @description Bygger mappingtabell for GEO
#' @noRd
build_geo_map <- function(con, parameters){
  geo_map <- DBI::dbGetQuery(con, "SELECT DISTINCT CAST(GEO AS VARCHAR) AS GEO FROM FILGRUPPE")
  data.table::setDT(geo_map)
  geo_map[, GEO_ORG := GEO]
  format_raw_geo(geo_map)
  recode_geo_from_name(dt = geo_map, parameters = parameters)
  geo_map[GEO != "0" & nchar(GEO) %in% c(1,3,5,7,9), GEO := paste0("0", GEO)]
  set_unknown_geo_99_map(dt = geo_map, parameters = parameters)
  # set_geoniv_map(dt = geo_map)
  # set_fylke_map(dt = geo_map)
  invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS geo_map"))
  DBI::dbWriteTable(con, "geo_map",
                    value = geo_map[, .(GEO_ORG, GEO_CLEAN = GEO)],
                    temporary = TRUE, overwrite = TRUE)
  invisible(NULL)
}

#' @title format_raw_geo
#' @description special fix for text variants
#' @noRd
format_raw_geo <- function(dt){
  dt[, GEO := trimws(GEO)]
  dt[GEO != "0" & (
    grepl("^0{1,2}(( hele|) landet| *)$", GEO, ignore.case = TRUE) |
      grepl("^(Hele +|)landet( i alt|) *$", GEO, ignore.case = TRUE)), GEO := "0"]
  dt[grepl("^Fylke (\\d{1,2})$", GEO), GEO := sub("^Fylke (\\d{1,2})$", "\\1", GEO)]
  dt[, GEO := gsub("^(\\d+).*", "\\1", GEO)]
}

#' @title recode_geo_from_name
#' @description special fix for geo provided as name. Should go into KODEBOK as "UNIVERSAL"
#' @noRd
recode_geo_from_name <- function(dt, parameters){
  names <- grep("^\\d+$", unique(dt$GEO), invert = T, value = T)
  if(length(names) == 0) return(invisible(NULL))
  geonames <- parameters$GeoNavn[NAVN %in% names]
  dt[geonames, on = c(GEO = "NAVN"), GEO := data.table::fifelse(!is.na(i.NYGEO), i.NYGEO, GEO)]
}

set_unknown_geo_99_map <- function(dt, parameters){
  unknown <- dt[!GEO %in% parameters$GeoKoder$GEO, unique(GEO)] 
  unknown <- setdiff(unknown, c("99", "9999", "999999", "9999999999"))
  if(length(unknown) == 0) return(invisible(NULL))
  
  unknown99 <- unknown
  unknown99 <- sub("^\\d{2}$","99",unknown99)
  unknown99 <- gsub("^(\\d{2})\\d{2}$","\\199",unknown99)
  unknown99 <- sub("^(\\d{2})(\\d{2})00$","\\19900",unknown99)
  unknown99 <- sub("^(\\d{4})(0[1-9]|[1-9]\\d)$","\\199",unknown99)
  unknown99 <- sub("^(\\d{6})\\d{4}$","\\19999",unknown99)
  recode <- data.table::data.table(GEO = unknown, RECODE = unknown99)
  
  # Gjør ugyldige 99-koder til helt ukjente (f.eks 8899 -> 9999)
  # GEO-koder som ikke er tall blir til geo-illegal standardverdi
  recode[!RECODE %in% parameters$GeoKoder$GEO & grepl("^\\d+$", RECODE), 
         RECODE := vapply(nchar(GEO), function(n) paste(rep("9", n), collapse = ""), character(1))]
  recode[!(RECODE %in% parameters$GeoKoder$GEO | RECODE %in% c("99", "9999", "999999", "9999999999")),
         RECODE := getOption("khfunctions.geo_illegal")]
  recode99 <- recode[RECODE != getOption("khfunctions.geo_illegal")]
  print_console_message("\n*** Setter ukjente numeriske koder til 99, fra originalkode(r): ", 
                        paste(recode99$GEO, collapse = ", "), sep = "")
  
  illegals <- recode[RECODE == getOption("khfunctions.geo_illegal")]
  if(nrow(illegals) > 0){
    print_console_message("\n!!OBS!!, Følgende GEO-koder kan ikke kodes til gyldige verdier", paste(illegals$GEO, collapse = ", "), sep = "")
  }
  dt[recode, on = "GEO", GEO := i.RECODE]
}

#' @title set_fylke
#' @description henter minste verdi av ok fra geo_map i duckdb per KOBLID, og skriver dette til cleanlog.
#' Hvis minste verdi er 0 er det minst en GEO som er ikke ok, ellers skal verdien være 1.
#' @noRd
update_geo_cleanlog <- function(con, cleanlog){
  geo_ok <- invisible(DBI::dbGetQuery(con, "SELECT KOBLID, MIN(GEO_ok) AS ok FROM FILGRUPPE GROUP BY KOBLID"))
  data.table::setDT(geo_ok)
  cleanlog[geo_ok, on = "KOBLID", GEO_ok := i.ok]
  n_not_ok <- geo_ok[ok == 0, .N]
  n_not_ok <- sum(geo_ok$ok == 0)
  if(n_not_ok > 0) print_console_message("\n*** Fant ugyldige GEO i ", n_not_ok, " originalfiler, ikke OK!", sep = "")
  if(n_not_ok == 0) print_console_message("\n*** Alle GEO ok")
  invisible(NULL)
}

# AAR ----
#' @Title do_clean_AAR_duckdb
#' @description
#' Renser AAR-verdier. Bygger en aar_map-tabell som skrives til duckdb, som brukes til å overskrive AAR-kolonnen
#' Legger også til AARl og AARh
#' @noRd
do_clean_AAR_duckdb <- function(con, cleanlog){
  print_console_message("\n** Renser AAR og legger til AARl/AARh")
  on.exit(invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS aar_map")), add = TRUE)
  build_aar_map(con = con)
  invisible(DBI::dbExecute(con, "ALTER TABLE FILGRUPPE ADD COLUMN IF NOT EXISTS AARl VARCHAR"))
  invisible(DBI::dbExecute(con, "ALTER TABLE FILGRUPPE ADD COLUMN IF NOT EXISTS AARh VARCHAR"))
  
  invisible(DBI::dbExecute(con, "UPDATE FILGRUPPE AS t SET 
                                AAR = m.AAR_CLEAN, 
                                AARl = m.AARl, 
                                AARh = m.AARh 
                                FROM aar_map AS m
                                WHERE t.AAR = m.AAR_ORG"))
  
  check_if_dimension_ok_duckdb(con = con, cleanlog = cleanlog,
                               col = "AAR", illegal = getOption("khfunctions.aar_illegal"))
  invisible(DBI::dbExecute(con, "ALTER TABLE FILGRUPPE DROP COLUMN AAR"))
  invisible(NULL)
}

#' @Title build_aar_map
#' @description Bygger mappingtabell for AAR
#' @noRd
build_aar_map <- function(con){
  aar_map <- DBI::dbGetQuery(con, "SELECT DISTINCT CAST(AAR AS VARCHAR) AS AAR FROM FILGRUPPE")
  data.table::setDT(aar_map)
  
  aar_illegal <- getOption("khfunctions.aar_illegal")
  if(grepl("_", aar_illegal, fixed = TRUE)){
    aar_illegal_split <- strsplit(aar_illegal,"_",fixed = TRUE)[[1]]
  } else {
    aar_illegal_split <- c(aar_illegal,aar_illegal)
  }
  
  aar_map[, AAR_ORG := AAR]
  aar_map[, AAR := trimws(AAR)]
  aar_map[grepl("^Høsten ", AAR), AAR := sub("^Høsten ", "", AAR)]
  aar_map[grepl("^(\\d+) *[_-] *(\\d+)$", AAR), AAR := sub("^(\\d+) *[_-] *(\\d+)$","\\1_\\2",AAR)]
  aar_map[grepl("^ *(\\d+) *$", AAR), AAR := sub("^ *(\\d+) *$","\\1_\\1",AAR)]
  aar_map[!grepl("^\\d{4}_\\d{4}$", AAR), AAR := aar_illegal]
  
  valid <- aar_map[["AAR"]] != aar_illegal
  aar_map[, c("AARl", "AARh") := character()]
  aar_map[valid, c("AARl", "AARh") := data.table::tstrsplit(AAR, "_", fixed = TRUE)]
  aar_map[valid & !is.na(AARl) & !is.na(AARh) & as.integer(AARl) > as.integer(AARh), AAR := aar_illegal]
  aar_map[AAR == aar_illegal, c("AARl", "AARh") := aar_illegal_split]
  
  invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS aar_map"))
  DBI::dbWriteTable(con, "aar_map", 
                    value = aar_map[, .(AAR_ORG, AAR_CLEAN = AAR, AARl, AARh)],
                    temporary = TRUE, overwrite = TRUE)
  
  invisible(NULL)
}

# ALDER ----

#' @Title do_clean_ALDER_duckdb
#' @description
#' Renser ALDER-verdier. Bygger en alder_map-tabell som skrives til duckdb, som brukes til å overskrive ALDER-kolonnen
#' Legger også til ALDERl og ALDERh
#' @noRd
do_clean_ALDER_duckdb <- function(con, parameters, cleanlog){
  print_console_message("\n** Renser ALDER og legger til ALDERl/ALDERh")
  
  on.exit(invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS alder_map")), add = TRUE)
  
  build_alder_map(con = con, parameters = parameters)
  
  invisible(DBI::dbExecute(con, "ALTER TABLE FILGRUPPE ADD COLUMN IF NOT EXISTS ALDERl VARCHAR"))
  invisible(DBI::dbExecute(con, "ALTER TABLE FILGRUPPE ADD COLUMN IF NOT EXISTS ALDERh VARCHAR"))
  
  invisible(DBI::dbExecute(con,"UPDATE FILGRUPPE AS t SET 
                                ALDER  = m.ALDER_CLEAN,
                                ALDERl = m.ALDERl,
                                ALDERh = m.ALDERh 
                                FROM alder_map AS m
                                WHERE t.ALDER = m.ALDER_ORG"))
  
  check_if_dimension_ok_duckdb(con = con, cleanlog = cleanlog,
                               col = "ALDER", illegal = getOption("khfunctions.alder_illegal"))
  invisible(DBI::dbExecute(con, "ALTER TABLE FILGRUPPE DROP COLUMN ALDER"))
  invisible(NULL)
}

#' @Title build_alder_map
#' @description Bygger mappingtabell for ALDER
#' @noRd
build_alder_map <- function(con, parameters){
  
  alder_map <- DBI::dbGetQuery(con, "SELECT DISTINCT CAST(ALDER AS VARCHAR) AS ALDER FROM FILGRUPPE")
  data.table::setDT(alder_map)
  
  isalder <- is_not_empty(parameters$filegroup_information$ALDER_ALLE)
  amin <- ifelse(isalder, parameters$filegroup_information$amin, getOption("khfunctions.amin"))
  amax <- ifelse(isalder,parameters$filegroup_information$amax,getOption("khfunctions.amax"))
  alder_illegal <- getOption("khfunctions.alder_illegal")
  
  if(grepl("_", alder_illegal, fixed = TRUE)){
    alder_illegal_split <- strsplit(alder_illegal,"_",fixed = TRUE)[[1]]
  } else {
    alder_illegal_split <- c(alder_illegal,alder_illegal)
  }
  
  alder_map[, ALDER_ORG := ALDER]
  alder_map[, ALDER := trimws(ALDER)]
  alder_map[grepl("_år$", ALDER), ALDER := sub("_år$", " år", ALDER)]
  
  pattern <- "^(\\d+)\\s*[-_]\\s*(\\d+).*" # XX-_YY
  alder_map[grepl(pattern, ALDER), ALDER := sub(pattern, "\\1_\\2", ALDER, ignore.case = TRUE)]
  pattern <- "^(\\d+)\\s*(?:år)?$" # XX (år)
  alder_map[grepl(pattern, ALDER), ALDER := sub(pattern, "\\1_\\1", ALDER, ignore.case = TRUE)]
  pattern <- "^(\\d+)\\s*(?:\\+\\s*(?:år)?|år\\s*\\+|\\+)$" # XX(+|år+|+år)
  alder_map[grepl(pattern, ALDER), ALDER := sub(pattern, paste0("\\1_", amax), ALDER, ignore.case = TRUE)]
  pattern <- "^(\\d+)\\s*(?:-\\s*(?:år)?|år\\s*-|-)$" # XX(-|år-|-år)
  alder_map[grepl(pattern, ALDER), ALDER := sub(pattern, paste0(amin, "_\\1"), ALDER, ignore.case = TRUE)]
  pattern <- "^-\\s*(\\d+)(?:\\s*år)$" # -XX(år)
  alder_map[grepl(pattern, ALDER), ALDER := sub(pattern, paste0(amin, "_\\1"), ALDER, ignore.case = TRUE)]
  pattern <- "^(\\d+)\\s*(?:år)?\\s*(og|eller)\\s*eldre" # XX (år)(og|eller) eldre
  alder_map[grepl(pattern, ALDER), ALDER := sub(pattern, paste0("\\1_", amax), ALDER, ignore.case = TRUE)]
  pattern <- "^over\\s*(\\d+)\\s*(?:år)?" # over xx (år)
  alder_map[grepl(pattern, ALDER), ALDER := sub(pattern, paste0("\\1_", amax), ALDER, ignore.case = TRUE)]
  pattern <- "^(\\d+)\\s*(?:år)?\\s*(og|eller)\\s*(yngre|under)"# xx (år)(og|eller)yngre
  alder_map[grepl(pattern, ALDER), ALDER := sub(pattern, paste0(amin, "_\\1"), ALDER, ignore.case = TRUE)]
  pattern <- "^Alle\\s*(aldre.*|)|(Totalt|I alt)"
  alder_map[grepl(pattern, ALDER), ALDER := sub(pattern, paste0(amin, "_", amax), ALDER, ignore.case = TRUE)]
  pattern <- "Ukjent|Uoppgitt|Ikke kjent"
  alder_map[grepl(pattern, ALDER), ALDER := sub(pattern, getOption("khfunctions.alder_ukjent"), ALDER, ignore.case = TRUE)]
  alder_map[!grepl("^\\d+_\\d+$", ALDER), ALDER := alder_illegal]
  
  valid <- alder_map[["ALDER"]] != alder_illegal
  alder_map[, c("ALDERl", "ALDERh") := character()]
  alder_map[valid, c("ALDERl", "ALDERh") := data.table::tstrsplit(ALDER,"_",fixed = TRUE)]
  
  alder_map[valid &!is.na(ALDERl) &!is.na(ALDERh) & as.integer(ALDERl) > as.integer(ALDERh), ALDER := alder_illegal]
  alder_map[ALDER == alder_illegal,c("ALDERl", "ALDERh") := alder_illegal_split]
  
  invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS alder_map"))
  DBI::dbWriteTable(con, "alder_map",
                    value = alder_map[,.(ALDER_ORG, ALDER_CLEAN = ALDER, ALDERl, ALDERh)],
                    temporary = TRUE,overwrite = TRUE)
  
  invisible(NULL)
}



# KJONN, UTDANN, INNVKAT, LANDBAK ----

#' @Title do_clean_dimension_duckdb
#' @description
#' Renser KJONN, UTDANN, INNVKAT og LANDBAK. Disse følger samme logikk med å bare oppdatere en enkelt kolonne.
#' @noRd
do_clean_dimension_duckdb <- function(con, col, cleanlog, illegal){
  if(!col %in% DBI::dbListFields(con, "FILGRUPPE")) return(invisible(NULL))
  print_console_message("\n** Renser", col)
  
  map_table_name <- paste0(tolower(col), "_map")
  on.exit(invisible(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", map_table_name))), add = TRUE)
  build_dimension_map(con = con, col = col, map_table_name = map_table_name)
  
  update <- DBI::dbGetQuery(con, 
                            sprintf("SELECT EXISTS(SELECT 1 FROM %s WHERE ORG != CLEAN) AS update",
                                    map_table_name))[["update"]]
  
  if(update){
    invisible(DBI::dbExecute(
      con,
      sprintf(
        "UPDATE FILGRUPPE AS t
         SET %s = m.CLEAN
         FROM %s AS m
         WHERE t.%s = m.ORG",
        col, map_table_name, col)
    ))
  } else {
    print_console_message("\n*** Alle ", col, "-verdier var gyldige", sep = "")
  }
  check_if_dimension_ok_duckdb(con = con, cleanlog = cleanlog, col = col, illegal = illegal)
  invisible(NULL)
}

build_dimension_map <- function(con, col, map_table_name){
  map <- DBI::dbGetQuery(con, sprintf("SELECT DISTINCT CAST(%s AS VARCHAR) AS %s FROM FILGRUPPE", col, col))
  data.table::setDT(map)
  data.table::set(map, j = "ORG", value = map[[col]])
  data.table::set(map, j = col, value = trimws(map[[col]]))
  
  clean_fun <- switch(col,
                      KJONN   = clean_kjonn_map,
                      UTDANN  = clean_utdann_map,
                      INNVKAT = clean_innvkat_map,
                      LANDBAK = clean_landbak_map,
                      stop("Ukjent kolonne: ", col))
  clean_fun(map)

  data.table::setnames(map, col, "CLEAN")
  invisible(DBI::dbExecute(con,paste0("DROP TABLE IF EXISTS ", map_table_name)))
  DBI::dbWriteTable(con, map_table_name,
                    value = map[, .(ORG, CLEAN)],
                    temporary = TRUE, overwrite = TRUE)
  invisible(NULL)
}

clean_kjonn_map <- function(map){
  map[grepl("^(M|Menn|Mann|gutt(er|)|g)$", KJONN, ignore.case = TRUE), KJONN := "1"]
  map[grepl("^(K|F|Kvinner|Kvinne|jente(r|)|j)$", KJONN, ignore.case = TRUE), KJONN := "2"]
  map[grepl("^(Tot(alt|)|Begge([\\s\\._]*kjønn|)|Alle|A|M\\+K)$", KJONN, ignore.case = TRUE), KJONN := "0"]
  map[grepl("^(Uspesifisert|Uoppgitt|Ikke\\s*(spesifisert|oppgitt)|Ukjent|)$", KJONN, ignore.case = TRUE), KJONN := getOption("khfunctions.ukjent")]
  map[is.na(KJONN), KJONN := getOption("khfunctions.ukjent")]
  map[!KJONN %in% c("0","1","2", getOption("khfunctions.ukjent")), KJONN := getOption("khfunctions.illegal")]
}

clean_utdann_map <- function(map){
  map[grepl("^0[0-4]$", UTDANN), UTDANN := sub("^0([0-4])$", "\\1", UTDANN)]
  map[grepl("^alle$", UTDANN, ignore.case = TRUE), UTDANN := "0"]
  map[is.na(UTDANN), UTDANN := getOption("khfunctions.ukjent")]
  map[!UTDANN %in% c(0,1,2,3,4, getOption("khfunctions.ukjent")), UTDANN := getOption("khfunctions.illegal")]
}

clean_innvkat_map <- function(map){
  map[grepl("^alle$", INNVKAT, ignore.case = TRUE), INNVKAT := "0"]
  map[is.na(INNVKAT), INNVKAT := getOption("khfunctions.innvkat_ukjent")]
  map[!INNVKAT %in% c(0, 2, 3, 20, getOption("khfunctions.innvkat_ukjent")), INNVKAT := getOption("khfunctions.innvkat_illegal")]
}

clean_landbak_map <- function(map){
  map[grepl("^alle$", LANDBAK, ignore.case = TRUE), LANDBAK := "0"]
  map[is.na(LANDBAK), LANDBAK := getOption("khfunctions.landbak_ukjent")] # illegal/8 = uoppgitt
  map[!LANDBAK %in% c(0:9, 20), LANDBAK := getOption("khfunctions.landbak_illegal")]
}
