clean_filegroup_dimensions <- function(dt, parameters, cleanlog){
  con <- parameters$duck
  print_console_message("\n\n* Starter rensing av dimensjoner...")
  do_clean_GEO_duckdb(con = con, parameters = parameters, cleanlog = cleanlog)
  # do_clean_GEO(dt = dt, parameters = parameters, cleanlog = cleanlog)
  do_clean_AAR(dt = dt, cleanlog = cleanlog)
  do_clean_ALDER(dt = dt, parameters = parameters, cleanlog = cleanlog)
  do_clean_KJONN(dt = dt, cleanlog = cleanlog)
  do_clean_UTDANN(dt = dt, cleanlog = cleanlog)
  do_clean_INNVKAT(dt = dt, cleanlog = cleanlog)
  do_clean_LANDBAK(dt = dt, cleanlog = cleanlog)
  
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
#' @noRd
do_clean_GEO_duckdb <- function(con, parameters, cleanlog){
  print_console_message("\n** Renser GEO og legger til GEOniv og FYLKE")
  build_geo_map(con = con, parameters = parameters)
  invisible(DBI::dbExecute(con, "ALTER TABLE FILGRUPPE ADD COLUMN IF NOT EXISTS GEOniv VARCHAR"))
  invisible(DBI::dbExecute(con, "ALTER TABLE FILGRUPPE ADD COLUMN IF NOT EXISTS FYLKE VARCHAR"))
  invisible(DBI::dbExecute(con, "UPDATE FILGRUPPE AS t SET
                                 GEO = m.GEO_CLEAN,
                                 GEOniv = m.GEOniv,
                                 FYLKE = m.FYLKE
                                 FROM geo_map AS m 
                                 WHERE t.GEO = m.GEO_ORG"))
  
  # SETT GEOniv S dersom nødvendig, gjelder for spesifikke KOBLID - sjekk om nødvendig
  sone6 <- parameters$read_parameters[grepl("6", SONER), unique(KOBLID)]
  if(length(sone6) > 0){
    DBI::dbWriteTable(con, "sone6", 
                      data.table::data.table(KOBLID = sone6),
                      temporary = TRUE, overwrite = TRUE)
    on.exit(DBI::dbExecute(con, "DROP TABLE IF EXISTS sone6"), add = TRUE)
    
    DBI::dbExecute(con,
                   "UPDATE FILGRUPPE AS f SET GEOniv = 'S' FROM sone6 AS s
                   WHERE f.KOBLID = s.KOBLID AND length(f.GEO) = 6")
    
    DBI::dbExecute(con,
    "UPDATE FILGRUPPE SET GEOniv = 'S' WHERE GEOniv = 'B' AND RIGHT(GEO, 2) = '00'")
  }
  
  check_if_dimension_ok_duckdb(con = con, cleanlog = cleanlog, 
                               col = "GEO", illegal = getOption("khfunctions.geo_illegal"))
  invisible(NULL)
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
  set_geoniv_map(dt = geo_map)
  set_fylke_map(dt = geo_map)
  invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS geo_map"))
  DBI::dbWriteTable(con, "geo_map",
                    value = geo_map[, .(GEO_ORG, GEO_CLEAN = GEO, GEOniv, FYLKE)],
                    temporary = TRUE, overwrite = TRUE)
  invisible(NULL)
}

#' @title format_raw_geo
#' @description special fix for text variants
#' @noRd
format_raw_geo <- function(dt){
  dt[, GEO := trimws(GEO)]
  if("LEVEL" %in% names(dt)) return(invisible(NULL))
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
  recode[!RECODE %in% parameters$GeoKoder$GEO & grepl("^\\d+$", GEO), 
         RECODE := vapply(nchar(GEO), function(n) paste(rep("9", n), collapse = ""), character(1))]
  recode[!(GEO %in% parameters$GeoKoder$GEO | GEO %in% c("99", "9999", "999999", "9999999999")),
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

#' @title set_fylke_map
#' @noRd
set_geoniv_map <- function(dt){
  dt[, let(GEOniv = NA_character_)]
  dt[nchar(GEO) == 10, let(GEOniv = "V")]
  dt[nchar(GEO) == 6, let(GEOniv = "B")]
  dt[nchar(GEO) == 4, let(GEOniv = "K")]
  dt[nchar(GEO) == 2, let(GEOniv = "F")]
  dt[GEO == "0", let(GEOniv = "L")]
  dt[GEO %in% c("81", "82", "83", "84"), let(GEOniv = "H")]
  dt[is.na(GEOniv), let(GEOniv = "U")]
}

#' @title set_fylke
#' @noRd
set_fylke_map <- function(dt){
  dt[, let(FYLKE = NA_character_)]
  dt[GEOniv %in% c("V", "S", "K", "F", "B"), let(FYLKE = sub("(\\d{2}).*", "\\1", GEO))]
  dt[GEOniv %in% c("L", "H"), let(FYLKE = "00")]
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
#' @title do_clean_AAR
#' @description formats AAR and generate AARl/AARh
#' @noRd
do_clean_AAR <- function(dt, cleanlog){
  print_console_message("\n** Renser AAR")
  dt[, let(AAR = trimws(AAR))]
  dt[grepl("^Høsten ", AAR), let(AAR = sub("^Høsten ", "", AAR))]
  dt[grepl("^(\\d+) *[_-] *(\\d+)$", AAR), let(AAR = sub("^(\\d+) *[_-] *(\\d+)$", "\\1_\\2", AAR))]
  dt[grepl("^ *(\\d+) *$", AAR), let(AAR = sub("^ *(\\d+) *$", "\\1_\\1", AAR))]
  dt[!grepl("^\\d{4}_\\d{4}$", AAR), let(AAR = getOption("khfunctions.aar_illegal"))]
  
  aarint <- c("AARl", "AARh")
  dt[, (aarint) := data.table::tstrsplit(AAR, "_")]
  dt[AARl > AARh, let(AAR = getOption("khfunctions.aar_illegal"))]
  dt[AARl > AARh, (aarint) := data.table::tstrsplit(getOption("khfunctions.aar_illegal"), "_")]
  check_if_dimension_ok(dt = dt, cleanlog = cleanlog, col = "AAR", illegal = getOption("khfunctions.aar_illegal"))
  dt[, let(AAR = NULL)]
}

#' @title do_clean_ALDER
#' @noRd
do_clean_ALDER <- function(dt, parameters, cleanlog){
  if(!"ALDER" %in% names(dt)) return(invisible(NULL))
  print_console_message("\n** Renser ALDER")
  
  isalder <- is_not_empty(parameters$filegroup_information$ALDER_ALLE)
  amin <- ifelse(isalder, parameters$filegroup_information$amin, getOption("khfunctions.amin"))
  amax <- ifelse(isalder, parameters$filegroup_information$amax, getOption("khfunctions.amax"))
  dt[, let(ALDER = trimws(ALDER))]
  dt[grepl("_år$", ALDER), let(ALDER = sub("_år$", " år", ALDER))]
  
  pattern <- "^(\\d+)\\s*[-_]\\s*(\\d+).*" # XX-_YY
  dt[grepl(pattern, ALDER), ALDER := sub(pattern, "\\1_\\2", ALDER, ignore.case = TRUE)]
  pattern <- "^(\\d+)\\s*(?:år)?$" # XX (år)
  dt[grepl(pattern, ALDER), ALDER := sub(pattern, "\\1_\\1", ALDER, ignore.case = TRUE)]
  pattern <- "^(\\d+)\\s*(?:\\+\\s*(?:år)?|år\\s*\\+|\\+)$" # XX(+|år+|+år)
  dt[grepl(pattern, ALDER), ALDER := sub(pattern, paste0("\\1_", amax), ALDER, ignore.case = TRUE)]
  pattern <- "^(\\d+)\\s*(?:-\\s*(?:år)?|år\\s*-|-)$" # XX(-|år-|-år)
  dt[grepl(pattern, ALDER), ALDER := sub(pattern, paste0(amin, "_\\1"), ALDER, ignore.case = TRUE)]
  pattern <- "^-\\s*(\\d+)(?:\\s*år)$" # -XX(år)
  dt[grepl(pattern, ALDER), ALDER := sub(pattern, paste0(amin, "_\\1"), ALDER, ignore.case = TRUE)]
  pattern <- "^(\\d+)\\s*(:?år)?\\s*(og|eller)\\s*eldre" # XX (år)(og|eller) eldre
  dt[grepl(pattern, ALDER), ALDER := sub(pattern, paste0("\\1_", amax), ALDER, ignore.case = TRUE)]
  pattern <- "^over\\s*(\\d+)\\s*(?:år)?" # over xx (år)
  dt[grepl(pattern, ALDER), ALDER := sub(pattern, paste0("\\1_", amax), ALDER, ignore.case = TRUE)]
  pattern <- "^(\\d+)\\s*(:?år)?\\s*(og|eller)\\s*(yngre|under)"# xx (år)(og|eller)yngre
  dt[grepl(pattern, ALDER), ALDER := sub(pattern, paste0(amin, "_\\1"), ALDER, ignore.case = TRUE)]
  pattern <- "^Alle\\s*(aldre.*|)|(Totalt|I alt)"
  dt[grepl(pattern, ALDER), ALDER := sub(pattern, paste0(amin, "_", amax), ALDER, ignore.case = TRUE)]
  pattern <- "Ukjent|Uoppgitt|Ikke kjent"
  dt[grepl(pattern, ALDER), ALDER := sub(pattern, getOption("khfunctions.alder_ukjent"), ALDER, ignore.case = TRUE)]
  dt[!grepl("^\\d+_\\d+$", ALDER), ALDER := getOption("khfunctions.alder_illegal")]
  
  alderint <- c("ALDERl", "ALDERh")
  dt[, (alderint) := data.table::tstrsplit(ALDER, "_")]
  dt[as.integer(ALDERl) > as.integer(ALDERh), let(ALDER = getOption("khfunctions.alder_illegal"))]
  dt[as.integer(ALDERl) > as.integer(ALDERh), (alderint) := data.table::tstrsplit(getOption("khfunctions.alder_illegal"), "_")]
  check_if_dimension_ok(dt = dt, cleanlog = cleanlog, col = "ALDER", illegal = getOption("khfunctions.alder_illegal"))
  dt[, let(ALDER = NULL)]
}

#' @title do_clean_KJONN
#' @noRd
do_clean_KJONN <- function(dt, cleanlog){
  if(!"KJONN" %in% names(dt)) return(invisible(NULL))
  print_console_message("\n** Renser KJONN")
  dt[, let(KJONN = trimws(KJONN))]
  dt[grepl("^(M|Menn|Mann|gutt(er|)|g)$", KJONN, ignore.case = TRUE), let(KJONN = "1")]
  dt[grepl("^(K|F|Kvinner|Kvinne|jente(r|)|j)$", KJONN, ignore.case = TRUE), let(KJONN = "2")]
  dt[grepl("^(Tot(alt|)|Begge([\\s\\._]*kjønn|)|Alle|A|M\\+K)$", KJONN, ignore.case = TRUE), let(KJONN = "0")]
  dt[grepl("^(Uspesifisert|Uoppgitt|Ikke\\s*(spesifisert|oppgitt)|Ukjent|)$", KJONN, ignore.case = TRUE), let(KJONN = getOption("khfunctions.ukjent"))]
  dt[is.na(KJONN), let(KJONN = getOption("khfunctions.ukjent"))]
  dt[!KJONN %in% c("0","1","2", getOption("khfunctions.ukjent")), let(KJONN = getOption("khfunctions.illegal"))]
  check_if_dimension_ok(dt = dt, cleanlog = cleanlog, col = "KJONN", illegal = getOption("khfunctions.illegal"))
}

#' @title do_clean_UTDANN
#' @noRd
do_clean_UTDANN <- function(dt, cleanlog){
  if(!"UTDANN" %in% names(dt)) return(invisible(NULL))
  print_console_message("\n** Renser UTDANN")
  dt[, let(UTDANN = trimws(UTDANN))]
  dt[grepl("^0[0-4]$", UTDANN), let(UTDANN = sub("^0([0-4])$", "\\1", UTDANN))]
  dt[grepl("^alle$", UTDANN, ignore.case = TRUE), let(UTDANN = "0")]
  dt[is.na(UTDANN), let(UTDANN = getOption("khfunctions.ukjent"))]
  dt[!UTDANN %in% c(0,1,2,3,4, getOption("khfunctions.ukjent")), let(UTDANN = getOption("khfunctions.illegal"))]
  check_if_dimension_ok(dt = dt, cleanlog = cleanlog, col = "UTDANN", illegal = getOption("khfunctions.illegal"))
}

#' @title do_clean_INNVKAT
#' @noRd
do_clean_INNVKAT <- function(dt, cleanlog){
  if(!"INNVKAT" %in% names(dt)) return(invisible(NULL))
  print_console_message("\n** Renser INNVKAT")
  dt[, let(INNVKAT = trimws(INNVKAT))]
  dt[grepl("^alle$", INNVKAT, ignore.case = TRUE), let(INNVKAT = "0")]
  dt[is.na(INNVKAT), let(INNVKAT = getOption("khfunctions.innvkat_ukjent"))]
  dt[!INNVKAT %in% c(0, 2, 3, 20, getOption("khfunctions.innvkat_ukjent")), let(INNVKAT = getOption("khfunctions.innvkat_illegal"))]
  check_if_dimension_ok(dt = dt, cleanlog = cleanlog, col = "INNVKAT", illegal = getOption("khfunctions.innvkat_illegal"))
}

#' @title do_clean_LANDBAK
#' @noRd
do_clean_LANDBAK <- function(dt, cleanlog){
  if(!"LANDBAK" %in% names(dt)) return(invisible(NULL))
  print_console_message("\n** Renser LANDBAK")
  dt[, let(LANDBAK = trimws(LANDBAK))]
  dt[grepl("^alle$", LANDBAK, ignore.case = TRUE), let(LANDBAK = "0")]
  dt[is.na(LANDBAK), let(LANDBAK = getOption("khfunctions.landbak_ukjent"))] # illegal/8 = uoppgitt
  dt[!LANDBAK %in% c(0:9, 20), let(LANDBAK = getOption("khfunctions.landbak_illegal"))]
  check_if_dimension_ok(dt = dt, cleanlog = cleanlog, col = "LANDBAK", illegal = getOption("khfunctions.landbak_illegal"))
}

#' @title check_if_dimension_ok
#' @description Check if any illegal values remain for each dimension after cleaning
#' @noRd
check_if_dimension_ok <- function(dt, cleanlog, col, illegal){
  dim_ok <- dt[, .SD, .SDcols = c(col, "KOBLID")][, let(ok = 1)]
  dim_ok[dim_ok[[col]] %in% illegal, let(ok = 0)]
  n_not_ok <- sum(dim_ok$ok == 0)
  dim_ok_log <- dim_ok[, .(ok = ifelse(sum(ok == 0) == 0, 1, 0)), by = KOBLID]
  rawfiles_not_ok <- dim_ok_log[ok == 0, unique(KOBLID)]
  cleanlog[dim_ok_log, on = "KOBLID", paste0(col, "_ok") := i.ok]
  if(n_not_ok > 0) print_console_message("\n*** Fant ", n_not_ok, " ugyldige verdier for ", col, 
                       "\n - Råfiler med ugyldige verdier (KOBLID): ", paste0(rawfiles_not_ok, collapse = ", "), sep = "")
  if(n_not_ok == 0) print_console_message("\n*** Alle ", col, " ok", sep = "")
}

# OLD ----
#' @title do_clean_GEO
#' @noRd
do_clean_GEO <- function(dt, parameters, cleanlog){
  print_console_message("\n** Renser GEO")
  dt[, let(GEO = trimws(GEO))]
  format_raw_geo(dt = dt)
  recode_geo_from_name(dt = dt, parameters = parameters)
  dt[GEO != "0" & nchar(GEO) %in% c(1,3,5,7,9), GEO := paste0("0", GEO)]
  set_unknown_geo_99(dt = dt, parameters = parameters)
  set_geoniv(dt = dt, parameters = parameters)
  set_fylke(dt = dt)
  
  check_if_dimension_ok(dt = dt, cleanlog = cleanlog, col = "GEO", illegal = getOption("khfunctions.geo_illegal"))
}

#' @title set_unknown_geo_99
#' @description Set 99-codes for unknown GEO-codes
#' @noRd
set_unknown_geo_99 <- function(dt, parameters){
  unknown <- unique(dt$GEO)[!unique(dt$GEO) %in% parameters$GeoKoder$GEO]
  if(length(unknown) > 0){
    org_geo_codes <- character()
    unknown99 <- unknown
    unknown99 <- sub("^\\d{2}$", 99, unknown99) # Ukjent fylke
    unknown99 <- gsub("^(\\d{2})\\d{2}$", paste("\\1", "99", sep = ""), unknown99) # Ukjent kommune
    unknown99 <- sub("^(\\d{2})(\\d{2})00$", paste("\\1", "9900", sep = ""), unknown99) # Ukjent kommune/sone
    unknown99 <- sub("^(\\d{4})(0[1-9]|[1-9]\\d)$", paste("\\1", "99", sep = ""), unknown99) # Ukjent bydel (ikke XXXX00)
    unknown99 <- sub("^(\\d{6})\\d{4}$", paste("\\1", "9999", sep = ""), unknown99) # Ukjent levekårssone
    valid99_ind <- which(unknown99 %in% parameters$GeoKoder$GEO)
    invalid99_ind <- which(!unknown99 %in% parameters$GeoKoder$GEO)
    
    recode_valid99 <- data.table::data.table(ORGGEO = unknown[valid99_ind], RECODE = unknown99[valid99_ind])
    n_valid99 <- dt[GEO %in% recode_valid99$ORGGEO, .N]
    if(n_valid99 > 0){
      org_geo_codes <- c(org_geo_codes, recode_valid99$ORGGEO)
      print_console_message("\n*** Setter ", n_valid99, " kjente 99-koder, fra originalkode(r): ", paste(unknown[valid99_ind], collapse = ", "), sep = "")
      dt[recode_valid99, on = c(GEO = "ORGGEO"), GEO := ifelse(!is.na(i.RECODE), i.RECODE, GEO)] 
    }
    
    recode_invalid99 <- data.table::data.table(ORGGEO = unknown[invalid99_ind], RECODE = getOption("khfunctions.geo_illegal"))
    recode_invalid99[grepl("^\\d+$", ORGGEO), RECODE := sapply(nchar(ORGGEO), function(x) paste0(rep(9, x), collapse = ""))]
    n_invalid99 <- dt[GEO %in% recode_invalid99[RECODE != getOption("khfunctions.geo_illegal"), ORGGEO], .N]
    if(n_invalid99 > 0){
      org_geo_codes <- c(org_geo_codes, recode_invalid99$ORGGEO)
      print_console_message("\n*** Setter ", n_invalid99, " helt ukjente 99-koder, fra originalkode(r): ", paste(unknown[invalid99_ind], collapse = ", "), sep = "")
      dt[recode_invalid99, on = c(GEO = "ORGGEO"), GEO := ifelse(!is.na(i.RECODE), i.RECODE, GEO)] 
    }
    org_geo_codes <<- org_geo_codes
  }
}

#' @title set_unknown_geo_99
#' @description Set 99-codes for unknown GEO-codes
#' @noRd
set_geoniv <- function(dt, parameters){
  dt[, let(GEOniv = NA_character_)]
  dt[nchar(GEO) == 10, let(GEOniv = "V")]
  dt[nchar(GEO) == 6, let(GEOniv = "B")]
  dt[nchar(GEO) == 4, let(GEOniv = "K")]
  dt[nchar(GEO) == 2, let(GEOniv = "F")]
  dt[GEO == 0, let(GEOniv = "L")]
  dt[GEO %in% 81:84, let(GEOniv = "H")]
  dt[is.na(GEOniv), let(GEOniv = "U")]
  if("LEVEL" %in% names(dt)) return(invisible(NULL))
  
  sone6 <- parameters$read_parameters[, .(KOBLID, SONER)][, let(SONE6 = ifelse(grepl("6", SONER), 1, 0))][SONE6 == 1, unique(KOBLID)]
  dt[nchar(GEO) == 6 & KOBLID %in% sone6, let(GEOniv = "S")]
  dt[GEOniv == "B" & grepl("^\\d{4}00$", GEO), let(GEO = gsub("^(\\d{4})00$", paste0("\\1", "99"), GEO))]
}

#' @title set_fylke
#' @noRd
set_fylke <- function(dt){
  dt[, let(FYLKE = NA_character_)]
  dt[GEOniv %in% c("V", "S", "K", "F", "B"), let(FYLKE = sub("(\\d{2}).*", "\\1", GEO))]
  dt[GEOniv %in% c("L", "H"), let(FYLKE = "00")]
}

#' @title check_if_geo_ok
#' @noRd
check_if_geo_ok <- function(dt, parameters, cleanlog){
  geo_ok <- dt[, .SD, .SDcols = c("GEO", "KOBLID")][, let(ok = 1)]
  geo_ok[!GEO %in% parameters$GeoKoder$GEO, let(ok = 0)]
  geo_ok <- geo_ok[, .(ok = ifelse(sum(ok == 0) == 0, 1, 0)), by = KOBLID]
  cleanlog[geo_ok, on = "KOBLID", GEO_ok := i.ok]
  n_not_ok <- sum(geo_ok$ok == 0)
  if(n_not_ok > 0) print_console_message("\n*** Fant ugyldige GEO i ", n_not_ok, " originalfiler, ikke OK!", sep = "")
  if(n_not_ok == 0) print_console_message("\n*** Alle GEO ok")
}

