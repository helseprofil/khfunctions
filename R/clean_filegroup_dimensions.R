clean_filegroup_dimensions <- function(dt, parameters, cleanlog){
  cat("\n* Starter rensing av dimensjoner...")
  do_clean_GEO(dt = dt, parameters = parameters, cleanlog = cleanlog)
  do_clean_AAR(dt = dt, cleanlog = cleanlog)
  do_clean_ALDER(dt = dt, parameters = parameters, cleanlog = cleanlog)
  do_clean_KJONN(dt = dt, cleanlog = cleanlog)
  do_clean_UTDANN(dt = dt, cleanlog = cleanlog)
  do_clean_INNVKAT(dt = dt, cleanlog = cleanlog)
  do_clean_LANDBAK(dt = dt, cleanlog = cleanlog)
  
  cat("\n* Dimensjoner ferdig renset")
  return(dt)
}

# GEO ----
#' @title do_clean_GEO
#' @noRd
do_clean_GEO <- function(dt, parameters, cleanlog){
  cat("\n** Renser GEO")
  dt[, let(GEO = trimws(GEO))]
  format_raw_geo(dt = dt)
  recode_geo_from_name(dt = dt, parameters = parameters)
  dt[GEO != "0" & nchar(GEO) %in% c(1,3,5,7,9), GEO := paste0("0", GEO)]
  set_unknown_geo_99(dt = dt, parameters = parameters)
  set_geoniv(dt = dt, parameters = parameters)
  set_fylke(dt = dt)
  
  check_if_dimension_ok(dt = dt, cleanlog = cleanlog, col = "GEO", illegal = getOption("khfunctions.geo_illegal"))
}

#' @title format_raw_geo
#' @description special fix for text variants
#' @noRd
format_raw_geo <- function(dt){
  if("LEVEL" %in% names(dt)) return(invisible(NULL))
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

#' @title set_unknown_geo_99
#' @description Set 99-codes for unknown GEO-codes
#' @noRd
set_unknown_geo_99 <- function(dt, parameters){
  unknown <- unique(dt$GEO)[!unique(dt$GEO) %in% parameters$GeoKoder$GEO]
  if(length(unknown) > 0){
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
    if(n_valid99 > 0) cat("\n*** Setter ", n_valid99, " kjente 99-koder", sep = "")
    dt[recode_valid99, on = c(GEO = "ORGGEO"), GEO := ifelse(!is.na(i.RECODE), i.RECODE, GEO)] 
    
    recode_invalid99 <- data.table::data.table(ORGGEO = unknown[invalid99_ind], RECODE = getOption("khfunctions.geo_illegal"))
    recode_invalid99[grepl("^\\d+$", ORGGEO), RECODE := sapply(nchar(ORGGEO), function(x) paste0(rep(9, x), collapse = ""))]
    n_invalid99 <- dt[GEO %in% recode_invalid99[RECODE != getOption("khfunctions.geo_illegal"), ORGGEO], .N]
    if(n_invalid99 > 0) cat("\n*** Setter ", n_invalid99, " helt ukjente 99-koder", sep = "")
    dt[recode_invalid99, on = c(GEO = "ORGGEO"), GEO := ifelse(!is.na(i.RECODE), i.RECODE, GEO)] 
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
  if(n_not_ok > 0) cat("\n*** Fant ugyldige GEO i ", n_not_ok, " originalfiler, ikke OK!", sep = "")
  if(n_not_ok == 0) cat("\n*** Alle GEO ok")
}

#' @title do_clean_AAR
#' @description formats AAR and generate AARl/AARh
#' @noRd
do_clean_AAR <- function(dt, cleanlog){
  cat("\n** Renser AAR")
  dt[, let(AAR = trimws(AAR))]
  dt[grepl("^Høsten ", AAR), let(AAR = sub("^Høsten ", "", AAR))]
  dt[grepl("^(\\d+) *[_-] *(\\d+)$", AAR), let(AAR = sub("^(\\d+) *[_-] *(\\d+)$", "\\1_\\2", AAR))]
  dt[grepl("^ *(\\d+) *$", AAR), let(AAR = sub("^ *(\\d+) *$", "\\1_\\1", AAR))]
  dt[!grepl("^\\d{4}_\\d{4}$", AAR), let(AAR = getOption("khfunctions.aar_illegal"))]
  
  aarint <- c("AARl", "AARh")
  dt[, (aarint) := tstrsplit(AAR, "_")]
  dt[AARl > AARh, let(AAR = getOption("khfunctions.aar_illegal"))]
  dt[AARl > AARh, (aarint) := tstrsplit(getOption("khfunctions.aar_illegal"), "_")]
  check_if_dimension_ok(dt = dt, cleanlog = cleanlog, col = "AAR", illegal = getOption("khfunctions.aar_illegal"))
  dt[, let(AAR = NULL)]
}

#' @title do_clean_ALDER
#' @noRd
do_clean_ALDER <- function(dt, parameters, cleanlog){
  if(!"ALDER" %in% names(dt)) return(invisible(NULL))
  cat("\n** Renser ALDER")
  
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
  dt[, (alderint) := tstrsplit(ALDER, "_")]
  dt[ALDERl > ALDERh, let(ALDER = getOption("khfunctions.aar_illegal"))]
  dt[ALDERl > ALDERh, (alderint) := tstrsplit(getOption("khfunctions.aar_illegal"), "_")]
  check_if_dimension_ok(dt = dt, cleanlog = cleanlog, col = "ALDER", illegal = getOption("khfunctions.alder_illegal"))
  dt[, let(ALDER = NULL)]
}

#' @title do_clean_KJONN
#' @noRd
do_clean_KJONN <- function(dt, cleanlog){
  if(!"KJONN" %in% names(dt)) return(invisible(NULL))
  cat("\n** Renser KJONN")
  dt[, let(KJONN = trimws(KJONN))]
  if("LEVEL" %in% names(dt)){
    dt[grepl("^(M|Menn|Mann|gutt(er|)|g)$", KJONN, ignore.case = TRUE), let(KJONN = "1")]
    dt[grepl("^(K|F|Kvinner|Kvinne|jente(r|)|j)$", KJONN, ignore.case = TRUE), let(KJONN = "2")]
    dt[grepl("^(Tot(alt|)|Begge([\\s\\._]*kjønn|)|Alle|A|M\\+K)$", KJONN, ignore.case = TRUE), let(KJONN = "0")]
    dt[grepl("^(Uspesifisert|Uoppgitt|Ikke\\s*(spesifisert|oppgitt)|Ukjent|)$", KJONN, ignore.case = TRUE), let(KJONN = getOption("khfunctions.ukjent"))]
  }
  dt[is.na(KJONN), let(KJONN = getOption("khfunctions.ukjent"))]
  dt[!KJONN %in% c("0","1","2", getOption("khfunctions.ukjent")), let(KJONN = getOption("khfunctions.illegal"))]
  check_if_dimension_ok(dt = dt, cleanlog = cleanlog, col = "KJONN", illegal = getOption("khfunctions.illegal"))
}

#' @title do_clean_UTDANN
#' @noRd
do_clean_UTDANN <- function(dt, cleanlog){
  if(!"UTDANN" %in% names(dt)) return(invisible(NULL))
  cat("\n** Renser UTDANN")
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
  cat("\n** Renser INNVKAT")
  dt[, let(INNVKAT = trimws(INNVKAT))]
  dt[grepl("^alle$", INNVKAT, ignore.case = TRUE), let(INNVKAT = "0")]
  dt[is.na(INNVKAT), let(INNVKAT = getOption("khfunctions.ukjent"))]
  dt[!INNVKAT %in% c(0, 2, 3, 20, getOption("khfunctions.ukjent")), let(INNVKAT = getOption("khfunctions.illegal"))]
  check_if_dimension_ok(dt = dt, cleanlog = cleanlog, col = "INNVKAT", illegal = getOption("khfunctions.illegal"))
}

#' @title do_clean_LANDBAK
#' @noRd
do_clean_LANDBAK <- function(dt, cleanlog){
  if(!"LANDBAK" %in% names(dt)) return(invisible(NULL))
  cat("\n** Renser LANDBAK")
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
  dim_ok[get(col) %in% illegal, let(ok = 0)]
  n_not_ok <- sum(dim_ok$ok == 0)
  dim_ok_log <- dim_ok[, .(ok = ifelse(sum(ok == 0) == 0, 1, 0)), by = KOBLID]
  rawfiles_not_ok <- dim_ok_log[ok == 0, unique(KOBLID)]
  cleanlog[dim_ok_log, on = "KOBLID", paste0(col, "_ok") := i.ok]
  if(n_not_ok > 0) cat("\n*** Fant ", n_not_ok, " ugyldige verdier for ", col, 
                       "\n - Råfiler med ugyldige verdier (KOBLID): ", paste0(rawfiles_not_ok, collapse = ", "), sep = "")
  if(n_not_ok == 0) cat("\n*** Alle ", col, " ok", sep = "")
}
  