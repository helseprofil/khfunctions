# Old functions deprecated when switching to duckdb processing
# Sorted by where the functions were defined originally (filename)

# KHfilgruppe ----
#' @keywords internal
#' @noRd
do_set_fg_column_order <- function(dt){
  colorder <- "GEO"
  dims <- c(grep("GEO", getOption("khfunctions.standarddimensions"), value = T, invert = T))
  for(i in c(dims, "TAB", "VAL", "GEOniv", "FYLKE", "KOBLID")){
    colorder <- c(colorder, (names(dt)[startsWith(names(dt), i)]))
  }
  data.table::setcolorder(dt, colorder)
}

#' @keywords internal
#' @noRd
do_set_fg_value_names <- function(dt, parameters){
  vals <- get_value_columns(names(dt))
  valnames <- as.character(parameters$filegroup_information[paste0(vals, "navn")])
  suffixes <- c("", ".a", ".f")
  vals <- unlist(lapply(vals, function(x) paste0(x, suffixes)))
  valnames <- unlist(lapply(valnames, function(x) paste0(x, suffixes)))
  data.table::setnames(dt, vals, valnames)
}

#' @title check_encoding
#' @description
#' Scans all character columns for potential encoding issues. 
#' Searches for `<c3><a6>`, etc., which indicates UTF-8 read by a single-byte locale.
#' Searches for `Ã`, which indicates UTF-8 bytes were misinterpreted as Latin-1 characters.
#'
#' @param dt file group
#' @returns a list of unique problematic values which indicates that a specific file should be read with different encoding
check_encoding <- function(dt) {
  encoding_error_pattern <- "<c3>|<c2>|<e2>|<c5>|Ã"
  char_cols <- names(dt)[sapply(dt, is.character)]
  setdiff(char_cols, "KOBLID")
  errors <- list()
  ok <- TRUE
  
  for (col in char_cols) {
    if (any(grepl(encoding_error_pattern, dt[[col]], ignore.case = TRUE))) {
      # If an error is found, store the column name and unique values
      values <- unique(dt[[col]][grepl(encoding_error_pattern, dt[[col]], ignore.case = TRUE)])
      koblid <- dt[dt[[col]] %in% values, unique(KOBLID)]
      errors[[col]] <- list(values = values, koblid = koblid)
    }
  }
  
  if (length(errors) > 0) {
    warning("Potential encoding issues detected in the following columns.
            The values below are examples of garbled characters.
            The file might have been read with the wrong encoding (e.g., Latin-1 instead of UTF-8).",
            immediate. = TRUE)
    
    # Print the detailed information in a readable format
    for (col in names(errors)) {
      message(paste0("\nColumn '", col, "' has the following values with encoding issues in files specified by koblid: "))
      print(errors[[col]])
    }
    ok <- FALSE
  } else {
    print_console_message("\n** Ingen encoding-problemer oppdaget")
  }
  
  if(!ok){
    choice <- utils::menu(c("Ja, fortsett", "Nei, stopp her"),
                          title = "\nPotensielle encodingproblemer funnet, vil du fortsette?")
    if(choice == 2) stop("Dataprosesseringen stoppet pga encodingproblematikk")
  }
}

#' @title remove_helper_columns
#' @noRd
remove_helper_columns <- function(dt){
  helpers <- c("LEVEL")
  helpers <- helpers[helpers %in% names(dt)]
  dt[, (helpers) := NULL]
}

#' @title initiate_cleanlog
#' @description
#' Initiates log for filegroup cleaning
#' @noRd
initiate_cleanlog <- function(dt, codebooklog, parameters){
  log <- parameters$read_parameters[KOBLID %in% unique(dt$KOBLID), .SD, .SDcols = c("KOBLID", "DELID")][, KOBLID := as.character(KOBLID)]
  n_rows <- dt[, .(N_rows = .N), by = KOBLID]
  log <- collapse::join(log, n_rows, on = "KOBLID", verbose = 0)
  n_recoded <- codebooklog[, .(N_values_recoded = sum(as.numeric(FREQ), na.rm = T)), by = KOBLID]
  log <- collapse::join(log, n_recoded, on = "KOBLID", verbose = 0)
  n_deleted <- codebooklog[OMK == "-", .(N_rows_deleted = sum(as.numeric(FREQ), na.rm = T)), by = KOBLID]
  log <- collapse::join(log, n_deleted, on = "KOBLID", verbose = 0)
  data.table::setnafill(log, fill = 0, cols = names(log)[sapply(log, is.numeric)])
  return(log)
}

# clean_filegroup_values ----
clean_filegroup_values <- function(dt, parameters, cleanlog){
  print_console_message("\n* Starter rensing av verdikolonner...")
  vals <- names(dt)[names(dt) %in% c("VAL1", "VAL2", "VAL3")]
  dt[, (paste0(vals, ".a")) := 1]
  
  for(val in vals){
    print_console_message("\n** ", val, sep = "")
    do_set_val_flag(dt = dt, val = val)
    do_scale_val(dt = dt, val = val, parameters = parameters)
    check_if_value_ok(dt = dt, val = val, cleanlog = cleanlog)
  }
  
  print_console_message("\n* Verdikolonner ferdig renset")
}

#' @title do_set_val_flag
#' @description Set flags, set flagged values to NA, and converts the value column to numeric
#' @noRd
do_set_val_flag <- function(dt, val){
  print_console_message("\n*** Setter flagg for ", val, sep = "")
  valF <- paste0(val, ".f")
  data.table::set(dt, j = valF, 
                  value = data.table::fcase(dt[[val]] == "..", 1L,
                                            dt[[val]] == ".", 2L,
                                            dt[[val]] == ":", 3L,
                                            default = 0L))
  na_idx <- which(is.na(suppressWarnings(as.numeric(dt[[val]]))) & dt[[valF]] == 0)
  data.table::set(dt, i = na_idx, j = valF, value = 8L)
  
  flag_idx <- which(dt[[valF]] > 0)
  data.table::set(dt, i = flag_idx, j = val, value = NA_character_)
  data.table::set(dt, j = val, value = as.numeric(dt[[val]]))
}

#' @title do_scale_val
#' @description
#' Scales value-columns based on information in "SKALA_VALX"-columns in ACCESS
do_scale_val <- function(dt, val, parameters){
  scalecol <- paste0("SKALA_", val)
  scales <- parameters$read_parameters[, .SD, .SDcols = c("KOBLID", scalecol)][, let(KOBLID = as.character(KOBLID))]
  data.table::setnames(scales, 2, "scale")
  is_scale <- sum(!is.na(scales$scale) & scales$scale != 1) > 0
  if(!is_scale) return(invisible(NULL))
  
  print_console_message("\n*** Skalerer ", val, " med ", scalecol, sep = "")
  dt[scales, on = "KOBLID", scale := i.scale]
  idx <- which(!is.na(dt[["scale"]]))
  data.table::set(dt, i = idx, j = val, value = dt[[val]][idx] * dt[["scale"]][idx])
  data.table::set(dt, j = "scale", value = NULL)
}

check_if_value_ok <- function(dt, val, cleanlog){
  valF <- paste0(val, ".f")
  val_ok <- dt[, .SD, .SDcols = c(valF, "KOBLID")][,let(ok = 1)]
  data.table::set(val_ok, i = which(val_ok[[valF]] == 8), j = "ok", value = 0)
  n_not_ok <- sum(val_ok$ok == 0)
  val_ok_log <- val_ok[, .(ok = ifelse(sum(ok == 0) == 0, 1, 0)), by = KOBLID]
  rawfiles_not_ok <- val_ok_log[ok == 0, unique(KOBLID)]
  cleanlog[val_ok_log, on = "KOBLID", paste0(val, "_ok") := i.ok]
  if(n_not_ok > 0) print_console_message("\n*** Fant ", n_not_ok, " ugyldige verdier for ", val, 
                                         "\n - Råfiler med ugyldige verdier (KOBLID): ", paste0(rawfiles_not_ok, collapse = ", "), sep = "")
  if(n_not_ok == 0) print_console_message("\n*** Alle ", val, " ok", sep = "")
}

# clean_filegroup_dimensions ----
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

# Write output ----
#' @title write_population_filegroup
#' @description
#' Writes a partitioned dataset for BEF_GKny, for quicker read times when used as nevner file
#' Generates two helper columns to partition the data into age groups and with/without lks
#' @noRd
write_population_filegroup <- function(table, root){
  table <- add_partition_columns(table = table)
  print_console_message("\n* Lagrer befolkningsfilgruppe splittet på AARl og GEOniv.....")
  do_write_parquet_dataset(table = table, 
                           path = file.path(root, getOption("khfunctions.fg.ny"), getOption("khfunctions.pop_aargeo")),
                           partitioncols = c("AARl", "lks"))
  print_console_message("\n* Lagrer befolkningsfilgruppe splittet på ALDERl, AARl og GEOniv.....")
  do_write_parquet_dataset(table = table, 
                           path = file.path(root, getOption("khfunctions.fg.ny"), getOption("khfunctions.pop_alderaargeo")),
                           partitioncols = c("alder", "AARl", "lks"))
}

#' @keywords internal
#' @noRd
add_partition_columns <- function(table){
  table <- arrow::as_arrow_table(
    table |>
      dplyr::mutate(
        lks = dplyr::if_else(GEOniv == "V", 1L, 0L),
        alder = dplyr::case_when(
          ALDERh <= 17 ~ "0_17",
          ALDERh <= 29 ~ "18_29",
          ALDERh <= 44 ~ "30_44",
          ALDERh <= 67 ~ "45_67",
          ALDERh <= 79 ~ "68_79",
          .default = "80_120"
        )
      )
  )
  return(table)
}

#' @keywords internal
#' @noRd
do_write_parquet_dataset <- function(table, path, partitioncols){
  dataset <- table |> 
    dplyr::group_by(!!!rlang::syms(partitioncols)) |>
    dplyr::arrange(!!!rlang::syms(partitioncols))
  
  arrow::write_dataset(dataset = dataset, path = path, format = "parquet", partitioning = partitioncols, compression = "snappy")
  print_console_message("Ferdig!")
}


write_population_filegroup <- function(table, root){
  
  if(!grepl("BEF_GKny", parameters$name, ignore.case = T)) return(invisible(NULL))
  root <- file.path(getOption("khfunctions.root"), getOption("khfunctions.fgdir"))
  con <- parameters$duck
  path_aargeo <- file.path(root, getOption("khfunctions.fg.ny"), getOption("khfunctions.pop_aargeo"))
  path_alderaargeo <- file.path(root, getOption("khfunctions.fg.ny"), getOption("khfunctions.pop_alderaargeo"))
  DBI::dbExecute()
  print_console_message("\n* Lagrer befolkningsfilgruppe splittet på ALDER, AARl og GEOniv.....")
  DBI::dbExecute(con,
                 sprintf("COPY FILGRUPPE TO '%s'
                          (
                            FORMAT PARQUET,
                            COMPRESSION ZSTD,
                            ROW_GROUP_SIZE 1000000,
                            PARTITION_BY (alder, AARl, lks)
                          )",
                         gsub("\\\\", "/", path_alderaargeo)))
  
  print_console_message("\n* Lagrer befolkningsfilgruppe splittet på AARl og GEOniv.....")
  
  DBI::dbExecute(con,
                 sprintf("COPY FILGRUPPE TO '%s'
                          (
                            FORMAT PARQUET,
                            COMPRESSION ZSTD,
                            ROW_GROUP_SIZE 1000000,
                            PARTITION_BY (AARl, lks)
                          )",
                         gsub("\\\\", "/", path_aargeo)))
}

# Merge teller nevner ----
#' @title do_redesign_file
#'
#' @param parameters global parameters
#' @keywords internal
#' @noRd
do_redesign_file <- function(filename, filedesign, tndesign, parameters, name){
  redesign <- find_redesign(orgdesign = filedesign, targetdesign = tndesign, parameters = parameters)
  if(nrow(redesign$Udekk) > 0) print_console_message("\n**Filen", filename, "mangler tall for ", nrow(redesign$Udekk), "strata. Disse får flagg = 9 under omkoding")
  filter_and_recode_table_duckdb(con = parameters$duck, 
                                 tablename = )
  file <- do_filter_and_recode_to_redesign(dt = fetch_duckdb_table(tablename = filename, con = parameters$duck),
                                           redesign = redesign, parameters = parameters)
  print_console_message("\n*** Skriver", name, "til duckdb...\n")
  DBI::dbWriteTable(parameters$duck, name = name, value = file, overwrite = T)
}

#' @title set_teller_nevner_names
#' @description Sets name of teller and nevner column to TELLER and NEVNER by reference
set_teller_nevner_names <- function(file, TNPparameters){
  newnames <- gsub(paste0("^", TNPparameters$TELLERKOL, "(\\.f|\\.a|)$"), "TELLER\\1", names(file))
  newnames <- gsub(paste0("^", TNPparameters$NEVNERKOL, "(\\.f|\\.a|)$"), "NEVNER\\1", newnames)
  # warn_duplicated_teller_nevner_names(TNPparameters$TELLERKOL, TNPparameters$NEVNERKOL, names(file))
  data.table::setnames(file, names(file), newnames)
  warn_duplicated_column_names(names(file))
  return(file)
}

warn_duplicated_column_names <- function(columnnames){
  if(any(duplicated(columnnames))){
    message(paste0("\nNB!!! DUPLICATED COLUMN NAMES!",
                   "\nThe following column names were duplicated when trying to set TELLER and NEVNER according to what is provided in TNP_PROD:\n", 
                   paste(" -", columnnames[duplicated(columnnames)], collapse = "\n"),
                   "\nAre you trying to e.g. add a separate NEVNER file to a file already containing NEVNER?"))
  } 
}

#' @title do_filter_file
#'
#' @param parameters global parameters
do_filter_file <- function(file, design, parameters){
  for (del in names(design)) {
    cols <- parameters$DefDesign$DelKols[[del]]
    if (all(cols %in% names(file))) {
      file <- collapse::join(file, design[[del]][, ..cols], on = cols, how = "right", multiple = T, overid = 0, verbose = 0)
    }
  }
  return(file)
}

# cubeparameters ----
#' @description
#' updates cubedesign after aggregating to moving average. Changes the year part, to reflect periods. 
#' This is crucial when recoding predteller before merging onto cube. 
#' @keywords internal
#' @noRd
#' @param dt cube
#' @param origdesign Cubedesign after merging teller and nevner. 
update_cubedesign_after_moving_average_old <- function(dt, origdesign, parameters){
  if(!parameters$MOVAV$is_movav) return(origdesign)
  aar <- unique(dt[, .SD, .SDcols = c("AARl", "AARh")])
  origdesign$Y <- aar
  return(origdesign)
}

# edit_columns ----
#' @title scale_rate_and_meisskala
#' @description
#' scales RATE and MEISskala according to ACCESS::KUBER::RATESKALA
#' @noRd
scale_rate_and_meisskala_old <- function(dt, parameters){
  is_rateskala <- is_not_empty(parameters$CUBEinformation$RATESKALA)
  scalevalue <- as.numeric(parameters$CUBEinformation$RATESKALA)
  if(!is_rateskala) return(invisible(NULL))
  print_console_message("\n* Skalerer RATE til per", scalevalue, "\n")
  
  if("RATE" %in% names(dt)) dt[, RATE := RATE * scalevalue]
  if("MEISskala" %in% names(dt)) dt[, MEISskala := MEISskala * scalevalue]
}

# load and format filegroups ----
#' @title fetch_filegroup_from_buffer
#' @description
#' fetches filegroup already loaded into buffer. 
#' @keywords internal
#' @noRd
fetch_filegroup_from_buffer <- function(filegroup){
  if(exists("BUFFER", envir = .GlobalEnv) && filegroup %in% names(.GlobalEnv$BUFFER)){
    print_console_message("\n** Henter FIL", filegroup, "fra BUFFER")
    return(data.table::copy(.GlobalEnv$BUFFER[[filegroup]]))
  }
  stop("Filgruppe ", filegroup, " ikke funnet i BUFFER")
}

# make table from file ----
  
#' @title do_reshape_var
#' @description
#' Reshapes the data to collect columns representing the same variable into long format
#' @noRd
do_reshape_var <- function(dt, filedescription, parameters){
  save_filedump_if_requested(dumpname = "RESHAPEpre", dt = NULL, parameters = parameters, koblid = filedescription$KOBLID, duck = TRUE, tablename = "temp_orgfile")
  on.exit({save_filedump_if_requested(dumpname = "RESHAPEpost", dt = NULL, parameters = parameters, koblid = filedescription$KOBLID, duck = TRUE, tablename = "temp_orgfile")}, add = TRUE)
  if(is_empty(filedescription$RESHAPEvar)) return(invisible(NULL))
  
  cols <- get_reshape_parameters(filedescription = filedescription, allcolumns = names(dt))
  if(!is.null(cols$id) && !all(cols$id %in% names(dt))) stop("Feil i RESHAPE: Kolonner angitt i RESHAPEid ikke funnet")
  if(!is.null(cols$measure) && !all(cols$measure %in% names(dt))) stop("Feil i RESHAPE: Kolonner angitt i RESHAPEmeas ikke funnet")
  if(!is.null(cols$id) && is.null(cols$measure)) stop("Feil i RESHAPE: Både RESHAPEid og RESHAPEmeas er tomme")
  reshape <- data.table::melt(dt, id.vars = cols$id, measure.vars = cols$measure, variable.name = cols$var, value.name = cols$val)
  dt[, names(dt) := NULL]
  dt[, (names(reshape)) := reshape]
  convert_all_columns_to_character(dt = dt)
}

#' @title do_set_default_values
#' @description
#' Sets default values for columns where the default value are provided in ACCESS::INNLESING within <...>
#' @noRd
do_set_default_values <- function(dt, filedescription, defaultcolumns){
  default <- filedescription[, ..defaultcolumns]
  default[, names(.SD) := lapply(.SD, function(x) sub("^<(.*)>$", "\\1", x))]
  dt[, names(default) := default]
}

#' @title convert_all_columns_to_character
#' @description
#' Make sure all columns are of type character
#' @param dt data
#' @noRd
convert_all_columns_to_character <- function(dt){
  non_char_cols <- names(dt)[!vapply(dt, is.character, FUN.VALUE = logical(1))]
  for (j in non_char_cols) {
    data.table::set(dt, j = j, value = as.character(dt[[j]]))
  }
}

#' @noRd
do_convert_na_to_empty <- function(dt){
  dt[, names(.SD) := lapply(.SD, function(x) data.table::fifelse(is.na(x), "", x))]
}

# moving average ----

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

# Standardization ----
#' @title add_predteller
#' @description
#' Adds predicted teller for age- and gender standardization
#' 
#' PREDRATE
#' Maa JUKSE DET TIL LITT MED NEVNER 0. Bruken her er jo slik at dette er tomme celler, 
#' og ikke minst vil raten nesten garantert skulle ganges med et PREDTELLER=0
#' Tillater TELLER<=2 for aa unngaa evt numeriske problemer. Virker helt uskyldig gitt bruken
#'
#' @param TNF merged teller-nevner file
#' @param parameters cube parameters
#' @keywords internal
#' @noRd
add_predteller_old <- function(dt, parameters){
  if(parameters$CUBEinformation$REFVERDI_VP != "P") return(invisible(NULL))
  print_console_message("\n* Skal estimere PREDTELLER for standardisering")
  designlist <- find_common_standard_teller_nevner_prednevner_design(parameters = parameters)
  predrate <- estimate_predrate(design = designlist$STNdesign, parameters = parameters)
  prednevner <- estimate_prednevner(design = designlist$STNPdesign, parameters = parameters)
  predteller <- estimate_predteller(predrate = predrate, prednevner = prednevner, parameters = parameters)
  
  print_console_message("\n* Merger PREDTELLER med KUBE")
  commontabs <- intersect(get_dimension_columns(names(dt)), names(predteller))
  dt[predteller, on = commontabs, let(PREDTELLER = i.PREDTELLER, 
                                      PREDTELLER.f = i.PREDTELLER.f, 
                                      PREDTELLER.a = i.PREDTELLER.a,
                                      PREDTELLER.n = TELLER.n)]
  set_implicit_null_after_merge(dt = dt, implicitnull_defs = parameters$fileinformation[[parameters$files[["TELLER"]]]]$vals)
  print_console_message("\n\n*** FERDIG MED Å ESTIMERE PREDTELLER\n")
}

#' @title estimate_predrate
#' @description finds predrate to be used to calculate predteller
#' @keywords internal
#' @noRd
estimate_predrate <- function(design, parameters){
  print_console_message("\n* Estimerer PREDRATE...")
  missyears <- parameters$MOVAV$missyears
  merge_teller_nevner(parameters = parameters, standardfiles = TRUE, design = design)
  predrate <- fetch_duckdb_table(parameters$duck, tablename = "STANDARD_KUBE")
  if(missyears$n > 0 && any(missyears$years %in% unique(predrate$AARl))){
    problem <- intersect(missyears$years, unique(predrate$AARl))
    warning("\n--\n** OBS! Mangler tall for år som skal standardiseres mot: ", paste(problem, collapse = ", "), 
            "\n*** Dette vil påvirke landsraten i standardiseringsperioden!\n--\n", immediate. = TRUE)
    predrate <- predrate[!AARl %in% missyears$years]
  }
  predrate <- aggregate_to_periods_old(dt = predrate, parameters = parameters)
  predrate[, (parameters$PredFilter$Predfiltercolumns) := NULL]
  predrate[NEVNER != 0 & NEVNER.f == 0, let(PREDRATE = TELLER/NEVNER, PREDRATE.f = pmax(TELLER.f, NEVNER.f))]
  predrate[NEVNER == 0 & NEVNER.f == 0, let(PREDRATE = 0, PREDRATE.f = pmax(TELLER.f, 2))]
  predrate[TELLER <= 2 & TELLER.f == 0 & NEVNER == 0 & NEVNER.f == 0, let(PREDRATE = 0, PREDRATE.f = 0)]
  predrate[, let(PREDRATE.a = pmax(TELLER.a, NEVNER.a))]
  
  ukurante <- predrate[is.na(TELLER) | is.na(NEVNER)]
  if (ukurante[, .N] > 0){
    print_console_message(paste0("\n\n!!! Missing verdier i standardteller og/eller standardnevner (", ukurante[, .N], ")"))
    print_console_message("\nDette KAN gi problemer, da PREDTELLER - og dermed MEIS - ikke kan beregnes for disse strataene: \n")
    print_console_message("\nDersom det faktisk mangler tall kan det være behov for å justere startår")
    print_console_message("\nFølgende unike verdier for ulike dimensjonene er påvirket: ")
    for(dim in get_dimension_columns(names(predrate))){
      print_console_message(paste0("\n- ", dim, ": ", paste(unique(ukurante[[dim]]), collapse = ", ")))
    }
  }
  predrate <- predrate[, .SD, .SDcols = c(get_dimension_columns(names(predrate)), paste0("PREDRATE", c("", ".f", ".a")))]
  return(predrate)
}

#' @title estimate_prednevner
#' @description finds prednevner, to be used to calculate predteller
#' @keywords internal
#' @noRd
estimate_prednevner <- function(design, parameters){
  print_console_message("\n\n* Estimerer PREDNEVNER...\n")
  missyears <- parameters$MOVAV$missyears
  redesign <- find_redesign(orgdesign = parameters$filedesign[[parameters$files$PREDNEVNER]], targetdesign = design, parameters = parameters)
  prednevner <- fetch_duckdb_table(tablename = parameters$files$PREDNEVNER, con = parameters$duck)
  prednevner <- do_filter_and_recode_to_redesign(dt = prednevner, redesign = redesign, parameters = parameters)
  PredNevnerKol <- gsub("^(.*):(.*)", "\\2", parameters$TNPinformation$PREDNEVNERFIL)
  if(is_empty(PredNevnerKol)) PredNevnerKol <- parameters$TNPinformation$NEVNERKOL
  PNnames <- gsub(paste0("^", PredNevnerKol, "(\\.f|\\.a|)$"), "PREDNEVNER\\1", names(prednevner))
  data.table::setnames(prednevner, names(prednevner), PNnames)
  prednevner <- prednevner[, .SD, .SDcols = c(get_dimension_columns(names(prednevner)), grep("^PREDNEVNER", names(prednevner), value= T))]
  if(missyears$n > 0) prednevner <- prednevner[!AARl %in% missyears$years]
  prednevner <- aggregate_to_periods_old(dt = prednevner, parameters = parameters)
  return(prednevner)
}

#' @title estimate_predteller
#' @description estimates predteller, used for age standardization
#' @keywords internal
#' @noRd
estimate_predteller <- function(predrate, prednevner, parameters){
  print_console_message("\n\n* Estimerer PREDTELLER...\n")
  commondims <- intersect(get_dimension_columns(names(prednevner)), get_dimension_columns(names(predrate)))
  mismatch <- collapse::join(predrate, prednevner, how = "anti", multiple = T, on = commondims, overid = 2, verbose = 0)[, .N]
  if(mismatch > 0) print_console_message("!!!!!ADVARSEL:", mismatch, "strata i predrate finnes ikke i prednevner!!!\n")
  
  predteller <- collapse::join(prednevner, predrate, how = "l", on = commondims, multiple = T, overid = 2, verbose = 0)
  predteller[, let(PREDTELLER = PREDRATE * PREDNEVNER,
                   PREDTELLER.f = pmax(PREDRATE.f, PREDNEVNER.f),
                   PREDTELLER.a = pmax(PREDRATE.a, PREDNEVNER.a))]
  predteller <- predteller[, .SD, .SDcols = c(get_dimension_columns(names(predteller)), paste0("PREDTELLER", c("", ".f", ".a")))]
  
  filename <- parameters$files$PREDNEVNER
  prednevnerdesign <- find_filedesign(file = prednevner, filename = filename, parameters = parameters)
  cubedesign <- list(Part = parameters$CUBEdesign)
  redesign <- find_redesign(orgdesign = prednevnerdesign, targetdesign = cubedesign, aggregate = parameters$DefDesign$AggVedStand, parameters = parameters)
  predteller <- do_filter_and_recode_to_redesign(dt = predteller, redesign = redesign, parameters = parameters)
  return(predteller)
}

#' @title add_meisskala
#' @description
#' Adds scale to standardize MEIS
#' @keywords internal
#' @noRd
add_meisskala_old <- function(dt, parameters){
  if(parameters$PredFilter$ref_year_type != "Specific") return(invisible(NULL))
  print_console_message("\n* Legger til MEISskala for standardisering\n")
  
  if(parameters$CUBEinformation$REFVERDI_VP != "P"){
    data.table::set(dt, j = "MEISskala", value = NA_real_)
    return(invisible(NULL))
  }
  subset_meisskala <- dt[x, env = list(x = str2lang(parameters$PredFilter$meisskalafilter))]
  if (nrow(subset_meisskala) == 0) stop("Noe er feil i ACCESS::KUBER::REFVERDI, klarer ikke lage meisskala")
  subset_meisskala[, MEISskala := RATE]
  joincolumns <- setdiff(intersect(names(subset_meisskala), parameters$DefDesign$DesignKolsFA), parameters$PredFilter$Predfiltercolumns)
  dt[subset_meisskala, on = joincolumns, MEISskala := i.MEISskala]
}

# Compute columns----

#' @noRd
add_crude_rate_old <- function(dt, parameters){
  if(!"NEVNER" %in% names(dt)){
    print_console_message("\n** Har ikke NEVNER, kan ikke beregne crude RATE")
    return(invisible(NULL))
  } 
  
  dt[, let(RATE = TELLER/NEVNER,
           RATE.f = pmax(TELLER.f, NEVNER.f, na.rm = T),
           RATE.a = pmax(TELLER.a, NEVNER.a, na.rm = T),
           RATE.n = pmax(TELLER.n, NEVNER.n, na.rm = T))]
  
  dt[is.nan(RATE) | is.infinite(RATE), let(RATE = NA)]
  # Sett .f = 2 dersom RATE ikke lar seg beregne og RATE.f ikke allerede er satt til max av TELLER.f/NEVNER.f
  dt[is.na(RATE) & RATE.f == 0, let(TELLER.f = 2, NEVNER.f = 2, RATE.f = 2, spv_tmp = 2L)]
  
  if(parameters$MOVAV$is_movav){
    dt[, (paste0("RATE", c(".fn1", ".fn3", ".fn9"))) := 0]
  }
}


