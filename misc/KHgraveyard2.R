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


#' @keywords internal
#' @noRd
get_etabs <- function(columnnames, parameters){
  spec <- parameters$fileinformation[[parameters$files$TELLER]]
  tabcols <- grep("^TAB\\d+$", columnnames, value = T)
  tabnames <- character(0)
  for(tab in tabcols){
    tabnames <- c(tabnames, spec[[tab]])
  }
  return(list(tabcols = tabcols, tabnames = tabnames))
}

#' @keywords internal
#' @noRd
set_etab_names <- function(dt, etablist){
  data.table::setnames(dt, old = etablist$tabcols, new = etablist$tabnames)
}

# load and format filegroups ----

#' @title load_and_format_files
#' @description
#' Loads filegroups as specified in parameters$files
#' Adds FileDesign to parameter list after loading files
#' Teller file is loaded first, and other files are filtered according to years present in this file
#' Files are always loaded fresh into BUFFER, and overwritten if they already exist. 
#' @param parameters list of parameters generated by get_cubeparameters()
#' @return updated parameter list after loading and formatting files
load_and_format_files <- function(parameters){
  print_console_message("\n* Laster og formatterer filgrupper\n")
  # if(!exists("BUFFER", envir = .GlobalEnv)) .GlobalEnv$BUFFER <- list()
  tabfilter_tellerfile <- set_filter_tab(cubeinformation = parameters$CUBEinformation)
  tellerfile <- parameters$files$TELLER
  # if(tellerfile %in% names(.GlobalEnv$BUFFER)) BUFFER[[tellerfile]] <- NULL
  if(is_duckdb_table(con = parameters$duck, tablename = tellerfile)) DBI::dbRemoveTable(parameters$duck, tellerfile)
  load_filegroup_to_buffer(filegroup = tellerfile, filter = tabfilter_tellerfile, parameters = parameters)
  
  nonteller_files <- unique(grep(paste0("^", tellerfile, "$"), parameters$files, invert = T, value = T))
  for (file in nonteller_files) {
    # if(file %in% names(.GlobalEnv$BUFFER)) BUFFER[[file]] <- NULL
    if(is_duckdb_table(con = parameters$duck, tablename = file)) DBI::dbRemoveTable(parameters$duck, file)
    load_filegroup_to_buffer(filegroup = file, filter = NULL, parameters = parameters)
  }
  do_clean_duckdb(con = parameters$duck)
  print_console_message("\n* Alle filer lest og formattert")
}



#' @title load_filegroup_to_buffer
#' @description
#' Loads the file into BUFFER in .GlobalEnv
#' The file is filtered using TABfilter and geoharmonized. 
#' If filefilter is defined, the file is formatted accordingly
#' @noRd
load_filegroup_to_buffer <- function(filegroup, filter = NULL, parameters, duck = TRUE){
  filefilter <- parameters$FILFILTRE[tolower(FILVERSJON) == tolower(filegroup)]
  isfilefilter <- nrow(filefilter) > 0
  alderfilter <- set_filter_age(parameters = parameters)
  yearfilter <- set_filter_year(parameters = parameters)
  filter <- paste0(c(filter, alderfilter, yearfilter), collapse = " & ")
  isfilter <- is_not_empty(filter)
  fileinfo <- parameters$fileinformation[[filegroup]]
  orgfile <- ifelse(isfilefilter, filefilter$ORGFIL, filegroup)
  
  if(grepl("^BEF_GKny$", orgfile, ignore.case = TRUE)){
    FIL <- read_population_file(alderfilter = alderfilter, yearfilter = yearfilter, parameters = parameters)
  } else {
    FIL <- read_filegroup(filegroup = orgfile)
  }
  
  if(isfilter) FIL <- do_filter_columns(file = FIL, filter = filter)
  
  FIL <- do_filter_KUIL(dt = FIL, parameters = parameters)
  
  if(!isfilefilter){
    FIL <- do_harmonize_geo(file = FIL, vals = fileinfo$vals, rectangularize = FALSE, parameters = parameters)
  } else {
    iskollapsdel <- grepl("\\S", filefilter$KOLLAPSdeler)
    if(iskollapsdel) FIL <- do_filfiltre_kollapsdeler(file = FIL, parts = filefilter$KOLLAPSdeler, parameters = parameters)
    
    isnyekolkolprerad <- grepl("\\S", filefilter$NYEKOL_KOL_preRAD)
    if(isnyekolkolprerad) compute_new_value_from_formula(dt = FIL, formulas = filefilter$NYEKOL_KOL_preRAD, post_moving_average = FALSE)
    
    Filter <- set_recode_filter_filfiltre(fileinfo = fileinfo, filefilter = filefilter, parameters = parameters)
    if (length(Filter) > 0){
      prefilterdesign <- find_filedesign(FIL, parameters = parameters)
      redesign_filter <- find_redesign(orgdesign = prefilterdesign, targetdesign = list(Parts = Filter), parameters = parameters)
      FIL <- do_filter_and_recode_to_redesign(dt = FIL, redesign = redesign_filter, parameters = parameters)
    }
    
    isgeoharm <- filefilter$GEOHARM == 1
    if(isgeoharm){
      rectangularize <- ifelse(filefilter$REKTISER == 1, TRUE, FALSE)
      FIL <- do_harmonize_geo(file = FIL, vals = fileinfo$vals, rectangularize = rectangularize, parameters = parameters)
    }
    
    isnyekolrad <- grepl("\\S", filefilter$NYEKOL_RAD)
    if(isnyekolrad) compute_new_value_from_row_sum(dt = FIL, formulas = filefilter$NYEKOL_RAD, fileinfo = fileinfo, parameters = parameters)
    
    # isnykolsmerge <- grepl("\\S", filefilter$NYKOLSmerge)
    # if(isnykolsmerge){
    #   newcols <- eval(str2lang(filefilter$NYKOLSmerge))
    #   do_filfiltre_nykolsmerge(file = FIL, newcols = newcols)
    # }
    
    isffrsynt <- grepl("\\S", filefilter$FF_RSYNT1)
    if(isffrsynt) FIL <- do_special_handling(name = "FF_RSYNT1", dt = FIL, dt_name = "FIL", code = filefilter$FF_RSYNT1, parameters = parameters)
  }
  
  if(filegroup == "BEFVEKST") add_leadyear_befvekst(dt = FIL)
  dimorder <- intersect(getOption("khfunctions.standarddimensions_full"), names(FIL))
  data.table::setcolorder(FIL, dimorder)
  
  if(duck){
    print_console_message("\n*** Skriver til duckdb...\n")
    write_duckdb_table(parameters$duck, filegroup, FIL, temp = FALSE, overwrite = TRUE)
  } else {
    print_console_message("\n*** Skriver til lokalt minne...")
    .GlobalEnv$BUFFER[[filegroup]] <- FIL
  }
  invisible(gc())
}

#' @title set_filter_tab
#' @description
#' Creates a filtering string based on the TABX and TABX_0 columns in table KUBER in ACCESS
#' If TABX_0 is provided, this is used for initial filtering of the filegroup, as these categories are needed during the data processing.
#' TABX is always used for final filtering of the cube.
#' @param cubeinformation information from table KUBER in ACCESS
#' @noRd
set_filter_tab <- function(cubeinformation){
  TabConds <- character()
  for (tab in names(cubeinformation)[grepl("^TAB\\d+$", names(cubeinformation))]){
    istab <- !is.na(cubeinformation[[tab]]) && cubeinformation[[tab]] != ""
    if (istab) {
      tab0 <- paste0(tab, "_0")
      istab0 <- !is.null(cubeinformation[[tab0]]) && !is.na(cubeinformation[[tab0]]) && cubeinformation[[tab0]] != ""
      tablist <- cubeinformation[[tab]]
      if(istab0) tablist <- cubeinformation[[tab0]]
      isminus <- grepl("^-\\[", tablist)
      tablist <- gsub("^-\\[(.*)\\]$", "\\1", tablist)
      tablist <- paste0("\"", gsub(",", "\",\"", tablist), "\"")
      tabcond <- paste0("(", tab, " %in% c(", tablist, "))")
      if (isminus) tabcond <- paste0("!", tabcond)
      TabConds <- c(TabConds, tabcond)
    }
  } 
  tabfilter <- paste0(TabConds, collapse = " & ")
  if(tabfilter == "") tabfilter <- NULL
  return(tabfilter)
}

#' @title set_filter_year
#' @description
#' Sets age filter according to age groups set in ACCESS::KUBER, 
#' @noRd
set_filter_age <- function(parameters){
  isalder <- is_not_empty(parameters$CUBEinformation$ALDER)
  con <- parameters$duck
  if(!isalder){
    tellerfile <- parameters$files[["TELLER"]]
    isduck <- is_duckdb_table(con = con, tablename = tellerfile)
    if(!isduck) return(NULL)
    if(isduck && all(c("ALDERl", "ALDERh") %in% get_duckdb_cols(con, tellerfile))){
      a <- DBI::dbGetQuery(con, paste0("SELECT MIN(ALDERl) as min, MAX(ALDERh) as max FROM ", tellerfile))
      amin <- a$min
      amax <- a$max
    } else if(isbuffer && all(c("ALDERl", "ALDERh") %in% names(.GlobalEnv$BUFFER[[tellerfile]]))){
      amin <- collapse::fmin(.GlobalEnv$BUFFER[[tellerfile]][["ALDERl"]])
      amax <- collapse::fmax(.GlobalEnv$BUFFER[[tellerfile]][["ALDERh"]])
    } else {
      return(NULL)
    }
  } else {
    accessalder <- unlist(strsplit(parameters$CUBEinformation$ALDER, ","))
    if(any(grepl("[^[:digit:]_]", accessalder))) return(NULL)
    
    aldersplit <- data.table::tstrsplit(accessalder, "_")
    amin <- ifelse(sum(is.na(aldersplit[[1]])) == 0,
                   min(as.numeric(aldersplit[[1]])),
                   getOption("khfunctions.amin"))
    amax <- ifelse(length(aldersplit) > 1 && sum(is.na(aldersplit[[2]])) == 0, 
                   max(as.numeric(aldersplit[[2]])),
                   getOption("khfunctions.amax"))
  }
  
  if(is.na(amin) | is.na(amax)) stop("Feil i aldersfiltreringen, som leser fra ACCESS::KUBER::ALDER. Denne må være tom, 'ALLE', eller angi aldersgrupper separert med komma (X_Y, X_Y, X_Y).")
  if(amin == getOption("khfunctions.amin") && amax == getOption("khfunctions.amax")) return(NULL)
  return(paste0("ALDERl >= ", amin, " & ALDERh <= ", amax))
}

#' @title set_filter_year
#' @description Filters filegroups to remove years prior to AAR_START in ACCESS::KUBER
#' If tellerfile is already loaded, use the maximum of AAR_START and min teller aar as filter
#' @keywords internal
#' @noRd
set_filter_year <- function(parameters){
  con <- parameters$duck
  tellerfile <- parameters$files[["TELLER"]]
  aarstart <- min_teller_aar <- parameters$CUBEinformation$AAR_START
  isbuffer <- tellerfile %in% names(.GlobalEnv$BUFFER)
  isduck <- is_duckdb_table(con = con, tablename = tellerfile)
  if(isduck && "AARl" %in% get_duckdb_cols(con, tellerfile)){
    min_teller_aar <- DBI::dbGetQuery(con, paste0("SELECT MIN(AARl) FROM ", tellerfile))[[1]]
  } else if(isbuffer && "AARl" %in% names(.GlobalEnv$BUFFER[[tellerfile]])){
    min_teller_aar <- collapse::fmin(.GlobalEnv$BUFFER[[tellerfile]][["AARl"]])
  }
  if(aarstart == 0) return(invisible(NULL))
  return(paste0("AARl >= ", pmax(aarstart, min_teller_aar)))
}

#' @title do_filter_KUIL
#' @description 
#' Filters out only needed categories of KJONN, UTDANN, INNVKAT, LANDBAK
#' If only total is needed, and total already exist, only keep this value.
#' Reduces the size of input files before further processing. 
#' @keywords internal
#' @noRd
do_filter_KUIL <- function(dt, parameters){
  for(dim in c("KJONN", "UTDANN", "INNVKAT", "LANDBAK")){
    keep <- trimws(strsplit(parameters$CUBEinformation[[dim]], split = ",")[[1]])
    exist <- collapse::funique(dt[[dim]])
    if(all(keep %in% exist) && any(exist %notin% keep)){
      dt <- dt[dt[[dim]] %in% keep]
    }
  }
  return(dt)
}

#' @keywords internal
#' @noRd
read_population_file <- function(alderfilter = NULL, yearfilter = NULL, parameters){
  root <- file.path(getOption("khfunctions.root"), getOption("khfunctions.fgdir"), getOption("khfunctions.fg.ny"))
  isalderfilter <- is_not_empty(alderfilter)
  isyearfilter <- is_not_empty(yearfilter)
  islks <- "V" %in% unlist(strsplit(parameters$CUBEinformation$GEOniv, ","))
  befcols <- grep("^BEF|^mBEF", unlist(parameters$TNPinformation[c("TELLERKOL", "NEVNERKOL")], use.names = F), value = T)
  befcols <- paste0(rep(befcols, each = 3), c("", ".f", ".a"))
  
  use_dataset <- isalderfilter || isyearfilter || !islks
  if(!use_dataset){
    print_console_message("\n** Leser inn full befolkningsfil (ingen filter angitt for ALDER, AAR eller GEOniv).....")
    file <- arrow::open_dataset(file.path(root, "BEF_GKny.parquet"))
    readcols <- c(grep(paste0("^", getOption("khfunctions.standarddimensions"), collapse = "|"), names(file), value = T), befcols)
    dt <- file |> 
      dplyr::select(dplyr::all_of(readcols)) |> 
      dplyr::collect() |> 
      data.table::setDT()
    return(dt)
  }
  
  folder <- ifelse(isalderfilter, getOption("khfunctions.pop_alderaargeo"), getOption("khfunctions.pop_aargeo"))
  
  if(isalderfilter){
    available_age_groups <- gsub("alder=", "", list.dirs(file.path(root, folder), full.names = F, recursive = F))
    alderfilter <- translate_age_filter(alderfilter = alderfilter, all = available_age_groups)
  }
  
  file <- arrow::open_dataset(file.path(root, folder))
  
  geofilter <- ifelse(islks, "lks %in% c(0, 1)", "lks == 0")
  completefilter <- paste(c(alderfilter, yearfilter, geofilter), collapse = " & ")
  readcols <- c(grep(paste0("^", getOption("khfunctions.standarddimensions"), collapse = "|"), names(file), value = T), befcols)
  print_console_message(paste0("\n** Leser inn befolkningsfil med filter: ", completefilter, "....."))
  dt <- file |> 
    dplyr::filter(!!rlang::parse_expr(completefilter)) |> 
    dplyr::select(dplyr::all_of(readcols)) |> 
    dplyr::collect() |> 
    data.table::setDT()
  print_console_message("Ferdig")
  return(dt)
}

#' @keywords internal
#' @noRd
translate_age_filter <- function(alderfilter, all){
  aldermin <- as.integer(sub("ALDERl >= (\\d+) & .*", "\\1", alderfilter))
  aldermax <- as.integer(sub(".* ALDERh <= (\\d+)", "\\1", alderfilter))
  tab <- data.table::data.table(all = all)
  tab[, c("l", "h") := data.table::tstrsplit(all, "_")][, included := 1L]
  tab[as.numeric(h) < aldermin | as.numeric(l) > aldermax, included := 0L]
  needed <- tab[included == 1, all]
  filterstr <- paste0("alder %in% c(", paste0(shQuote(needed), collapse = ", "), ")")
  return(filterstr)
}

#' @title read_filegroup
#' @description Reads original filegroup. Will always read from "PARQUET" folder first. 
#' If filegroup does not exist it will look in "NYESTE" to read the .rds file, and throw an error if not found.
#' Can be used alone to read filegroups from PARQUET/NYESTE by name. 
#' @returns data.table
#' @export
read_filegroup <- function(filegroup){
  file <- file.path(getOption("khfunctions.root"), getOption("khfunctions.fgdir"), getOption("khfunctions.fg.ny"), paste0(filegroup, ".parquet"))
  if(file.exists(file)){
    print_console_message(paste0("\n** Leser inn fil: ", file, "....."))
    dt <- arrow::open_dataset(file)
    readcols <- grep("^KOBLID$|^ROW$", names(dt), value = TRUE, invert = TRUE)
    dt <- dt |> 
      dplyr::select(all_of(readcols)) |> 
      dplyr::collect() |> 
      data.table::setDT()
  } else {
    file <- file.path(getOption("khfunctions.root"), getOption("khfunctions.fgdir"), getOption("khfunctions.fg.ny"), paste0(filegroup, ".rds"))
    if(!file.exists(file)) stop("Finner ikke filgruppe: ", filegroup, " i STABLAORG/R/NYESTE! Filgruppen må kjøres først.")
    print_console_message("\n** Leser inn fil: ", file, " ..... ")
    dt <- collapse::qDT(readRDS(file))
    delcols <- names(dt)[names(dt) %in% c("KOBLID", "ROW")]
    dt[, (delcols) := NULL]
  }
  set_integer_columns(dt = dt)
  return(dt)
}

#' @title do_filfiltre_kollapsdeler
#' @description 
#' collapses file according to the KOLLAPSdeler column in table FILFILTRE
#' The selected columns are set to the total value, and all non-dimension values are aggregated usin fsum
#' NB! If the column already contains the total value, this will generate an error as the total will be included in the sum. 
#' @noRd
do_filfiltre_kollapsdeler <- function(file, parts, parameters){
  tabcols <- get_dimension_columns(names(file))
  parts <- unlist(strsplit(parts, ","))
  totals <- parameters$TotalKoder[parts]
  columns <- as.character(parameters$DefDesign$DelKolsF[parts])
  for(i in seq_along(parts)){
    if(totals[[i]] %in% collapse::funique(file[[columns[i]]])){
      stop(sprintf("FILFILTRE: Kan ikke kollapse '%s' – inneholder allerede totalverdi '%s'", columns[i], totals[[i]]))
    }
  }
  file[, (columns) := totals]
  print_console_message(paste0("\n*** Kollapser kolonnene: ", paste0(columns, collapse = ", ")))
  file <- file[, collapse::fsum(collapse::gby(.SD, tabcols))]
  return(file)
}  

#' @keywords internal
#' @noRd
set_integer_columns <- function(dt){
  integers <- names(dt)[grep("^(AAR(l|h)|ALDER(l|h)|KJONN|UTDANN|LANDBAK|INNVKAT)$", names(dt))]
  non_int_cols <- names(dt)[!sapply(dt, is.integer)]
  to_integer <- intersect(non_int_cols, integers)
  dt[, names(.SD) := lapply(.SD, as.integer), .SDcols = to_integer]
}

#' @title add_leadyear_befvekst
#' @description
#' Adds lead years for filegroup BEFVEKST
#' @keywords internal
#' @noRd
add_leadyear_befvekst <- function(dt){
  d <- data.table::copy(dt)
  data.table::set(d, j = c("AARl", "AARh"), 
                  value = list(d[["AARl"]] - 1, d[["AARh"]] - 1))
  tabcols <- get_dimension_columns(names(d))
  if(!d[, max(.N), by = tabcols][, max(V1)] == 1){
    stop("")
  }
  prefix <- "Yp1_A_"
  oldval_names <- paste0("BEF0101", c("", ".f", ".a"))
  newval_names <- paste0(prefix, oldval_names)
  data.table::setnames(d, old = oldval_names, new = newval_names)
  
  newval_vals <- d[dt, on = tabcols, ..newval_names]
  data.table::set(dt, j = newval_names, value = newval_vals)
}

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


# Geohandling

fix_geo_special_old <- function(dt, parameters){
  geonivs <- unique(dt[["GEOniv"]])
  specs <- parameters$fileinformation[[parameters$files$TELLER]] 
  vals <- get_value_columns(names(dt))
  flags <- intersect(c("spv_tmp", grep("\\.f$", names(dt), value = T)), names(dt))
  bydelstart <- specs[["B_STARTAAR"]]
  dk2020 <- as.character(c(5055, 5056, 5059, 1806, 1875))
  dk2020start <- specs[["DK2020_STARTAAR"]]
  isbydelstart <- !is.na(bydelstart) && bydelstart > 0 & any(geonivs %in% c("B", "V"))
  isdk2020 <- !is.na(dk2020start) && dk2020start > 0 & "K" %in% geonivs
  
  # if(!isbydelstart && !isdk2020) return(invisible(NULL))
  
  if (isbydelstart) {
    print_console_message("\n* Håndterer bydelsstartår (bydeler og levekårssoner)\n")
    print_console_message(" - Sletter tall for år før ", bydelstart, " dersom de finnes\n", sep = "")
    idx <- which(dt[["GEOniv"]] %in% c("B", "V") & dt[["AARl"]] < bydelstart)
    data.table::set(dt, i = idx, j = flags, value = 9L)
    data.table::set(dt, i = idx, j = "geoprikket", value = 1L)
  }
  
  # Fjerner tall før startår for LKS, som definert i tabell ACCESS::LKS_STARTAAR
  # Dette gjøres uansett om bydelstart er satt i access eller ikke, dersom kuben har levekårssonedata. 
  if("V" %in% unique(dt[["GEOniv"]])){
    print_console_message("\n* Håndterer startår for levekårssoner\n")
    dt[parameters$LKS_STARTAAR, lks_startaar := i.lks_startaar, on = "GEO"]
    idx <- which(dt[["AARl"]] < dt[["lks_startaar"]])
    data.table::set(dt, i = idx, j = flags, value = 9L)
    data.table::set(dt, i = idx, j = "geoprikket", value = 1L)
    data.table::set(dt, j = "lks_startaar", value = NULL)
  }
  
  if (isdk2020) {
    print_console_message("\n* Håndterer delingskommuner 2020 (DK2020) \n")
    print_console_message(" - Sletter kommunetall for delingskommuner for år før ", dk2020start, "\n", sep = "")
    idx <- which(dt[["GEOniv"]] == "K" & dt[["GEO"]] %chin% dk2020 & dt[["AARl"]] < dk2020start)
    data.table::set(dt, i = idx, j = flags, value = 9L)
    data.table::set(dt, i = idx, j = "geoprikket", value = 1L)
    
    # Add fix for AAlesund/Haram split, which should not get data in 2020-2023, except for VALGDELTAKELSE
    print_console_message(" - Håndterer Ålesund/Haram for årene 2020-2023\n")
    ystart <- ifelse(parameters$name == "VALGDELTAKELSE", 2019, 2020)
    ystop <- ystart + 3
    idx <- which(dt[["GEO"]] %in% c("1508", "1580") & (dt[["AARl"]] <= ystop & dt[["AARh"]] >= ystart))
    data.table::set(dt, i = idx, j = flags, value = 9L)
    data.table::set(dt, i = idx, j = "geoprikket", value = 1L)
  }
  
  # idx <- which(dt[["spv_tmp"]] == 9)
  # data.table::set(dt, i = idx, j = "geoprikket", value = 1L)
  return(invisible(NULL))
}

#' @title add_missing_lks
#' @noRd
add_missing_lks_old <- function(dt, parameters){
  # Legg til soner for kommuner med bare en sone
  single <- data.table::data.table(lks = parameters$GeoKoder[GEOniv == "V", unique(GEO)])
  single[, overniv := sub("00$", "", substr(lks, 1, 6))]
  single[, N := .N, by = overniv]
  single <- single[N == 1]
  if(nrow(single) > 0){
    add_single <- dt[GEO %in% single$overniv]
    data.table::set(add_single, j = "GEOniv", value = "V")
    add_single[single, on = setNames("overniv", "GEO"), GEO := lks]
  } else {
    add_single <- dt[0]
  }
  
  # Legg til ugyldige soner, som skal eksistere men være prikket
  invalid <- data.table::data.table(lks = parameters$GeoKoder[GEOniv == "V" & TYP == "U" & !GEO %in% unique(add_single[["GEO"]]), unique(GEO)])
  if(nrow(invalid) > 0){
    invalid[, overniv := sub("00$", "", substr(lks, 1, 6))]
    add_invalid <- dt[GEO %in% invalid$overniv]
    data.table::set(add_invalid, j = "GEOniv", value = "V")
    add_invalid[invalid, on = setNames("overniv", "GEO"), GEO := lks]
    add_invalid[, let(spv_tmp = 2, geoprikket = 1)]
    vals <- union(get_value_columns(names(dt)), c("sumTELLER", "sumNEVNER", "MEIS", "RATE", "SMR"))
    data.table::set(add_invalid, j = vals, value = NA)
  } else {
    add_invalid <- dt[0]
  }
  
  dt <- data.table::rbindlist(list(dt, add_single, add_invalid))
  return(dt)
}

# friskvik ----
#' @title generate_friskvik_indicator
#' @param dt ALLVIS cube
#' @param parameters parameters
#' @param id friskvik ID
generate_friskvik_indicator <- function(dt, id, parameters) {
  FVdscr <- parameters$friskvik[ID == id]
  if(!FVdscr$MODUS %in% c("K", "F", "B")){
    print_console_message("\n***ADVARSEL!!!!!!!! modus ", FVdscr$MODUS, "kan ikke brukes i FRISKVIK ikke\nFriskvikfil for ID =", id, "kan ikke genereres")
    return(invisible(NULL))
  }
  
  switch(FVdscr$MODUS, 
         "K" = {
           FriskVDir <- ifelse(FVdscr$PROFILTYPE == "FHP", getOption("khfunctions.fhpK"), getOption("khfunctions.ovpK"))
           GEOfilter <- c("K", "F", "L")
         },
         "B" = {
           FriskVDir <- ifelse(FVdscr$PROFILTYPE == "FHP", getOption("khfunctions.fhpB"), getOption("khfunctions.ovpB"))
           GEOfilter <- c("B", "K", "F", "L")
         },
         "F" = {
           FriskVDir <- ifelse(FVdscr$PROFILTYPE == "FHP", getOption("khfunctions.fhpF"), getOption("khfunctions.ovpF"))
           GEOfilter <- c("F", "L")
         })
  
  d <- dt[GEOniv %in% GEOfilter]
  d <- do_filter_friskvik_age(dt = d, age_filter = FVdscr$ALDER, parameters = parameters)
  d <- do_filter_friskvik_tabs(dt = d, dscr = FVdscr)
  exprows <- parameters$GeoKoder[TYP == "O" & GEOniv %in% GEOfilter & FRA <= parameters$year & TIL > parameters$year, .N]
  if(nrow(d) != exprows && !grepl("UNGDATA", parameters$name)){
    warning("\nFEIL I FRISKVIKFILTER som gir ", nrow(d), " / ", exprows, " forventede rader, er dette forventet?")
  }
  
  d[, (setdiff(getOption("khfunctions.profiltabs"), names(d))) := NA_character_]
  missing <- setdiff(c(getOption("khfunctions.profiltabs"), getOption("khfunctions.profilvals")), names(d))
  if (length(missing) > 0) {
    print_console_message("\n!!OBS, Kolonnene", missing, "mangler i friskvikfilen, settes til NA!")
    d[, (missing) := NA]
  }
  
  if(is_not_empty(FVdscr$ALTERNATIV_MALTALL)){
    d[, MALTALL := d[[FVdscr$ALTERNATIV_MALTALL]]]
    d[, setdiff(getOption("khfunctions.profilvals"), "MALTALL") := NA]
  }
  
  d[SPVFLAGG > 0, getOption("khfunctions.profilvals") := NA]
  d <- d[, .SD, .SDcols = c(getOption("khfunctions.profiltabs"), getOption("khfunctions.profilvals"))]
  
  setPath <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kubedir"), FriskVDir, parameters$year, "csv")
  
  if (!fs::dir_exists(setPath)) fs::dir_create(setPath)
  
  utfiln <- file.path(setPath, paste0(FVdscr$INDIKATOR, "_", parameters$batchdate, ".csv"))
  msgpath <- paste0(FriskVDir, "/", getOption("khfunctions.year"), "/csv/", basename(utfiln))
  if(nrow(d) > 0){
    print_console_message("\n-->> SKRIVER", msgpath)
    data.table::fwrite(d, utfiln, sep = ";", row.names = FALSE)
  } else {
    print_console_message("\n!!-->> INGEN RADER I FRISKVIKFIL, IKKE GENERERT:", msgpath)
  }
} 

#' @keywords internal
#' @noRd
do_filter_friskvik_age <- function(dt, age_filter, parameters){
  if(is_empty(age_filter) || age_filter == "-") return(dt)
  amin <- parameters$fileinformation[[parameters$files$TELLER]]$amin
  amax <- parameters$fileinformation[[parameters$files$TELLER]]$amax
  age_filter <- gsub("^(\\d+)$", "\\1_\\1", age_filter)
  age_filter <- gsub("^(\\d+)_$", paste0("\\1_", amax), age_filter)
  age_filter <- gsub("^_(\\d+)$", paste0(amin, "_\\1"), age_filter)
  age_filter <- gsub("^ALLE$", paste0(amin, "_", amax), age_filter)
  if(!grepl("^\\d+_\\d+$", age_filter)) stop("FRISKVIK::ALDER har feil format, må være X_X, X_, _X eller ALLE")
  return(dt[ALDER == age_filter])
}

#' @keywords internal
#' @noRd
do_filter_friskvik_tabs <- function(dt, dscr){
  for (tab in c("AARh", "KJONN", "INNVKAT", "UTDANN", "LANDBAK")) {
    if(is_not_empty(dscr[[tab]]) && dscr[[tab]] != "-"){
      dt <- dt[dt[[tab]] == dscr[[tab]]]
    }
  }
  
  if(is_not_empty(dscr$EKSTRA_TAB) && dscr$EKSTRA_TAB != "-"){
    dt <- dt[eval(rlang::parse_expr(dscr$EKSTRA_TAB))]
    dt[, ETAB := dscr$EKSTRA_TAB]
  }
  return(dt)
}

#' @title generate_specific_friskvik_indicators
#' @param cubename name of cube
#' @param friskvik_id optional, specify the ID of the indicators you want to generate
#' @param year year, defaults to getOption("khfunctions.year")
#' @export
#' @examples
#' # implement_specific_friskvik_indicators(cubename = "UTDN", friskvik_id = c(1719, 1771), year = 2025) # Specific indicators and year
#' # implement_specific_friskvik_indicators(cubename = "UTDN") # All indicators, default production year
generate_specific_friskvik_indicators_old <- function(cubename = NULL, friskvik_id = NULL, year = getOption("khfunctions.year")){
  on.exit(RODBC::odbcCloseAll())
  if(is.null(cubename)) stop("cubename must be provided, cannot be NULL")
  overwritewarning <- "\n** Files are overwritten if they already exist.\n\n*** NB! Only csv folder is affected, not preexisting godkjent folders!!"
  
  user_args <- as.list(environment())
  user_args[["name"]] <- cubename
  invisible(parameters <- get_cubeparameters(user_args = user_args))
  
  valid_cube <- read_kubestatus(parameters$dbh, year)[KUBE_NAVN == cubename]
  if(nrow(valid_cube) == 0){
    print_console_message("\n*Ingen godkjent kube funnet i KUBESTATUS, kan ikke lage friskvikfiler")
    return(invisible(NULL))
  }
  if(nrow(valid_cube) > 1){
    print_console_message("\n*>1 godkjent kube funnet i KUBESTATUS som matchet cubename, kan ikke lage friskvikfiler")
    return(invisible(NULL))
  }
  
  parameters[["batchdate"]] <- valid_cube$DATOTAG_KUBE
  indicators <- parameters$friskvik[, .SD, .SDcols = c("INDIKATOR", "ID")]
  if(is.null(friskvik_id)){
    print_console_message("\n* Generating all friskvik files as 'friskvik_id = NULL', ID(s):", paste(indicators$ID, collapse = ", "), overwritewarning)
  } else {
    if(any(!friskvik_id %in% indicators$ID)) stop("At least 1 of the requested friskvik_id(s) (", paste(friskvik_id, collapse = ", "), ") does not exist!")
    indicators <- indicators[ID %in% friskvik_id]
    print_console_message("\n* Generating requested friskvik ID(s):", paste(indicators$ID, collapse = ", "), overwritewarning)
  }
  
  cube_name <- paste0(valid_cube$KUBE_NAVN, "_", valid_cube$DATOTAG_KUBE, ".parquet")
  path <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kubedir"), getOption("khfunctions.kube.dat"), "R", cube_name)
  if(!file.exists(path)) stop("Can't find cube file ", path, "\n Check if date tag in KUBESTATUS is correct!!")
  cube <- data.table::setDT(arrow::read_parquet(path))
  do_remove_censored_observations(dt = cube, outvalues = get_outvalues_allvis(parameters = parameters))
  
  for(i in 1:nrow(indicators)){
    generate_friskvik_indicator(dt = cube, id = indicators[i, ID], parameters = parameters)
  }
} 

# write_output ----
#' @title LagQCKube (vl)
#' @description
#' Adds uncensored columns sumTELLER/sumNEVNER/RATE.n to the ALLVISkube
#' @param allvis Censored ALLVIs kube
#' @param uprikk Uncensored KUBE
#' @param allvistabs Dimensions included in ALLVIS kube
#' @param allvisvals All columns 
#' @keywords internal
#' @noRd
LagQCKube <- function(data, allvistabs){
  qcvals <- intersect(getOption("khfunctions.qcvals"), names(data[["KUBE"]]))
  prikkvals <- intersect(getOption("khfunctions.prikkeinfo"), names(data[["KUBE"]]))
  uprikk <- data.table::copy(data[["KUBE"]])[, .SD, .SDcols = c(allvistabs, qcvals, prikkvals)]
  data.table::setnames(uprikk, qcvals, paste0(qcvals, "_uprikk"))
  
  QC <- collapse::join(data[["ALLVIS"]], uprikk, on = allvistabs, overid = 2, verbose = 0)
  return(QC)
}


#' @title convert_dt_to_arrow_table
#' @description
#' Removes attributes except column names before converting to an arrow table (for writing)
#' @param dt data.table
#' @noRd
convert_dt_to_arrow_table <- function(dt){
  attremove <- grep("^(class|names)$", names(attributes(dt)), value = T, invert = T)
  for(att in attremove) data.table::setattr(dt, att, NULL)
  table <- arrow::as_arrow_table(dt)
  return(table)
}

#' @title do_write_parquet
#' @description
#' Wrapper around arrow::write_parquet, which first applies convert_dt_to_arrow_table() to generate a arrow table for writing.
#' @param dt data.table
#' @param filepath filepath to save file
#' @keywords internal
#' @noRd
do_write_parquet <- function(dt, filepath){
  table <- convert_dt_to_arrow_table(dt)
  arrow::write_parquet(table, sink = filepath, compression = "snappy")
}

#' @title write_cube_output
#' @description Writes KUBE, ALLVIS, and QC files from lagKUBE
#' @param outputlist list of output to write
#' @param parameters global parameters
#' @keywords internal
#' @noRd
write_cube_output_old <- function(outputlist, parameters){
  if(!parameters$write) return(invisible(NULL))
  basepath <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kubedir"))
  name <- ifelse(!parameters$geonaboprikk, paste0("ikkegeoprikket_", parameters$name), parameters$name)
  datert_parquet_full <- file.path(basepath, getOption("khfunctions.kube.dat"), "R", paste0(name, "_", parameters$batchdate, ".parquet"))
  datert_csv <- file.path(basepath, getOption("khfunctions.kube.dat"), "csv", paste0(name, "_", parameters$batchdate, ".csv"))
  datert_parquet <- file.path(basepath, getOption("khfunctions.kube.dat"), "parquet", paste0(name, "_", parameters$batchdate, ".parquet"))
  qc_parquet <- file.path(basepath, getOption("khfunctions.kube.qc"), paste0("QC_", name, "_", parameters$batchdate, ".parquet"))
  qc_csv <- file.path(basepath, getOption("khfunctions.kube.qc"), paste0("QC_", name, "_", parameters$batchdate, ".csv"))
  
  print_console_message("\nSAVING OUTPUT FILES:\n")
  # Full cube .parquet format
  do_write_parquet(outputlist$KUBE, filepath = datert_parquet_full)
  print_console_message("\n", datert_parquet_full)
  # Main output file for stat bank (csv and parquet)
  data.table::fwrite(outputlist$ALLVIS, file = datert_csv, sep = ";")
  do_write_parquet_allvis(dt = outputlist$ALLVIS, filepath = datert_parquet, parameters = parameters)
  print_console_message("\n", datert_csv, "\n", datert_parquet)
  
  # QC files (parquet and csv)
  data.table::fwrite(outputlist$QC, file = qc_csv, sep = ";")
  do_write_parquet(dt = outputlist$QC, filepath = qc_parquet) # QC .parquet format
}

#' @title do_write_parquet_allvis
#' @description Saves allvis file with correct column types for statbank
#' @param dt data.table
#' @param filepath filepath
#' @keywords internal
#' @noRd
do_write_parquet_allvis <- function(dt, filepath, parameters){
  table <- convert_dt_to_arrow_table(dt)
  schema <- generate_allvis_schema(table = table, parameters = parameters)
  arrow::write_parquet(table$cast(schema), sink = filepath, compression = "snappy")
}


generate_allvis_schema <- function(table, parameters) {
  tmp_sch <- table$schema
  
  fields <- lapply(names(table), function(name){
    if (name %in% parameters$outdimensions) {
      arrow::field(name, arrow::utf8())
    } else if (name %in% parameters$outvalues) {
      arrow::field(name, arrow::float64())
    } else {
      f <- tmp_sch$GetFieldByName(name)
      arrow::field(name, f$type)
    }
  }
  ) 
  do.call(arrow::schema, fields)
}

