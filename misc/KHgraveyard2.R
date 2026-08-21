# Old functions deprecated when switching to duckdb processing

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

