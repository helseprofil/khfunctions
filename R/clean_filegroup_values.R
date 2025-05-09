clean_filegroup_values <- function(dt, parameters, cleanlog){
  cat("\n* Starter rensing av verdikolonner...")
  vals <- names(dt)[names(dt) %in% c("VAL1", "VAL2", "VAL3")]
  dt[, (paste0(vals, ".a")) := 1]
  
  for(val in vals){
    cat("\n** ", val, sep = "")
    do_set_val_flag(dt = dt, val = val)
    do_scale_val(dt = dt, val = val, parameters = parameters)
    check_if_value_ok(dt = dt, val = val, cleanlog = cleanlog)
  }
  
  cat("\n* Verdikolonner ferdig renset")
  return(dt)
}

#' @title do_set_val_flag
#' @description Set flags, set flagged values to NA, and converts the value column to numeric
#' @noRd
do_set_val_flag <- function(dt, val){
  cat("\n*** Setter flagg for ", val, sep = "")
  valF <- paste0(val, ".f")
  dt[, (valF) := 0]
  dt[get(val) == "..", (valF) := 1]
  dt[get(val) == ".", (valF) := 2]
  dt[get(val) == ":", (valF) := 3]
  dt[is.na(suppressWarnings(as.numeric(get(val)))) & get(valF) == 0, (valF) := 8]
  dt[get(valF) > 0, (val) := NA_character_]
  dt[, (val) := as.numeric(get(val))]
}

#' @title do_scale_val
#' @description
#' Scales value-columns based on information in "SKALA_VALX"-columns in ACCESS
do_scale_val <- function(dt, val, parameters){
  scalecol <- paste0("SKALA_", val)
  scales <- parameters$read_parameters[, .SD, .SDcols = c("KOBLID", scalecol)][, let(KOBLID = as.character(KOBLID))]
  setnames(scales, 2, "scale")
  is_scale <- sum(!is.na(scales$scale) & scales$scale != 1) > 0
  if(!is_scale) return(invisible(NULL))
  
  cat("\n*** Skalerer ", val, " med ", scalecol, sep = "")
  dt[scales, on = "KOBLID", scale := i.scale]
  dt[!is.na(scale), (val) := get(val) * scale][, let(scale = NULL)]
}

check_if_value_ok <- function(dt, val, cleanlog){
  valF <- paste0(val, ".f")
  val_ok <- dt[, .SD, .SDcols = c(valF, "KOBLID")][,let(ok = 1)]
  val_ok[get(valF) == 8, let(ok = 0)]
  n_not_ok <- sum(val_ok$ok == 0)
  val_ok_log <- val_ok[, .(ok = ifelse(sum(ok == 0) == 0, 1, 0)), by = KOBLID]
  rawfiles_not_ok <- val_ok_log[ok == 0, unique(KOBLID)]
  cleanlog[val_ok_log, on = "KOBLID", paste0(val, "_ok") := i.ok]
  if(n_not_ok > 0) cat("\n*** Fant ", n_not_ok, " ugyldige verdier for ", val, 
                       "\n - Råfiler med ugyldige verdier (KOBLID): ", paste0(rawfiles_not_ok, collapse = ", "), sep = "")
  if(n_not_ok == 0) cat("\n*** Alle ", val, " ok", sep = "")
}

do_set_value_names <- function(dt, parameters){
  vals <- get_value_columns(names(dt))
  valnames <- as.character(parameters$filegroup_information[paste0(vals, "navn")])
  suffixes <- c("", ".a", ".f")
  vals <- unlist(lapply(vals, function(x) paste0(x, suffixes)))
  valnames <- unlist(lapply(valnames, function(x) paste0(x, suffixes)))
  data.table::setnames(dt, vals, valnames)
}
