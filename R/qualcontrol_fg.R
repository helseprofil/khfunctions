control_fg_output <- function(outputlist){
  cat("\n\n---\n* Kvalitetskontroll:\n---")
  analyze_cleanlog(log = outputlist$cleanlog)
  warn_geo_99(dt = outputlist$Filgruppe)
}

#' @title analyze_cleanlog
#' @description
#' Analyzes log after cleaning dimensions and values, and stops data processing 
#' if any invalid values are found and prints a report. 
#' @noRd
analyze_cleanlog <- function(log){
  ok_cols <- grep("_ok$", names(log), value = T)
  any_not_ok <- log[rowSums(log[, .SD, .SDcols = ok_cols] == 0) > 0]
  if(nrow(any_not_ok) == 0){
    cat("\n** Ingen ugyldige verdier funnet i filen (alle celler i cleanlog = 1)")
    return(invisible(NULL))
  }
  not_ok_cols <- ok_cols[sapply(any_not_ok[, .SD, .SDcols = ok_cols], function(col) any(col == 0))]
  out <- any_not_ok[, .SD, .SDcols = c("KOBLID", not_ok_cols)]
  cat("\n***** OBS! Feil funnet\n-----")
  cat("\nTabellen viser hvilke filer og hvilke kolonner det er funnet feil i\n-----\n")
  print(out)
  cat("\n-----\n")
  warning(paste0("Kolonnene i tabellen over med verdi = 0 indikerer at det finnes ugyldige verdier. Dette må sannsynligvis ordnes i kodebok før ny kjøring.",
                 "\nSe fullstendig logg her: ", getOption("khfunctions.fgdir"), getOption("khfunctions.fg.sjekk")))
}

#' @title warn99
#' @description
#' Checks if any 99-geocodes exists in the file. If any code is converted to 99, the original codes should be listed when cleaning.
#' @noRd
warn_geo_99 <- function(dt){
  any99 <- dt[grepl("99$", GEO), .N]
  if(any99 > 0){
    cat("\n**", any99, "99-koder funnet, kodet om fra følgende originalkode(r):", paste(.GlobalEnv$org_geo_codes, collapse = ", "))
  } else {
    cat("\n** Ingen 99-koder funnet, OK!")
  }
}
