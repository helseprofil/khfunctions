control_fg_output <- function(outputlist){
  khtools::msg("\n\n---\n* Kvalitetskontroll:\n---")
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
    khtools::msg("\n** Ingen ugyldige verdier funnet i filen (alle celler i cleanlog = 1)")
    return(invisible(NULL))
  }
  not_ok_cols <- ok_cols[sapply(any_not_ok[, .SD, .SDcols = ok_cols], function(col) any(col == 0))]
  out <- any_not_ok[, .SD, .SDcols = c("KOBLID", not_ok_cols)]
  khtools::msg("\n***** OBS! Feil funnet\n-----")
  khtools::msg("\nTabellen viser hvilke filer og hvilke kolonner det er funnet feil i\n-----\n")
  print(out)
  khtools::msg("\n-----\n")
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
    khtools::msg("\n**", any99, "99-koder funnet. ")
    if(exists("org_geo_codes", envir = .GlobalEnv)){
      khtools::msg("Disse kan være 99 originalt, men er også omkodet fra følgende originalkode(r):", paste(.GlobalEnv$org_geo_codes, collapse = ", "))
    } else {
      khtools::msg("Disse er ikke omkodet, og har vært 99 originalt.")
    }
  } else {
    khtools::msg("\n** Ingen 99-koder funnet, OK!")
  }
}
