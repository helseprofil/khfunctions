#' @title clean_filegroup_values_duckdb
#' @description Looper gjennom verdikolonner, renser og sjekker disse. 
#' @noRd
clean_filegroup_values_duckdb <- function(parameters, cleanlog){
  print_console_message("\n* Starter rensing av verdikolonner...")
  con <- parameters$duck
  cols <- intersect(c("VAL1", "VAL2", "VAL3"), DBI::dbListFields(con, "FILGRUPPE"))
  for(val in cols){
    print_console_message("\n** ", val, sep = "")
    clean_value_column_duckdb(con = con, parameters = parameters, cleanlog = cleanlog, val = val)
  }
  print_console_message("\n* Verdikolonner ferdig renset")
  invisible(NULL)
}

#' @title clean_value_column_duckdb
#' @description Setter flagg, fjerner tall som skal fjernes, og gjør verdikolonnen numerisk
#' @noRd
clean_value_column_duckdb <- function(con, parameters, cleanlog, val){
  
  print_console_message("\n*** Setter flagg-kolonne") 
  # 1,2,3 som angitt i ACCESS, og 8 dersom verdikolonnen ikke kan leses som tall
  # Flaggede verdier settes til missing. 
  valF <- paste0(val, ".f")
  valA <- paste0(val, ".a")
  invisible(DBI::dbExecute(con, sprintf(
    "ALTER TABLE FILGRUPPE ADD COLUMN IF NOT EXISTS \"%s\" INTEGER;
    ALTER TABLE FILGRUPPE ADD COLUMN IF NOT EXISTS \"%s\" INTEGER;
    UPDATE FILGRUPPE SET \"%s\" = 1;", 
    valF, valA, valA)))
  invisible(DBI::dbExecute(con, sprintf(
      "UPDATE FILGRUPPE
      SET \"%s\" =
        CASE
          WHEN %s = '..' THEN 1
          WHEN %s = '.'  THEN 2
          WHEN %s = ':'  THEN 3
          WHEN TRY_CAST(%s AS DOUBLE) IS NULL THEN 8
          ELSE 0
        END,
      %s =
      CASE
        WHEN %s = '..' THEN NULL
        WHEN %s = '.' THEN NULL
        WHEN %s = ':' THEN NULL
        WHEN TRY_CAST(%s AS DOUBLE) IS NULL THEN NULL
        ELSE %s
      END",
      valF, val, val, val, val, # Flagg
      val, val, val, val, val, val))) # Fjerne verdier
  
  # Gjør verdi til numerisk
  invisible(DBI::dbExecute(con, sprintf(
      "ALTER TABLE FILGRUPPE
      ALTER COLUMN %s TYPE DOUBLE USING TRY_CAST(%s AS DOUBLE)",
      val,val)))
  
  scale_value_duckdb(con = con, parameters = parameters, val = val)
  
  check_value_ok_duckdb(
    con = con,
    cleanlog = cleanlog,
    val = val
  )
  invisible(NULL)
}

#' @title scale_value_duckdb
#' @description Skalerer verdier dersom angitt
#' @noRd
scale_value_duckdb <- function(con, parameters, val){
  scalecol <- paste0("SKALA_", val)
  scales <- parameters$read_parameters[, .(KOBLID, scale = get(scalecol))]
  if(!any(!is.na(scales$scale) & scales$scale != 1)) return(invisible(NULL))
  
  print_console_message("\n*** Skalerer ", val, " med ", scalecol, sep = "")
  
  DBI::dbWriteTable(con, "tmp_scale", scales, temporary = TRUE, overwrite = TRUE)
  on.exit(invisible(DBI::dbExecute(con, "DROP TABLE IF EXISTS tmp_scale")),add = TRUE)
  
  invisible(DBI::dbExecute(con, sprintf(
      "UPDATE FILGRUPPE AS f
      SET %s = f.%s * s.scale FROM tmp_scale AS s
      WHERE f.KOBLID = s.KOBLID AND s.scale IS NOT NULL",
      val,val)))
  
  invisible(NULL)
}

#' @title check_value_ok_duckdb
#' @description Sjekker om noen har fått flagg 8 (ikke mulig å representere numerisk). Skriver til cleanlog.
#' @noRd
check_value_ok_duckdb <- function(con, cleanlog, val){
  valF <- paste0(val, ".f")
  val_ok <- DBI::dbGetQuery(con, sprintf(
  "SELECT KOBLID, MIN(CASE WHEN \"%s\" = 8 THEN 0 ELSE 1 END) 
  AS ok FROM FILGRUPPE 
  GROUP BY KOBLID",
  valF))
  data.table::setDT(val_ok)
  
  cleanlog[val_ok, on = "KOBLID", (paste0(val, "_ok")) := i.ok]
  rawfiles_not_ok <- val_ok[ok == 0, unique(KOBLID)]
  n_not_ok <- length(rawfiles_not_ok)
  if(n_not_ok > 0){
    print_console_message("\n*** Fant ", n_not_ok, " ugyldige verdier for ", val,
      "\n - Råfiler med ugyldige verdier (KOBLID): ", paste(rawfiles_not_ok, collapse = ", "), sep = "")
  } else {
    print_console_message("\n*** Alle ", val, " ok", sep = "")
  }
  invisible(NULL)
}
