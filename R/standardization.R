add_predteller <- function(parameters){
  if(parameters$CUBEinformation$REFVERDI_VP != "P") return(invisible(NULL))
  print_console_message("* Estimerer forventet teller (PREDTELLER) for standardisering")
  con <- parameters$duck
  tmp_tables <- c(PREDRATE = "tmp_predrate", PREDNEVNER = "tmp_prednevner", PREDTELLER = "tmp_predteller")
  drop_tables_duckdb(con, tmp_tables)
  on.exit(drop_tables_duckdb(con, tmp_tables), add = TRUE)
  
  designlist <- find_common_standard_teller_nevner_prednevner_design(parameters = parameters)
  generate_tmp_predrate(con = con, gentable = tmp_tables[["PREDRATE"]], design = designlist$STNdesign, parameters = parameters) # Lag tmp_predteller og skriv til duckdb
  generate_tmp_prednevner(con = con, gentable = tmp_tables[["PREDNEVNER"]], design = designlist$STNPdesign, parameters = parameters) # lag tmp_prednevner og skriv til duckdb
  generate_tmp_predteller(con = con, tables = tmp_tables, parameters = parameters) # Merge sammen tmp_predrate og tmp_prednevner, og lag predteller. Kod om til kubedesign
  
  print_console_message("\n")
  merge_duckdb_table(con = con, mergeto = "KUBE",  mergefrom = "tmp_predteller")
  set_implicit_null_after_merge_duckdb(con = con, implicitnull_defs = parameters$fileinformation[[parameters$files[["TELLER"]]]]$vals, table = "KUBE")
  
  invisible(NULL)
}

# METADATA ----
#' @title find_common_standard_teller_nevner_prednevner_design
#' @description
#' Finds the design of the standard teller and nevner file
#' @keywords internal
#' @noRd
find_common_standard_teller_nevner_prednevner_design <- function(parameters){
  print_console_message("\n** Finner felles design for STANDARDTELLER, STANDARDNEVNER og PREDNEVNER\n")
  
  STdesign <- find_design_after_filter(file = "STANDARDTELLER", parameters = parameters)
  if ("STANDARDNEVNER" %in% names(parameters$files)) {
    SNdesign <- find_design_after_filter(file = "STANDARDNEVNER", parameters = parameters)
    STNdesign <- FinnFellesTab(DF1 = STdesign, DF2 = SNdesign, parameters = parameters)
  } else {
    STNdesign <- STdesign
  }
  PNdesign <- parameters$filedesign[[parameters$files$PREDNEVNER]]
  STNPdesign <- FinnFellesTab(DF1 = STNdesign, DF2 = PNdesign, parameters = parameters)
  
  # Maa filtrere standardtellerdesign med parameters$PredFilter igjen for aa finne fellesdesign som gir til-design for ST og SN
  STNdesign <- find_design_after_filter(file = "STANDARDTELLER", parameters = parameters, originaldesign = STNPdesign, outpredfilter = FALSE)
  
  return(list(STNdesign = STNdesign, STNPdesign = STNPdesign))
}

#' @title find_design_after_filter
#'
#' @param file name of file to design
#' @param originaldesign original design if existing. Default is design for file.
#' @param outpredfilter shuld columns in predfilter be removed, default = TRUE
#' @param parameters parameters
#' @keywords internal
#' @noRd
find_design_after_filter <- function(file, parameters, originaldesign = NULL, outpredfilter = TRUE) {
  filename <- parameters$files[[file]]
  designfilter <- parameters$PredFilter$Design
  if(is.null(originaldesign)) originaldesign <- parameters$filedesign[[filename]]
  filtercolumns <- character()
  if(outpredfilter) filtercolumns <- parameters$PredFilter$Predfiltercolumns
  
  outdesign <- FinnRedesignForFilter(ORGd = originaldesign, Filter = designfilter, parameters = parameters)$Dekk
  outcols <- setdiff(names(outdesign), filtercolumns)
  outdesign <- outdesign[, ..outcols]
  return(find_filedesign(file = outdesign, filename = filename, parameters = parameters))
}

#' @title FinnRedesignForFilter (kb)
#' @keywords internal
#' @noRd
FinnRedesignForFilter <- function(ORGd, Filter, parameters) {
  MODd <- Filter
  for (del in setdiff(names(ORGd$Part), names(Filter))) {
    MODd[[del]] <- data.table::copy(ORGd$Part[[del]])
  }
  return(find_redesign(orgdesign = ORGd, targetdesign = list(Part = MODd), parameters = parameters))
}

# ESTIMATION ----

#' @title generate_tmp_predrate
#' @description
#' Lager en STANDARD_KUBE med design fra set_predictionfilter til å hente ut radene det skal 
#' standardiseres mot, og beregner predrate (landsrate for prediksjonsperioden). 
#' Dersom det skal standardiseres mot periode, blir STANDARD_KUBE aggregert før rate beregnes.
#' @family duckdb
#' @noRd
generate_tmp_predrate <- function(con, gentable, design, parameters){
  print_console_message("\n* Henter ut RATE å standardisere mot\n")
  missyears <- parameters$MOVAV$missyears
  merge_teller_nevner(parameters = parameters, standardfiles = TRUE, design = design)
  standardkube_name <- "STANDARD_KUBE"
  standardkube_sql <- DBI::dbQuoteIdentifier(con, standardkube_name)
  tmp_tbl_sql <- DBI::dbQuoteIdentifier(con, gentable)
  
  aar_exist <- DBI::dbGetQuery(con, sprintf("SELECT DISTINCT AARl AS AAR FROM %s", standardkube_sql))$AAR
  if(missyears$n > 0 && any(missyears$years %in% aar_exist)){
    problem <- intersect(missyears$years, aar_exist)
    warning("\n--\n** OBS! Mangler tall for år som skal standardiseres mot: ", paste(problem, collapse = ", "), 
            "\n*** Dette vil påvirke landsraten i standardiseringsperioden!\n--\n", immediate. = TRUE)
    sql <- sprintf('DELETE FROM %s WHERE AARl IN (%s)', standardkube_sql, paste(missyears$years, collapse = ", "))
    invisible(DBI::dbExecute(con, sql))
  }
  
  aggregate_to_periods_duckdb(tablename = standardkube_name, parameters = parameters)
  allcols <- DBI::dbListFields(con, standardkube_name)
  dims <- setdiff(get_dimension_columns(allcols), parameters$PredFilter$Predfiltercolumns)
  dims_sql <- DBI::dbQuoteIdentifier(con, dims)
  
  sql_generate <- sprintf(
    'CREATE TABLE %s AS 
    SELECT %s,
    CASE
      WHEN TELLER <= 2 AND "TELLER.f" = 0 AND NEVNER = 0 AND "NEVNER.f" = 0 THEN 0
      WHEN NEVNER <> 0 AND "NEVNER.f" = 0 THEN TELLER * 1.0 / NEVNER
      WHEN NEVNER = 0 AND "NEVNER.f" = 0 THEN 0
      ELSE NULL 
    END AS PREDRATE,
    CASE
      WHEN TELLER <= 2 AND "TELLER.f" = 0 AND NEVNER = 0 AND "NEVNER.f" = 0 THEN 0
      WHEN NEVNER <> 0 AND "NEVNER.f" = 0 THEN GREATEST("TELLER.f", "NEVNER.f")
      WHEN NEVNER = 0 AND "NEVNER.f" = 0 THEN GREATEST("TELLER.f", 2)
      ELSE NULL
    END AS "PREDRATE.f",
    GREATEST("TELLER.a", "NEVNER.a") AS "PREDRATE.a",
    CASE
      WHEN TELLER IS NULL OR NEVNER IS NULL THEN 1 ELSE 0
    END AS ukurant
    FROM %s',
    tmp_tbl_sql, paste(dims_sql, collapse = ", "), standardkube_sql)
  
  invisible(DBI::dbExecute(con, sql_generate))
  
  sql_ukurante <- sprintf('SELECT %s FROM %s WHERE ukurant = 1', paste(dims_sql, collapse = ", "), tmp_tbl_sql)
  ukurante <- data.table::setDT(DBI::dbGetQuery(con, sql_ukurante))
  
  if(ukurante[, .N] > 0){
    print_console_message(paste0("\n!!! Missing verdier i standardteller og/eller standardnevner (", ukurante[, .N], ")"))
    print_console_message("-Dette KAN gi problemer, da PREDTELLER - og dermed MEIS - ikke kan beregnes for disse strataene: \n")
    print_console_message("-Dersom det faktisk mangler tall kan det være behov for å justere startår")
    print_console_message("-Følgende unike verdier for ulike dimensjonene er påvirket: ")
    for(dim in dims){print_console_message(paste0("- ", dim, ": ", paste(unique(ukurante[[dim]]), collapse = ", ")))}
  }
  
  sql_cleanup <- sprintf(
  'CREATE OR REPLACE TABLE %s AS SELECT %s, PREDRATE, "PREDRATE.f", "PREDRATE.a" FROM %s',
  tmp_tbl_sql, paste(dims_sql, collapse = ", "), tmp_tbl_sql)
  
  invisible(DBI::dbExecute(con, sql_cleanup))
  invisible(NULL)
}

#' @title generate_tmp_prednevner
#' @description Lager prednevner som sammen med predrate er grunnlag for predteller
#' @family duckdb
#' @noRd
generate_tmp_prednevner <- function(con, gentable, design, parameters){
  print_console_message("\n* Henter ut NEVNER som grunnlag for PREDTELLER\n")
  missyears <- parameters$MOVAV$missyears
  tmp_prednevner_sql <- DBI::dbQuoteIdentifier(con, gentable)
  prednevnerfile <- parameters$files$PREDNEVNER
  prednevnerfile_sql <- DBI::dbQuoteIdentifier(con, prednevnerfile)
  
  # UBRUKT KOLONNE, BRUKER ALLTID NEVNER, aldri PREDNEVNERFIL/PREDNEVNERCOL
  prednevner_col <- gsub("^(.*):(.*)", "\\2", parameters$TNPinformation$PREDNEVNERFIL)
  if(is_empty(prednevner_col)) prednevner_col <- parameters$TNPinformation$NEVNERKOL
  
  allcols <- DBI::dbListFields(con, prednevnerfile_sql)
  dims <- get_dimension_columns(allcols)
  dims_sql <- DBI::dbQuoteIdentifier(con, dims)
  pred_cols <- grep(sprintf("^%s(\\.f|.a|)$", prednevner_col),allcols, value = TRUE)
  rename_cols <- gsub(sprintf("^%s(\\.f|.a|)$", prednevner_col),"PREDNEVNER\\1",pred_cols)
  predvalue_sql <- sprintf("%s AS %s", DBI::dbQuoteIdentifier(con, pred_cols), DBI::dbQuoteIdentifier(con, rename_cols))
  
  sql_generate <- sprintf("CREATE TABLE %s AS SELECT %s, %s FROM %s", 
                          tmp_prednevner_sql, 
                          paste(dims_sql, collapse = ", "),
                          paste(predvalue_sql, collapse = ",\n"),
                          prednevnerfile_sql)
  invisible(DBI::dbExecute(con, sql_generate))
  
  redesign <- find_redesign(orgdesign = parameters$filedesign[[prednevnerfile]], targetdesign = design, parameters = parameters)
  filter_and_recode_table_duckdb(con = con, tablename = gentable, redesign = redesign, parameters = parameters)
  
  if(missyears$n > 0){
    sql_delete <- sprintf('DELETE FROM %s WHERE AARl IN (%s)',
      output_sql, paste(missyears$years, collapse = ", "))
    invisible(DBI::dbExecute(con, sql_delete))
  }
  
  aggregate_to_periods_duckdb(tablename = tmp_prednevner_sql, parameters = parameters)
  invisible(NULL)
}

generate_tmp_predteller <- function(con, tables, parameters){
  print_console_message("\n* Beregner PREDTELLER\n")
  tmp_predrate_sql <- DBI::dbQuoteIdentifier(con, tables[["PREDRATE"]])
  tmp_prednevner_sql <- DBI::dbQuoteIdentifier(con, tables[["PREDNEVNER"]])
  tmp_predteller_sql <- DBI::dbQuoteIdentifier(con, tables[["PREDTELLER"]])
  
  predrate_dims <- get_dimension_columns(DBI::dbListFields(con, tables[["PREDRATE"]]))
  prednevner_dims <- get_dimension_columns(DBI::dbListFields(con, tables[["PREDNEVNER"]]))
  commondims <- DBI::dbQuoteIdentifier(con, intersect(prednevner_dims, predrate_dims))
  
  all_dims <- union(prednevner_dims, predrate_dims)
  dim_select <- c(
    sprintf("pn.%s", DBI::dbQuoteIdentifier(con, intersect(all_dims, prednevner_dims))),
    sprintf("pr.%s", DBI::dbQuoteIdentifier(con, setdiff(all_dims, prednevner_dims)))
  )
  
  join_condition <- paste(sprintf("pn.%s = pr.%s", commondims, commondims),collapse = "\n  AND ")
  
  sql_mismatch <- sprintf("SELECT COUNT(*) AS N FROM %s pr ANTI JOIN %s pn ON %s",
                          tmp_predrate_sql, tmp_prednevner_sql, join_condition)
  
  mismatch <- DBI::dbGetQuery(con, sql_mismatch)$N
  if(mismatch > 0) print_console_message(sprintf('!!!!!ADVARSEL: %s strata i predrate finnes ikke i prednevner!!!', mismatch))
  
  sql_generate <- sprintf(
    'CREATE TABLE %s AS
    SELECT %s,
    pr.PREDRATE * pn.PREDNEVNER AS PREDTELLER,
    GREATEST(pr."PREDRATE.f", pn."PREDNEVNER.f") AS "PREDTELLER.f",
    GREATEST(pr."PREDRATE.a", pn."PREDNEVNER.a") AS "PREDTELLER.a"
    FROM %s pn
    LEFT JOIN %s pr ON %s',
    tmp_predteller_sql,
    paste(dim_select, collapse = ", "),
    tmp_prednevner_sql,
    tmp_predrate_sql,
    join_condition
  )
  
  
  invisible(DBI::dbExecute(con, sql_generate))
  print_console_message("- Redesigner for å matche KUBE")
  prednevnerdesign <- find_filedesign(filename = tables[["PREDNEVNER"]], parameters = parameters, copy_fileinfo_from = parameters$files$PREDNEVNER)
  cubedesign <- list(Part = parameters$CUBEdesign)
  redesign <- find_redesign(orgdesign = prednevnerdesign, targetdesign = cubedesign, aggregate = parameters$DefDesign$AggVedStand, parameters = parameters)
  filter_and_recode_table_duckdb(con = con, tablename = tables[["PREDTELLER"]], redesign = redesign, parameters = parameters)
  invisible(NULL)
}

#' @title add_meisskala
#' @description Henter ut meisskala (= RATE for utvalget det standardiseres mot)
#' @noRd
add_meisskala <- function(parameters){
  if(parameters$PredFilter$ref_year_type != "Specific") return(invisible(NULL))
  print_console_message("* Legger til MEISskala for standardisering\n")
  
  con <- parameters$duck
  tbl_sql <- DBI::dbQuoteIdentifier(con, "KUBE")
  
  if(parameters$CUBEinformation$REFVERDI_VP != "P"){
    sql <- sprintf('ALTER TABLE %s 
    ADD COLUMN IF NOT EXISTS MEISskala DOUBLE;
    UPDATE %s SET MEISskala = NULL', tbl_sql, tbl_sql)
    invisible(DBI::dbExecute(con, sql))
    return(invisible(NULL))
  }
  
  subset_table <- "tmp_meisskala"
  subset_sql <- DBI::dbQuoteIdentifier(con, subset_table)
  drop_tables_duckdb(con, subset_table)
  
  filter_sql <- r_filter_to_sql(parameters$PredFilter$meisskalafilter)
  sql <- sprintf('CREATE TABLE %s AS SELECT *, RATE AS MEISskala FROM %s WHERE %s',
                 subset_sql, tbl_sql, filter_sql)
  invisible(DBI::dbExecute(con, sql))
  
  n_subset <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS N FROM %s", subset_sql))$N
  if(n_subset == 0) stop("Noe er feil i ACCESS::KUBER::REFVERDI, klarer ikke lage meisskala")
  
  subset_cols <- DBI::dbListFields(con, subset_table)
  
  joincolumns <- DBI::dbQuoteIdentifier(con, setdiff(intersect(subset_cols, parameters$DefDesign$DesignKolsFA), 
                                                     parameters$PredFilter$Predfiltercolumns))
  
  join_sql <- paste(sprintf("k.%s = m.%s", joincolumns, joincolumns), collapse = "\n AND ")
  sql <- sprintf(
  'ALTER TABLE %s 
  ADD COLUMN IF NOT EXISTS MEISskala DOUBLE;
  UPDATE %s k SET MEISskala = m.MEISskala FROM %s m WHERE %s',
  tbl_sql, tbl_sql, subset_sql,join_sql)
  
  invisible(DBI::dbExecute(con, sql))
  drop_tables_duckdb(con, subset_table)
  invisible(NULL)
}
