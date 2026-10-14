#' @title add_smr_and_meis
#' @description
#' Beregner SMR og MEIS
#' Hvis kuben standardiseres, beregnes MEIS som
#' sumTELLER / sumPREDTELLER * meisskala (raten for strataet fra standardiseringsutvalget/landstall)
#' SMR settes deretter til MEIS / MEIS fra standardiseringsutvalget * 100
#' 
#' For kuber som ikke standardiseres, settes MEIS = MALTALL (ofte RATE), 
#' mens SMR settes til MALTALL relativt til MALTALL på landsnivå slik at lands-SMR alltid blir 100.
#' 
#' @family duckdb
#' @noRd
add_smr_and_meis <- function(parameters){
  con <- parameters$duck
  ref_year_type <- parameters$PredFilter$ref_year_type
  refverdi_vp <- parameters$CUBEinformation$REFVERDI_VP
  on.exit(khtools::duckdb_drop_tables(con, "normsubset"), add = TRUE)
  
  if(ref_year_type == "Specific") {
    khtools::duckdb_ensure_columns(con = con, table = "KUBE", cols = c(SMR = "DOUBLE", MEIS = "DOUBLE"))
    
    if(refverdi_vp == "P") {
      invisible(DBI::dbExecute(con,
      "UPDATE KUBE SET MEIS = CASE WHEN sumPREDTELLER = 0 THEN NULL ELSE sumTELLER * 1.0 / sumPREDTELLER * MEISskala END"))
    }
    
    khtools::duckdb_drop_tables(con, "normsubset")
    design_cols <- intersect(khtools::duckdb_get_columns(con, "KUBE"), parameters$DefDesign$DesignKolsFA)
    keep_cols <- c(setdiff(design_cols, c("GEOniv", "GEO", "FYLKE")), "LANDSNORMAL")
    keep_sql <- paste(khtools::sql_quote_I(con, keep_cols), collapse = ", ")
    
    sql_norm <- sprintf(
      "SELECT %s FROM 
      (SELECT *, MEIS AS LANDSNORMAL FROM KUBE WHERE GEOniv = 'L')",
      keep_sql
    )
    khtools::duckdb_drop_tables(con, "normsubset")
    khtools::duckdb_create_new_table(con, target = "normsubset", select_sql = sql_norm)
    khtools::duckdb_merge_tables(con = con, mergeto = "KUBE", mergefrom = "normsubset",
                                 join_cols = get_duckdb_join_cols(con, "KUBE", "normsubset"))
    invisible(DBI::dbExecute(con, "UPDATE KUBE SET SMR = MEIS / LANDSNORMAL * 100.0"))
    
  } else { # Moving
    
    normsmr_expr <- if(refverdi_vp == "P"){
      "sumTELLER * 100.0 / sumPREDTELLER"
    } else if(refverdi_vp == "V"){
      "100.0"
    }
    
    khtools::duckdb_drop_tables(con, "normsubset")
    
    design_cols <- intersect(khtools::duckdb_get_columns(con, "KUBE"), parameters$DefDesign$DesignKolsFA)
    keep_cols <- c(setdiff(design_cols, parameters$PredFilter$Predfiltercolumns), "NORM", "NORMSMR")
    keep_sql <- paste(khtools::sql_quote_I(con, keep_cols), collapse = ", ")
    
    sql_norm <- sprintf(
      "SELECT %s FROM 
      (SELECT *, %s AS NORMSMR, %s AS NORM FROM KUBE WHERE %s) x",
      keep_sql, normsmr_expr, khtools::sql_quote_I(con, parameters$MALTALL),
      r_filter_to_sql(parameters$PredFilter$meisskalafilter))
    
    khtools::duckdb_drop_tables(con, "normsubset")
    khtools::duckdb_create_new_table(con, target = "normsubset",select_sql = sql_norm)
    khtools::duckdb_merge_tables(con = con, mergeto = "KUBE", mergefrom = "normsubset",
                                 join_cols = get_duckdb_join_cols(con, "KUBE", "normsubset"))
    khtools::duckdb_ensure_columns(con, "KUBE", c(SMR = "DOUBLE", MEIS = "DOUBLE"))
    
    smr0_expr <- if(refverdi_vp == "P") {
      "sumTELLER * 100.0 / sumPREDTELLER"
    } else if(refverdi_vp == "V") {
      sprintf("%s * 100.0 / NORM", khtools::sql_quote_I(con, parameters$MALTALL))
    }
    
    sql <- sprintf(
    "UPDATE KUBE SET
    SMR = 100.0 * ((%s) / NORMSMR),
    MEIS = ((%s) / NORMSMR) * NORM",
    smr0_expr, smr0_expr)
    
    invisible(DBI::dbExecute(con, sql))
    
  }
}
