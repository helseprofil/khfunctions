#' @title scale_rate_and_meisskala
#' @description Skalerer RATE og MEISskala til verdi angitt i ACCESS::KUBER::RATESKALA
#' @noRd
scale_rate_and_meisskala <- function(parameters){
  is_rateskala <- is_not_empty(parameters$CUBEinformation$RATESKALA)
  if(!is_rateskala) return(invisible(NULL))
  
  con <- parameters$duck
  scalevalue <- as.numeric(parameters$CUBEinformation$RATESKALA)
  
  print_console_message("* Skalerer RATE til per", scalevalue)
  tbl_sql <- sqlquote(con, "KUBE")
  cols <- DBI::dbListFields(con, tbl_sql)
  
  update_cols <- character()
  
  if("RATE" %in% cols) update_cols <- c(update_cols, sprintf("RATE = RATE * %s", scalevalue))
  if("MEISskala" %in% cols) update_cols <- c(update_cols, sprintf("MEISskala = MEISskala * %s", scalevalue))
  
  if(length(update_cols) > 0){
    sql <- sprintf("UPDATE %s SET %s", tbl_sql, paste(update_cols, collapse = ", "))
    invisible(DBI::dbExecute(con, sql))
  }
  invisible(NULL)
}


#' @title do_format_cube_columns
#' @description
#' Legger til manglende kolonner som må være med
#' Beregner sum-kolonnene
#' Beregner årlige tall
#' Legger til MALTALL
#' Eventuelt lage ny kolonne basert på ACCESS::TNP_PROD::NYEKOL_RAD_postMA
#' @noRd
do_format_cube_columns <- function(parameters){
  print_console_message("\n* Formatterer kolonner i KUBE")
  con = parameters$duck
  tablename <- "KUBE"
  cols <- DBI::dbListFields(con, tablename)
  tbl_sql <- sqlquote(con, tablename)
  
  obligcolumns <- c("TELLER","NEVNER","RATE")
  obligcolumns <- c(paste0(rep(obligcolumns, each = 4),c("", ".f", ".a", ".n")), "PREDTELLER", "PREDTELLER.f")
  
  required_cols <- c(
    setNames(rep("DOUBLE", length(obligcolumns)), obligcolumns),
    sumTELLER = "DOUBLE",
    sumNEVNER = "DOUBLE",
    sumPREDTELLER = "DOUBLE",
    MALTALL = "DOUBLE")
  
  if(all(c("AARl", "AARh") %in% cols)) required_cols["AAR"] <- "VARCHAR"
  if(all(c("ALDERl", "ALDERh") %in% cols)) required_cols["ALDER"] <- "VARCHAR"
  
  missing_cols <- setdiff(names(required_cols), cols)
  
  if(length(missing_cols) > 0){
    print_console_message("- Initierer manglende kolonner:", paste(missing_cols, collapse = ", "))
    init_new_duckdb_cols(con, "KUBE", required_cols[missing_cols])
  }
  
  # Oppdater kolonner (sumkolonner, nonsumkolonner, ALDER, AAR og MALTALL)
  cols <- DBI::dbListFields(con, tablename)
  update <- character()
  factor <- parameters$MOVAV$orgintMult
  
  update <- c(update, 
              sprintf("sumTELLER = %s * TELLER", factor),
              sprintf("sumNEVNER = %s * NEVNER", factor),
              sprintf("sumPREDTELLER = %s * PREDTELLER", factor))
  
  nonsumvalues <- setdiff(get_value_columns(DBI::dbListFields(con, tablename)), c("RATE", "SMR"))
  
  for(val in nonsumvalues){
    # PREDTELLER bruker TELLER.n for å lage årlige tall, 
    # i stedet for å lage PREDTELLER.n som en ekstra kolonne som == TELLER.n
    valn_sql <- if(val == "PREDTELLER"){
      sqlquote(con, "TELLER.n") 
    } else {
      sqlquote(con, paste0(val, ".n"))
    }
    update <- c(update,
                sprintf("%s = %s / %s",
                        sqlquote(con, val),
                        sqlquote(con, val),
                        valn_sql))
  }
  
  if("AAR" %in% cols) update <- c(update, "AAR = printf('%d_%d', AARl, AARh)")
  if("ALDER" %in% cols) update <- c(update, "ALDER = printf('%d_%d', ALDERl, ALDERh)")
  update <- c(update, sprintf("%s = %s", sqlquote(con, "MALTALL"), sqlquote(con, parameters$MALTALL)))
  update_sql <- sprintf("UPDATE %s SET %s", 
                        sqlquote(con, tablename), 
                        paste(update, collapse = ", "))
  invisible(DBI::dbExecute(con, update_sql))
  
  if(is_not_empty(parameters$TNPinformation$NYEKOL_RAD_postMA)){
    dt <- fetch_duckdb_table(con = con, tablename = tablename) 
    compute_new_value_from_formula(dt = dt, formulas = parameters$TNPinformation$NYEKOL_RAD_postMA, post_moving_average = TRUE)
    write_to_tmp_and_replace_table(con = con, tablename = tablename, data = dt)
  }
  
  invisible(NULL)
}


#' @title rename_tab_columns
#' @description
#' Omdøper TAB-kolonnene til sitt faktiske navn, angitt i ACCESS::FILGRUPPER
#' @noRd
rename_tab_columns <- function(parameters){
  con <- parameters$duck
  spec <- parameters$fileinformation[[parameters$files$TELLER]]
  cols <- get_duckdb_cols(con, "KUBE")
  tabcols <- grep("^TAB\\d+$", cols, value = TRUE)
  if(length(tabcols) == 0) return(invisible(NULL))
  tabnames <- vapply(tabcols, function(x) spec[[x]], character(1))
  
  sql <- paste(
    sprintf(
      "ALTER TABLE KUBE RENAME COLUMN %s TO %s",
      sqlquote(con, tabcols), sqlquote(con, tabnames)
    ), collapse = ";\n")
  
  invisible(DBI::dbExecute(con, sql))
  return(tabnames)
}

#' @keywords internal
#' @noRd
get_outdimensions <- function(parameters){
  con <- parameters$duck
  dims <- c(getOption("khfunctions.khtabs"), as.character(parameters$tabnames))
  if(is_not_empty(parameters$CUBEinformation$DIMDROPP)){
    dimdropp <- unlist(strsplit(parameters$CUBEinformation$DIMDROPP, ","))
    dims <- setdiff(dims, dimdropp)
  }
  
  cols <- get_duckdb_cols(con, "KUBE")
  if("ALDER" %notin% cols) dims <- setdiff(dims, "ALDER")
  if("KJONN" %notin% cols) dims <- setdiff(dims, "KJONN")
  return(dims)
}

#' @title get_outvalues_allvis
#' @description finds value columns to be included in output
#' @param parameters cube parameters   
#' @noRd
get_outvalues_allvis <- function(parameters){
  cols <- character(0)
  if(parameters$CUBEinformation$REFVERDI_VP == "P") cols <- c("T", "RATE", "SMR", "MEIS")
  if(is_not_empty(parameters$CUBEinformation$NESSTARTUPPEL)){
    cols <- gsub("\\s", "", parameters$CUBEinformation$NESSTARTUPPEL)
    cols <- unlist(strsplit(cols, ","))
    if(any(!cols %in% names(getOption("khfunctions.valcols")))){
      stop("Feil i ACCESS::KUBER::NESSTARTUPPEL, aksepterte verdier (kommaseparert): ",
           paste0(names(getOption("khfunctions.valcols")), collapse = ","))
    } 
  }
  cols <- as.character(getOption("khfunctions.valcols")[cols])
  
  if(is_not_empty(parameters$CUBEinformation$EKSTRAVARIABLE)){
    extravalue <- unlist(stringr::str_split(parameters$CUBEinformation$EKSTRAVARIABLE, ","))
    cols <- c(cols, extravalue)
  }
  return(cols)
}

# DEPRECATED ----
#' @title do_format_cube_columns
#' @description
#' Adds missing columns, creates sumvalues and nonsumvalues, sets alder and aar columns
#' Creates new columns post moving average, as defined in ACCESS::TNP_PROD::NYEKOL_RAD_postMA
#' @noRd
do_format_cube_columns_old <- function(dt, parameters){
  add_missing_columns(dt = dt)
  add_sumvalues(dt = dt, factor = parameters$MOVAV$orgintMult)
  set_nonsumvalues(dt = dt)
  set_alder_aar(dt = dt)
  if(is_not_empty(parameters$TNPinformation$NYEKOL_RAD_postMA)) compute_new_value_from_formula(dt = dt, formulas = parameters$TNPinformation$NYEKOL_RAD_postMA, post_moving_average = TRUE)
  data.table::set(dt, j = "MALTALL", value = dt[[parameters$MALTALL]])
  data.table::set(dt, j = getOption("khfunctions.prikkeinfo"), value = 0L)
  return(dt)
}

#' @title add_missing_columns
#' @description sets obligatory columns = NA if they do not exist. 
#' @param dt dataset
#' @noRd
add_missing_columns <- function(dt){
  obligcolumns <- c("TELLER","NEVNER","RATE","PREDTELLER")
  obligcolumns <- paste0(rep(obligcolumns, each = 4), c("", ".f", ".a", ".n"))
  missingcolumns <- setdiff(obligcolumns, names(dt))
  if(length(missingcolumns) > 0) data.table::set(dt, j = missingcolumns, value = NA_real_)
}

#' @title add_sumvalues
#' @description adds sumTELLER, sumNEVNER, sumPREDTELLER to data
#' @param dt dataset
#' @param factor factor to multiply orignial variables by, generated as part of MOVAV-parameters
#' @keywords internal
#' @noRd
add_sumvalues <- function(dt, factor){
  dt[, let(sumTELLER = factor * TELLER,
           sumNEVNER = factor * NEVNER,
           sumPREDTELLER = factor * PREDTELLER)]
}

#' @title set_nonsumvalues
#' @description sets original values (exept RATE/SMR) by dividing by the corresponding .n column. 
#' @param dt dataset
#' @keywords internal
#' @noRd
set_nonsumvalues <- function(dt){
  values <- setdiff(get_value_columns(names(dt)), c("RATE", "SMR"))
  if(length(values) == 0) return(dt)
  for(val in values){
    valN = paste0(val, ".n")
    data.table::set(dt, j = val, value = dt[[val]] / dt[[valN]])
  }
}

#' @keywords internal
#' @noRd
set_alder_aar <- function(dt){
  data.table::set(dt, j = "AAR", value = paste0(dt[["AARl"]], "_", dt[["AARh"]]))
  if(all(c("ALDERl", "ALDERh") %in% names(dt))){
    data.table::set(dt, j = "ALDER", value = paste0(dt[["ALDERl"]], "_", dt[["ALDERh"]]))
  } 
}

#' @title filter_invalid_outcodes
#' @description remove GEO codes not listed in ACCESS:GEOkoder
#' @param dt dataset
#' @param parameters global parameters
#' @keywords internal
#' @noRd
filter_invalid_geo_alder_kjonn <- function(dt, parameters){
  valid_geo <- parameters[["GeoKoder"]][TYP == "O" & TIL == 9999, GEO]
  dt <- dt[GEO %in% valid_geo]
  if("ALDER" %in% names(dt)) dt <- dt[!ALDER %in% c(getOption("khfunctions.alder_illegal"), getOption("khfunctions.alder_illegal"))]
  if("KJONN" %in% names(dt)) dt <- dt[!KJONN %in% c(getOption("khfunctions.illegal"), getOption("khfunctions.ukjent"))]
  return(dt)
}
