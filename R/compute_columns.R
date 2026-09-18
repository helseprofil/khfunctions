#' @title compute_new_value_from_formula
#' @description adds new value columns from a set of formulas by reference
#' Used with manual formula or when specified in ACCESS columns: 
#' - FILFILTRE::NYEKOL_KOL_preRAD
#' - TNP_PROD::NYEKOL_KOL
#' - TNP_PROD::NYEKOL_RAD_postMA
#' @param formulas Formula(s) to make new column(s), usually provided in ACCESS
#' @param post_moving_average if used after aggregating to moving average, additional columns should be generated.
#' @keywords internal
#' @noRd
compute_new_value_from_formula <- function(dt, formulas, post_moving_average = FALSE){
  if(is_empty(formulas)) return(invisible(dt))
  values <- get_value_columns(names(dt))
  formulas <- trimws(unlist(strsplit(formulas, ";")))
  for(f in formulas){
    print_console_message("\n*** Legger til nye kolonner: ", f)
    name <-  gsub("^(.*?)=(.*)$", "\\1", f)
    formula <- gsub("^(.*?)=\\{(.*)\\}$", "\\2", f)
    included_columns <- character()
    for(col in values){
      if(grepl(col, formula)) included_columns <- c(included_columns, col)
    }
    dt[, (name) := eval(rlang::parse_expr(formula))]
    dt[, paste0(name, ".f") := do.call(pmax, c(.SD, list(na.rm = T))), .SDcols = paste0(included_columns, ".f")]
    dt[, paste0(name, ".a") := do.call(pmax, c(.SD, list(na.rm = T))), .SDcols = paste0(included_columns, ".a")]
    
    if(post_moving_average){
      dt[, paste0(name, ".n") := do.call(pmax, c(.SD, list(na.rm = T))), .SDcols = paste0(included_columns, ".n")]
      dt[, (paste0(name, c(".fn1", ".fn3", ".fn9"))) := list(0,0,0)]
    }  
    dt[is.na(dt[[name]]) | is.infinite(dt[[name]]) | is.nan(dt[[name]]), (paste0(name, c("", ".f"))) := list(NA, 2)]
  }
  return(invisible(dt))
}

compute_new_value_from_formula_duckdb <- function(con, tablename, formulas, post_moving_average = FALSE){
  
  if(is_empty(formulas)) return(invisible(NULL))
  
  cols <- get_duckdb_cols(con, tablename)
  values <- get_value_columns(cols)
  
  formulas <- trimws(
    unlist(
      strsplit(formulas, ";", fixed = TRUE)
    )
  )
  
  table_sql <- sqlquote(con, tablename)
  
  for(f in formulas){
    
    print_console_message(
      paste0("\n*** Legger til nye kolonner: ", f)
    )
    
    name <- sub(
      "^(.*?)=(.*)$",
      "\\1",
      f
    )
    
    formula <- sub(
      "^(.*?)=\\{(.*)\\}$",
      "\\2",
      f
    )
    
    included_columns <- values[
      vapply(
        values,
        grepl,
        logical(1),
        x = formula,
        fixed = TRUE
      )
    ]
    
    DBI::dbExecute(
      con,
      sprintf(
        "ALTER TABLE %s ADD COLUMN %s DOUBLE",
        table_sql,
        sqlquote(con, name)
      )
    )
    
    DBI::dbExecute(
      con,
      sprintf(
        "ALTER TABLE %s ADD COLUMN %s INTEGER",
        table_sql,
        sqlquote(con, paste0(name, ".f"))
      )
    )
    
    DBI::dbExecute(
      con,
      sprintf(
        "ALTER TABLE %s ADD COLUMN %s INTEGER",
        table_sql,
        sqlquote(con, paste0(name, ".a"))
      )
    )
    
    if(post_moving_average){
      
      DBI::dbExecute(
        con,
        sprintf(
          "ALTER TABLE %s ADD COLUMN %s INTEGER",
          table_sql,
          sqlquote(con, paste0(name, ".n"))
        )
      )
      
      DBI::dbExecute(
        con,
        sprintf(
          paste(
            "ALTER TABLE %s",
            "ADD COLUMN %s INTEGER DEFAULT 0"
          ),
          table_sql,
          sqlquote(con, paste0(name, ".fn1"))
        )
      )
      
      DBI::dbExecute(
        con,
        sprintf(
          paste(
            "ALTER TABLE %s",
            "ADD COLUMN %s INTEGER DEFAULT 0"
          ),
          table_sql,
          sqlquote(con, paste0(name, ".fn3"))
        )
      )
      
      DBI::dbExecute(
        con,
        sprintf(
          paste(
            "ALTER TABLE %s",
            "ADD COLUMN %s INTEGER DEFAULT 0"
          ),
          table_sql,
          sqlquote(con, paste0(name, ".fn9"))
        )
      )
    }
    
    f_sql <- paste0(
      sqlquote(
        con,
        paste0(included_columns, ".f")
      ),
      collapse = ", "
    )
    
    a_sql <- paste0(
      sqlquote(
        con,
        paste0(included_columns, ".a")
      ),
      collapse = ", "
    )
    
    sql <- sprintf(
      paste(
        "UPDATE %s",
        "SET",
        "%s = %s,",
        "%s = GREATEST(%s),",
        "%s = GREATEST(%s)"
      ),
      table_sql,
      
      sqlquote(con, name),
      formula,
      
      sqlquote(con, paste0(name, ".f")),
      f_sql,
      
      sqlquote(con, paste0(name, ".a")),
      a_sql
    )
    
    DBI::dbExecute(con, sql)
    
    if(post_moving_average){
      
      n_sql <- paste0(
        sqlquote(
          con,
          paste0(included_columns, ".n")
        ),
        collapse = ", "
      )
      
      DBI::dbExecute(
        con,
        sprintf(
          paste(
            "UPDATE %s",
            "SET %s = GREATEST(%s)"
          ),
          table_sql,
          sqlquote(con, paste0(name, ".n")),
          n_sql
        )
      )
    }
    
    DBI::dbExecute(
      con,
      sprintf(
        paste(
          "UPDATE %s",
          "SET",
          "%s = NULL,",
          "%s = 2",
          "WHERE",
          "NOT ISFINITE(%s)",
          "OR %s IS NULL"
        ),
        table_sql,
        sqlquote(con, name),
        sqlquote(con, paste0(name, ".f")),
        sqlquote(con, name),
        sqlquote(con, name)
      )
    )
  }
  
  invisible(NULL)
}


#' @title add_crude_rate
#' @family duckdb
#' @noRd
add_crude_rate <- function(con, tablename){
  print_console_message("* Legger til crude RATE")
  cols <- get_duckdb_cols(con, tablename)
  if(!"NEVNER" %in% cols){
    print_console_message("- Har ikke NEVNER, kan ikke beregne crude RATE")
    return(invisible(NULL))
  } 
  tbl_sql <- sqlquote(con, tablename)
  sql_addcols <- sprintf(
    'ALTER TABLE %s ADD COLUMN IF NOT EXISTS RATE DOUBLE;
    ALTER TABLE %s ADD COLUMN IF NOT EXISTS "RATE.f" INTEGER;
    ALTER TABLE %s ADD COLUMN IF NOT EXISTS "RATE.a" INTEGER;
    ALTER TABLE %s ADD COLUMN IF NOT EXISTS "RATE.n" INTEGER;',
    tbl_sql, tbl_sql, tbl_sql, tbl_sql)
  invisible(DBI::dbExecute(con, sql_addcols))
  
  sql_rate <- sprintf(
    'UPDATE %s SET
    RATE = TELLER / NULLIF(NEVNER, 0),
    "RATE.f" = GREATEST("TELLER.f", "NEVNER.f"),
    "RATE.a" = GREATEST("TELLER.a", "NEVNER.a"),
    "RATE.n" = GREATEST("TELLER.n", "NEVNER.n")',
    tbl_sql)
  
  invisible(DBI::dbExecute(con, sql_rate))
  
  sql_ratemissing <- sprintf(
    'UPDATE %s SET
    "TELLER.f" = 2, "NEVNER.f" = 2, "RATE.f" = 2, spv_tmp = 2
    WHERE RATE IS NULL AND "RATE.f" = 0',
    tbl_sql)
  
  invisible(DBI::dbExecute(con, sql_ratemissing))
  invisible(NULL)
}



#' @title compute_new_value_from_row_sum
#' @description
#' Used to compute columns described in ACCESS columns
#' - FILFILTRE::NYEKOL_RAD
#' - TNP_PROD::NYEKOL_RAD
#' @keywords internal
#' @noRd
compute_new_value_from_row_sum <- function(dt, formulas, fileinfo, parameters){
  if(is_empty(formulas)) return(invisible(dt))
  formulas <- unlist(strsplit(formulas, ";"))
  if(any(!grepl("^\\S+?\\s*=\\s*\\S+?\\{(.*)\\}$", formulas))){
    stop("FILFILTRE::NYEKOL_RAD har feil format: \n\n'", formulas, 
         "'\n\nMå være 'NYTTNAVN=KOLONNESOMSKALSUMMERES{subset}', hvor subset er KOLONNE==VERDI eller KOLONNENAVN %in% c('verdi1', 'verdi2')",
         "\n\nFlere nye kolonner kan angis semikolonseparart: V1=KOL1{subset};V2=KOL2{subset}")
  }
  
  for(formula in formulas){
    formula <- trimws(formula)
    print_console_message("\n*** Legger til kolonner som sum av rader: ", formula)
    fparts <- extract_formula_parts(formula = formula, fileinfo = fileinfo, parameters = parameters)
    
    newdata <- EkstraherRadSummer(dt, pstrorg = fparts$filter, FGP = fileinfo, parameters = parameters)
    oldcols <- paste0(fparts$old, c("", ".f", ".a"))
    newcols <- paste0(fparts$new, c("", ".f", ".a"))
    data.table::setnames(newdata, oldcols, newcols)
    newdims <- get_dimension_columns(names(newdata))
    merge_cols_by_reference(orgdata = dt, newdata = newdata[, .SD, .SDcols = c(newdims, newcols)])
    valdef <- fileinfo$vals
    valdef[fparts$new] <- ifelse(grepl("BEF_GKny", fileinfo$FILGRUPPE, ignore.case = T), valdef["BEF"], valdef[fparts$old])
    set_implicit_null_after_merge(dt = dt, implicitnull_defs = valdef)
  }
  return(invisible(dt))
}

#' @keywords internal
#' @noRd
extract_formula_parts <- function(formula, fileinfo, parameters){
  fparts <- list()
  fparts[["new"]] <- sub("^(.+?)\\s*=.*", "\\1", formula)
  fparts[["old"]] <- sub("^(.*)=(.*)\\{.*", "\\2", formula)
  fparts[["filter"]] <- format_filter(formula = formula, fileinfo = fileinfo)
  return(fparts)
}

#' @keywords internal
#' @noRd
format_filter <- function(formula, fileinfo){
  filter <- sub("^(.*)=(.*)\\{(.*)\\}", "\\3", formula)
  filter <- gsub("(?<!=)=(?!=)", "==", filter, perl = TRUE)
  filter <- gsub("==(?=c\\()", " %in% ", filter, perl = TRUE)
  if(grepl("^ALDER|^AAR", filter)){
    filter <- gsub("ALDER==\"ALLE\"", paste0("ALDERl==", fileinfo$amin, " & ALDERh==", fileinfo$amax), filter)
    filter <- gsub("ALDER==(\\d+)$", "ALDERl==\\1 & ALDERh == \\2", filter)
    filter <- gsub("AAR==(\\d+)$", "AARl==\\1 & AARh == \\2", filter)
  }
  return(filter)
}

#' @keywords internal
#' @noRd
add_filtercols_and_recodecols <- function(fparts, parameters){
  filters <- unlist(strsplit(fparts$filter, " *& *"))
  fparts[["filtercols"]] <- gsub("^(\\S+)\\s*(?:%in%|==).*$", "\\1", filters)
  fparts[["filtervals"]] <- gsub("^(?:\\S+)\\s*(?:%in%|==)\\s*(.*)$", "\\1", filters)
  fparts[[""]]  <- fparts$filtercols[fparts$filtercols %in% parameters$DefDesign$DesignKols]
  return(fparts)
}

#' @title merge_cols_by_reference
#' @description
#' Adds columns from newdata to orgdata by reference
#' @keywords internal
#' @noRd
merge_cols_by_reference <- function(orgdata, newdata){
  commoncols <- intersect(
    get_dimension_columns(names(orgdata)),
    get_dimension_columns(names(newdata))
  )
  newcols_names <- setdiff(names(newdata), commoncols)
  
  dup_check <- newdata[, .N, by = commoncols][N > 1]
  
  if (nrow(dup_check) > 0) {
    stop(
      sprintf(
        "merge_cols_by_reference(): newdata har duplikate nøkler i commoncols (%s). Eksempel:\n%s",
        paste(commoncols, collapse = ", "),
        paste(utils::capture.output(print(head(dup_check))), collapse = "\n")
      ),
      call. = FALSE
    )
  }
  
  newcols_vals <- newdata[orgdata, on = commoncols, ..newcols_names]
  data.table::set(orgdata, j = newcols_names, value = newcols_vals)
}

# Existing functions to be replaced and removed ---- 

#' @title EkstraherRadSummer (kb) 
#' @keywords internal
#' @noRd
EkstraherRadSummer <- function(dt, pstrorg, FGP = list(amin = 0, amax = 120), parameters) {
  dt <- data.table::copy(dt)
  amin <- FGP$amin
  amax <- FGP$amax
  
  pstrorg <- gsub("(?<!=)=(?!=)", "==", pstrorg, perl = TRUE)
  pstrorg <- gsub(" *== *(?=c\\()", " %in% ", pstrorg, perl = TRUE)
  
  pstrorg <- gsub("(^ *|& *)ALDER( *&| *$)", "ALDER==\\1\"ALLE\"\\2", pstrorg)
  pstrorg <- gsub("(^ *|& *)(KJONN|UTD|LAND|INNVKAT)( *&| *$)", "\\1\\2==0\\3", pstrorg)
  
  # Er det mulig aa abstrahere her, dvs aa ta alle "INT"-deler med samme syntaks???
  pstrorg <- gsub("ALDER *(={1,2}) *\"*ALLE\"*", paste0("ALDERl==", amin, " & ALDERh==", amax), pstrorg)
  pstrorg <- gsub("ALDER *(={1,2}) *(\\d+)$", "ALDERl==\\2 & ALDERh==\\2", pstrorg)
  pstrorg <- gsub("AAR *(={1,2}) *(\\d+)$", "AARl==\\2 & AARh==\\2", pstrorg)
  
  alletabs <- stringr::str_replace(unlist(stringr::str_split(pstrorg, " *& *")), "^(\\w*?) *(%in%.*|==.*| *$)", "\\1")
  
  # Fjern de som ikke er del av subset betingelse
  subsetstr <- gsub("^ *\\w+ *(&|$)|(^|&) *\\w+ *$", "", pstrorg, perl = TRUE)
  subsetstr <- gsub("& *\\w+ *&", "&", subsetstr, perl = TRUE)
  
  # Splitt i kolonnenavn og verdi
  subtabs <- stringr::str_replace(unlist(stringr::str_split(subsetstr, " *& *")), "^(\\w+) *(%in%.*|==.*)", "\\1")
  subvals <- stringr::str_replace(unlist(stringr::str_split(subsetstr, " *& *")), "^.*(%in% *|== *)(\\w+)", "\\2")
  subvals <- setNames(subvals, subtabs)
  # Filtrer til de som er aktuelle for omkoding
  subvals <- subvals[names(subvals) %in% parameters$DefDesign$DesignKols]
  
  # Omkod disse
  if (length(subvals) > 0) {
    # For omkodbare kolonner maa disse omkodes til soekte verdier (for generalitet maa det omkodes selv om disse finnes)
    OmkParts <- list()
    for (del in names(parameters$DefDesign$DelKols)) {
      if (all(parameters$DefDesign$DelKols[[del]] %in% names(subvals))) {
        dvals <- subvals[parameters$DefDesign$DelKols[[del]]]
        if (parameters$DefDesign$DelFormat[[del]] == "integer") {
          dvals <- setNames(as.integer(dvals), names(dvals))
        }
        OmkParts[[del]] <- setNames(data.frame(matrix(dvals, ncol = length(dvals))), names(dvals))
      } else if (any(parameters$DefDesign$DelKols[[del]] %in% names(subvals))) {
        print("VARSKU HER!!!!!!!!!!!!!!! FEIL i EkstraherRadSummer!")
      }
    }
    # print("Til OmkodFil fra EkstraherRadSummer, dette kan fort gi udekt ved ubalansert design. Dette faller bort igjen ved NF[TNF")
    orgdesign <- find_filedesign(dt, parameters = parameters)
    redesign <- find_redesign(orgdesign = orgdesign, targetdesign = list(Parts = OmkParts), parameters = parameters)
    dt <- do_filter_and_recode_to_redesign(dt = dt, redesign = redesign, parameters = parameters)
  }
  if(subsetstr != "") dt <- do_filter_columns(dt, subsetstr)
  dt <- do_aggregate_file(dt[, .SD, .SDcols = names(dt)[!names(dt) %in% alletabs]])
  return(dt)
}

# TO DELETE ---- 
