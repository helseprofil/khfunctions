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
  if(is_empty(formulas)) return(invisible(NULL))
  values <- get_value_columns(names(dt))
  formulas <- trimws(unlist(strsplit(formulas, ";")))
  for(f in formulas){
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
      dt[, (paste0(name, c(".n", ".fn1", ".fn3", ".fn9"))) := list(1,0,0,0)]
    }  
    dt[is.na(get(name)) | is.infinite(get(name)) | is.nan(get(name)), (paste0(name, c("", ".f"))) := list(NA, 2)]
  }
}

#' @title compute_new_value_from_strata_sum
#' @description
#' WIP: replacement for LeggTilSumFraRader()
#' 
#' Used to compute columns described in ACCESS columns
#' - FILFILTRE::NYEKOL_RAD
#' - TNP_PROD::NYEKOL_RAD
#' @param dt 
#' @keywords internal
#' @noRd
compute_new_value_from_strata_sum <- function(dt, formulas, fileinfo, parameters){
  formulas <- trimws(unlist(strsplit(formulas, ";")))
  for(f in formulas){
    # f <- gsub("\\s=|=\\s|\\s=\\s", "=", f)
    if(!grepl("^(.+)=(.+)\\{(.+)\\}$", f)) stop('FEIL format i FILFILTRE::NYEKOL_RAD. Må være NY=sumkolonne{filter}, f.eks. TOTBEF=BEF{ALDER="ALLE"}, evt separert med ;')
    fparts <- extract_formula_parts(formula = f, fileinfo = fileinfo, parameters = parameters)
    newcol <- extract_strata_sums(dt = dt, fparts = fparts, parameters = parameters)
    # commoncols <- intersect(get_dimension_columns(names(dt)), get_dimension_columns(names(newcol)))
    # dt <- collapse::join(dt, newcol, on = commoncols, how = "l", overid = 2, verbose = 0)
    dt <- set_implicit_null_after_merge(file = dt, implicitnull_defs = fileinfo$vals[c(fparts$new, fparts$old)])
  } 
    
    ###
    # NF <- EkstraherRadSummer(dt = dt, pstrorg = formula, FGP = fileinfo, parameters = parameters)
    # gmlcols <- paste0(gmlcol, c("", ".f", ".a"))
    # nycols <- paste0(nycol, c("", ".f", ".a"))
    # data.table::setnames(NF, gmlcols, nycols)
    # commontabs <- intersect(get_dimension_columns(names(dt)), get_dimension_columns(names(NF)))
    # 
    # dt <- collapse::join(dt, NF, on = commontabs, how = "l", overid = 2, verbose = 0)
    # dt <- set_implicit_null_after_merge(file = dt, list(gmlcol = FGP$vals, nycol = FGP$vals[gmlcol]))
    
  return(dt)
}


#' @description
#' WIP: replace ekstraherradsummer, 
#' Create the sum based on the formula including the strata defined. 
#' @keywords internal
#' @noRd  
extract_strata_sums <- function(dt, fparts, parameters){
  d <- data.table::copy(dt)
  
  if(length(fparts$filtercols) > 0){
    
  }
  
  
  
  if(subsetstr != "") dt <- eval(parse(text = paste0("subset(dt,", subsetstr, ")"))) # BYTTE MED do_filter_columns????
  dt <- do_aggregate_file(dt[, .SD, .SDcols = names(dt)[!names(dt) %in% alletabs]])
  return(dt)
  
  return(d)
}

#' @keywords internal
#' @noRd
extract_formula_parts <- function(formula, fileinfo, parameters){
  fparts <- list()
  fparts[["new"]] <- sub("^(.*)=(.*)\\{(.*)\\}", "\\1", formula)
  fparts[["old"]] <- sub("^(.*)=(.*)\\{(.*)\\}", "\\2", formula)
  fparts[["filter"]] <- format_filter(formula = formula, fileinfo = fileinfo)
  fparts <- add_filtercols_and_recodecols(fparts = fparts, parameters = parameters)
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
  # delkols <- parameters$DefDesign$DelKols
  # fparts[["recodeparts"]] <- names(delkols)[sapply(delkols, function(x) all(x %in% fparts$filtercols))]
  # fparts[["recodecols"]] <- parameters$DefDesign[]  
  #  parameters$DefDesign$DelKols$A == fparts$filtercols
  return(fparts)
}

#' @description
#' 
#' 
#' @keywords internal
#' @noRd
get_recode_design <- function(filter){
  
  # Omkod disse
  if (length(subvals) > 0) {
    # For omkodbare kolonner maa disse omkodes til soekte verdier (for generalitet maa det omkodes selv om disse finnes)
    OmkParts <- list()
    parameters$DefDesign$DelKols
    for (part in names(parameters$DefDesign$DelKols)) {
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
    print("Til OmkodFil fra EkstraherRadSummer, dette kan fort gi udekt ved ubalansert design. Dette faller bort igjen ved NF[TNF")
    orgdesign <- find_filedesign(dt, parameters = parameters)
    redesign <- find_redesign(orgdesign = orgdesign, targetdesign = list(Parts = OmkParts), parameters = parameters)
    dt <- do_filter_and_recode_to_redesign(dt = dt, redesign = redesign, parameters = parameters)
  }
}

#' @title set_lead_value
#' 
#' @description
#' CAN REPLACE YALAGVAL, BUT MUST FIND A WAY OF USING IT IN ACCESS
#' MAKE NEW COLUMN "LAGVAL" 
#' @keywords internal
#' @noRd
set_lead_value <- function(file, yearlag = -1, vals = NULL){
  if(is.null(vals)) stop("vals must be specified to generate lead values")
  d <- data.table::copy(file)
  d[, lagAARl := AARl + yearlag]
  d[, c("AARl", "AARh") := list(NULL)]
  data.table::setnames(d, "lagAARl", "AARl")
  tabcols <- get_dimension_columns(names(d))
  # m/p means plus/minus
  lagprefix <- paste0("Y", ifelse(yearlag < 0, "p", "m"), abs(yearlag), "_A_") 
  oldvals <- paste0(rep(vals, each = 3), c("", ".f", ".a"))
  newvals <- paste0(lagprefix, allvals)
  data.table::setnames(d, old = allvals, new = newvals)
  d <- d[, mget(c(tabcols, newvals))]
  return(d)
}


# Existing functions to be replaced and removed ---- 

#' @title LeggTilSumFraRader (kb)
#' Helper function in KlargjorFil and LagTNtabell
#' @keywords internal
#' @noRd
LeggTilSumFraRader <- function(dt, NYdscr, FGP = list(amin = 0, amax = 120), parameters) {
  if(is_empty(NYdscr)) return(dt)
  
  for (sumfra in unlist(stringr::str_split(NYdscr, ";"))) {
    sumfra <- trimws(sumfra)
    if(!grepl("^(.+?) *= *(.+?)\\{(.*)\\}$", sumfra)) stop("FEIL!!!!!: FILFILTRE::NYEKOL_RAD har feil format:", NYdscr, "\n")
    cat("\n*** Legger til kolonner som sum av rader: ", sumfra)
    nycol <- gsub("^ *(.+?) *= *(.+?)\\{(.*)\\} *$", "\\1", sumfra)
    gmlcol <- gsub("^ *(.+?) *= *(.+?)\\{(.*)\\} *$", "\\2", sumfra)
    expr <- gsub("^ *(.+?) *= *(.+?)\\{(.*)\\} *$", "\\3", sumfra)
    NF <- EkstraherRadSummer(dt = dt, pstrorg = expr, FGP = FGP, parameters = parameters)
    gmlcols <- paste0(gmlcol, c("", ".f", ".a"))
    nycols <- paste0(nycol, c("", ".f", ".a"))
    data.table::setnames(NF, gmlcols, nycols)
    commontabs <- intersect(get_dimension_columns(names(dt)), get_dimension_columns(names(NF)))
    
    dt <- collapse::join(dt, NF, on = commontabs, how = "l", overid = 2, verbose = 0)
    dt <- set_implicit_null_after_merge(file = dt, list(gmlcol = FGP$vals, nycol = FGP$vals[gmlcol]))
  }
  return(dt)
}

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
    print("Til OmkodFil fra EkstraherRadSummer, dette kan fort gi udekt ved ubalansert design. Dette faller bort igjen ved NF[TNF")
    orgdesign <- find_filedesign(dt, parameters = parameters)
    redesign <- find_redesign(orgdesign = orgdesign, targetdesign = list(Parts = OmkParts), parameters = parameters)
    dt <- do_filter_and_recode_to_redesign(dt = dt, redesign = redesign, parameters = parameters)
  }
  if(subsetstr != "") dt <- eval(parse(text = paste0("subset(dt,", subsetstr, ")"))) # BYTTE MED do_filter_columns????
  dt <- do_aggregate_file(dt[, .SD, .SDcols = names(dt)[!names(dt) %in% alletabs]])
  return(dt)
}

#' @title YAlagVal (kb)
#' @description 
#' used in ACCESS, only for befvekst. AGE lag is not used, only year. 
#' Generate a new file with all dimension columns and the selected value column. 
#' AARl and ALDERl is changed by the factors specified in YL and AL, to be merged onto different rows. 
#' 
#' This function can probably be replaced by collapse-functions flag/flead. 
#' @param FG file
#' @param YL year lag
#' @param AL age lag
#' @param vals value to create lag value
#' @keywords internal
#' @noRd
YAlagVal <- function(FG, YL, AL, vals = get_value_columns(names(FG))) {
  ltag <- function(lag) {
    ltag <- ""
    if (lag > 0) {
      ltag <- paste("m", abs(lag), sep = "")
    } else if (lag < 0) {
      ltag <- paste("p", abs(lag), sep = "")
    }
    return(ltag)
  }
  FGl <- data.table::copy(FG)
  FGl[, c("lAARl", "lALDERl") := list(AARl + YL, ALDERl + AL)]
  FGl[, c("AARl", "AARh", "ALDERl", "ALDERh") := list(NULL)]
  data.table::setnames(FGl, c("lAARl", "lALDERl"), c("AARl", "ALDERl"))
  tabkols <- setdiff(names(FGl), get_value_columns(names(FG), full = TRUE))
  lvals <- paste("Y", ltag(YL), "_A", ltag(AL), "_", vals, c("", ".f", ".a"), sep = "")
  data.table::setnames(FGl, unlist(lapply(vals, function(x) {
    paste(x, c("", ".f", ".a"), sep = "")
  })), lvals)
  FGl <- FGl[, c(tabkols, lvals), with = FALSE]
  data.table::setkeyv(FG, tabkols)
  data.table::setkeyv(FGl, tabkols)
  return(FGl)
}
