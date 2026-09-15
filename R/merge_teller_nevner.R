#' @title merge_teller_nevner
#' @description
#' Merge TELLER/NEVNER files, create initial KUBE and update the empty data.table by reference. 
#' Return the initial design, to be used later. 
#' 
#' @param parameters parameters generated with get_cubeparameters()
#' @param standardfiles Should standard teller and nevner files be used? default = FALSE
#' @param design Design list
merge_teller_nevner <- function(parameters, standardfiles = FALSE, design = NULL){
  if(standardfiles){
    print_console_message("\n* Merger standardteller- og standardnevnerfil\n")
    tellerfile <- "STANDARDTELLER"
    nevnerfile <- "STANDARDNEVNER"
  } else {
    print_console_message("\n* Merger teller- og nevnerfil")
    tellerfile <- "TELLER"
    nevnerfile <- "NEVNER"
  }
  tellerfilnavn <- parameters$files[[tellerfile]]
  tellerfildesign <- parameters$filedesign[[tellerfilnavn]]
  isnevnerfil <- is_not_empty(parameters$files[[nevnerfile]])
  nevnerfildesign <- NULL
  if(isnevnerfil) {
    nevnerfilnavn <- parameters$files[[nevnerfile]]
    nevnerfildesign <- parameters$filedesign[[nevnerfilnavn]]
  }
  InitDesign <- get_initialdesign(design = design, tellerfildesign = tellerfildesign, nevnerfildesign = nevnerfildesign, parameters = parameters)
  
  if(!standardfiles){
    KUBEdesign <- FinnKubeDesignB(InitDesign = InitDesign, filename = tellerfilnavn, parameters = parameters)
    TNdesign <- list(Part = KUBEdesign$TMP)
  } else {
    KUBEdesign <- list()
    TNdesign <- InitDesign
  }
  
  con <- parameters$duck
  
  tablename_teller <- ifelse(standardfiles, "STANDARD_TELLER", "TELLER")
  print_console_message("** Lager", tablename_teller, "fra", tellerfilnavn)
  do_redesign_file_duckdb(con = con, tablename = tablename_teller, orgfilename = tellerfilnavn,
                          filedesign = tellerfildesign, targetdesign = TNdesign, parameters = parameters)
  
  if(isnevnerfil) {
    tablename_nevner <- ifelse(standardfiles, "STANDARD_NEVNER", "NEVNER")
    print_console_message("\n** Lager", tablename_nevner, "fra", nevnerfilnavn)
    do_redesign_file_duckdb(con = con, tablename = tablename_nevner, orgfilename = nevnerfilnavn,
                            filedesign = nevnerfildesign, targetdesign = TNdesign, parameters = parameters)
  }
  
  implicitnull_defs <- parameters$fileinformation[[tellerfilnavn]]$vals
  if(isnevnerfil) implicitnull_defs <- c(implicitnull_defs, parameters$fileinformation[[nevnerfilnavn]]$vals)
  
  tntype <- ifelse(standardfiles, "STANDARD_KUBE", "KUBE")
  
  if(length(KUBEdesign) > 0) {
    print_console_message("\n** Rektangulariserer")
    set_rectangularized_cube_design(colnames = DBI::dbListFields(parameters$duck, tablename_teller), 
                                    design = KUBEdesign$TMP, parameters = parameters, tnfname = tntype)
    report_removed_codes(orgtable = tablename_teller, recttable = tntype, parameters = parameters)
    merge_duckdb_table(con = con, mergeto = tntype, mergefrom = tablename_teller)
    if(isnevnerfil){
      merge_duckdb_table(con = con, mergeto = tntype, mergefrom = tablename_nevner)
    }
    set_implicit_null_after_merge_duckdb(table = tntype, implicitnull_defs = implicitnull_defs, con = con)
    print_console_message("\n* Ferdig rektangularisert og merget", tntype)
  } else if (isnevnerfil) {
    merge_duckdb_table(result = tntype, mergeto = tablename_teller, mergefrom = tablename_nevner, con = con)
    set_implicit_null_after_merge_duckdb(table = tntype, implicitnull_defs = implicitnull_defs, con = con)
    print_console_message("\n* Ferdig merget", tntype)
  } else {
    invisible(
      DBI::dbExecute(con, paste0("CREATE OR REPLACE TABLE ", tntype, " AS SELECT * FROM ", tablename_teller))
    )
    print_console_message("\n* Ferdig merget", tntype, ". Har ikke nevnerfil, så", tntype, " = tellerfil")
  }
  
  isNYEKOL_RAD <- is_not_empty(parameters$TNPinformation$NYEKOL_RAD)
  isNYEKOL_KOL <- is_not_empty(parameters$TNPinformation$NYEKOL_KOL)
  if(isNYEKOL_RAD || isNYEKOL_KOL){
    dt <- fetch_duckdb_table(con = con, tablename = tntype)
    if(isNYEKOL_RAD) compute_new_value_from_row_sum(dt = dt, formulas = parameters$TNPinformation$NYEKOL_RAD, fileinfo = parameters$fileinformation[[tellerfilnavn]], parameters = parameters)
    if(isNYEKOL_KOL) compute_new_value_from_formula(dt = dt, formulas = parameters$TNPinformation$NYEKOL_KOL, post_moving_average = FALSE)
    write_duckdb_table(con, tablename = tntype, data = dt)
  }
  
  do_filter_dimensions_duckdb(con = con, tablename = tntype, filters = KUBEdesign$MAIN)
  set_teller_nevner_names_duckdb(con = con, tablename = tntype, TNPparameters = parameters$TNPinformation)
  do_clean_duckdb(con = parameters$duck)
  if(!standardfiles) parameters[["CUBEdesign"]] <- KUBEdesign$MAIN
  return(invisible(parameters))
}

#' @title get_initialdesign
#' @param parameters global parameters
get_initialdesign <- function(design, tellerfildesign, nevnerfildesign, parameters){
  
  if(!is.null(design)) return(design)
  if(is.null(nevnerfildesign)) return(tellerfildesign)
  
  fellesdesign <- FinnFellesTab(DF1 = tellerfildesign, DF2 = nevnerfildesign, parameters = parameters)
  for(del in setdiff(names(tellerfildesign$Part), names(nevnerfildesign$Part))) {
    fellesdesign$Part[[del]] <- tellerfildesign$Part[[del]]
  }
  #Potensielt unødvendig, er det noen gang
  for(del in setdiff(names(nevnerfildesign$Part), names(tellerfildesign$Part))) {
    fellesdesign$Part[[del]] <- nevnerfildesign$Part[[del]]
  }
  return(fellesdesign)
}

#' FinnFellesTab (kb)
#' @param parameters global parameters
FinnFellesTab <- function(DF1, DF2, parameters) {
  print_console_message("Starter i FinnFellesTab.")
  FTabs <- list()
  for (del in intersect(names(DF1$Part), names(DF2$Part))) {
    FTabs[[del]] <- unique(rbind(DF1$Part[[del]], DF2$Part[[del]]))
  }
  RD1 <- find_redesign(orgdesign = DF1, targetdesign = list(Part = FTabs), parameters = parameters)$FULL
  RD2 <- find_redesign(orgdesign = DF2, targetdesign = list(Part = FTabs), parameters = parameters)$FULL
  omktabs <- grep("_omk$", names(RD1), value = T)
  Dekk <- collapse::join(unique(RD1[, ..omktabs]), 
                         unique(RD2[, ..omktabs]), 
                         how = "i", on = omktabs, overid = 2, verbose = 0)
  rm(RD1, RD2)
  gc()
  data.table::setnames(Dekk, old = names(Dekk), new = gsub("_omk$", "", names(Dekk)))
  FDes <- find_filedesign(Dekk, parameters = parameters)
  print_console_message(" Ferdig i FinnFellesTab\n")
  return(FDes)
}

#' FinnKubeDesignB (kb)
#'
#' @param parameters global parameters
#' @keywords internal
#' @noRd
FinnKubeDesignB <- function(InitDesign, filename, parameters) {
  KubeD <- list(
    TMP = FinnKubeDesign(parameters$CUBEinformation, InitDesign, bruk0 = TRUE, FGP = parameters$fileinformation[[filename]], parameters = parameters),
    MAIN = FinnKubeDesign(parameters$CUBEinformation, InitDesign, bruk0 = FALSE, FGP = parameters$fileinformation[[filename]], parameters = parameters)
  )
  return(KubeD)
}

#' FinnKubeDesign (kb)
#'
#' @param bruk0 finn design med _0-kolonnene, brukes  
#' @param parameters global parameters
#' @keywords internal
#' @noRd
FinnKubeDesign <- function(KUBEdscr, ORGd, bruk0 = TRUE, FGP = list(amin = 0, amax = 120), parameters) {
  Deler <- list()
  for (del in names(parameters$DefDesign$DelKolN)) {
    if (del %in% names(ORGd$Part)) {
      koldel <- parameters$DefDesign$DelKolN[del]
      koldel0 <- paste0(koldel, "_0")
      iskoldel0 <- !is.null(KUBEdscr[[koldel0]]) && !is.na(KUBEdscr[[koldel0]]) && KUBEdscr[[koldel0]] != ""
      
      if (bruk0 == TRUE && iskoldel0) {
        delListStr <- KUBEdscr[[koldel0]]
      } else {
        delListStr <- KUBEdscr[[koldel]]
      }
      isdelListStr <- !is.null(delListStr) && !is.na(delListStr) && delListStr != ""
      if (isdelListStr) {
        minus <- grepl("^-\\[", delListStr)
        delListStr <- gsub("^-\\[(.*)\\]$", "\\1", delListStr)
        delListA <- unlist(stringr::str_split(delListStr, ","))
        if (parameters$DefDesign$DelType[del] == "INT") {
          if (del == "A") {
            delListA <- gsub("ALLE", paste0(FGP$amin, "_", FGP$amax), delListA)
            delListA <- gsub("^_(\\d+)", paste0(FGP$amin, "_\\1"), delListA)
            delListA <- gsub("(\\d+)_$", paste0("\\1_", FGP$amax), delListA)
          }
          delListA <- gsub("^(\\d+)$", "\\1_\\1", delListA)
          delListA <- data.table::as.data.table(matrix(as.integer(stringr::str_split_fixed(delListA, "_", 2)), ncol = 2))
        } else if (parameters$DefDesign$DelFormat[del] == "integer") {
          delListA <- as.integer(delListA)
        } else if (parameters$DefDesign$DelFormat[del] == "numeric") {
          delListA <- as.numeric(delListA)
        }
        listDT <- data.table::setnames(data.table::as.data.table(delListA), parameters$DefDesign$DelKols[[del]])
        if (minus == TRUE) {
          data.table::setkeyv(listDT, data.table::key(ORGd$Part[[del]]))
          Deler[[del]] <- ORGd$Part[[del]][!listDT, ]
        } else {
          Deler[[del]] <- listDT
        }
      } else if (parameters$DefDesign$DelType[del] == "INT") {
        delN <- parameters$DefDesign$DelKolN[del]
        start <- KUBEdscr[[paste0(delN, "_START")]]
        stopp <- KUBEdscr[[paste0(delN, "_STOP")]]
        if (!(is.null(start) | is.null(stopp))) {
          if(start > stopp) stop(paste0("Kan ikke ha ACCESS::KUBER::", delN, "start (", start, ") > ", delN, "stopp (", stopp, ")"))
          delL <- paste0(delN, "l")
          delH <- paste0(delN, "h")
          DT <- data.table::copy(ORGd$Part[[del]])
          Deler[[del]] <- DT[DT[[delL]] >= start & DT[[delH]] <= stopp, .SD, .SDcols = c(delL, delH)]
        } else {
          Deler[[del]] <- ORGd$Part[[del]]  
        }
      } else {
        Deler[[del]] <- ORGd$Part[[del]][, ..koldel]
      }
    } 
  }
  return(Deler)
}

#' @title do_redesign_file_duckdb
#' @description
#' Tar originalfilgruppene som er lest inn, og bruker ønsket design til å filtrere og kode om verdier for videre bruk
#' @noRd
do_redesign_file_duckdb <- function(con, tablename, orgfilename, filedesign, targetdesign, parameters){
  invisible(DBI::dbExecute(con, sprintf("CREATE OR REPLACE TABLE %s AS SELECT * FROM %s", tablename, orgfilename)))
  redesign <- find_redesign(orgdesign = filedesign, targetdesign = targetdesign, parameters = parameters)
  if(nrow(redesign$Udekk) > 0) print_console_message("\n**Filen", filename, "mangler tall for ", nrow(redesign$Udekk), "strata. Disse får flagg = 9 under omkoding")
  filter_and_recode_table_duckdb(con = con, tablename = tablename, redesign = redesign, parameters = parameters)
}



#' @title set_rectangularized_cube_design
#' @description
#' rectangularizes cube based on the given design, writes to duckdb
#' @keywords internal
#' @noRd
set_rectangularized_cube_design <- function(colnames, design, parameters, tnfname) {
  DTlist <- list()
  delkolonner <- character(0)
  for (del in names(design)) {
    delkols <- parameters$DefDesign$DelKols[[del]]
    if (all(delkols %in% colnames)) {
      delkolonner <- c(delkolonner, delkols)
      DTlist[[del]] <- data.table::setDT(design[[del]])
    }
  }
  deler <- do.call(expand.grid.dt, DTlist)[, ..delkolonner]
  
  rektangularisert <- data.table::data.table()    
  for (Gn in design[["Gn"]][["GEOniv"]]) {
    GEOK <- parameters$GeoKoder[GEOniv == Gn & FRA <= parameters$year & TIL > parameters$year]
    subfylke <- which(GEOK$GEOniv %in% c("G", "V", "S", "K", "F", "B"))
    GEOK$FYLKE <- NA_character_
    GEOK$FYLKE[subfylke] <- substr(GEOK$GEO[subfylke], 1, 2)
    GEOK$FYLKE[GEOK$GEOniv %in% c("H", "L")] <- "00"
    DELERg <- subset(deler, GEOniv == Gn)
    rektangularisert <- data.table::rbindlist(list(expand.grid.dt(DELERg, GEOK[, .(GEO, FYLKE)]),
                                                   rektangularisert))
  }
  
  print_console_message("- Skriver rektangularisert", paste0(tnfname, "-design"), "til duckdb...")
  drop_tables_duckdb(parameters$duck, tnfname)
  write_duckdb_table(parameters$duck, tablename = tnfname, data = rektangularisert)
  # DBI::dbWriteTable(parameters$duck, name = tnfname, value = rektangularisert, overwrite = T)
}

#' @title get_removed_codes
#' @description fetches info on geo codes removed during rectangularization
report_removed_codes <- function(orgtable, recttable, parameters){
  orggeo <- DBI::dbGetQuery(parameters$duck, paste0("SELECT DISTINCT GEO FROM ", orgtable))[[1]]
  rectgeo <- DBI::dbGetQuery(parameters$duck, paste0("SELECT DISTINCT GEO FROM ", recttable))[[1]]
  remove <- setdiff(orggeo, rectgeo)
  if(length(remove) == 0) return(invisible(NULL))
  remove_valid <- remove[!grepl("99$", remove)]
  if (length(remove_valid) > 0) {
    print_console_message("!! GEO ", paste(remove_valid, collapse = ","), " kastes ved rektangularisering!!\n")
    print_console_message("!! Dessuten kastes ", length(setdiff(remove, remove_valid)), "99-koder!\n")
    r <- paste0(remove, collapse = ", ")
    print(DBI::dbGetQuery(parameters$duck, paste0("SELECT * FROM ", orgtable, " WHERE GEO IN (", r, ")")))
    print_console_message("#---#\n")
  } else if (length(setdiff(remove, remove_valid)) > 0) {
    print_console_message("Kaster ", length(setdiff(remove, remove_valid)), "99-koder ved rektangulerisering.\n")
  }
}

set_teller_nevner_names_duckdb <- function(con, tablename, TNPparameters) {
  
  cols <- DBI::dbListFields(con, tablename)
  newnames <- gsub(sprintf("^%s(\\.f|\\.a|)$", TNPparameters$TELLERKOL), "TELLER\\1", cols)
  newnames <- gsub(sprintf("^%s(\\.f|\\.a|)$", TNPparameters$NEVNERKOL), "NEVNER\\1", newnames)
  
  dup_names <- unique(newnames[duplicated(newnames)])
  if(length(dup_names)){
    warning(paste0("\nNB!!! DUPLICATED COLUMN NAMES!",
        "\nThe following column names were duplicated when trying to set ",
        "TELLER and NEVNER according to what is provided in TNP_PROD:\n",
        paste(" -", dup_names, collapse = "\n"), "\nAre you trying to e.g. add a separate NEVNER file to a file already containing NEVNER?"),
      call. = FALSE, immediate. = TRUE)
  }
  
  idx <- which(cols != newnames)
  cols <- DBI::dbQuoteIdentifier(con, cols)
  newnames <- DBI::dbQuoteIdentifier(con, newnames)
  
  sql <- sprintf("ALTER TABLE %s RENAME COLUMN %s TO %s",
                 tablename, cols[idx], newnames[idx])
  sql <- paste(sql, collapse = ";\n")
  invisible(DBI::dbExecute(con, sql))
  invisible(NULL)
}

