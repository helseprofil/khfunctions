#' @title merge_teller_nevner
#' @description
#' Merge TELLER/NEVNER files, create initial KUBE and update the empty data.table by reference. 
#' Return the initial design, to be used later. 
#' 
#' @param outdata empty data.table, to be updated by reference
#' @param parameters parameters generated with get_cubeparameters()
#' @param standardfiles Should standard teller and nevner files be used? default = FALSE
#' @param design Design list
merge_teller_nevner <- function(outdata, parameters, standardfiles = FALSE, design = NULL){
  if(standardfiles){
    print_console_message("\n\n* Merger standardteller- og standardnevnerfil\n")
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
  
  tellerfiltype <- ifelse(standardfiles, "standardtellerfil", "tellerfil")
  print_console_message("\n** Lager", tellerfiltype, "fra", tellerfilnavn, "\n")
  do_redesign_file(filename = tellerfilnavn, filedesign = tellerfildesign, tndesign = TNdesign, parameters = parameters, name = tellerfiltype)
  
  if(isnevnerfil) {
    nevnerfiltype <- ifelse(standardfiles, "standardnevnerfil", "nevnerfil")
    print_console_message("\n* Lager", nevnerfiltype, "fra", nevnerfilnavn, "\n")
    do_redesign_file(filename = nevnerfilnavn, filedesign = nevnerfildesign, tndesign = TNdesign, parameters = parameters, name = nevnerfiltype)
  }
  
  implicitnull_defs <- parameters$fileinformation[[tellerfilnavn]]$vals
  if(isnevnerfil) implicitnull_defs <- c(implicitnull_defs, parameters$fileinformation[[nevnerfilnavn]]$vals)
  
  tnftype <- ifelse(standardfiles, "standardTNF", "TNF")
  
  if (length(KUBEdesign) > 0) {
    print_console_message("\n** Rektangulariserer")
    set_rectangularized_cube_design(colnames = DBI::dbListFields(parameters$duck, tellerfiltype), 
                                    design = KUBEdesign$TMP, parameters = parameters, tnfname = tnftype)
    report_removed_codes(orgtable = tellerfiltype, recttable = tnftype, parameters = parameters)
    merge_duckdb_table(result = tnftype, mergeto = tnftype, mergefrom = tellerfiltype, con = parameters$duck)
    if(isnevnerfil){
      merge_duckdb_table(result = tnftype, mergeto = tnftype, mergefrom = nevnerfiltype, con = parameters$duck)
    }
    set_implicit_null_after_merge_duckdb(table = tnftype, implicitnull_defs = implicitnull_defs, con = parameters$duck)
    print_console_message("\n*** Ferdig rektangularisert og merget teller-nevner-fil")
  } else if (isnevnerfil) {
    merge_duckdb_table(result = tnftype, mergeto = tellerfiltype, mergefrom = nevnerfiltype, con = parameters$duck)
    set_implicit_null_after_merge_duckdb(table = tnftype, implicitnull_defs = implicitnull_defs, con = parameters$duck)
    # TNF <- collapse::join(tellerfil, nevnerfil, how = "l", overid = 0, verbose = 0)
    # rm(tellerfil, nevnerfil)
    # set_implicit_null_after_merge(dt = TNF, implicitnull_defs = implicitnull_defs)
    print_console_message("\n*** Ferdig merget teller-nevner-fil")
  } else {
    invisible(
      DBI::dbExecute(parameters$duck, paste0("CREATE OR REPLACE TABLE ", tnftype, " AS SELECT * FROM ", tellerfiltype))
    )
    # TNF <- tellerfil
    # rm(tellerfil)
    print_console_message("\n*** Ferdig merget teller-nevner-fil, har ikke nevnerfil, så TNF == tellerfil")
  }
  
  TNF <- data.table::setDT(DBI::dbReadTable(parameters$duck, tnftype))
  
  isNYEKOL_RAD <- is_not_empty(parameters$TNPinformation$NYEKOL_RAD)
  if(isNYEKOL_RAD) compute_new_value_from_row_sum(dt = TNF, formulas = parameters$TNPinformation$NYEKOL_RAD, fileinfo = parameters$fileinformation[[tellerfilnavn]], parameters = parameters)
  isNYEKOL_KOL <- is_not_empty(parameters$TNPinformation$NYEKOL_KOL)
  if(isNYEKOL_KOL) compute_new_value_from_formula(dt = TNF, formulas = parameters$TNPinformation$NYEKOL_KOL, post_moving_average = FALSE)
  
  dimorg <- dim(TNF)
  TNF <- do_filter_file(file = TNF, design = KUBEdesign$MAIN, parameters = parameters)
  if (!identical(dimorg, dim(TNF))) print_console_message("\n*** Siste filtrering til kubedesign, hadde dim:", dimorg, "fikk dim:", dim(TNF), "\n")
  
  TNF <- set_teller_nevner_names(file = TNF, TNPparameters = parameters$TNPinformation)
  outdata[, (names(TNF)) := TNF]
  rm(TNF)
  do_clean_duckdb(parameters = parameters)
  return(KUBEdesign$MAIN)
}

set_initial_spvtmp <- function(dt){
  tncols <- intersect(c("TELLER.f", "NEVNER.f"), names(dt))
  if (length(tncols) > 0L) {
    dt[, let(spv_tmp = do.call(pmax, c(.SD, list(na.rm = TRUE)))), .SDcols = tncols]
  }
}

#' @title get_initialdesign
#' @param design 
#' @param tellerfildesign 
#' @param nevnerfildesign 
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
#'
#' @param DF1 
#' @param DF2 
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
#' @param ORGd 
#' @param FGP 
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
#' @param KUBEdscr 
#' @param ORGd 
#' @param bruk0 finn design med _0-kolonnene, brukes  
#' @param FGP 
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

#' @title do_redesign_file
#'
#' @param filename 
#' @param filedesign 
#' @param tndesign 
#' @param parameters global parameters
#' @keywords internal
#' @noRd
do_redesign_file <- function(filename, filedesign, tndesign, parameters, name){
  redesign <- find_redesign(orgdesign = filedesign, targetdesign = tndesign, parameters = parameters)
  if(nrow(redesign$Udekk) > 0) print_console_message("\n**Filen", filename, "mangler tall for ", nrow(redesign$Udekk), "strata. Disse får flagg = 9 under omkoding")
  file <- do_filter_and_recode_to_redesign(dt = fetch_filegroup_from_duckdb(filegroup = filename, parameters = parameters),
                                           redesign = redesign, parameters = parameters)
  print_console_message("\n*** Skriver", name, "til duckdb...\n")
  DBI::dbWriteTable(parameters$duck, name = name, value = file, overwrite = T)
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
  
  print_console_message("\n*** Skriver", tnfname, "til duckdb...\n")
  DBI::dbWriteTable(parameters$duck, name = tnfname, value = rektangularisert, overwrite = T)
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

#' @title do_filter_file
#'
#' @param file 
#' @param design 
#' @param parameters global parameters
do_filter_file <- function(file, design, parameters){
  for (del in names(design)) {
    cols <- parameters$DefDesign$DelKols[[del]]
    if (all(cols %in% names(file))) {
      file <- collapse::join(file, design[[del]][, ..cols], on = cols, how = "right", multiple = T, overid = 0, verbose = 0)
    }
  }
  return(file)
}

#' @title set_teller_nevner_names
#' @description Sets name of teller and nevner column to TELLER and NEVNER by reference
set_teller_nevner_names <- function(file, TNPparameters){
  newnames <- gsub(paste0("^", TNPparameters$TELLERKOL, "(\\.f|\\.a|)$"), "TELLER\\1", names(file))
  newnames <- gsub(paste0("^", TNPparameters$NEVNERKOL, "(\\.f|\\.a|)$"), "NEVNER\\1", newnames)
  # warn_duplicated_teller_nevner_names(TNPparameters$TELLERKOL, TNPparameters$NEVNERKOL, names(file))
  data.table::setnames(file, names(file), newnames)
  warn_duplicated_column_names(names(file))
  return(file)
}

warn_duplicated_column_names <- function(columnnames){
  if(any(duplicated(columnnames))){
    message(paste0("\nNB!!! DUPLICATED COLUMN NAMES!",
                   "\nThe following column names were duplicated when trying to set TELLER and NEVNER according to what is provided in TNP_PROD:\n", 
                   paste(" -", columnnames[duplicated(columnnames)], collapse = "\n"),
                   "\nAre you trying to e.g. add a separate NEVNER file to a file already containing NEVNER?"))
  } 
}
