#' merge_teller_nevner
#' 
#' @param parameters parameters generated with get_cubeparameters()
#' @param standardfiles Should standard teller and nevner files be used? default = FALSE
#' @param design Design list
merge_teller_nevner <- function(parameters, standardfiles = FALSE, design = NULL){
  if(standardfiles){
    cat("\n* Merger standardteller- og standardnevnerfil\n")
    tellerfile <- "STANDARDTELLER"
    nevnerfile <- "STANDARDNEVNER"
  } else {
    cat("\n* Merger teller- og nevnerfil\n")
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
  
  tellerfiltype <- ifelse(standardfiles, "standard tellerfil", "tellerfil")
  cat("\n* Lager", tellerfiltype, "fra", tellerfilnavn)
  tellerfil <- do_redesign_file(filename = tellerfilnavn, filedesign = tellerfildesign, tndesign = TNdesign, parameters = parameters)
  
  if(isnevnerfil) {
    nevnerfiltype <- ifelse(standardfiles, "standard nevnerfil", "nevnerfilfil")
    cat("\n* Lager", nevnerfiltype, "fra", nevnerfilnavn)
    nevnerfil <- do_redesign_file(filename = nevnerfilnavn, filedesign = nevnerfildesign, tndesign = TNdesign, parameters = parameters)
  }
  
  implicitnull_defs <- parameters$fileinformation[[tellerfilnavn]]$vals
  if(isnevnerfil) implicitnull_defs <- c(implicitnull_defs, parameters$fileinformation[[nevnerfilnavn]]$vals)
  
  if (length(KUBEdesign) > 0) {
    cat("\n** Rektangulariserer teller-nevner-fil")
    rectangularizedcube <- set_rectangularized_cube_design(colnames = names(tellerfil), design = KUBEdesign$TMP, parameters = parameters)
    report_removed_codes(file = tellerfil, cube = rectangularizedcube)
    TNF <- collapse::join(rectangularizedcube, tellerfil, how = "l", overid = 0, verbose = 0)
    if(isnevnerfil) TNF <- collapse::join(TNF, nevnerfil, how = "l", overid = 0, verbose = 0)
    TNF <- set_implicit_null_after_merge(file = TNF, implicitnull_defs = implicitnull_defs)
    cat("\n*** Ferdig rektangularisert og merget teller-nevner-fil, dim:", dim(TNF))
  } else if (isnevnerfil) {
    TNF <- collapse::join(tellerfil, nevnerfil, how = "l", overid = 0, verbose = 0)
    TNF <- set_implicit_null_after_merge(file = TNF, implicitnull_defs = implicitnull_defs)
    cat("\n*** Ferdig merget teller-nevner-fil, dim:", dim(TNF))
  } else {
    TNF <- tellerfil
    cat("\n*** Ferdig merget teller-nevner-fil, har ikke nevnerfil, så TNF == tellerfil. dim:", dim(TNF))
  }
  
  isNYEKOL_RAD <- is_not_empty(parameters$TNPinformation$NYEKOL_RAD)
  if(isNYEKOL_RAD) TNF <- LeggTilSumFraRader(dt = TNF, NYdscr = parameters$TNPinformation$NYEKOL_RAD, FGP = parameters$fileinformation[[tellerfilnavn]], parameters = parameters)
  isNYEKOL_KOL <- is_not_empty(parameters$TNPinformation$NYEKOL_KOL)
  if(isNYEKOL_KOL) compute_new_value_from_formula(dt = TNF, formulas = parameters$TNPinformation$NYEKOL_KOL, post_moving_average = FALSE)
  
  dimorg <- dim(TNF)
  TNF <- do_filter_file(file = TNF, design = KUBEdesign$MAIN, parameters = parameters)
  if (!identical(dimorg, dim(TNF))) cat("\n*** Siste filtrering til kubedesign, hadde dim:", dimorg, "fikk dim:", dim(TNF), "\n")
  
  TNF <- set_teller_nevner_names(file = TNF, TNPparameters = parameters$TNPinformation)
  return(list(TNF = TNF, KUBEd = KUBEdesign))
}

#' @title get_initialdesign
#' @param design 
#' @param tellerfildesign 
#' @param nevnerfildesign 
#' @param parameters global parameters
get_initialdesign <- function(design, tellerfildesign, nevnerfildesign, parameters){
  
  if(!is.null(design)) return(design)
  if(is.null(nevnerfildesign)) return(tellerfildesign)
  
  fellesdesign <- FinnFellesTab(DF1 = tellerfildesign, DF2 = nevnerfildesign, parameters = parameters)$FDes
  for(del in setdiff(names(tellerfildesign$Part), names(nevnerfildesign$Part))) {
    fellesdesign$Part[[del]] <- tellerfildesign$Part[[del]]
  }
  for(del in setdiff(names(nevnerfildesign$Part), names(tellerfildesign$Part))) {
    InitDes$Part[[del]] <- nevnerfildesign$Part[[del]]
  }
  return(fellesdesign)
}

#' FinnFellesTab (kb)
#'
#' @param DF1 
#' @param DF2 
#' @param parameters global parameters
FinnFellesTab <- function(DF1, DF2, parameters) {
  cat("Starter i FinnFellesTab.")
  FTabs <- list()
  for (del in intersect(names(DF1$Part), names(DF2$Part))) {
    FTabs[[del]] <- unique(rbind(DF1$Part[[del]], DF2$Part[[del]]))
  }
  # RD1 <- FinnRedesign(DF1, list(Part = FTabs), parameters = parameters)
  RD1 <- find_redesign(orgdesign = DF1, targetdesign = list(Part = FTabs), parameters = parameters)
  # RD2 <- FinnRedesign(DF2, list(Part = FTabs), parameters = parameters)
  RD2 <- find_redesign(orgdesign = DF2, targetdesign = list(Part = FTabs), parameters = parameters)
  ### kommet hit ---
  omktabs <- names(RD1$FULL)[grepl("_omk$", names(RD1$FULL))]
  data.table::setkeyv(RD1$FULL, omktabs)
  data.table::setkeyv(RD2$FULL, omktabs)
  Dekk1 <- unique(RD1$FULL[, omktabs, with = FALSE])
  Dekk2 <- unique(RD2$FULL[, omktabs, with = FALSE])
  Dekk12 <- Dekk1[Dekk2, nomatch = 0]
  data.table::setnames(Dekk12, names(Dekk12), gsub("_omk$", "", names(Dekk12)))
  FDes <- find_filedesign(Dekk12, parameters = parameters)
  cat(" Ferdig i FinnFellesTab\n")
  gc()
  return(list(Dekk = Dekk12, FDes = FDes))
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
          Deler[[del]] <- ORGd$Part[[del]][get(delL) >= start & get(delH) <= stopp, mget(c(delL, delH))]
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
do_redesign_file <- function(filename, filedesign, tndesign, parameters){
  # redesign <- FinnRedesign(fradesign = filedesign, tildesign = tndesign, parameters = parameters)
  redesign <- find_redesign(orgdesign = filedesign, targetdesign = tndesign, parameters = parameters)
  if(nrow(redesign$Udekk) > 0) cat("\n**Filen", filename, "mangler tall for ", nrow(redesign$Udekk), "strata. Disse får flagg = 9 under omkoding")
  file <- fetch_filegroup_from_buffer(filegroup = filename, parameters = parameters)
  file <- do_filter_and_recode_to_redesign(dt = file, redesign = redesign, parameters = parameters)
  return(file)
}

#' @title do_rectangularize_cube
#' @description
#' rectangularizes cube based on the given design
#' @param colnames 
#' @param design 
#' @param parameters global parameters
set_rectangularized_cube_design <- function(colnames, design, parameters) {
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
  
  return(rektangularisert)
}

#' @title get_removed_codes
#' @description fetches info on geo codes removed during rectangularization
report_removed_codes <- function(file, cube){
  remove <- unique(setdiff(file$GEO, cube$GEO))
  remove_valid <- remove[!grepl("99$", remove)]
  if (length(remove_valid) > 0) {
    cat("!! GEO ", paste(remove_valid, collapse = ","), " kastes ved rektangularisering!!\n")
    cat("!! Dessuten kastes ", length(setdiff(remove, remove_valid)), "99-koder!\n")
    print(file[GEO %in% remove])
    cat("#---#\n")
  } else if (length(setdiff(remove, remove_valid)) > 0) {
    cat("Kaster ", length(setdiff(remove, remove_valid)), "99-koder ved rektangulerisering.\n")
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
