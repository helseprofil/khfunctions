#' merge_teller_nevner
#' 
#' BØR VÆRE EGEN FIL
#'
#' @param filer 
#' @param FilDesL 
#' @param FGPs 
#' @param TNPdscr 
#' @param TT 
#' @param NN 
#' @param Design 
#' @param KUBEdscr 
#' @param globs 
merge_teller_nevner <- function(filer, FilDesL, FGPs, TNPdscr, TELLERFIL = "TELLER", NEVNERFIL = "NEVNER", Design = NULL, KUBEdscr = NULL, globs = SettGlobs()) {
  is_kh_debug()
  
  tellerfilnavn <- filer[[TELLERFIL]]
  tellerfildesign <- FilDesL[[tellerfilnavn]]
  isnevnerfil <- NEVNERFIL %in% names(filer)
  nevnerfildesign <- NULL
  if(isnevnerfil) {
    nevnerfilnavn <- filer[[NEVNERFIL]]
    nevnerfildesign <- FilDesL[[nevnerfilnavn]]
  }
  InitDesign <- get_initialdesign(design = Design, tellerfildesign = tellerfildesign, nevnerfildesign = nevnerfildesign, globs = globs)
  
  if(!is.null(KUBEdscr)){
    KUBEdesign <- FinnKubeDesignB(KUBEdscr = KUBEdscr, ORGd = InitDesign, FGP = FGPs[[tellerfilnavn]], globs = globs)
    TNdesign <- list(Part = KUBEdesign$TMP)
  } else {
    KUBEdesign <- list()
    TNdesign <- InitDesign
  }
  
  cat("***Lager tellerfil fra", tellerfilnavn, "\n")
  tellerfil <- do_redesign_recode_file(filename = tellerfilnavn, filedesign = tellerfildesign, tndesign = TNdesign, globs = globs)
  
  if(isnevnerfil) {
    cat("Lager nevnerfil fra", nevnerfilnavn, "\n")
    nevnerfil <- do_redesign_recode_file(filename = nevnerfilnavn, filedesign = nevnerfildesign, tndesign = TNdesign, globs = globs)
  }
  
  implicitnull_defs <- FGPs[[tellerfilnavn]]$vals
  if(isnevnerfil) implicitnull_defs <- c(implicitnull_defs, FGPs[[nevnerfilnavn]]$vals)
  
  if (length(KUBEdesign) > 0) {
    rectangularizedcube <- set_rectangularized_cube_design(colnames = names(tellerfil), design = KUBEdesign$TMP, globs = globs)
    report_removed_codes(file = tellerfil, cube = rectangularizedcube)
    TNF <- collapse::join(rectangularizedcube, tellerfil, how = "l", overid = 0, verbose = 0)
    if(isnevnerfil) TNF <- collapse::join(TNF, nevnerfil, how = "l", overid = 0, verbose = 0)
    TNF <- set_implicit_null_after_merge(file = TNF, implicitnull_defs = implicitnull_defs)
    cat("--TNF ferdig rektangularisert og merget, dim(TNF)", dim(TNF), "\n")
  } else if (isnevnerfil) {
    TNF <- collapse::join(tellerfil, nevnerfil, how = "l", overid = 0, verbose = 0)
    TNF <- set_implicit_null_after_merge(file = TNF, implicitnull_defs = implicitnull_defs)
    cat("--TNF ferdig merget, dim(TNF)", dim(TNF), "\n")
  } else {
    TNF <- tellerfil
    cat("--TNF ferdig, har ikke nevnerfil, så TNF == tellerfil\n")
  }
  
  isNYEKOL_RAD <- !is.na(TNPdscr$NYEKOL_RAD) && TNPdscr$NYEKOL_RAD != ""
  if (isNYEKOL_RAD) TNF <- LeggTilSumFraRader(TNF, TNPdscr$NYEKOL_RAD, FGP = FGPs[[tellerfilnavn]], globs = globs)
  isNYEKOL_KOL <- !is.na(TNPdscr$NYEKOL_KOL) && TNPdscr$NYEKOL_KOL != ""
  if (isNYEKOL_KOL) TNF <- LeggTilNyeVerdiKolonner(TNF, TNPdscr$NYEKOL_KOL, slettInf = TRUE)
  
  dimorg <- dim(TNF)
  TNF <- do_filter_file(TNF, KUBEdesign$MAIN, globs = globs)
  if (!identical(dimorg, dim(TNF))) cat("Siste filtrering av TNF, hadde dim(TNF)", dimorg, "fikk dim(TNF)", dim(TNF), "\n")
  
  TNF <- set_teller_nevner_names(file = TNF, TNPdscr = TNPdscr)
  
  cat("---Ferdig i LagTNtabell\n")
  return(list(TNF = TNF, KUBEd = KUBEdesign))
}

#' @title get_initialdesign
#' @param design 
#' @param tellerfildesign 
#' @param nevnerfildesign 
#' @param globs 
get_initialdesign <- function(design, tellerfildesign, nevnerfildesign, globs){
  
  if(!is.null(design)) return(design)
  if (is.null(nevnerfildesign)) return(tellerfildesign)
  
  fellesdesign <- FinnFellesTab(tellerfildesign, nevnerfildesign, globs = globs)$FDes
  for(del in setdiff(names(tellerfildesign$Part), names(nevnerfildesign$Part))) {
    fellesdesign$Part[[del]] <- tellerfildesign$Part[[del]]
  }
  for(del in setdiff(names(nevnerfildesign$Part), names(tellerfildesign$Part))) {
    InitDes$Part[[del]] <- nevnerfildesign$Part[[del]]
  }
  return(fellesdesign)
}

#' FinnKubeDesignB (kb)
#'
#' @param KUBEdscr 
#' @param ORGd 
#' @param FGP 
#' @param globs 
FinnKubeDesignB <- function(KUBEdscr, ORGd, FGP = list(amin = 0, amax = 120), globs = SettGlobs()) {
  KubeD <- list(
    TMP = FinnKubeDesign(KUBEdscr, ORGd, bruk0 = TRUE, FGP = FGP, globs = globs),
    MAIN = FinnKubeDesign(KUBEdscr, ORGd, bruk0 = FALSE, FGP = FGP, globs = globs)
  )
  return(KubeD)
}

#' @title do_redesign_recode_file
#'
#' @param filename 
#' @param filedesign 
#' @param tndesign 
#' @param globs 
do_redesign_recode_file <- function(filename, filedesign, tndesign, globs){
  redesign <- FinnRedesign(DesFRA = filedesign, DesTIL = tndesign, globs = globs)
  if (nrow(redesign$Udekk) > 0) KHerr(paste0("UDEKKA i redesign av", filename))
  file <- OmkodFil(FinnFilT(filename), redesign, globs = globs)
  return(file)
}

#' @title do_rectangularize_cube
#' @description
#' rectangularizes cube based on the given design
#' @param colnames 
#' @param design 
#' @param globs 
set_rectangularized_cube_design <- function(colnames, design, globs = SettGlobs()) {
  GEOstdAAR <- getOption("khfunctions.year")
  DTlist <- list()
  delkolonner <- character(0)
  for (del in names(design)) {
    delkols <- globs$DefDesign$DelKols[[del]]
    if (all(delkols %in% colnames)) {
      delkolonner <- c(delkolonner, delkols)
      DTlist[[del]] <- data.table::setDT(design[[del]])
    }
  }
  deler <- do.call(expand.grid.dt, DTlist)[, ..delkolonner]
  
  rektangularisert <- data.table::data.table()    
  for (Gn in design[["Gn"]][["GEOniv"]]) {
    GEOK <- globs$GeoKoder[GEOniv == Gn & FRA <= getOption("khfunctions.year") & TIL > getOption("khfunctions.year")]
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

#' @title set_implicit_null_after_merge (kb)
#' @description
#' Fixing implicit 0 occurring after merging, using information from VALXmiss in access. 
#' Previous name SettMergeNAs
#' @param file
#' @param implicitnull_defs 
set_implicit_null_after_merge <- function(file, implicitnull_defs = list()) {
  
  vals <- get_value_columns(names(file))
  
  for (val in vals) {
    if (val %in% names(implicitnull_defs)) {
      VALmiss <- implicitnull_defs[[val]]$miss
      replacemissing <- list()
      if(VALmiss == "..") replacemissing <- list(0, 1, 1)
      if(VALmiss == ".") replacemissing <- list(0, 2, 1)
      if(VALmiss == ":") replacemissing <- list(0, 3, 1)
      if(!grepl("\\D", VALmiss)) replacemissing <- list(as.numeric(VALmiss), 0, 1)
      if(length(replacemissing) == 0) stop(val, " listed in VALXnavn, but VALXmiss is not '..', '.', ':', or numeric")
    } else {
      replacemissing <- list(0, 0, 1)
    }
    
    valF <- paste0(val, ".f")
    valA <- paste0(val, ".a")
    missingrows <- which((is.na(file[[val]]) & file[[valF]] == 0) | is.na(file[[valF]]))
    n_missing <- length(missingrows)
    if(n_missing > 0) cat("Implisitte nuller: Setter", val, "=", replacemissing[[1]], "and", valF, "=", replacemissing[[2]], "for",  n_missing, "rader\n")
    file[missingrows, names(.SD) := replacemissing, .SDcols = c(val, valF, valA)]
  }
  return(file)
}

#' @title do_filter_file
#'
#' @param file 
#' @param design 
#' @param globs 
do_filter_file <- function(file, design, globs = SettGlobs()) {
  for (del in names(design)) {
    cols <- globs$DefDesign$DelKols[[del]]
    if (all(cols %in% names(file))) {
      file <- collapse::join(file, design[[del]][, ..cols], on = cols, how = "right", multiple = T, overid = 0, verbose = 0)
    }
  }
  return(file)
}

#' @title set_teller_nevner_names
#' @description Sets name of teller and nevner column to TELLER and NEVNER by reference
set_teller_nevner_names <- function(file, TNPdscr){
  newnames <- gsub(paste0("^", TNPdscr$TELLERKOL, "(\\.f|\\.a|)$"), "TELLER\\1", names(file))
  newnames <- gsub(paste0("^", TNPdscr$NEVNERKOL, "(\\.f|\\.a|)$"), "NEVNER\\1", newnames)
  data.table::setnames(file, names(file), newnames)
}


