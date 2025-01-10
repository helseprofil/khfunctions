#' merge_teller_nevner
#' 
#' @param files 
#' @param s 
#' @param fileparameters 
#' @param TNPparameters 
#' @param TELLERFIL 
#' @param NEVNERFIL 
#' @param Design 
#' @param KUBEparameters 
#' @param globs 
merge_teller_nevner <- function(files, filedesigns, fileparameters, TNPparameters, TELLERFIL = NULL, NEVNERFIL = NULL, Design = NULL, KUBEparameters = NULL, globs = SettGlobs()) {
  is_kh_debug()
  if(is.null(TELLERFIL)) TELLERFIL <- "TELLER"
  if(is.null(NEVNERFIL)) NEVNERFIL <- "NEVNER"
  
  tellerfilnavn <- files[[TELLERFIL]]
  tellerfildesign <- filedesigns[[tellerfilnavn]]
  isnevnerfil <- NEVNERFIL %in% names(files)
  nevnerfildesign <- NULL
  if(isnevnerfil) {
    nevnerfilnavn <- files[[NEVNERFIL]]
    nevnerfildesign <- filedesigns[[nevnerfilnavn]]
  }
  InitDesign <- get_initialdesign(design = Design, tellerfildesign = tellerfildesign, nevnerfildesign = nevnerfildesign, globs = globs)
  
  if(!is.null(KUBEparameters)){
    KUBEdesign <- FinnKubeDesignB(KUBEdscr = KUBEparameters, ORGd = InitDesign, FGP = fileparameters[[tellerfilnavn]], globs = globs)
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
  
  implicitnull_defs <- fileparameters[[tellerfilnavn]]$vals
  if(isnevnerfil) implicitnull_defs <- c(implicitnull_defs, fileparameters[[nevnerfilnavn]]$vals)
  
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
  
  isNYEKOL_RAD <- !is.na(TNPparameters$NYEKOL_RAD) && TNPparameters$NYEKOL_RAD != ""
  if (isNYEKOL_RAD) TNF <- LeggTilSumFraRader(TNF, TNPparameters$NYEKOL_RAD, FGP = fileparameters[[tellerfilnavn]], globs = globs)
  isNYEKOL_KOL <- !is.na(TNPparameters$NYEKOL_KOL) && TNPparameters$NYEKOL_KOL != ""
  if (isNYEKOL_KOL) TNF <- LeggTilNyeVerdiKolonner(TNF, TNPparameters$NYEKOL_KOL, slettInf = TRUE)
  
  dimorg <- dim(TNF)
  TNF <- do_filter_file(TNF, KUBEdesign$MAIN, globs = globs)
  if (!identical(dimorg, dim(TNF))) cat("Siste filtrering av TNF, hadde dim(TNF)", dimorg, "fikk dim(TNF)", dim(TNF), "\n")
  
  TNF <- set_teller_nevner_names(file = TNF, TNPparameters = TNPparameters)
  
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

#' FinnFellesTab (kb)
#'
#' @param DF1 
#' @param DF2 
#' @param globs 
FinnFellesTab <- function(DF1, DF2, globs = SettGlobs()) {
  # Diff<-union(setdiff(names(DF1$Part),names(DF2$Part)),setdiff(names(DF2$Part),names(DF1$Part)))
  is_kh_debug()
  cat("Starter i FinnFellesTab.")
  FTabs <- list()
  for (del in intersect(names(DF1$Part), names(DF2$Part))) {
    FTabs[[del]] <- unique(rbind(DF1$Part[[del]], DF2$Part[[del]]))
  }
  RD1 <- FinnRedesign(DF1, list(Part = FTabs), globs = globs)
  RD2 <- FinnRedesign(DF2, list(Part = FTabs), globs = globs)
  ### kommet hit ---- 
  omktabs <- names(RD1$FULL)[grepl("_omk$", names(RD1$FULL))]
  data.table::setkeyv(RD1$FULL, omktabs)
  data.table::setkeyv(RD2$FULL, omktabs)
  Dekk1 <- unique(RD1$FULL[, omktabs, with = FALSE])
  Dekk2 <- unique(RD2$FULL[, omktabs, with = FALSE])
  Dekk12 <- Dekk1[Dekk2, nomatch = 0]
  data.table::setnames(Dekk12, names(Dekk12), gsub("_omk$", "", names(Dekk12)))
  FDes <- FinnDesign(Dekk12, globs = globs)
  cat(" Ferdig i FinnFellesTab\n")
  gc()
  return(list(Dekk = Dekk12, FDes = FDes))
}



#' FinnKubeDesignB (kb)
#'
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
  redesign <- FinnRedesign(fradesign = filedesign, tildesign = tndesign, globs = globs)
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
set_teller_nevner_names <- function(file, TNPparameters){
  newnames <- gsub(paste0("^", TNPparameters$TELLERKOL, "(\\.f|\\.a|)$"), "TELLER\\1", names(file))
  newnames <- gsub(paste0("^", TNPparameters$NEVNERKOL, "(\\.f|\\.a|)$"), "NEVNER\\1", newnames)
  data.table::setnames(file, names(file), newnames)
}
