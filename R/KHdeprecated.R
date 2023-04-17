# Functions that apparently are no longer in use, with comment

# Not used anywhere
SySammenFiler <- function(FILID1, FILID2, batch1 = NA, batch2 = NA, ROLLE1 = "", ROLLE2 = "", globs = FinnGlobs()) {
  is_kh_debug()
  
  return(SySammenTabeller(
    FinnFilT(FILID1, batch = batch1, ROLLE = ROLLE1, globs = globs),
    FinnFilT(FILID2, batch = batch2, ROLLE = ROLLE2, globs = globs)
  ))
}

# Only used by SySammenFiler(), which is not used anywhere
SySammenTabeller <- function(F1, F2, SJEF = 0, FGP1 = list(amin = 0, amax = 120), FGP2 = list(amin = 0, amax = 120), SkalAggregeresOpp1 = character(), SkalAggregeresOpp2 = character(), globs = FinnGlobs(), etabmatchOK = TRUE, rapport = list(), FullResult = FALSE, echo = FALSE) {
  is_kh_debug()
  
  ok <- 1
  if (identical(class(F1), "data.frame")) {
    F1 <- data.table(F1)
  }
  if (identical(class(F2), "data.frame")) {
    F2 <- data.table(F2)
  }
  orgkey1 <- key(F1)
  orgkey2 <- key(F2)
  
  # MÅ FIKSE EVT KOLLISJON AV FELTNAVN!
  
  FU <- data.table()
  
  etabs1 <- names(F1)[grepl("^TAB\\d+$", names(F1))]
  etabs2 <- names(F2)[grepl("^TAB\\d+$", names(F2))]
  
  atabs <- globs$DefDesign$DesignKolsFA
  atabs1 <- atabs[atabs %in% names(F1)]
  atabs2 <- atabs[atabs %in% names(F2)]
  atabs <- intersect(atabs1, atabs2)
  
  if (etabmatchOK == FALSE && length(etabs1) > 0 && length(etabs2) > 0) {
    ok <- 0
    print("FEIL!!!!! Noe tull med etabmatchOK") # Usikker på hvorfor jeg har satt denne muligheten, bruker den ikke
  } else {
    DF1 <- FinnDesign(F1, FGP = FGP1)
    DF2 <- FinnDesign(F2, FGP = FGP2)
    
    if (SJEF == 0) {
      DFF <- FinnFellesTab(DF1, DF2, SkalAggregeresOpp1 = SkalAggregeresOpp1, SkalAggregeresOpp2 = SkalAggregeresOpp2, globs = globs)
      # Er dette riktig mht Dekk??
      rapport["TRINN"] <- rapport["F1"]
      rapport["KALL"] <- "SySammenFil-0"
      F1 <- OmkodFil(F1, DFF$KB12, rapport = rapport, globs = globs)
      rapport["TRINN"] <- rapport["F2"]
      F2 <- OmkodFil(F2, DFF$KB21, rapport = rapport, globs = globs)
      setkeym(F1, atabs)
      setkeym(F2, atabs)
      FU <- merge(F1, F2, all = TRUE, allow.cartesian = TRUE)
    } else if (SJEF == 1) {
      RD21 <- FinnRedesign(DF2, DF1, SkalAggregeresOpp = SkalAggregeresOpp2, globs = globs)
      rapport["KALL"] <- "SySammenFil-1"
      F2 <- OmkodFil(F2, RD21, rapport = rapport, globs = globs)
      setkeym(F1, atabs)
      setkeym(F2, atabs)
      print("Tung merge")
      FU <- F2[F1, allow.cartesian = TRUE]
      print("OVER")
    }
    # RAPPORTER INNSETTING???
    FU <- SettMergeNAs(FU, c(FGP1$vals, FGP2$vals))
    
    if (echo == TRUE) {
      print(F1)
      print(F2)
      prit(FU)
    }
  }
  setkeym(F1, orgkey1)
  setkeym(F2, orgkey2)
  if (FullResult == TRUE) {
    return(list(SF = FU, F1 = F1, F2 = F2))
  } else {
    return(FU)
  }
}

# Not used anywhere
OmkodFilFraPartM <- function(Fil, Part, FGP = list(amin = 0, amax = 120), rapport = list(), globs = FinnGlobs()) {
  is_kh_debug()
  
  rapport["KALL"] <- "OmkodFilFraPart"
  for (del in names(Part)) {
    Fil <- OmkodFilFraPart(Fil, Part[[del]], FGP, rapport = rapport, globs = globs)
  }
  return(Fil)
}

# Not used anywhere
OmkodFilFraDesign <- function(Fil, Design, FGP = list(amin = 0, amax = 120), rapport = list(), globs = FinnGlobs()) {
  is_kh_debug()
  
  rapport["KALL"] <- "OmkodFilFraDesign"
  Dorg <- FinnDesign(Fil, FGP = FGP, globs = globs)
  RD <- FinnRedesign(Dorg, Design, globs = globs)
  return(OmkodFil(Fil, RD, rapport = rapport, globs = globs))
}

# Not used anywhere
FinnSnittOverAar <- function(KUBE, ma = 1, AntYMiss = 0, globs = FinnGlobs()) {
  is_kh_debug()
  KUBEd <- FinnDesign(KUBE)
  if (ma > 1) {
    PERIODER <- KUBEd$Part$Y
    PERIODER$AARl <- PERIODER$AARh - ma + 1
    PERd <- KUBEd
    PERd$Part$Y <- PERIODER
    PERd$OmkDesign <- KUBEd$OmkDesign
    PERd$OmkDesign$AARl <- PERd$OmkDesign$AARh - ma + 1
    if (AntYMiss > 0) {
      globs$DefDesign$IntervallHull[["Y"]] <- paste("NTOT-NHAR<=", AntYMiss, sep = "")
    }
    RD <- FinnRedesign(KUBEd, PERd, globs = globs)
    maKUBE <- OmkodFil(KUBE, FinnRedesign(KUBEd, PERd, globs = globs), snitt = TRUE, globs = globs)
    valnames <- setdiff(names(maKUBE), globs$DefDesign$DesignKolsFA)
    setnames(maKUBE, valnames, paste("ma", ma, valnames, sep = ""))
  } else {
    maKUBE <- KUBE
  }
  return(maKUBE)
}
