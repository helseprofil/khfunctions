#' @title find_filedesign
#' @description
#' Finds design parameters for a file
#' 
#' @noRd
#' 
#' Erstatter FinnDesign()
#'
#' @param file 
#' @param fileparameters 
#' @param globs 
find_filedesign <- function(file, fileparameters = NULL, globs = SettGlobs()) {
  is_kh_debug()
  if(is.null(fileparameters)) fileparameters <- list(amin = 0, amax = 120)
  if(identical(class(file), "data.frame")) file <- data.table::data.table(file)
  
  keyorg <- data.table::key(file)
  DelKols <- globs$DefDesign$DelKols
  UBeting <- globs$DefDesign$UBeting
  BetingOmk <- globs$DefDesign$BetingOmk
  BetingF <- globs$DefDesign$BetingF
  
  DesignKols <- globs$DefDesign$DesignKolsF[globs$DefDesign$DesignKolsF %in% names(file)]
  
  DesignKols <- globs$DefDesign$DesignKolsF[globs$DefDesign$DesignKolsF %in% names(file)]
  OmkKols <- globs$DefDesign$DesignKols[globs$DefDesign$DesignKols %in% names(file)]
  
  Design <- list()
  Design[["KolNavn"]] <- names(file)
  # Finn faktisk design
  setkeym(file, c(DesignKols))
  ObsDesign <- unique(file[, ..DesignKols])
  
  # Finn deler inneholdt i tabell
  Deler <- character()
  for (del in names(DelKols)) {
    if (all(DelKols[[del]] %in% DesignKols)) {
      Deler <- c(Deler, del)
    }
  }
  
  # Sett omkodingskombinasjoner
  Design[["UBeting"]] <- UBeting[UBeting %in% Deler]
  Design[["BetingOmk"]] <- BetingOmk[BetingOmk %in% Deler]
  Design[["BetingF"]] <- BetingF[BetingF %in% Deler]
  Alle <- c(Design[["UBeting"]], Design[["BetingOmk"]], Design[["BetingF"]])
  Design[["OmkDeler"]] <- c(Design[["UBeting"]], Design[["BetingOmk"]])
  
  # Sett alle partielle tabuleringer (Gn,Y,K,A,T1,T2,T3),
  for (del in Deler) {
    kols <- DelKols[[del]]
    data.table::setkeyv(ObsDesign, kols)
    # SETT HAR
    Design[["Part"]][[del]] <- data.table::data.table(setNames(cbind(unique(ObsDesign[, kols, with = FALSE]), 1), c(kols, paste(del, "_HAR", sep = ""))), key = kols)
  }
  
  # Fyll evt hull i aldersintervaller
  if ("A" %in% names(Design$Part)) {
    mangler <- intervals::interval_difference(Intervals(c(fileparameters$amin, fileparameters$amax), type = "Z"), Intervals(Design$Part$A[, DelKols$A, with = FALSE], type = "Z"))
    if (nrow(mangler) > 0) {
      mangler <- setNames(cbind(as.data.frame(mangler), 0), c("ALDERl", "ALDERh", "A_HAR"))
      Design[["Part"]][["A"]] <- rbind(Design[["Part"]][["A"]], mangler)
    }
  }
  
  # Finn fullt design, dvs kryssing av alle partielle.
  delerlist <- paste("as.data.frame(Design[[\"Part\"]][[\"", Alle, "\"]])", sep = "", collapse = ",")
  FullDesign <- data.table::data.table(eval(parse(text = paste("expand.grid.df(", delerlist, ")", sep = ""))))
  setkeym(ObsDesign, names(ObsDesign))
  setkeym(FullDesign, names(ObsDesign))
  # Sett HAR=1 om denne finnes i fakttisk design
  FullDesign[, HAR := 0]
  FullDesign[ObsDesign, HAR := 1]
  Design[["Design"]] <- FullDesign
  
  # Sett omkodingskombinasjone
  # Noen dimensjoner faar variere fritt (UBeting). Andre maa vaere fast for alle versjoner av UBeting
  # Def er at Gn og Y er frie, mens K og A maa vaere fast for hver Gn,Y kombinasjon
  Beting <- c("", Design[["BetingOmk"]], Design[["BetingF"]])
  komb <- Design[["UBeting"]]
  for (del in Beting) {
    if (del != "") {
      komb <- c(Design[["UBeting"]], del)
    }
    if (length(komb) > 0) {
      kols <- character(0)
      for (k in komb) {
        kols <- c(kols, DelKols[[k]])
      }
      data.table::setkeyv(ObsDesign, kols)
      data.table::setkeyv(FullDesign, kols)
      kombFull <- data.table::data.table(unique(FullDesign[, kols, with = FALSE]))
      kombObs <- data.table::data.table(unique(ObsDesign[, kols, with = FALSE]))
      kombFull[, HAR := 0]
      kombFull[kombObs, HAR := 1]
      kombn <- paste("bet", del, sep = "")
      Design[["SKombs"]][[kombn]] <- kombFull
    }
  }
  
  setkeym(ObsDesign, names(ObsDesign))
  setkeym(file, keyorg)
  gc()
  return(Design)
}