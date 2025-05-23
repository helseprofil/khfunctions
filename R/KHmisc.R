# Helper functions used in the data processing, both in LagKUBE and LagFilgruppe


#' KHerr (kb)
KHerr <- function(error) {
  cat(
    "***************************************************************************\n",
    "*KHFEIL!! ", error, "\n***************************************************************************\n"
  )
}

#' @title DumpTabell
#' To save file dumps
#' @keywords internal
#' @noRd
DumpTabell <- function(TABELL, TABELLnavn, format = NULL) {
  if(is.null(format)) format <- getOption("khfunctions.defdumpformat")
  for(fmt in format){
    if (fmt == "CSV") {
      write.table(TABELL, file = file.path(getOption("khfunctions.root"), getOption("khfunctions.dumpdir"), paste0(TABELLnavn, ".csv")), sep = ";", na = "", row.names = FALSE)
    } else if (fmt == "R") {
      .GlobalEnv$DUMPtabs[[TABELLnavn]] <- TABELL
      saveRDS(TABELL, file = file.path(getOption("khfunctions.root"), getOption("khfunctions.dumpdir"), paste0(TABELLnavn, ".rds")))
    } else if (fmt == "STATA") {
      TABELL[TABELL == ""] <- " " # STATA stoetter ikke "empty-string"
      names(TABELL) <- gsub("^(\\d.*)$", "S_\\1", names(TABELL)) # STATA 14 taaler ikke numeriske kolonnenavn
      names(TABELL) <- gsub("^(.*)\\.([afn].*)$", "\\1_\\2", names(TABELL)) # Endre .a, .f, .n til _
      foreign::write.dta(TABELL, file.path(getOption("khfunctions.root"), getOption("khfunctions.dumpdir"), paste0(TABELLnavn, ".dta"), sep = ""))
    }
  }
}

#' @title get_value_columns
get_value_columns <- function(columnnames, full = FALSE) {
  valcols <- grep("^(.*?)\\.f$", columnnames, value = T)
  valcols <- gsub("\\.f$", "", valcols)
  if(full) valcols <- paste0(rep(valcols, each = 7), c("", ".f", ".a", ".n", ".fn1", ".fn3", ".fn9"))
  return(intersect(columnnames, valcols))
}

#' @title get_dimension_columns
get_dimension_columns <- function(columnnames) {
  nodim <- c(get_value_columns(columnnames, full = TRUE), "KOBLID", "ROW")
  return(setdiff(columnnames, nodim))
}

get_tab_columns <- function(columnnames){
  grep("^TAB\\d+$", columnnames, value = T)
}


#' FinnFilGruppeFraKoblid (kb)
#'
#' @param koblid 
#' @param globs global parameters, defaults to SettGlobs
FinnFilGruppeFraKoblid <- function(koblid, globs = get_global_parameters()) {
  return(as.character(sqlQuery(globs$dbh, paste("SELECT FILGRUPPE FROM ORGINNLESkobl WHERE KOBLID=", koblid, sep = ""), stringsAsFactors = FALSE)))
}

#' SkrivKBLogg (kb)
#'
#' @param KB 
#' @param type 
#' @param filbesk 
#' @param gruppe 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
SkrivKBLogg <- function(KB, type, filbesk, gruppe, batchdate = SettKHBatchDate(), globs = get_global_parameters()) {
  sqlQuery(globs$log, paste("DELETE * FROM KODEBOK_LOGG WHERE KOBLID=", filbesk$KOBLID, " AND TYPE='", type, "' AND SV='S'", sep = ""))
  sqlSave(globs$log, cbind(KOBLID = filbesk$KOBLID, FILGRUPPE = gruppe, FELTTYPE = type, SV = "S", KB[, c("ORG", "KBOMK", "OMK", "FREQ", "OK")], BATCHDATE = batchdate), "KODEBOK_LOGG", rownames = FALSE, append = TRUE)
}

#' SVcloneRecord (kb)
#'
#' @param dbh 
#' @param table 
#' @param koblid 
SVcloneRecord <- function(dbh, table, koblid) {
  design <- names(sqlQuery(dbh, paste("SELECT * FROM ", table, " WHERE KOBLID=-1", sep = "")))
  felt <- paste(design, collapse = ",")
  feltm <- sub("SV", "'V' AS SV", felt)
  sql <- paste(
    "INSERT INTO ", table, "(", felt, ")",
    "SELECT ", feltm, "FROM ", table,
    "WHERE KOBLID=", koblid, "AND SV='S'"
  )
  sqlQuery(dbh, sql)
}

#' readRDS_KH (kb)
#'
#' @param file 
#' @param IDKOLS 
#' @param ... 
readRDS_KH <- function(file, IDKOLS = FALSE, ...) {
  FIL <- readRDS(file, ...)
  if (IDKOLS == FALSE) {
    if ("KOBLID" %in% names(FIL)) {
      FIL$KOBLID <- NULL
    }
    if ("ROW" %in% names(FIL)) {
      FIL$ROW <- NULL
    }
  }
  return(FIL)
}

#' TilFilLogg (kb)
#'
#' @param koblid 
#' @param felt 
#' @param verdi 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
TilFilLogg <- function(koblid, felt, verdi, batchdate = SettKHBatchDate(), globs = get_global_parameters()) {
  # Sjekk om finnes rad for filid, eller lag ny
  if (nrow(sqlQuery(globs$log, paste("SELECT * FROM INNLES_LOGG WHERE KOBLID=", koblid, " AND SV='S' AND BATCH='", batchdate, "'", sep = ""))) == 0) {
    print("**************Hvorfor er jeg egentlig her?*********************'")
    sqlQuery(globs$log, paste("DELETE * FROM INNLES_LOGG WHERE KOBLID=", koblid, "AND SV='S'", sep = ""))
    upd <- paste("INSERT INTO INNLES_LOGG ( KOBLID, BATCH, SV, FILGRUPPE ) SELECT=", koblid, ",'", batchdate, "', 'S',", FinnFilGruppeFraKoblid(koblid, globs = globs), sep = "")
    sqlQuery(globs$log, upd)
  }
  if (is.character(verdi)) {
    verdi <- paste("'", verdi, "'", sep = "")
    verdi <- gsub("\\n", "' & Chr(13) & Chr(10) & '", verdi) # Veldig saer \n i Access!
  }
  upd <- paste("UPDATE INNLES_LOGG SET ", felt, "=", verdi, " WHERE KOBLID=", koblid, " AND SV='S' AND BATCH='", batchdate, "'", sep = "")
  tmp <- sqlQuery(globs$log, upd)
  # cat("********\n",tmp,"__________\n")
}

## Try to handle problem with "memory exhausted (limit reached?)" the solution above
#' expand.grid.df (ybk)
#'
#' @param ... 
expand.grid.df <- function(...) {
  DFs <- list(...)
  
  ddt <- lapply(DFs, function(x) is(x, "data.table"))
  dx <- which(ddt == 0)
  
  if (length(dx) > 0){
    for (i in dx){
      DFs[[i]] <- data.table::as.data.table(DFs[[i]])
    }
  }
  
  rows <- do.call(data.table::CJ, lapply(DFs, function(x) seq(nrow(x))))
  
  for (i in seq_along(DFs))
    names(DFs)[i] <- paste0("data", i)
  
  DFlength <- length(DFs)
  DFnames <- names(DFs)
  
  res <- DFs[[1L]][rows[[1L]]]
  DFs[[1L]] <- NULL
  for (i in seq_len(DFlength)[-1L]){
    x <- DFnames[i]
    res <- res[, c(.SD, DFs[[x]][rows[[i]]])]
    DFs[[x]] <- NULL
  }
  
  rm(DFs, rows)
  gc()
  data.table::setDT(res)
}

expand.grid.dt <- function(...){
  DTs <- list(...)
  if(length(DTs) == 0) stop("No tables (empty list) passed to expand.grid.dt")
  for(i in seq_along(DTs)){
    if(!is(DTs[[i]], "data.table")){
      DTs[[i]] <- data.table::setDT(DTs[[i]])
    }
  }
  
  rows <- do.call(data.table::CJ, lapply(DTs, function(x) seq(nrow(x))))
  
  for (i in seq_along(DTs)){
    DTs[[i]] <- DTs[[i]][rows[[i]]]
  }
    
  res <- DTs[[1L]]
  if(length(DTs) > 1){
    for(i in 2:length(DTs)){
      res[, names(DTs[[i]]) := DTs[[i]]]
    }
  }
  
  rm(DTs, rows)
  gc()
  return(res)
}

#' setkeym (kb)
#'
#' @param DTo 
#' @param keys 
setkeym <- function(DTo, keys) {
  
  # Foroesk paa aa speede opp naar setkeyv brukes for aa sikre key(DTo)=keys
  if (!("data.table" %in% class(DTo) && identical(data.table::key(DTo), keys))) {
    data.table::setDT(DTo)
    data.table::setkeyv(DTo, keys)
  }
}



#' @title is_not_empty
#' @description Checks if a parameter value is not empty
#' @param value The value to check
#' @returns TRUE/FALSE
#' @noRd
is_not_empty <- function(value){
  !is.null(value) && !is.na(value) && value != ""
}

#' @title is_empty
#' @description Checks if a parameter value is empty
#' @param value The value to check
#' @returns TRUE/FALSE
#' @noRd
is_empty <- function(value){
  is.null(value) || is.na(value) || value == "" 
}


#' FinnDesign (kb)
#' 
#' Brukes i lagkube og lagfilgruppe
#'
#' @param FIL 
#' @param FGP 
#' @param parameters global parameters
FinnDesign <- function(FIL, FGP = list(amin = 0, amax = 120), parameters) {
  if (identical(class(FIL), "data.frame")) {
    FIL <- data.table::data.table(FIL)
  }
  keyorg <- data.table::key(FIL)
  DelKols <- parameters$DefDesign$DelKols
  UBeting <- parameters$DefDesign$UBeting
  BetingOmk <- parameters$DefDesign$BetingOmk
  BetingF <- parameters$DefDesign$BetingF
  
  DesignKols <- parameters$DefDesign$DesignKolsF[parameters$DefDesign$DesignKolsF %in% names(FIL)]
  
  DesignKols <- parameters$DefDesign$DesignKolsF[parameters$DefDesign$DesignKolsF %in% names(FIL)]
  OmkKols <- parameters$DefDesign$DesignKols[parameters$DefDesign$DesignKols %in% names(FIL)]
  
  Design <- list()
  Design[["KolNavn"]] <- names(FIL)
  # Finn faktisk design
  setkeym(FIL, c(DesignKols))
  ObsDesign <- unique(FIL[, ..DesignKols])
  
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
    mangler <- intervals::interval_difference(intervals::Intervals(c(FGP$amin, FGP$amax), type = "Z"), intervals::Intervals(Design$Part$A[, DelKols$A, with = FALSE], type = "Z"))
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
  setkeym(FIL, keyorg)
  gc()
  return(Design)
}