#' SettGlobs (kb)
SettGlobs <- function() {
  is_kh_debug()
  RODBC::odbcCloseAll()
  path <- getOption("khfunctions.root")
  dbFile <- getOption("khfunctions.db")
  logFile <- getOption("khfunctions.logg")
  
  if (!dir.exists(path)) {
    stop(paste0(path, " ikke funnet, Har du tilgang til O:/?"))
  }

  if (!file.exists(file.path(path, dbFile))) {
    stop(dbFile, " ikke funnet i ", path)
  }
  
  # # If local path is set:
  # if (path != "" & exists("setLocalPath", envir = .GlobalEnv)) {
  #   path <- setLocalPath
  # }
  # 
  # If path is valid, connect to database and reset path to rawPath for global parameters to work
  # if(path != ""){
  #   KHELSA <- RODBC::odbcConnectAccess2007(file.path(path, dbFile))
  #   KHLOGG <- RODBC::odbcConnectAccess2007(file.path(path, logFile))
  # 
  #   path <- getOption("khfunctions.root")
  # }
  
  KHELSA <- connect_khelsa()
  KHLOGG <- connect_khlogg()
  globs <- list(dbh = KHELSA, log = KHLOGG)
  
  # globs[["GeoNavn"]] <- data.table::data.table(RODBC::sqlQuery(KHELSA, "SELECT * from GeoNavn", as.is = TRUE))
  globs[["GeoKoder"]] <- data.table::setDT(RODBC::sqlQuery(KHELSA, "SELECT * from GEOKoder", as.is = TRUE), key = c("GEO"))
  globs[["UtGeoKoder"]] <- globs$GeoKoder[TYP == "O" & TIL == 9999]$GEO
  globs[["TKNR"]] <- data.table::setDT(RODBC::sqlQuery(KHELSA, "SELECT * from TKNR", as.is = TRUE), key = c("ORGKODE"))
  globs[["HELSEREG"]] <- data.table::setDT(RODBC::sqlQuery(KHELSA, "SELECT * from HELSEREG", as.is = TRUE), key = c("FYLKE"))
  KnrHarm <- data.table::setDT(RODBC::sqlQuery(KHELSA, "SELECT * from KnrHarm", as.is = TRUE), key = c("GEO"))
  KnrHarmS <- data.table::copy(KnrHarm)[, let(GEO = paste0(GEO, "00"), GEO_omk = paste0(GEO_omk, "00"))]
  globs[["KnrHarm"]] <- data.table::rbindlist(list(KnrHarm, KnrHarmS))
  globs[["GkBHarm"]] <- data.table::setDT(RODBC::sqlQuery(KHELSA, "SELECT * FROM GKBydel2004T", as.is = TRUE), key = c("GK", "Bydel2004"))
  globs[["DefDesign"]] <- SettDefDesignKH(dbcon = KHELSA)
  globs[["KB"]] <- SettKodeBokGlob(globs = globs)
  globs[["LegKoder"]] <- SettLegitimeKoder(globs = globs) 
  globs[["TotalKoder"]] <- getOption("khfunctions.totals")
  Stata <- FinnStataExe()
  globs[["StataExe"]] <- Stata$Exe
  globs[["StataVers"]] <- Stata$Vers
  return(globs)
}

#' SettDefDesignKH (kb)
#' 
#' Setter standard designegenskaper, slik som delenes kolonnenavn og status i omkoding
#' Se tabell KH_DELER
SettDefDesignKH <- function(dbcon) {
  is_kh_debug()
  
  Deler <- data.table::setDT(RODBC::sqlQuery(dbcon, "SELECT * FROM KH_DELER WHERE DEL <> 'S'", as.is = TRUE))
  Deler <- Deler[order(ID)]
  DelKolN <- setNames(Deler$DelKol, Deler$DEL)
  DelKolE <- setNames(Deler$DelKolE, Deler$DEL)
  DelType <- setNames(Deler$TYPE, Deler$DEL)
  DelFormat <- setNames(Deler$FORMAT, Deler$DEL)
  AggPri <- Deler$DEL[order(Deler$AGGREGERPRI)]
  AggVedStand <- Deler$DEL[Deler$AGGREGERvedPRED == 1]
  IntervallHull <- setNames(Deler$INTERVALLHULL, Deler$DEL)
  IntervallHull <- IntervallHull[!(is.na(IntervallHull) | IntervallHull == "")]
  
  DelKols <- as.list(DelKolN)
  DelKolsF <- DelKols
  for (del in names(DelKols)) {
    if (DelType[del] == "INT") {
      DelKols[[del]] <- paste0(DelKols[[del]], c("l", "h"))
      DelKolsF[[del]] <- DelKols[[del]]
    }
    if (!(is.na(DelKolE[[del]]) | DelKolE[[del]] == "")) {
      DelKolsF[[del]] <- c(DelKolsF[[del]], unlist(stringr::str_split(DelKolE[[del]], ",")))
    }
  }
  
  UBeting <- Deler$DEL[Deler$OMKODbet == "U"]
  BetingOmk <- Deler$DEL[Deler$OMKODbet == "B"]
  BetingF <- Deler$DEL[Deler$OMKODbet == "F"]
  OmkDel <- c(UBeting, BetingOmk)
  
  DesignKols <- c(unlist(DelKols[c(UBeting, BetingOmk)]))
  DesignKolsF <- c(DesignKols, unlist(DelKols[BetingF]))
  DesignKolsFA <- c(DesignKolsF, setdiff(unlist(DelKolsF[c(UBeting, BetingOmk)]), unlist(DelKols[c(UBeting, BetingOmk)])))
  
  return(
    list(
      DelKols = DelKols,
      DelKolsF = DelKolsF,
      DelKolN = DelKolN,
      DelType = DelType,
      DelFormat = DelFormat,
      UBeting = UBeting,
      BetingOmk = BetingOmk,
      BetingF = BetingF,
      OmkDel = OmkDel,
      DesignKols = DesignKols,
      DesignKolsF = DesignKolsF,
      DesignKolsFA = DesignKolsFA,
      AggPri = AggPri,
      AggVedStand = AggVedStand,
      IntervallHull = IntervallHull
    )
  )
}

#' SettKodeBokGlob (kb)
#'
#' @param globs 
SettKodeBokGlob <- function(globs = SettGlobs()) {
  is_kh_debug()
  
  OmkodD <- data.table::setDT(RODBC::sqlQuery(globs$dbh, "SELECT * FROM KH_OMKOD
                                              UNION SELECT ID, DEL, KODE as NYKODE, KODE as ORGKODE, 0 as PRI_OMKOD, 1 AS OBLIG FROM KH_KODER", as.is = TRUE, stringsAsFactors = FALSE))
  OmkodD <- OmkodD[DEL != "S"]
  KB <- list()
  
  for (del in names(globs$DefDesign$DelKolN)) {
    KBD <- OmkodD[DEL == del]
    if (globs$DefDesign$DelType[del] == "INT") {
      if (nrow(KBD) > 0) {
        KBD[, c("ORGKODEl", "ORGKODEh", "NYKODEl", "NYKODEh") := NA_integer_]
      } else {
        KBD[, c("ORGKODEl", "ORGKODEh", "NYKODEl", "NYKODEh") := integer()]
      }
    } else if (globs$DefDesign$DelFormat[del] == "integer") {
      if(del == "L") KBD <- KBD[!grepl("[^0-9]", ORGKODE)]
      KBD[, names(.SD) := lapply(.SD, as.integer), .SDcols = c("ORGKODE", "NYKODE")]
    }
    kbdnames <- names(KBD)
    kbdnames <- gsub("ORGKODE", globs$DefDesign$DelKolN[del], kbdnames)
    kbdnames <- gsub("NYKODE(h|l|)", paste0(globs$DefDesign$DelKolN[del], "\\1_omk"), kbdnames)
    kbdnames <- gsub("NYKODE(h|l|)", paste0(globs$DefDesign$DelKolN[del], "\\1_omk"), kbdnames)
    kbdnames <- gsub("PRI_OMKOD", paste0(del, "_pri"), kbdnames)
    kbdnames <- gsub("OBLIG", paste0(del, "_obl"), kbdnames)
    data.table::setnames(KBD, names(KBD), kbdnames)
    KBD[, c("ID", "DEL") := NULL]
    KB[[del]] <- KBD
  }
  return(KB)
}

#' SettLegitimeKoder (kb)
#'
#' @param globs 
SettLegitimeKoder <- function(globs = SettGlobs()) {
  is_kh_debug()

  Koder <- data.table::setDT(RODBC::sqlQuery(globs$dbh, "SELECT * FROM KH_KODER WHERE DEL <> 'S'", as.is = TRUE))
  KodeL <- list()
  for (del in unique(Koder$DEL)) {
    KodeD <- Koder[DEL == del]
    if (globs$DefDesign$DelType[del] == "INT") {
      KodeD[, (globs$DefDesign$DelKols[[del]]) := tstrsplit(KODE, "_", fixed = TRUE)]
    } else if (globs$DefDesign$DelFormat[del] == "integer") {
      KodeD[, (globs$DefDesign$DelKols[[del]]) := NA_integer_]
      KodeD[!grepl("[^0-9]", KODE), (globs$DefDesign$DelKols[[del]]) := as.integer(KODE)]
    } else if (globs$DefDesign$DelFormat[del] == "character") {
      KodeD[, (globs$DefDesign$DelKols[[del]]) := KODE]
    }
    KodeL[[del]] <- KodeD
  }
  return(KodeL)
}

#' FinnStataExe (ybk)
#'
#' Find the most recent version of locally installed Stata
FinnStataExe <- function() {
  stata_bin <- "StataSE-64.exe"
  program_path <- c("C:/Program Files/", "C:/Program Files (x86)/")
  stata_prog <- grep("Stata", fs::dir_ls(program_path), value = TRUE)
  stata_ver <- stringi::stri_extract(stata_prog, regex = "\\d{2}$")
  Vers <- max(as.numeric(stata_ver))
  stata_path <- grep(Vers, stata_prog, value = TRUE)
  Exe <- file.path(stata_path, stata_bin)
  
  return(list(Exe = Exe, Vers = Vers))
}

connect_khelsa <- function(){
  RODBC::odbcConnectAccess2007(file.path(getOption("khfunctions.root"), 
                                         getOption("khfunctions.db")))
}

connect_khlogg <- function(){
  RODBC::odbcConnectAccess2007(file.path(getOption("khfunctions.root"), 
                                         getOption("khfunctions.logg")))
}