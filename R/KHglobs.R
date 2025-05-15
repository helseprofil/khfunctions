#' @title SettGlobs
#' @description
#' Sets global parameters used for filgruppe and kube. 
#' @returns list
SettGlobs <- function(){
  RODBC::odbcCloseAll()
  path <- getOption("khfunctions.root")
  dbFile <- getOption("khfunctions.db")
  logFile <- getOption("khfunctions.logg")
  
  if (!dir.exists(path)) stop(paste0(path, " ikke funnet, Har du tilgang til O:/?"))
  if (!file.exists(file.path(path, dbFile))) stop(dbFile, " ikke funnet i ", path)
  globs <- list(dbh = connect_khelsa(), log = connect_khlogg())
  
  globs[["GeoKoder"]] <- data.table::setDT(RODBC::sqlQuery(globs$dbh, "SELECT * from GEOKoder", as.is = TRUE), key = "GEO")
  globs[["UtGeoKoder"]] <- globs[["GeoKoder"]][TYP == "O" & TIL == 9999, GEO]
  globs[["HELSEREG"]] <- data.table::setDT(RODBC::sqlQuery(globs$dbh, "SELECT * from HELSEREG", as.is = TRUE), key = c("FYLKE"))
  KnrHarm <- data.table::setDT(RODBC::sqlQuery(globs$dbh, "SELECT * from KnrHarm", as.is = TRUE), key = c("GEO"))
  KnrHarmS <- data.table::copy(KnrHarm)[, let(GEO = paste0(GEO, "00"), GEO_omk = paste0(GEO_omk, "00"))]
  globs[["KnrHarm"]] <- data.table::rbindlist(list(KnrHarm, KnrHarmS))
  globs[["GkBHarm"]] <- data.table::setDT(RODBC::sqlQuery(globs$dbh, "SELECT * FROM GKBydel2004T", as.is = TRUE), key = c("GK", "Bydel2004"))

  DELER <- data.table::setDT(RODBC::sqlQuery(globs$dbh, "SELECT * FROM KH_DELER WHERE DEL <> 'S'", as.is = TRUE))
  globs[["DefDesign"]] <- SettDefDesignKH(deler = DELER)
  globs[["KB"]] <- SettKodeBokGlob(globs = globs)
  globs[["LegKoder"]] <- SettLegitimeKoder(globs = globs)
  globs[["TotalKoder"]] <- getOption("khfunctions.totals")
  Stata <- FinnStataExe()
  globs[["StataExe"]] <- Stata$Exe
  globs[["StataVers"]] <- Stata$Vers
  return(globs)
}

#' @title SettDefDesignKH
#' @author Kåre Bævre
#' @description Setter standard designegenskaper, slik som delenes kolonnenavn og status i omkoding
#' Se tabell KH_DELER
#' @param deler 
SettDefDesignKH <- function(deler) {
  data.table::setorder(deler, "ID")
  DelKolN <- setNames(deler$DelKol, deler$DEL)
  DelKolE <- setNames(deler$DelKolE, deler$DEL)
  DelType <- setNames(deler$TYPE, deler$DEL)
  DelFormat <- setNames(deler$FORMAT, deler$DEL)
  AggPri <- deler[order(AGGREGERPRI), DEL]
  AggVedStand <- deler[AGGREGERvedPRED == 1, DEL]
  IH <- deler[!is.na(INTERVALLHULL) & INTERVALLHULL != "", .(INTERVALLHULL, DEL)]
  IH <- setNames(IH$INTERVALLHULL, IH$DEL)
  
  DelKols <- as.list(DelKolN)
  DelKolsF <- DelKols
  for (del in names(DelKols)) {
    if (DelType[del] == "INT") {
      DelKols[[del]] <- paste0(DelKols[[del]], c("l", "h"))
      DelKolsF[[del]] <- DelKols[[del]]
    }
    if (!(is.na(DelKolE[[del]]) | DelKolE[[del]] == "")) {
      DelKolsF[[del]] <- c(DelKolsF[[del]], unlist(strsplit(DelKolE[[del]], ",")))
    }
  }
  
  UBeting <- deler$DEL[deler$OMKODbet == "U"]
  BetingOmk <- deler$DEL[deler$OMKODbet == "B"]
  BetingF <- deler$DEL[deler$OMKODbet == "F"]
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
      IntervallHull = IH
    )
  )
}

#' @title SettKodeBokGlob
#' @author Kåre Bævre
#' @param globs global parameters, defaults to SettGlobs
SettKodeBokGlob <- function(globs) {
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

#' @title SettLegitimeKoder
#' @author Kåre Bævre
#' @param globs global parameters, defaults to SettGlobs
SettLegitimeKoder <- function(globs) {
  Koder <- data.table::setDT(RODBC::sqlQuery(globs$dbh, "SELECT * FROM KH_KODER WHERE DEL <> 'S'", as.is = TRUE))
  KodeL <- list()
  for (del in unique(Koder$DEL)) {
    KodeD <- Koder[DEL == del]
    if (globs$DefDesign$DelType[del] == "INT") {
      KodeD[, (globs$DefDesign$DelKols[[del]]) := data.table::tstrsplit(KODE, "_", fixed = TRUE)]
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

#' @title FinnStataExe 
#' @author Yusman Kamaleri
#' @description Find the most recent version of locally installed Stata
#' @noRd
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


#' @title connect_khelsa
#' @description connects to khelsa.mdb
connect_khelsa <- function(){
  RODBC::odbcConnectAccess2007(file.path(getOption("khfunctions.root"), 
                                         getOption("khfunctions.db")))
}

#' @title connect_khlogg
#' @description connects to khlogg.mdb
connect_khlogg <- function(){
  RODBC::odbcConnectAccess2007(file.path(getOption("khfunctions.root"), 
                                         getOption("khfunctions.logg")))
}
