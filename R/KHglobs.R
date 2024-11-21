# Global parameters, starting point for SettGlobs()/FinnGlobs()
globglobs <- list(
  HOVEDmodus = "NH",
  KHaargang = 2025,
  KHgeoniv = "K",
  KHdbname = dbNameFile,
  KHlogg = dbLogFile,
  StablaDirNy = "PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE",
  StablaDirDat = "PRODUKTER/MELLOMPROD/R/STABLAORG/DATERT",
  KubeDir_NH = "PRODUKTER/KUBER/NORGESHELSA",
  KubeDirNy_NH = "PRODUKTER/KUBER/NORGESHELSA/NYESTE/R",
  KubeDirDat_NH = "PRODUKTER/KUBER/NORGESHELSA/DATERT",
  KubeDirQC_NH = "PRODUKTER/KUBER/NORGESHELSA/QC",
  KubeDir_KH = "PRODUKTER/KUBER/KOMMUNEHELSA",
  KubeDirNy_KH = "PRODUKTER/KUBER/KOMMUNEHELSA/NYESTE/R",
  KubeDirDat_KH = "PRODUKTER/KUBER/KOMMUNEHELSA/DATERT",
  KubeDirQC_KH = "PRODUKTER/KUBER/KOMMUNEHELSA/QC",
  KubeStataPrikkFil ="BIN/Z_Statasnutter/StataPrikking.do",
  KubeStataPrikkFil_geo = "BIN/Z_Statasnutter/StataPrikking_geo.do",
  FriskVDir_F = "PRODUKTER/KUBER/FRISKVIK_FYLKE",
  FriskVDir_K = "PRODUKTER/KUBER/FRISKVIK_KOMM",
  FriskVDir_B = "PRODUKTER/KUBER/FRISKVIK_BYDEL",
  ovpDir_F = "PRODUKTER/KUBER/OVP_FYLKE",
  ovpDir_K = "PRODUKTER/KUBER/OVP_KOMM",
  ovpDir_B = "PRODUKTER/KUBER/OVP_BYDEL",
  TNPDirNy = "PRODUKTER/MELLOMPROD/R/TNP/NYESTE",
  TNPDirDat = "PRODUKTER/MELLOMPROD/R/TNP/DATERT",
  BUFFERdir = "BIN/BUFFER",
  DUMPdir = "RUNTIMEDUMP",
  kolorgs = c("GEO", "AAR", "KJONN", "ALDER", "UTDANN", "INNVKAT", "LANDBAK", "TAB1", "TAB2", "TAB3", "VAL1", "VAL2", "VAL3"),
  taborgs = c("GEO", "AAR", "KJONN", "ALDER", "TAB1", "TAB2", "TAB3"),
  NesstarOutputDef = c(MT = "MALTALL", T = "TELLER", N = "NEVNER", RATE = "RATE", SMR = "SMR", MEIS = "MEIS", ST = "sumTELLER", SN = "sumNEVNER", SPT = "sumPREDTELLER", RN = "RATE.n"),
  FriskvikTabs = c("GEO", "AAR", "KJONN", "ALDER", "UTDANN", "INNVKAT", "LANDBAK", "ETAB"),
  FriskvikVals = c("sumTELLER", "sumNEVNER", "RATE", "MALTALL", "sumPREDTELLER", "PREDTELLER", "SMR", "NORM", "MEIS", "RATE.n"),
  QCVals = c("TELLER", "NEVNER", "sumTELLER", "sumNEVNER", "RATE.n"),
  KubeKols = c("sumTELLER", "sumNEVNER", "RATE", "MALTALL", "sumPREDTELLER", "PREDTELLER", "SMR", "NORM", "MEIS", "RATE.n", "ALDER", "AAR", "SMRtmp"),
  binDir = "bin",
  tmpfilerpath = "bin\tmpfiler",
  geo_illeg = "GGG",
  alder_illeg = "888_888",
  alder_ukjent = "999_999",
  kjonn_illeg = "8",
  kjonn_ukjent = "9",
  aar_illeg = "8888_8888",
  utdann_illeg = "8",
  utdann_ukjent = "9",
  landbak_illeg = "8",
  landbak_ukjent = "9",
  innvkat_illeg = "8",
  innvkat_ukjent = "9",
  SisteBatch = "9999-01-01-01-01",
  DefDumpFormat = "CSV",
  stjstr = "************************************************************\n",
  XLScols = as.vector(sapply(c("", as.vector(paste(sapply(c("", LETTERS[]), paste, LETTERS[], sep = "")))), paste, LETTERS[], sep = ""))
)

#' SettDefDesignKH (kb)
#' 
#' Setter standard designegenskaper, slik som delenes kolonnenavn og status i omkoding
#' Se tabell KH_DELER
#'
#' @param globs 
SettDefDesignKH <- function(globs = FinnGlobs()) {
  is_kh_debug()
  
  Deler <- RODBC::sqlQuery(globs$dbh, "SELECT * FROM KH_DELER", as.is = TRUE, stringsAsFactors = FALSE)
  # DelKols<-lapply(as.list(setNames(Deler$DelKols, Deler$DEL)),function(x){unlist(stringr::str_split(x,pattern=","))})
  # Tilrettelegging for enkle oppslag:
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
  KolsDel <- list()
  for (del in names(DelKols)) {
    if (DelType[del] == "INT") {
      DelKols[[del]] <- paste(DelKols[[del]], c("l", "h"), sep = "")
      DelKolsF[[del]] <- DelKols[[del]]
    }
    if (!(is.na(DelKolE[[del]]) | DelKolE[[del]] == "")) {
      DelKolsF[[del]] <- c(DelKolsF[[del]], unlist(stringr::str_split(DelKolE[[del]], ",")))
    }
    for (kol in DelKols[[del]]) {
      KolsDel[[kol]] <- del
    }
  }
  
  UBeting <- Deler$DEL[Deler$OMKODbet == "U"]
  BetingOmk <- Deler$DEL[Deler$OMKODbet == "B"]
  BetingF <- Deler$DEL[Deler$OMKODbet == "F"]
  OmkDel <- c(UBeting, BetingOmk)
  # IntervallHull<-list(A="DekkInt/TotInt>0.999 | (NTOT>=10 & NHAR/NTOT>0.8) | (TotInt<=20 & DekkInt>=10) | TotInt<=10")
  
  DesignKols <- c(unlist(DelKols[c(UBeting, BetingOmk)]))
  DesignKolsF <- c(DesignKols, unlist(DelKols[BetingF]))
  DesignKolsFA <- c(DesignKolsF, setdiff(unlist(DelKolsF[c(UBeting, BetingOmk)]), unlist(DelKols[c(UBeting, BetingOmk)])))
  
  return(
    list(
      DelKols = DelKols,
      DelKolsF = DelKolsF,
      KolsDel = KolsDel,
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
      IntervallHull = IntervallHull,
      AMissAllow = TRUE
    )
  )
}

#' SettKodeBokGlob (kb)
#'
#' @param globs 
SettKodeBokGlob <- function(globs = FinnGlobs()) {
  is_kh_debug()
  
  # Produces warning: NAs introduced by coercion
  # Happens when DefDesign$Delformat[del] == "integer", for dimensions where some values 
  # are not possible to convert to integer (e.g. LANDBAK = "1c").
  # Probably benign as these values are already recoded 
  
  OmkodD <- RODBC::sqlQuery(globs$dbh, "SELECT * FROM KH_OMKOD
                            UNION SELECT ID, DEL, KODE as NYKODE, KODE as ORGKODE, 0 as PRI_OMKOD, 1 AS OBLIG FROM KH_KODER", as.is = TRUE, stringsAsFactors = FALSE)
  KB <- list()
  
  for (del in names(globs$DefDesign$DelKolN)) {
    KBD <- subset(OmkodD, DEL == del)
    if (globs$DefDesign$DelType[del] == "INT") {
      if (nrow(KBD) > 0) {
        KBD[, c("ORGKODEl", "ORGKODEh", "NYKODEl", "NYKODEh")] <- as.integer(NA)
      } else {
        KBD <- cbind(KBD, data.frame(ORGKODEl = integer(0), ORGKODEh = integer(0), NYKODEl = integer(0), NYKODEh = integer(0)))
      }
    } else if (globs$DefDesign$DelFormat[del] == "integer") {
      KBD$ORGKODE <- as.integer(KBD$ORGKODE)
      KBD$NYKODE <- as.integer(KBD$NYKODE)
    }
    kbdnames <- names(KBD)
    kbdnames <- gsub("ORGKODE", globs$DefDesign$DelKolN[del], kbdnames)
    kbdnames <- gsub("NYKODE(h|l|)", paste(globs$DefDesign$DelKolN[del], "\\1_omk", sep = ""), kbdnames)
    kbdnames <- gsub("NYKODE(h|l|)", paste(globs$DefDesign$DelKolN[del], "\\1_omk", sep = ""), kbdnames)
    kbdnames <- gsub("PRI_OMKOD", paste(del, "_pri", sep = ""), kbdnames)
    kbdnames <- gsub("OBLIG", paste(del, "_obl", sep = ""), kbdnames)
    data.table::setnames(KBD, names(KBD), kbdnames)
    KB[[del]] <- KBD[, names(KBD)[!names(KBD) %in% c("ID", "DEL")]]
  }
  return(KB)
}

#' SettLegitimeKoder (kb)
#'
#' @param globs 
SettLegitimeKoder <- function(globs = FinnGlobs()) {
  is_kh_debug()
  
  # Produces warning: In data.frame(..., check.names = FALSE) : NAs introduced by coercion
  # Happens when DefDesign$Delformat[del] == "integer", for dimensions where some values 
  # are not possible to convert to integer (e.g. "1c").
  
  Koder <- RODBC::sqlQuery(globs$dbh, "SELECT * FROM KH_KODER", as.is = TRUE, stringsAsFactors = FALSE)
  KodeL <- list()
  for (del in unique(Koder$DEL)) {
    KodeD <- subset(Koder, DEL == del)
    if (globs$DefDesign$DelType[del] == "INT") {
      KodeD <- cbind(KodeD, setNames(matrix(as.integer(stringr::str_split_fixed(KodeD$KODE, "_", 2)), ncol = 2), globs$DefDesign$DelKols[[del]]))
    } else if (globs$DefDesign$DelFormat[del] == "integer") {
      KodeD <- setNames(cbind(KodeD, as.integer(KodeD$KODE)), c(names(KodeD), globs$DefDesign$DelKols[[del]]))
    } else if (globs$DefDesign$DelFormat[del] == "character") {
      KodeD <- setNames(cbind(KodeD, KodeD$KODE), c(names(KodeD), globs$DefDesign$DelKols[[del]]))
    }
    KodeL[[del]] <- KodeD
  }
  return(KodeL)
}

SettTotalKoder <- function(globs = FinnGlobs()) {
  is_kh_debug()
  
  Koder <- RODBC::sqlQuery(globs$dbh, "SELECT KH_KODER.DEL,KODE, FORMAT FROM KH_KODER INNER JOIN KH_DELER ON KH_KODER.DEL=KH_DELER.DEL WHERE TOTAL=1", as.is = TRUE, stringsAsFactors = FALSE)
  TotKoder <- list()
  for (del in Koder$DEL) {
    if (Koder$FORMAT[Koder$DEL == del] == "integer") {
      TotKoder[[del]] <- as.integer(Koder$KODE[Koder$DEL == del])
    } else {
      TotKoder[[del]] <- Koder$KODE[Koder$DEL == del]
    }
  }
  return(TotKoder)
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

#' SettGlobs (kb)
#'
#' @param path 
#' @param modus 
SettGlobs <- function(path = "", modus = NA) {
  is_kh_debug()
  
  # Close all active connections to avoid many connection open simultaneously
  RODBC::odbcCloseAll()
  
  # Les globglobs (se topp av fil)
  globs <- globglobs
  if (is.na(modus)) {
    modus <- globs$HOVEDmodus
  }
  
  if (modus == "KH") {
    globs$KubeDir <- globs$KubeDir_KH
    globs$KubeDirNy <- globs$KubeDirNy_KH
    globs$KubeDirDat <- globs$KubeDirDat_KH
  } else {
    globs$KubeDir <- globs$KubeDir_NH
    globs$KubeDirNy <- globs$KubeDirNy_NH
    globs$KubeDirDat <- globs$KubeDirDat_NH
  }
  
  dbFile <- globs$KHdbname
  logFile <- globs$KHlogg
  
  # If path is not provided, set it to defpath
  if (path == "" & file.exists(paste(defpath, dbFile, sep = "/"))) {
    path <- defpath
    cat("Setter path = ", path, "\n")
  }
  
  # If path is not given, and defpath is not found, print warning
  if (path == "") {
      cat(globs$stjstr, "******KRITISK FEIL: path ikke funnet\n******Har du tilgang til F:/?", globs$stjstr, sep = "")
  }
  
  # if path does not contain db file, print error and set path = ""
  if (isFALSE(file.exists(paste(path, dbFile, sep = "/")))) {
    cat(globs$stjstr, "******KRITISK FEIL: path har ikke hovedfila", KHdbname, globs$stjstr, sep = "")
    path <- ""
  }
  
  # If local path is set:
  if (path != "" & exists("setLocalPath", envir = .GlobalEnv)) {
    ## Use other location of KHELSA.mdb and KHlogg.mdb
    ## This is needed due to constant crash with unstable network
    path <- setLocalPath
  }
  
  # If path is valid, connect to database and reset path to rawPath for global parameters to work
  if(path != ""){
    # Sys.getenv("R_ARCH")   gir "/x64"eller "/i386"
    KHOc <- RODBC::odbcConnectAccess2007(paste(path, dbFile, sep = "/"))
    KHLc <- RODBC::odbcConnectAccess2007(paste(path, logFile, sep = "/"))
    
    path <- rawPath
  }
  
  globs <- c(globs, list(dbh = KHOc, log = KHLc, path = path))
  
  GeoNavn <- data.table::data.table(RODBC::sqlQuery(KHOc, "SELECT * from GeoNavn", as.is = TRUE))
  GeoKoder <- data.table::data.table(RODBC::sqlQuery(KHOc, "SELECT * from GEOKoder", as.is = TRUE), key = c("GEO"))
  UtGeoKoder <- GeoKoder[TYP == "O" & TIL == 9999]$GEO
  KnrHarm <- data.table::data.table(RODBC::sqlQuery(KHOc, "SELECT * from KnrHarm", as.is = TRUE), key = c("GEO"))
  TKNR <- data.table::data.table(RODBC::sqlQuery(KHOc, "SELECT * from TKNR", as.is = TRUE), key = c("ORGKODE"))
  HELSEREG <- data.table::data.table(RODBC::sqlQuery(KHOc, "SELECT * from HELSEREG", as.is = TRUE), key = c("FYLKE"))
  # Gjelder også for soner
  KnrHarmS <- lapply(KnrHarm[, c("GEO", "GEO_omk"), with = FALSE], function(x) {
    paste(x, "00", sep = "")
  })
  KnrHarmS <- cbind(as.data.frame(KnrHarmS, stringsAsFactors = FALSE), HARMstd = KnrHarm$HARMstd)
  KnrHarm <- rbind(KnrHarm, KnrHarmS)
  # Må legge til de som ikke omkodes for å lette bruk i merge
  # KnrHarm<-rbind(KnrHarm,data.frame(KNRorg=GeoKoder$GEO[TIL<2008],KNRharm=GeoKoder$GEO[TIL<2008],HARMstd=2008))
  
  # GK til bydel. Bør konsolideres med KnrHarm
  GkBHarm <- data.table::data.table(RODBC::sqlQuery(KHOc, "SELECT * FROM GKBydel2004T", as.is = TRUE), key = c("GK,Bydel2004"))
  
  globs$DefDesign <- SettDefDesignKH(globs = globs)
  globs$KB <- SettKodeBokGlob(globs = globs)
  globs$LegKoder <- SettLegitimeKoder(globs = globs)
  globs$TotalKoder <- SettTotalKoder(globs = globs)
  Stata <- FinnStataExe()
  globs$StataExe <- Stata$Exe
  globs$StataVers <- Stata$Vers
  RODBC::odbcCloseAll()
  return(c(globs, list(GeoNavn = GeoNavn, GeoKoder = GeoKoder, UtGeoKoder = UtGeoKoder, KnrHarm = KnrHarm, GkBHarm = GkBHarm, TKNR = TKNR, HELSEREG = HELSEREG)))
}

#' FinnGlobs (kb)
#'
#' Hjelperutine, bruker KHglobs eller SettGlobs()
FinnGlobs <- function() {
  is_kh_debug()
  
  globs <- NA
  if (exists("KHglobs")) {
    globs <- KHglobs
  } else {
    globs <- SettGlobs()
  }
  return(globs)
}

# Definer KHglobs
KHglobs <- SettGlobs()
RODBC::odbcCloseAll()
