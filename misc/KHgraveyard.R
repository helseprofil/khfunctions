# Functions that apparently are no longer in use, with comment

# Not used 
SySammenFiler <- function(FILID1, FILID2, batch1 = NA, batch2 = NA, ROLLE1 = "", ROLLE2 = "", globs = FinnGlobs()) {
  is_kh_debug()
  
  return(SySammenTabeller(
    FinnFilT(FILID1, batch = batch1, ROLLE = ROLLE1, globs = globs),
    FinnFilT(FILID2, batch = batch2, ROLLE = ROLLE2, globs = globs)
  ))
}

# Only used by SySammenFiler (deprecated)
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

# Not used
OmkodFilFraPartM <- function(Fil, Part, FGP = list(amin = 0, amax = 120), rapport = list(), globs = FinnGlobs()) {
  is_kh_debug()
  
  rapport["KALL"] <- "OmkodFilFraPart"
  for (del in names(Part)) {
    Fil <- OmkodFilFraPart(Fil, Part[[del]], FGP, rapport = rapport, globs = globs)
  }
  return(Fil)
}

# Not used
OmkodFilFraDesign <- function(Fil, Design, FGP = list(amin = 0, amax = 120), rapport = list(), globs = FinnGlobs()) {
  is_kh_debug()
  
  rapport["KALL"] <- "OmkodFilFraDesign"
  Dorg <- FinnDesign(Fil, FGP = FGP, globs = globs)
  RD <- FinnRedesign(Dorg, Design, globs = globs)
  return(OmkodFil(Fil, RD, rapport = rapport, globs = globs))
}

# Not used
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

# New function exists
Xls2R.KH.Gammel <- function(xlsfil, ark = "", globs = FinnGlobs(), brukfread = TRUE, na.strings = c("NA"), ryddOpp = 1, ...) {
  is_kh_debug()
  
  err <- ""
  ok <- 1
  DF <- data.frame()
  # step 1: Validate sheetname with fuzzy match
  rdbh <- odbcConnectExcel2007(xlsfil)
  # rdbh<-odbcConnectExcel(xlsfil)
  tables <- sqlTables(rdbh)$TABLE_NAME
  close(rdbh)
  
  tables <- gsub("\'", "", tables)
  tables <- gsub("\\$", "", tables) # Something is strange with resepct to $ in R's regexp-syntax, but should work
  if (ark == "" | is.na(ark)) {
    ark <- tables[1]
  } else if (!(ark %in% tables)) {
    kand <- tables[grepl(ark, tables, ignore.case = TRUE)]
    if (length(kand) == 1) {
      ark <- kand[1]
    } else if (length(kand) > 1) {
      err <- paste("Arknavn ", ark, " ikke unik, passer med flere av (", paste(tables, collapse = ","), ")", sep = "")
      ok <- 0
    } else {
      err <- paste("Arknavn ", ark, " finnes ikke (", paste(tables, collapse = ","), ")", sep = "")
      ok <- 0
    }
  }
  if (ok == 1) {
    # Step 2: convert xls to temporary csv
    tmpcsvfil <- Xls2TmpCsv(xlsfil, sheet = ark, globs = globs)
    # Step 3: read csv. Note: fread (from data.table) can be picky if number of columns in each row is not equal
    # But it is considerably faster than read.csv
    if (brukfread == TRUE) {
      INNLES <- try(as.data.frame(fread(tmpcsvfil, sep = ",", colClasses = "character", header = FALSE, skip = 0, na.strings = na.strings)))
    } else {
      INNLES <- try(read.csv(tmpcsvfil, sep = ",", colClasses = "character", header = FALSE, skip = 0, blank.lines.skip = FALSE, na.strings = na.strings))
    }
    if (class(INNLES) == "try-error") {
      err <- INNLES
      ok <- 0
    } else {
      DF <- INNLES
    }
    
    # Clean up!!!
    if (ryddOpp == 1) {
      file.remove(tmpcsvfil)
    }
  }
  return(list(DF = DF, ok = ok, err = err))
}

# Only called by Xls2R.KH.Gammel (deprecated)
Xls2TmpCsv <- function(xlsfil, sheet, globs = FinnGlobs()) {
  # Calls on VB-script that converts sheet in xlsfil to csv
  # Should use tempdir()?
  is_kh_debug()
  
  orig_wd <- getwd()
  setwd(paste(globs$path, globs$binDir, sep = "/"))
  xlsfil <- gsub("/", "\\\\", xlsfil)
  # print(paste("XLStoCSVconverter.vbs \"", xlsfil, "\" \"",sheet,"\"",sep=""))
  shell(paste("XLStoCSVconverter.vbs \"", xlsfil, "\" \"", sheet, "\"", sep = ""), intern = TRUE)
  # shell(paste("XLStoCSVconverter.vbs \"", xlsfil, "\"",sep=""), intern = TRUE)
  setwd(orig_wd)
  return(paste(globs$path, "/", globs$binDir, "/tmpfiler/XLStoCSVconverterTmpCsv", ".csv", sep = ""))
}

# Not used
FullDuplikatSjekk <- function(globs = FinnGlobs()) {
  is_kh_debug()
  
  FILGRUPPER <- unlist(sqlQuery(globs$dbh, "SELECT DISTINCT FILGRUPPE FROM FILGRUPPER"))
  FGRapport <- c(FILGRUPPE = character(0), ANTdNO = integer(0), fANTV1 = integer(0), ANTdNOp = integer(0), fANTV1p = integer(0), ANTdNOg = integer(0), fANTV1g = integer(0))
  for (fgruppe in FILGRUPPER) {
    # if (grepl("_L$",fgruppe) || !grepl("_",fgruppe)){
    if (!(grepl("_", fgruppe) | fgruppe %in% c("NETTOFLYTT"))) {
      print(fgruppe)
      print(SjekkDuplikaterFG(fgruppe, FullResult = FALSE))
      FGRapport <- rbind(FGRapport, c(FILGRUPPE = fgruppe, SjekkDuplikaterFG(fgruppe, FullResult = FALSE)))
    }
  }
  return(FGRapport)
}

# Only called by FullDuplikatSjekk
SjekkDuplikaterFG <- function(filgruppe, FullResult = TRUE) {
  is_kh_debug()
  
  return(SjekkDuplikater(FinnFilT(filgruppe, IDKOLS = TRUE), filgruppe, FullResult = FullResult))
}

# Not used
LesFilNo <- function(id, y = "", globs = FinnGlobs()) {
  is_kh_debug()
  
  filbesk <- FinnFilBeskFilid(id, globs = globs)
  print(filbesk$FILNAVN)
  filbesk$filn <- paste(globs$path, filbesk$FILNAVN, sep = "/")
  filbesk$filn <- gsub("\\\\", "/", filbesk$filn)
  # filn<-paste(globs$path,filbesk$FILNAVN,sep="\\")
  # print(filn)
  # filn<-sub("\\[\\$y\\]",y,filn)
  # return(LesFil(filn=filn,format=filbesk$FORMAT,opt=filbesk$INNLESARG))
  return(LesFil(filbesk, globs = globs)$DF)
}

# Not used
LagTabellFraFilNo <- function(id, batchdate = SettKHBatchDate(), y = "", globs = FinnGlobs(), echo = FALSE) {
  is_kh_debug()
  
  filbesk <- FinnFilBeskFilid(id, batchdate = batchdate, globs = globs)
  FGP <- FinnFilgruppeParametre(filbesk$FILGRUPPE, batchdate = batchdate, globs = globs)
  return(LagTabellFraFil(filbesk, FGP = FGP, globs = globs, echo = echo))
}

# Not used
ListAlleOriginalFiler <- function(globs = FinnGlobs()) {
  is_kh_debug()
  
  print(sqlQuery(globs$log, "DROP TABLE ALLEFILER"))
  setwd(paste(globs$path, "ORGDATA", sep = "/"))
  Filer <- setNames(as.data.frame(list.files(recursive = TRUE), stringsAsFactors = FALSE), c("FILNAVN"))
  Filer$TYP <- NA
  Filer$TYP[grepl("/ORG/", Filer$FILNAVN)] <- "ORG"
  Filer$TYP[grepl("/GML/", Filer$FILNAVN)] <- "GML"
  Filer$TYP[grepl("/MOTTAK/", Filer$FILNAVN)] <- "MOTTAK"
  Filer$TYP[grepl("/ARKIV/", Filer$FILNAVN)] <- "ARKIV"
  Filer$FILNAVN <- paste("ORGDATA", Filer$FILNAVN, sep = "/")
  Filer$FILNAVN <- gsub("/", "\\\\", Filer$FILNAVN)
  
  print(head(Filer))
  sqlSave(globs$log, Filer, "ALLEFILER", rownames = FALSE)
}

# Only called by LesFilNo (deprecated) and LagTabellFraFilNo (deprecated)
FinnFilBeskFilid <- function(filid, batchdate = NULL, globs = FinnGlobs()) {
  is_kh_debug()
  
  # Default er å finne filbesk gyldige nå (Sys.time)
  datef <- format(Sys.time(), "#%Y-%m-%d#")
  # ALternativt kan man finne for en historisk batchdate
  if (!is.null(batchdate)) {
    datef <- format(strptime(batchdate, "%Y-%m-%d-%H-%M"), "#%Y-%m-%d#")
  }
  sqlt <- paste("SELECT KOBLID, ORIGINALFILER.FILID AS FILID, FILNAVN, FORMAT, DEFAAR, INNLESING.*
              FROM INNLESING INNER JOIN
              (  ORGINNLESkobl INNER JOIN ORIGINALFILER
              ON ORGINNLESkobl.FILID = ORIGINALFILER.FILID)
              ON   (INNLESING.DELID = ORGINNLESkobl.DELID)
              AND (INNLESING.FILGRUPPE = ORGINNLESkobl.FILGRUPPE)
              WHERE ORIGINALFILER.FILID=", filid, "
              AND ORIGINALFILER.IBRUKFRA<=", datef, "
              AND ORIGINALFILER.IBRUKTIL>", datef, "
              AND INNLESING.VERSJONFRA<=", datef, "
              AND INNLESING.VERSJONTIL>", datef, sep = "")
  fb <- sqlQuery(globs$dbh, sqlt, as.is = TRUE, stringsAsFactors = FALSE)
  fb$filn <- paste(globs$path, fb$FILNAVN, sep = "/")
  fb$filn <- gsub("\\\\", "/", fb$filn)
  fb$AAR <- gsub("<\\$y>", paste("<", fb$DEFAAR, ">", sep = ""), fb$AAR)
  return(fb)
}

# Only used to create object BUFFER, which has been inactive
# BUFFER<-SetBuffer(filer=c("NPR","DAAR","BEFOLK","RESEPT"))
SetBuffer <- function(filer = c("BEFOLK"), globs = FinnGlobs()) {
  is_kh_debug()
  BUFFER <- list()
  for (filtag in filer) {
    fil <- FinnFilN(filtag, globs = globs)
    if (fil$ok == 1) {
      BUFFER[[filtag]] <- readRDS_KH(fil$filn)
      cat("Lest inn ", filtag, "=", fil$filn, "i buffer\n")
    } else {
      print(fil$err)
    }
  }
  return(BUFFER)
}

# Only called in SetBuffer (deprecated)
FinnFilN <- function(filstr, versjonert = FALSE, batch = NA, globs = FinnGlobs()) {
  is_kh_debug()
  
  if (!is.na(batch)) {
    filn <- paste(globs$path, globs$StablaDirDat, filstr, "_", batch, ".rds", sep = "")
  } else if (versjonert == TRUE) {
    orgwd <- getwd()
    path <- paste(globs$path, "/", globs$StablaDirDat, sep = "")
    print(path)
    setwd(path)
    Filer <- unlist(list.files(include.dirs = FALSE))
    Filer <- Filer[grepl(paste("^", filstr, "_", sep = ""), Filer)]
    filn <- paste(path, "/", Filer[order(Filer)][length(Filer)], sep = "")
    batch <- gsub(".*_(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2})$", "\\1", Filer[order(Filer)][length(Filer)])
    setwd(orgwd)
  } else {
    filn <- paste(globs$path, globs$StablaDirDat, filstr, "_", batch, ".rds", sep = "")
  }
  ok <- 1
  err <- ""
  if (file.access(filn, mode = 0) == -1) {
    err <- paste("KRITISK FEIL: ", filn, " finnes ikke", sep = "")
    ok <- 0
  } else if (file.access(filn, mode = 4) == -1) {
    err <- paste("KRITISK FEIL: ", filn, " finnes, men lar seg ikke lese", sep = "")
    ok <- 0
  }
  return(list(filn = filn, ok = ok, err = err, batch = batch))
}

# KHfunctions is backed up on github, KHELSA is backed up separately
backup <- function(filename = c("KHfunctions.R", "KHELSA.mdb"), force = FALSE, ...) {
  is_kh_debug()
  
  ## force : TRUE hvis man skal arkivere filen uansett ellers
  ## for KHFunction.R sjekkes det dato filen er lagret først
  
  if (isTRUE(grepl("function", filename))) {
    valgFil <- "fun"
  }
  if (isTRUE(grepl("KHELSA", filename))) {
    valgFil <- "mdb"
  }
  
  ## if (!require(RODBC)) {install.packages("RODBC")}
  ## require(RODBC)
  date <- format(Sys.time(), "%Y%m%d%H%M")
  
  switch(valgFil,
         
         ## Access Tabell
         "mdb" = {
           styrpath <- file.path(defpath, "STYRING")
           styrpath_b <- file.path(defpath, "STYRING", "VERSJONSARKIV")
           styrvfiles <- list.files(path = styrpath_b)
           
           KHcFN <- paste(styrpath, filename, sep = "/")
           KHvFN <- paste(styrpath_b, sort(styrvfiles[grepl("^KHELSA\\d+.mdb$", styrvfiles)], decreasing = TRUE)[1], sep = "/")
           
           khc <- RODBC::odbcConnectAccess2007(KHcFN)
           khv <- RODBC::odbcConnectAccess2007(KHvFN)
           
           
           nytt <- 0
           # Sammenlign
           for (TAB in c("INNLESING", "KUBER", "TNP_PROD", "FILGRUPPER", "ORGINNLESkobl", "ORIGINALFILER")) {
             TABc <- sqlQuery(khc, paste("SELECT * FROM", TAB))
             TABv <- sqlQuery(khv, paste("SELECT * FROM", TAB))
             if (!identical(TABc, TABv)) {
               nytt <- 1
             }
           }
           
           if (nytt == 1) {
             KHnFN <- sub(filename, paste("KHELSA", date, ".mdb", sep = ""), KHcFN)
             KHnFN <- sub(styrpath, styrpath_b, KHnFN)
             file.copy(KHcFN, KHnFN)
           }
         },
         
         ## KHFunction
         "fun" = {
           binpath <- file.path(defpath, "BIN")
           binpath_b <- file.path(defpath, "BIN", "VERSJONSARKIV")
           binvfiles <- list.files(path = binpath_b)
           
           fil <- "KHfunctions"
           
           arkivFil <- sort(binvfiles[grepl(
             paste("^", fil, "\\d+\\.r", sep = ""),
             binvfiles
           )], decreasing = TRUE)[1]
           
           ## Fil i BIN som brukes
           FILc <- paste(binpath, filename, sep = "/")
           
           ## Fil i VERSJONSARKIV
           FILv <- paste(binpath_b, arkivFil, sep = "/")
           
           if (file.info(FILc)$mtime > file.info(FILv)$mtime) {
             FILn <- sub(filename, paste(fil, date, ".R", sep = ""), FILc)
             FILn <- sub(binpath, binpath_b, FILn)
             file.copy(FILc, FILn)
           } else {
             cat("## --- Filen er ikke nyere enn i akrivet --- ##\n")
           }
           
           ## Arkiveres uansett
           if (isTRUE(force)) {
             FILn <- paste(binpath_b, "/", fil, date, ".R", sep = "")
             file.copy(FILc, FILn)
           }
         }
  )
}

# Not used
SettPredFilterGml <- function(refvstr, FGP = list(amin = 0, amax = 120), globs = FinnGlobs()) {
  is_kh_debug()
  PredFilter <- list()
  Pcols <- character(0)
  # Må utvikles til å lese KUBEdscr$REFVERDI
  if (is.null(refvstr) || is.na(refvstr)) {
    PredFilter <- list(Gn = data.frame(GEOniv = "L"))
  } else {
    refvstr <- gsub("(.*)ALDER=='*ALLE'*(.*)", paste("\\1", "ALDER==", FGP$amin, "_", FGP$amax, "\\2", sep = ""), refvstr)
    for (del in names(globs$DefDesign$DelKolN)) {
      delN <- globs$DefDesign$DelKolN[del]
      if (globs$DefDesign$DelType[del] == "COL") {
        if (grepl(paste("(^|\\&) *", delN, " *== *'*(.*?)'* *(\\&|$)", sep = ""), refvstr)) {
          Pcols <- c(Pcols, globs$DefDesign$DelKolsF[[del]])
          val <- gsub(paste(".*(^|\\&) *", delN, " *== *'*(.*?)'* *(\\&|$).*", sep = ""), "\\2", refvstr)
          if (globs$DefDesign$DelFormat[del] == "integer") {
            PredFilter[[del]] <- eval(parse(text = paste("data.frame(", delN, "=", as.integer(val), ",stringsAsFactors=FALSE)", sep = "")))
          } else {
            PredFilter[[del]] <- eval(parse(text = paste("data.frame(", delN, "=\"", val, "\",stringsAsFactors=FALSE)", sep = "")))
          }
        }
      } else if (globs$DefDesign$DelType[del] == "INT") {
        if (grepl(paste("(^|\\&) *", delN, "l *== *'*(.*?)'* *($|\\&)", sep = ""), refvstr) &&
            grepl(paste("(^|\\&) *", delN, "h *== *'*(.*?)'* *($|\\&)", sep = ""), refvstr)) {
          Pcols <- c(Pcols, globs$DefDesign$DelKolsF[[del]])
          vall <- gsub(paste(".*(^|\\&) *", delN, "l *== *'*(.*?)'* *($|\\&).*", sep = ""), "\\2", refvstr)
          valh <- gsub(paste(".*(^|\\&) *", delN, "h *== *'*(.*?)'* *($|\\&).*", sep = ""), "\\2", refvstr)
          PredFilter[[del]] <- eval(parse(text = paste("data.frame(", delN, "l=", as.integer(vall), ",", delN, "h=", as.integer(valh), ",stringsAsFactors=FALSE)", sep = "")))
        } else if (grepl(paste("(^|\\&) *", delN, " *== *'*(.*?)'* *($|\\&)", sep = ""), refvstr)) {
          intval <- as.integer(unlist(stringr::str_split(gsub(paste("(^|.*\\&) *", delN, " *== *'*(.*?)'* *($|\\&.*)", sep = ""), "\\2", refvstr), "_")))
          if (length(intval) == 1) {
            intval <- c(intval, intval)
          }
          # val<-gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),"\\2",refvstr)
          # refvstr<-gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),
          #          paste("\\1", paste(delN,"l",sep=""),"==\\2 &"," \\1 ",paste(delN,"l",sep=""),"==\\2"," \\3",sep=""),refvstr)
          refvstr <- gsub(
            paste("(^|.*\\&) *", delN, " *== *'*(.*?)'* *($|\\&.*)", sep = ""),
            paste("\\1", paste(delN, "l", sep = ""), "==", intval[1], " & ", paste(delN, "h", sep = ""), "==", intval[2], "\\3", sep = ""), refvstr
          )
          Pcols <- c(Pcols, globs$DefDesign$DelKolsF[[del]])
          PredFilter[[del]] <- eval(parse(text = paste("data.frame(", delN, "l=", intval[1], ",", delN, "h=", intval[2], ",stringsAsFactors=FALSE)", sep = "")))
        }
      }
    }
  }
  return(list(Design = PredFilter, PfiltStr = refvstr, Pkols = Pcols))
}

# Not used
KHaggregerM <- function(FILn = "FG", orgcols, vals = list(), snitt = FALSE, globs = FinnGlobs()) {
  is_kh_debug()
  
  sumexp <- list()
  
  sumexp$tabnames <- globs$DefDesign$DesignKolsFA[globs$DefDesign$DesignKolsFA %in% orgcols]
  # tabnames<-names(FIL)[!grepl("^VAL\\d+(f|)$",names(FIL))]
  valkols <- names(FIL)[!orgcols %in% tabnames]
  valkols <- valkols[!grepl("\\.(f|a)", valkols)]
  valkols <- valkols[!valkols %in% c("KOBLID", "ROW")]
  setkeym(FIL, tabnames) # Sjekk om key ok for å effektivisere?
  if (snitt == FALSE) {
    sumexp$tr1 <- paste("list(",
                        paste(valkols, "=sum(", valkols, "),",
                              valkols, ".f=max(", valkols, ".f),",
                              valkols, ".a=sum(", valkols, ".a*(!is.na(", valkols, ") & ", valkols, "!=0))",
                              sep = "", collapse = ","
                        ),
                        ")",
                        sep = ""
    )
    # FILa<-FIL[, eval(parse(text=lp)), by=tabnames]
  } else {
    # Sett også hjelpestørrelser to vurdering av snitt
    sumexp$tr1 <- paste("list(",
                        paste(valkols, "=sum(", valkols, ",na.rm=TRUE),",
                              valkols, ".f=max(", valkols, ".f),",
                              valkols, ".a=sum(", valkols, ".a*(!is.na(", valkols, ") & ", valkols, "!=0)),",
                              valkols, ".fn1=sum(", valkols, ".f==1),",
                              valkols, ".fn3=sum(", valkols, ".f>1),",
                              valkols, ".n=.N",
                              sep = "", collapse = ","
                        ),
                        ")",
                        sep = ""
    )
    # FILa<-FIL[, eval(parse(text=lp)), by=tabnames]
    # Anonymiser, trinn 1
    # Filtrer snitt som ikke skal brukes pga for mye anonymt
    anon_tot_tol <- 0.2
    sumexp$anon <- paste(FGn, "[,':='(",
                         paste(valkols, "=ifelse(", valkols, ".fn3/", valkols, ".n>=", anon_tot_tol, ",NA,", valkols, "),",
                               valkols, ".f=ifelse(", valkols, ".fn3/", valkols, ".n>=", anon_tot_tol, ",3,", valkols, ".f)",
                               sep = "", collapse = ","
                         ),
                         ")]",
                         sep = ""
    )
    # eval(parse(text=lp))
    # FILa<-FILa[,orgcols,with=FALSE]
  }
  
  return(sumexp)
  
  vals <- vals[valkols]
  usumbar <- valkols[unlist(lapply(vals[valkols], function(x) {
    x$sumbar == 0
  }))]
  for (val in valkols) {
    if (!is.null(vals[[val]]) && vals[[val]]$sumbar == 0) {
      eval(parse(text = paste(
        "FILa[", val, ".a>1,c(\"", val, "\",\"", val, ".f\"):=list(NA,2)]",
        sep = ""
      )))
    }
  }
  setkeym(FIL, orgkeys)
  if (identical(orgclass, "data.frame")) {
    FIL <- data.frame(FIL)
  }
  return(FILa)
}

# Not used
LeggDelTilDesign <- function(OrgDes, NyDel, globs = FinnGlobs()) {
  is_kh_debug()
  
  OrgDes$Part <- c(OrgDes$Part, NyDel)
  # OrgDes$OmkDesign<-
  # Kryss NyDeler med gamle, HAR er OK
  delerlist <- paste("as.data.frame(NyDel[[\"", names(NyDel), "\"]])", sep = "", collapse = ",")
  OrgDes$OmkDesign <- data.table(eval(parse(text = paste("expand.grid.df(as.data.frame(OrgDes$OmkDesign),", delerlist, ")", sep = ""))))
  setkeym(OrgDes$OmkDesign, globs$DefDesign$DesignKols[globs$DefDesign$DesignKols %in% names(OrgDes$OmkDesign)])
  return(OrgDes)
}

# Not used
ModifiserDesignFullRekt <- function(Nytt, Org = list(), globs = FinnGlobs()) {
  is_kh_debug()
  
  Nkombs <- 1
  for (del in names(Org$Part)) {
    Nkombs <- Nkombs * nrow(Org$Part[[del]])
  }
  
  for (del in names(Nytt)) {
    delT <- as.data.table(Nytt[[del]])
    delT[, paste(del, "_HAR", sep = "")] <- 1
    Org$Part[[del]] <- delT
    Nkombs <- Nkombs * nrow(delT)
  }
  
  
  delerlist <- paste("as.data.frame(Org[[\"Part\"]][[\"", names(Org$Part), "\"]])", sep = "", collapse = ",")
  # delerlist<-paste("as.data.frame(Org[[\"Part\"]][[\"",names(Org$Part)[names(Org$Part) %in% c("Gn","Y","K","A")],"\"]])",sep="",collapse=",")
  FullDesign <- data.table(eval(parse(text = paste("expand.grid.df(", delerlist, ")", sep = ""))))
  FullDesign[, HAR := 1]
  OmkKols <- globs$DefDesign$DesignKols[globs$DefDesign$DesignKols %in% names(FullDesign)]
  setkeym(FullDesign, OmkKols)
  Org[["OmkDesign"]] <- FullDesign[, list(HAR = max(HAR)), by = OmkKols]
  
  # Merk, det gir bare mening å bruke denne for å lage et TIL-design, da trengs ikke de følgende delene
  # Om det modifiserte designet skal brukes som et FRA-design må også disse endres. Det er en klønete operasjon (og som vel knapt er veldefinert)
  # Kan altså IKKE bruke FinnFellesTab(Org,ModifiserDesign(PredFilter,Org))
  
  Org[["Design"]] <- NULL
  Org[["SKombs"]] <- NULL
  Org[["FKombs"]] <- NULL
  
  return(Org)
}

# Not used
# NEINEI dette funker ikke!!!!!
ModifiserDesignFraParts <- function(NyPart, Org = list(), FGP = list(amin = 0, amax = 120), globs = FinnGlobs()) {
  is_kh_debug()
  
  if (length(Org) == 0) {
    delerlist <- paste("as.data.frame(NyPart[[\"", names(NyPart), "\"]])", sep = "", collapse = ",")
    FullDesign <- data.table(eval(parse(text = paste("expand.grid.df(", delerlist, ")", sep = ""))))
  } else {
    FullDesign <- Org$Design
    # Filtrer FullDesign til NyPart
    for (del in names(NyPart)) {
      setkeyv(NyPart[[del]], globs$DefDesign$DelKols[[del]])
      setkeyv(FullDesign, globs$DefDesign$DelKols[[del]])
      FullDesign <- FullDesign[NyPart[[del]]]
    }
    FullDesign <- subset(FullDesign, HAR == 1)
  }
  return(FinnDesign(FullDesign, FGP = FGP, globs = globs))
}

# Not used
KHsum <- function(x) {
  is_kh_debug()
  
  sum <- NA
  if (length(grep("\\d", x)) > 0) {
    if ("." %in% x || ".." %in% x || ":" %in% x) {
      sum <- "."
    } else {
      sum <- sum(as.numeric(x))
    }
  } else if (all(x == "..")) {
    sum <- ".."
  } else if (all(x == ":")) {
    sum <- ":"
  } else {
    sum <- "."
  }
  return(sum)
}

# not used
DFHeadToString2 <- function(innDF) {
  is_kh_debug()
  
  # return(paste(capture.output(print(head(innDF))),collapse="' & Chr(13) & Chr(10) & '"))
  return(paste(capture.output(print(head(innDF))), collapse = "\n"))
}

# not used
YAlagLH <- function(FG, YL, AL, vals = FinnValKols(names(FG)), globs = FinnGlobs()) {
  is_kh_debug()
  
  setDT(FG)
  FGl <- copy(FG)
  tabkols <- setdiff(names(FG), FinnValKolsF(names(FG)))
  FGl[, c("lAARl", "lAARh", "lALDERl", "lALDERh") := list(AARl + YL, AARh + YL, ALDERl + AL, ALDERh + AL)]
  FGl[, c("AARl", "AARh", "ALDERl", "ALDERh") := list(NULL)]
  setnames(FGl, c("lAARl", "lAARh", "lALDERl", "lALDERh"), c("AARl", "AARh", "ALDERl", "ALDERh"))
  lvals <- paste("YL", YL, "_AL", AL, "_", vals, c("", ".f", ".a"), sep = "")
  setnames(FGl, paste(vals, c("", ".f", ".a"), sep = ""), lvals)
  FGl <- FGl[, c(tabkols, lvals), with = FALSE]
  setkeyv(FG, tabkols)
  setkeyv(FGl, tabkols)
  FG <- FGl[FG]
  return(FG)
}

#not used
YAlagMerge <- function(FG, YL, AL, vals = FinnValKols(names(FG)), globs = FinnGlobs()) {
  is_kh_debug()
  
  setDT(FG)
  orgkols <- names(FG)
  FGl <- copy(FG)
  FGl[, c("lAARl", "lALDERl") := list(AARl + YL, ALDERl + AL)]
  FGl[, c("AARl", "AARh", "ALDERl", "ALDERh") := list(NULL)]
  setnames(FGl, c("lAARl", "lALDERl"), c("AARl", "ALDERl"))
  tabkols <- setdiff(names(FGl), FinnValKolsF(names(FG)))
  lvals <- paste("YL", YL, "_AL", AL, "_", vals, c("", ".f", ".a"), sep = "")
  setnames(FGl, paste(vals, c("", ".f", ".a"), sep = ""), lvals)
  FGl <- FGl[, c(tabkols, lvals), with = FALSE]
  setkeyv(FG, tabkols)
  setkeyv(FGl, tabkols)
  FG <- FGl[FG, allow.cartesian = TRUE][, c(orgkols, lvals), with = FALSE]
  return(FG)
}

# Not used
# http://ctszkin.com/2012/03/11/generating-a-laglead-variables/
shift <- function(x, shift_by) {
  is_kh_debug()
  
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by) > 1) {
    return(sapply(shift_by, shift, x = x))
  }
  
  out <- NULL
  abs_shift_by <- abs(shift_by)
  if (shift_by > 0) {
    out <- c(tail(x, -abs_shift_by), rep(NA, abs_shift_by))
  } else if (shift_by < 0) {
    out <- c(rep(NA, abs_shift_by), head(x, -abs_shift_by))
  } else {
    out <- x
  }
  out
}

# Not used
LagDesignRapportAlle <- function() {
  is_kh_debug()
  
  globs <- FinnGlobs()
  FILGRP <- unlist(sqlQuery(globs$dbh, "SELECT FILGRUPPE FROM FILGRUPPER"))
  for (FILgrp in FILGRP) {
    LagDesignRapport(FILgrp)
  }
}

# Not used
LagFilgrupperForFilter <- function(filter, versjonert = FALSE) {
  is_kh_debug()
  
  globs <- FinnGlobs()
  FILGRP <- unlist(sqlQuery(globs$dbh, paste("SELECT FILGRUPPE FROM FILGRUPPER WHERE ", filter, "='1'", sep = ""), as.is = TRUE))
  print(FILGRP)
  # for (FILgrp in FILGRP){
  batchdate <- SettKHBatchDate()
  LagFlereFilgrupper(filgrupper = FILGRP, batchdate = batchdate, globs = globs, versjonert = versjonert)
  # FG<-LagFilgruppe(FILgrp,versjonert=versjonert,globs=globs)
  # }
}

# Only called by LagDesignRapportAlle (deprecated)
LagDesignRapport <- function(filgrp) {
  is_kh_debug()
  
  print(filgrp)
  FG <- FinnFilT(filgrp)
  FGd <- FinnDesign(FG)$Design
  Ubalans <- nrow(subset(FGd, HAR == 0))
  cat("filgrp ", Ubalans, "/", nrow(FGd), "\n")
  return(FGd)
}

# not used
FinnNegVal <- function() {
  is_kh_debug()
  
  globs <- FinnGlobs()
  FILGRP <- unlist(sqlQuery(globs$dbh, "SELECT FILGRUPPE FROM FILGRUPPER"))
  for (FILgrp in FILGRP) {
    FG <- FinnFilT(FILgrp)
    valkols <- FinnValKols(names(FG))
    valkols <- valkols[!grepl("\\.(f|a)$", valkols)]
    for (valk in valkols) {
      cat(FILgrp, valk, sum(FG[, valk, with = FALSE] < 0), "\n")
    }
  }
}

# Not used
SjekkVersjoner <- function(commoncols = FALSE, dropcols = character(0)) {
  is_kh_debug()
  
  globs <- FinnGlobs()
  path <- paste(globs$path, "/", globs$KubeDirDat, "/R/", sep = "")
  setwd(path)
  Filer <- setNames(as.data.frame(list.files(include.dirs = FALSE), stringsAsFactors = FALSE), c("FILNAVN"))
  Filer$KUBE <- gsub("(.*)_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}\\.rds", "\\1", Filer$FILNAVN)
  Filer$VERS <- 0
  
  for (Kube in setdiff(unique(Filer$KUBE), "tmp")) {
    versj <- 0
    filer <- Filer$FILNAVN[Filer$KUBE == Kube]
    filer <- filer[order(filer, decreasing = TRUE)]
    Filer$VERS[Filer$FILNAVN == filer[1]] <- versj
    VC <- readRDS(filer[1])
    i <- 2
    while (i <= length(filer)) {
      VN <- readRDS(filer[i])
      if (commoncols == TRUE | identical(names(VC), names(VN))) {
        comparecols <- setdiff(intersect(names(VC), names(VN)), dropcols)
        if (!identical(VC[, comparecols, with = FALSE], VN[, comparecols, with = FALSE])) {
          versj <- versj + 1
          VC <- VN
        }
      } else {
        versj <- versj + 1
        VC <- VN
      }
      Filer$VERS[Filer$FILNAVN == filer[i]] <- versj
      i <- i + 1
    }
  }
  sqlQuery(globs$log, "DELETE * FROM FILVERSJONER")
  sqlSave(globs$log, Filer, "FILVERSJONER", append = TRUE)
}


# Last update on GitHub
.lastvers <- "2024.09.02"

# Find latest local update date
.localvers <- character()
if(file.exists("R/KHupdate.R")){
  .localvers <- grep(".lastvers <-", readLines("R/KHupdate.R", n = 3), value = T)
  .localvers <- sub(".*\"(.*)\".*", "\\1", .localvers)
} 

if(length(.localvers) == 0){
  .localvers <- "unknown"
}

# Old KHupdate.R, used to update .Rprofile and renv.lock. Which is no longer used as the project is loaded from `production` 

#' Looking for updates to master/production branch
.updateproject <- function(lu, loc){
  
  # Check if master branch is active, if not return and let the user keep on dev work. 
  b <- system("git branch --show-current", intern = TRUE)
  
  if(b != "master"){
    message("\nYou are on the branch '", b, 
            "'.\nKeep on the good dev work or switch to the main branch `system(git checkout main)` to continue data processing!")
    return(invisible(NULL))
  } 
  
  message("\nYou are on the master/production branch")
  
  if(isTRUE(lu == loc)){
    message("\nThe project is up to date, you are ready to go!")
    return(invisible(NULL))
  }
  
  # Update all files if on master branch and updates available
  if(lu != loc){
    choice <- menu(choices = c("Yes", "No"),
                   title = paste0("\nUpdates available!!",
                                  "\n\nLast version on GitHub: ", lu,
                                  "\nYour local version: ", loc,
                                  "\n\nUpdate files (recommended)?"))
    
    if(choice == 1){
      message("\nFetching updates...")
      invisible(system("git fetch origin master"))
      invisible(system("git reset --hard origin/master"))
      invisible(system("git pull"))
    } else {
      message("\nSkipping updates, the project files might be outdated.")
    }
  }
}

#' SettKubedirs (kb)
#'
#' @param globs 
#' @param modus 
SettKubedirs <- function(globs, modus = NA) {
  is_kh_debug()
  
  if (modus == "KH") {
    globs$KubeDir <- globs$KubeDir_KH
    globs$KubeDirNy <- globs$KubeDirNy_KH
    globs$KubeDirDat <- globs$KubeDirDat_KH
    globs$FriskVDir <- globs$FriskVDir_KH
  } else {
    globs$KubeDir <- globs$KubeDir_NH
    globs$KubeDirNy <- globs$KubeDirNy_NH
    globs$KubeDirDat <- globs$KubeDirDat_NH
    globs$FriskVDir <- globs$FriskVDir_NH
  }
  return(globs)
}

#' KonverterKUBER (kb)
#'
#' @param Format 
#' @param versjonert 
#' @param globs 
KonverterKUBER <- function(Format = "CSV", versjonert = FALSE, globs = FinnGlobs()) {
  is_kh_debug()
  
  # Rmappe<-paste(getOption("khfunctions.root"),"/",getOption("khfunctions.kubedir")Ny,sep="")
  Rmappe <- getOption("khfunctions.kube.ny")
  if (versjonert == TRUE) {
    Rmappe <- paste0(getOption("khfunctions.kube.dat"), "/R/")
  }
  Utmappe <- gsub("/R/", paste("/", Format, "/", sep = ""), Rmappe)
  KonverterRMappe(Rmappe = Rmappe, Utmappe = Utmappe, Format = Format, globs = globs)
}

#' KonverterStablaFilgrupper (kb)
#'
#' @param Format 
#' @param globs 
KonverterStablaFilgrupper <- function(Format = "CSV", globs = FinnGlobs()) {
  is_kh_debug()
  
  # Rmappe<-paste(getOption("khfunctions.root"),"/",getOption("khfunctions.kubedir")Ny,sep="")
  Rmappe <- getOption("khfunctions.filegroups.ny")
  if (versjonert == TRUE) {
    Rmappe <- getOption("khfunctions.filegroups.dat")
  }
  Utmappe <- gsub("(.*/)R(/.*)", paste("\\1", Format, "\\2", sep = ""), Rmappe)
  KonverterRMappe(Rmappe = Rmappe, Utmappe = Utmappe, Format = Format, globs = globs)
}

#' KH2014v2015 (kb)
#'
#' @param kube 
#' @param batchdate 
#' @param globs 
KH2014v2015 <- function(kube, batchdate = SettKHBatchDate(), globs = FinnGlobs()) {
  is_kh_debug()
  
  KH2014dir <- "F:/Prosjekter/Kommunehelsa/Data og databehandling/Databehandling/2014/csvNESSTAR"
  setwd(KH2014dir)
  datef <- format(strptime(batchdate, "%Y-%m-%d-%H-%M"), "#%Y-%m-%d#")
  K15dscr <- FinnKubeInfo(kube)
  FellesTabs <- c("GEO", "AAR", "ALDER", "KJONN")
  KLenke <- sqlQuery(globs$dbh, paste("SELECT KH2015v2014.*, KH2015_KUBESTATUS.VERSJON AS BATCH
                                   FROM KH2015v2014 LEFT JOIN KH2015_KUBESTATUS
                                   ON KH2015v2014.KH2015_KUBE = KH2015_KUBESTATUS.KUBE_NAVN
                                   WHERE KH2015_KUBE='", kube, "'", sep = ""), stringsAsFactors = FALSE)[1, ]
  F2015 <- FinnKubeT(kube, batch = KLenke$BATCH)
  F2014 <- data.table::as.data.table(read.csv(KLenke$KH2014FIL, sep = ";", stringsAsFactors = FALSE))
  # print(F2014)
  # print(sapply(F2014,class))
  F2014$AAR <- as.character(F2014$AAR)
  # F2014$AAR<-gsub("^(\\d{4})$","\\1_\\1",F2014$AAR)
  F2015$AAR <- gsub("^(\\d{4})_(\\d{4})$", "\\2", F2015$AAR)
  F2014$GEO <- as.character(F2014$GEO)
  F2014$GEO <- gsub("^(\\d{3})$", "0\\1", F2014$GEO)
  F2014$GEO <- gsub("^([1-9])$", "0\\1", F2014$GEO)
  if (!is.null(F2014$KJONN)) {
    F2014$KJONN <- as.integer(F2014$KJONN)
  } else if (!is.null(F2014$KJONN)) {
    F2014$KJONN <- 0
  }
  alle <- "0_120"
  if (!is.na(K15dscr$ALDER_ALLE)) {
    alle <- K15dscr$ALDER_ALLE
  }
  allev <- unlist(stringr::str_split(alle, "_"))
  if (!is.null(F2014$ALDER)) {
    F2014$ALDER <- as.character(F2014$ALDER)
    if (!is.na(KLenke$Alder14TOM)) {
      int <- which(grepl("_", F2014$ALDER))
      atmp <- unlist(stringr::str_split(F2014$ALDER[int], "_"))
      F2014$ALDER[int] <- paste(atmp[1], "_", as.integer(atmp[2]) - 1, sep = "")
    }
    F2014$ALDER <- gsub("^_(\\d+)$", paste(allev[1], "_", "\\1", sep = ""), F2014$ALDER)
    F2014$ALDER <- gsub("^(\\d+)_$", paste("\\1", "_", allev[2], sep = ""), F2014$ALDER)
    F2014$ALDER <- gsub("ALLE", alle, F2014$ALDER)
  } else {
    F2014$ALDER <- alle
  }
  data.table::setnames(F2014, names(F2014), gsub("_MA\\d+$", "", names(F2014)))
  data.table::setnames(F2014, names(F2014), gsub("RATE\\d+$", "RATE", names(F2014)))
  Ftab15 <- intersect(FellesTabs, names(F2015))
  Ftab14 <- intersect(FellesTabs, names(F2014))
  Ftab <- intersect(Ftab15, Ftab14)
  tabs14 <- KLenke[c("TAB1_14", "TAB2_14")]
  tabs14 <- tabs14[!(is.na(tabs14) | tabs14 == "NA")]
  tabs15 <- K15dscr[c("TAB1", "TAB2", "TAB3")]
  tabs15 <- tabs15[!(is.na(tabs15) | tabs15 == "NA")]
  
  if (length(tabs15) > 0 & length(tabs14) == length(tabs15)) {
    data.table::setnames(F2014, tabs14, tabs15)
  }
  
  for (tab in c("TAB1", "TAB2")) {
    tabT <- paste(tab, "map_14", sep = "")
    if (!is.na(KLenke[1, tabT])) {
      tab1415 <- unlist(stringr::str_split(KLenke[1, tabT], ","))
      tabT <- paste(tab, "org_14", sep = "")
      tab1414 <- unlist(stringr::str_split(KLenke[1, tabT], ","))
      tab15n <- as.character(K15dscr[tab])
      F2014 <- data.frame(F2014)
      print(tab15n)
      F2014[, tab15n] <- as.character(plyr::mapvalues(F2014[, tab15n], tab1414, tab1415))
      data.table::setDT(F2014)
    }
  }
  
  if (length(tabs15) > 0) {
    F2014[, (tabs15)] <- F2014[, lapply(.SD, as.character), .SDcols = tabs15]
  }
  
  
  Ftab <- c(Ftab, tabs15)
  
  if (!is.na(KLenke$TELLER14)) {
    data.table::setnames(F2014, KLenke$TELLER14, "TELLER")
  }
  if (!is.na(KLenke$MALTALL14)) {
    data.table::setnames(F2014, KLenke$MALTALL14, "MALTALL")
  }
  valcols <- c("TELLER", "NEVNER", "RATE", "MALTALL", "SMR", "MEIS", "SPVFLAGG")
  valcols14 <- intersect(valcols, names(F2014))
  valcols15 <- intersect(valcols, names(F2015))
  valcolsF <- intersect(valcols15, valcols14)
  
  cols <- c(Ftab, valcolsF)
  F2015 <- F2015[, intersect(cols, names(F2015)), with = F]
  F2014 <- F2014[, intersect(cols, names(F2015)), with = F]
  cat("Ftab:", Ftab, "\n")
  
  MM <- merge(F2015, F2014, by = Ftab, all = TRUE, suffixes = c("15", "14"), allow.cartesian = T)
  utcolso <- c("GEO", "AAR", "ALDER", "KJONN", tabs15, unlist(lapply(c("TELLER", "NEVNER", "RATE", "MALTALL", "SMR", "MEIS", "SPVFLAGG"), function(x) {
    paste(x, c("15", "14"), sep = "")
  })))
  utcolso <- intersect(utcolso, names(MM))
  MM <- MM[, utcolso, with = F]
  utfil <- paste("F:/Prosjekter/Kommunehelsa/PRODUKSJON/VALIDERING/NESSTAR_KUBER/KH2014v2015/", kube, "_KH15v14_", KLenke$BATCH, ".csv", sep = "")
  write.table(MM, file = utfil, sep = ";", row.names = FALSE)
  return(MM)
}

#' FullKH2014v2015 (kb)
#'
#' @param escape 
#' @param globs 
FullKH2014v2015 <- function(escape = character(0), globs = FinnGlobs()) {
  is_kh_debug()
  
  kuber <- sqlQuery(globs$dbh, "SELECT KH2015_KUBE FROM KH2015v2014", stringsAsFactors = FALSE)[, 1]
  for (kube in setdiff(kuber[!is.na(kuber)], escape)) {
    try(KH2014v2015(kube, globs = globs))
  }
}

#' FinnKubeInfo (kb)
#'
#' @param kube 
FinnKubeInfo <- function(kube) {
  is_kh_debug()
  
  globs <- FinnGlobs()
  return(sqlQuery(globs$dbh, paste("
        SELECT DISTINCT KUBE_NAVN, TELLERKOL, NEVNERKOL, EKSTRAVARIABLE, FILGRUPPER.VAL1navn,FILGRUPPER.VAL2navn,FILGRUPPER.VAL3navn,
                                  FILGRUPPER.TAB1, FILGRUPPER.TAB2, FILGRUPPER.TAB3, FILGRUPPER.ALDER_ALLE
                                  FROM (KUBER INNER JOIN TNP_PROD ON KUBER.TNP = TNP_PROD.TNP_NAVN)
                                  INNER JOIN FILGRUPPER ON TNP_PROD.TELLERFIL = FILGRUPPER.FILGRUPPE
                                  WHERE KUBER.KUBE_NAVN='", kube, "'", sep = ""), stringsAsFactors = FALSE)[1, ])
}

#' KonverterRMappe (kb)
#'
#' @param Rmappe 
#' @param Utmappe 
#' @param Format 
#' @param globs 
KonverterRMappe <- function(Rmappe, Utmappe, Format = "csv", globs = FinnFlobs) {
  is_kh_debug()
  
  print(paste(getOption("khfunctions.root"), Rmappe, sep = "/"))
  setwd(paste(getOption("khfunctions.root"), Rmappe, sep = "/"))
  filer <- list.files()
  for (fil in filer[grepl("\\.rds$", filer)]) {
    filn <- gsub("(.*)\\.rds$", "\\1", fil)
    TABELL <- readRDS(fil)
    cat("Eksporterer ", filn, "\n")
    if (tolower(Format) == "csv") {
      filen <- paste(getOption("khfunctions.root"), "/", Utmappe, filn, ".csv", sep = "")
      write.table(TABELL, file = filen, sep = ";", row.names = FALSE)
      cat("CSV til", filen, "\n")
    } else if (tolower(Format) == "stata") {
      foreign::write.dta(TABELL, file = paste(getOption("khfunctions.root"), "/", Utmappe, filn, ".dta", sep = ""))
    }
  }
}

#' SammelignAarganger
#'
#' @param globs 
#' @param aar1 
#' @param aar2 
#' @param modus 
SammelignAarganger <- function(globs = FinnGlobs(), aar1 = getOption("khfunctions.year"), aar2 = getOption("khfunctions.year") - 1, modus = "KH") {
  is_kh_debug()
  
  KUBE1 <- data.table::as.data.table(sqlQuery(globs$dbh, paste("SELECT KUBE_NAVN, VERSJON FROM ", modus, aar1, "_KUBESTATUS", sep = ""), stringsAsFactors = F))
  KUBE2 <- data.table::as.data.table(sqlQuery(globs$dbh, paste("SELECT KUBE_NAVN, VERSJON FROM ", modus, aar2, "_KUBESTATUS", sep = ""), stringsAsFactors = F))
  data.table::setkey(KUBE1, "KUBE_NAVN")
  data.table::setkey(KUBE2, "KUBE_NAVN")
  PAR <- merge(KUBE1, KUBE2, by = "KUBE_NAVN", suffixes = c("1", "2"))
  data.table::setnames(PAR, c("KUBE_NAVN"), c("KUBE_NAVN1"))
  PAR[, KUBE_NAVN2 := KUBE_NAVN1]
  SammenlignKubePar(PAR, modus = modus, globs = globs)
}

#' SammenlignKubePar
#'
#' @param PAR 
#' @param modus 
#' @param globs 
SammenlignKubePar <- function(PAR, modus = NA, globs = FinnGlobs()) {
  is_kh_debug()
  
  for (i in 1:nrow(PAR)) {
    paret <- PAR[i, ]
    if (grepl("^\\d{4}", paret$VERSJON1) & grepl("^\\d{4}", paret$VERSJON2)) {
      cat(paste("Skal merge", paret$KUBE_NAVN1, "\n"))
      KUBE1 <- FinnKubeT(paret$KUBE_NAVN1, batch = paret$VERSJON1, globs = globs)
      KUBE2 <- FinnKubeT(paret$KUBE_NAVN2, batch = paret$VERSJON2, globs = globs)
      tabs1 <- FinnTabKolsKUBE(names(KUBE1))
      tabs2 <- FinnTabKolsKUBE(names(KUBE2))
      if (nrow(KUBE1) > 0 & nrow(KUBE2) & length(setdiff(tabs1, tabs2)) == 0) {
        data.table::setkeyv(KUBE1, tabs1)
        data.table::setkeyv(KUBE2, tabs1)
        # VERSJON 1 INNER JOIN
        KOMP <- KUBE2[KUBE1]
        
        # VERSJON 2 FULL OUTER JOIN, RESUUSRKREVENDE!
        # KOMP<-merge(KUBE1,KUBE2,all=TRUE)
        
        # Maa BAREBERE NED KOLONNNER OG EVT OMDoePE
        
        utfil <- paste("F:/Prosjekter/Kommunehelsa/PRODUKSJON/VALIDERING/NESSTAR_KUBER/KH2016v2015/", paret$KUBE_NAVN1, "_", paret$VERSJON1, "v", paret$VERSJON2, ".csv", sep = "")
        cat(paste("Skriver ut", utfil, "\n"))
        write.table(KOMP, file = utfil, sep = ";", row.names = FALSE)
      } else {
        cat("!!!!!! ", paret$VERSJON1, "har ulike kolonner og kan ikke matches")
      }
    }
  }
}

#' TmpRutineSammenlignKHkuber (kb)
#'
#' @param kubefilnavn1 
#' @param kubefilnavn2 
#' @param KUBENAVN 
#' @param tabs 
#' @param globs 
TmpRutineSammenlignKHkuber <- function(kubefilnavn1, kubefilnavn2, KUBENAVN, tabs = character(0), globs = FinnGlobs()) {
  is_kh_debug()
  
  ## KUBE1<-data.table::as.data.table(read.csv(kube1filnavn,header=TRUE,sep=";"))
  ## KUBE2<-data.table::as.data.table(read.csv(kube2filnavn,header=TRUE,sep=";"))
  
  fileD <- fs::file_exists(c(kube1filnavn, kube2filnavn))
  if (sum(fileD) != 2) {
    print(names(fileD)[!(fileD)])
    stop("Fant ingen fil/filer i mappen du skrev oppe")
  }
  
  KUBE1 <- data.table::fread(kube1filnavn)
  KUBE2 <- data.table::fread(kube2filnavn)
  
  print(names(KUBE1))
  print(names(KUBE2))
  tabs <- unique(c(getOption("khfunctions.profiltabs"), tabs))
  
  
  tabs1 <- intersect(names(KUBE1), tabs)
  tabs2 <- intersect(names(KUBE2), tabs)
  
  print(tabs1)
  print(tabs2)
  KHglobs$DefDesign$DesignKols
  
  ## Folder to keep the output if not allready there
  currYr <- KHgetOption("khfunctions.year") - 1
  nextYr <- KHgetOption("khfunctions.year")
  foldName <- paste0("Batch", currYr, "vs", nextYr)
  
  validDir <- file.path(getOption("khfunctions.root"), "VALIDERING/NESSTAR_KUBER", foldName)
  
  if (isFALSE(fs::dir_exists(validDir))) {
    fs::dir_create(validDir)
  }
  
  
  if (length(setdiff(tabs1, tabs2)) == 0 && nrow(KUBE1) > 0 && nrow(KUBE2) > 0) {
    data.table::setkeyv(KUBE1, tabs1)
    data.table::setkeyv(KUBE2, tabs1)
    # VERSJON 1 INNER JOIN
    KOMP <- KUBE2[KUBE1]
    
    # VERSJON 2 FULL OUTER JOIN, RESUUSRKREVENDE!
    # KOMP<-merge(KUBE1,KUBE2,all=TRUE)
    
    # Maa BAREBERE NED KOLONNNER OG EVT OMDoePE
    
    fileName <- paste0(KUBENAVN, ".csv")
    utfil <- file.path(validDir, fileName)
    cat(paste("Skriver ut", utfil, "\n"))
    ## write.table(KOMP,file=utfil,sep=";",row.names=FALSE)
    data.table::fwrite(KOMP, file = utfil, sep = ";")
  } else {
    stop("!!!!!! tabellene har ulike kolonner og kan ikke matches")
  }
}

#' @param fila 
#' @param batch 
#' @param globs 
FinnKubeT <- function(fila, batch = NA, globs = FinnGlobs()) {
  is_kh_debug()
  
  if (is.na(batch)) {
    filn <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kube.ny"), paste0(fila, ".rds"))
  } else {
    filn <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kube.dat"), "R", paste0(fila, "_", batch, ".rds"))
  }
  KUBE <- data.table::data.table()
  if (file.access(filn, mode = 0) == -1) {
    cat("KRITISK FEIL: ", filn, " finnes ikke\n")
  } else if (file.access(filn, mode = 4) == -1) {
    cat("KRITISK FEIL: ", filn, " finnes, men lar seg ikke lese\n")
  } else {
    KUBE <- readRDS(filn)
  }
  return(KUBE)
}

#' CompNyOgKlarAlle (kb)
#'
#' @param globs 
CompNyOgKlarAlle <- function(globs = FinnGlobs()) {
  is_kh_debug()
  
  batchdate <- SettKHBatchDate()
  KUBER <- sqlQuery(globs$dbh, "SELECT KUBE_NAVN FROM KUBER", as.is = TRUE)
  utfil <- paste(getOption("khfunctions.root"), "/", getOption("khfunctions.kubedir"), "/LOGG/CompNyOgKlar_", batchdate, ".txt", sep = "")
  sink(utfil, split = TRUE)
  for (i in 1:nrow(KUBER)) {
    comp <- try(CompNyOgKlar(KUBER[i, 1], streng = FALSE, globs = globs))
    if (!inherits(comp, "try-error")) {
      cat("__________________________________________\nKUBE: ", KUBER[i, 1], "\n")
      print(comp$checkm)
      cat("___________________________________________________\n")
    }
  }
  sink()
}

#' CompNyOgKlar (kb)
#'
#' @param KUBEid 
#' @param streng 
#' @param globs 
CompNyOgKlar <- function(KUBEid, streng = TRUE, globs = FinnGlobs()) {
  is_kh_debug()
  
  okvers <- as.character(sqlQuery(globs$dbh, paste("SELECT VERSJON FROM KH2015_KUBESTATUS WHERE KUBE_NAVN='", KUBEid, "'", sep = ""), as.is = TRUE))
  comp <- SammenlignKuber(FinnDatertKube(KUBEid), FinnDatertKube(KUBEid, batch = okvers), streng = streng)
  return(comp)
}

#' PrintCompCheck (kb)
#'
#' @param TT 
#' @param cols 
PrintCompCheck <- function(TT, cols = c("TELLER", "NEVNER", "RATE", "SMR", "MEIS")) {
  is_kh_debug()
  
  comp <- suppressWarnings(as.integer(gsub("^Component (\\d+):.*", "\\1", TT$check)))
  err <- gsub("^Component \\d+:(.*)", "\\1", TT$check)
  keepcomp <- which(names(TT$V1) %in% cols)
  compM <- comp[comp %in% keepcomp]
  err <- err[comp %in% keepcomp]
  checkm <- paste(names(TT$V1)[compM], ":", err)
  print(checkm)
}

#' SammenlignKuber (kb)
#'
#' @param V1 
#' @param V2 
#' @param FULL 
#' @param streng 
#' @param comparecols 
SammenlignKuber <- function(V1, V2, FULL = TRUE, streng = TRUE, comparecols = character(0)) {
  is_kh_debug()
  
  Tab1 <- FinnTabKolsKUBE(names(V1))
  Tab2 <- FinnTabKolsKUBE(names(V2))
  tabdiff <- union(setdiff(Tab1, Tab2), setdiff(Tab2, Tab1))
  check <- FALSE
  checkm <- FALSE
  mismatch <- integer(0)
  if (streng == TRUE & length(tabdiff) > 0) {
    cat("Kan ikke sammenligne KUBER naar foelgende kolonner ikke er i begge:", tabdiff, "\n")
    V12 <- data.table::data.table(0)
  } else {
    if (length(tabdiff) > 0) {
      Tab <- intersect(Tab1, Tab2)
      cat("OBS! tabdiff:", tabdiff, "\n")
    } else {
      Tab <- Tab1
    }
    if (length(comparecols) == 0) {
      comparecols <- intersect(setdiff(names(V1), Tab1), setdiff(names(V2), Tab2))
    }
    data.table::setkeyv(V1, Tab)
    data.table::setkeyv(V2, Tab)
    V1 <- V1[eval(parse(text = paste("order(", Tab, ")", sep = ""))), c(Tab, comparecols), with = FALSE]
    V2 <- V2[eval(parse(text = paste("order(", Tab, ")", sep = ""))), c(Tab, comparecols), with = FALSE]
    
    if (streng == FALSE) {
      V1 <- V1[!grepl("99|9900$", GEO), ]
      V2 <- V2[!grepl("99|9900$", GEO), ]
      if ("ALDER" %in% Tab) {
        V1 <- V1[!ALDER %in% c("999_999", "888_888"), ]
        V2 <- V2[!ALDER %in% c("999_999", "888_888"), ]
      }
      if ("KJONN" %in% Tab) {
        V1 <- V1[!KJONN %in% c(8, 9), ]
        V2 <- V2[!KJONN %in% c(8, 9), ]
      }
    }
    # comtabs<-V1[V2,nomatch=0][,Tab,with=F]
    # if (streng<0){
    #  check<-all.equal(V2[comtabs,],V1[comtabs,],check.attributes=FALSE,check.names=FALSE)
    # } else {
    check <- all.equal(V2, V1, check.attributes = FALSE, check.names = FALSE)
    # }
    comp <- suppressWarnings(as.integer(gsub("^Component (\\d+):.*", "\\1", check)))
    err <- gsub("^Component \\d+:(.*)", "\\1", check)
    comp <- comp[!is.na(comp)]
    err <- err[!is.na(comp)]
    checkm <- paste(names(V1)[comp], ":", err)
    
    V12 <- data.table::data.table(0)
    if (FULL == TRUE) {
      V12 <- merge(V1, V2, all = TRUE, by = Tab, suffixes = c("_1", "_2"))
      colorder <- c(Tab, unlist(lapply(comparecols, function(x) {
        paste(x, c("_1", "_2"), sep = "")
      })))
      V12 <- V12[, colorder, with = FALSE]
      mismatch <- which(V12[, paste(comparecols, "_1", sep = ""), with = FALSE] != V12[, paste(comparecols, "_2", sep = ""), with = FALSE])
    }
  }
  return(list(check = check, checkm = checkm, V12 = V12, V1 = V1, V2 = V2, cc = comparecols, mm = mismatch))
}

#' FinnKubeT (kb)
#'
#' FinnDatertKube (kb)
#'
#' @param KUBEid 
#' @param batch 
#' @param silent 
#' @param hist 
FinnDatertKube <- function(KUBEid, batch = NA, silent = FALSE, hist = 0) {
  is_kh_debug()
  
  globs <- FinnGlobs()
  path <- paste(getOption("khfunctions.root"), "/", getOption("khfunctions.kubedir"), "/R/", sep = "")
  # Finner nyeste daterte versjon om ikke batcdate er gitt
  if (is.na(batch)) {
    orgwd <- getwd()
    setwd(path)
    Filer <- setNames(as.data.frame(list.files(include.dirs = FALSE), stringsAsFactors = FALSE), c("FILNAVN"))
    Filer$KUBE <- gsub("(.*)_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}\\.rds", "\\1", Filer$FILNAVN)
    Filer <- subset(Filer, KUBE == KUBEid)
    Filer <- Filer[order(Filer$FILNAVN), ]
    row <- nrow(Filer) - hist
    if (row > 0) {
      filn <- paste(path, Filer$FILNAVN[row], sep = "")
    } else {
      if (silent == FALSE) {
        cat("Finnes ikke hist=", hist, "eldre versjon\n")
      }
      filn <- NA
    }
    setwd(orgwd)
  } else {
    filn <- paste(path, KUBEid, "_", batch, ".rds", sep = "")
  }
  if (!is.na(filn)) {
    cat("LESER inn", filn, "\n")
    KUBE <- readRDS(filn)
  } else {
    KUBE <- FALSE
  }
  
  return(KUBE)
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

#' FinnTabKolsKUBE (kb)
#'
#' Finds column names not in NESSTARTUPPEL
FinnTabKolsKUBE <- function(allnames, globs = SettGlobs()) {
  is_kh_debug()
  
  annet <- union(union(unlist(getOption("khfunctions.valcols")), FinnValKolsF(allnames)), c("NORMSMR", "SMRtmp"))
  return(setdiff(allnames, annet))
}

#' SettTotalKoder (kb)
#' replaed by config
SettTotalKoder <- function(globs = SettGlobs()) {
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
