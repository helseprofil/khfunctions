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

#' GeoHarm (kb)
#' 
#' superseeded by do_harmonize_geo()
GeoHarm <- function(FIL, vals = list(), rektiser = TRUE, batchdate = SettKHBatchDate(), globs = SettGlobs(), GEOstdAAR = getOption("khfunctions.year")) {
  is_kh_debug()
  
  if (identical(class(FIL), "data.frame")) {
    FIL <- data.table::setDT(FIL)
  }
  keyorg <- data.table::key(FIL)
  geoomk <- globs$KnrHarm
  
  if(any(FIL$GEO %in% geoomk$GEO)){
    FIL <- collapse::join(FIL, geoomk, on = "GEO", how = "left", verbose = 0)
    FIL[!is.na(GEO_omk), let(GEO = GEO_omk)][, let(GEO_omk = NULL, HARMstd = NULL)]
  }
  FIL[, let(FYLKE = NULL)]
  
  FIL <- KHaggreger(FIL, vals = vals, globs = globs)
  # Rektangulariser
  if (rektiser == TRUE) {
    REKT <- data.table::data.table()
    FDesign <- FinnDesign(FIL, globs = globs)
    
    FDes <- FDesign$Design
    
    # SAMME LOGIKK SOM I set_rectangularized_cube_design, bør ekstraheres ut.
    for (Gn in FDesign$Part[["Gn"]][["GEOniv"]]) {
      GEOK <- subset(globs$GeoKoder, FRA <= GEOstdAAR & TIL > GEOstdAAR & GEOniv == Gn)$GEO
      FDesG <- FDes[HAR == 1 & GEOniv == Gn, intersect(names(FIL), names(FDes)), with = FALSE]
      REKT <- rbind(data.table::data.table(expand.grid.df(data.frame(FDesG), data.frame(GEO = GEOK))), REKT)
    }
    data.table::setkeyv(REKT, names(REKT))
    data.table::setkeyv(FIL, names(REKT))
    FIL <- FIL[REKT]
    FIL <- set_implicit_null_after_merge(FIL, vals = vals)
  }
  
  FIL[, FYLKE := ifelse(GEOniv %in% c("H", "L"), "00", substr(GEO, 1, 2))]
  return(FIL)
}

#' Klargjorfil
#' superseeded by load_file_to_buffer
KlargjorFil <- function(filename, tabfilter  = "", versjonert = FALSE, batchdate = SettKHBatchDate(), filefilter = NULL, globs = SettGlobs()) {
  istabfilter <- tabfilter != ""
  
  if(isfilter) FGP <- FinnFilgruppeParametre(filter$ORGFIL, batchdate = batchdate, globs = globs)
  if(!isfilter) FGP <- FinnFilgruppeParametre(filename, batchdate = batchdate, globs = globs)
  
  if(isbuffer){
    cat("Henter", filename, "fra BUFFER")
    FIL <- data.table::copy(.GlobalEnv$BUFFER[[filename]])
  } else if(!isfilter){
    FIL <- FinnFil(filename, versjonert = versjonert, globs = globs)$FT
    if(istabfilter) FIL <- do_tabfilter(file = FIL, tabfilter = tabfilter)
    FIL <- do_harmonize_geo(file = FIL, vals = FGP$vals, rectangularize = FALSE, globs = globs)
    .GlobalEnv$BUFFER[[filename]] <- FIL
  } else if(isfilter){
    FIL <- FinnFil(filter$ORGFIL, versjonert = versjonert, globs = globs)$FT
    
    if(istabfilter) FIL <- do_tabfilter(file = FIL, tabfilter = tabfilter)
    
    iskollapsdel <- grepl("\\S", filter$KOLLAPSdeler)
    if(iskollapsdel) FIL <- do_filfiltre_kollapsdeler(file = FIL, parts = filter$KOLLAPSdeler, globs = globs)
    
    isnyekolkolprerad <- grepl("\\S", filter$NYEKOL_KOL_preRAD)
    # TODO: optimalisere leggtilnyeverdikolonner
    if(isnyekolkolprerad) FIL <- LeggTilNyeVerdiKolonner(FIL, filter$NYEKOL_KOL_preRAD, slettInf = TRUE)
    
    Filter <- SettFilterDesign(filter, bruk0 = FALSE, FGP = FGP, globs = globs)
    if (length(Filter) > 0) FIL <- OmkodFil(FIL, FinnRedesign(FinnDesign(FIL, globs = globs), list(Parts = Filter), globs = globs), globs = globs)
    
    isgeoharm <- filter$GEOHARM == 1
    if(isgeoharm){
      rectangularize <- ifelse(filefilter$REKTISER == 1, TRUE, FALSE)
      FIL <- do_harmonize_geo(file = FIL, vals = FGP$vals, rectangularize = rectangularize, globs = globs)
    }
    
    # INAKTIV, FJERNE FRA ACCESS?
    # isnyetab <- grepl("\\S", filter$NYETAB)
    # if(isnyetab) FIL <- AggregerRader(FIL, filter$NYETAB, FGP = FGP)
    
    isnyekolrad <- grepl("\\S", filter$NYEKOL_RAD)
    if(isnyekolrad) FIL <- LeggTilSumFraRader(FIL, filter$NYEKOL_RAD, FGP = FGP, globs = globs)
    
    # INAKTIV, FJERNE FRA ACCESS?
    # isnyekolkol <- grepl("\\S", filter$NYEKOL_KOL)
    # if(isnyekolkol) FIL <- LeggTilNyeVerdiKolonner(FIL, filter$NYEKOL_KOL, slettInf = TRUE)
    
    isnykolsmerge <- grepl("\\S", filter$NYKOLSmerge)
    if(isnykolsmerge){
      # FIL <- do_filfiltre_nykolsmerge()
      cat("\nFILFILTRE:NYKOLSmerge\n")
      NY <- eval(parse(text = filter$NYKOLSmerge))
      tabK <- intersect(get_dimension_columns(names(NY)), get_dimension_columns(names(FIL)))
      data.table::setkeyv(NY, tabK)
      data.table::setkeyv(FIL, tabK)
      FIL <- NY[FIL]
    }
    
    isrsynt1 <- grepl("\\S", filter$FF_RSYNT1)
    if(isrsynt1){
      filter$FF_RSYNT1 <- gsub("\\\r", "\\\n", filter$FF_RSYNT1)
      rsynterr <- try(eval(parse(text = filter$FF_RSYNT1)), silent = TRUE)
      if ("try-error" %in% class(rsynt1err)) {
        print(rsynterr)
        stop("Something went wrong in R, RSYNT1")
      }
    }
    .GlobalEnv$BUFFER[[filename]] <- FIL
  }
  
  FILd <- FinnDesign(FIL, FGP = FGP, globs = globs)
  return(list(FIL = FIL, FGP = FGP, FILd = FILd))
} 

#' superseeded by find_design_after_filter()
FinnDesignEtterFiltrering <- function(ORGd, Filter, FilterKols = character(0), FGP = list(amin = 0, amax = 120), globs = SettGlobs()) {
  is_kh_debug()
  
  FiltD <- FinnRedesignForFilter(ORGd, Filter, globs = globs)$Dekk
  FiltD <- FiltD[, setdiff(names(FiltD), FilterKols), with = FALSE]
  return(FinnDesign(FiltD, FGP = FGP, globs = globs))
}

#' superseeded by calculate_period_sums()
FinnSumOverAar <- function(KUBE, per = 0, FyllMiss = FALSE, AntYMiss = 0, globs = SettGlobs()) {
  is_kh_debug()
  UT <- KUBE[0, ]
  tabs <- setdiff(get_dimension_columns(names(KUBE)), c("AARl", "AARh"))
  valkols <- get_value_columns(names(KUBE))
  # Utrykk for KH-aggregering (med hjelpestoerrelses for snitt)
  lpv <- paste0(valkols, "=sum(", valkols, ",na.rm=TRUE),",
                valkols, ".f=0,",
                valkols, ".a=sum(", valkols, ".a*(!is.na(", valkols, ") & ", valkols, "!=0)),",
                valkols, ".fn1=sum(", valkols, ".f %in% 1:2),",
                valkols, ".fn3=sum(", valkols, ".f==3),",
                valkols, ".fn9=sum(", valkols, ".f==9),",
                valkols, ".n=sum(", valkols, ".f==0)",
                # valkols,".n=sum(as.numeric(!is.na(",valkols,")))",
                collapse = ","
  )
  lpsvars <- unlist(lapply(valkols, function(x) {
    paste0(x, c(".fn1", ".fn3", ".fn9", ".n"))
  }))
  UT[, (lpsvars) := NA_integer_]
  
  aara <- unique(KUBE$AARh)
  if (FyllMiss == TRUE) {
    aara <- (min(aara) + per - 1):max(aara)
  } else {
    aara <- intersect((min(aara) + per - 1):max(aara), aara)
  }
  cat("Finner", per, "-aars sum for ")
  for (aar in aara) {
    cat(aar, " ")
    lp <- paste0("list(AARl=", aar - per + 1, ",AARh=", aar, ",", lpv, ")")
    UT <- rbind(UT, KUBE[AARh %in% c((aar - per + 1):aar), eval(parse(text = lp)), by = tabs][, names(UT), with = FALSE])
  }
  cat("\n")
  for (valkol in valkols) {
    eval(parse(text = paste0("UT[", valkol, ".f>0,", valkol, ":=list(NA)]")))
  }
  
  if (AntYMiss <= per) {
    for (valkol in valkols) {
      eval(parse(text = paste0("UT[", valkol, ".fn9>", AntYMiss, ",c(\"", valkol, "\",\"", valkol, ".f\"):=list(NA,9)]")))
    }
  }
  
  f9s <- names(UT)[grepl(".f9$", names(UT))]
  if (length(f9s) > 0) {
    UT[, (f9s) := NULL]
  }
  
  return(UT)
}

#superseeded by get_read_parameters()
FinnFilBeskGruppe <- function(filgruppe, batchdate = SettKHBatchDate(), globs = SettGlobs()) {
  datef <- FormatSqlBatchdate(batchdate)
  sqlt <- paste("SELECT KOBLID, ORIGINALFILER.FILID AS FILID, FILNAVN, FORMAT, DEFAAR, INNLESING.*
              FROM INNLESING INNER JOIN
              (  ORGINNLESkobl INNER JOIN ORIGINALFILER
              ON ORGINNLESkobl.FILID = ORIGINALFILER.FILID)
              ON   (INNLESING.DELID = ORGINNLESkobl.DELID)
              AND (INNLESING.FILGRUPPE = ORGINNLESkobl.FILGRUPPE)
              WHERE INNLESING.FILGRUPPE='", filgruppe, "'
              AND ORIGINALFILER.IBRUKFRA<=", datef, "
              AND ORIGINALFILER.IBRUKTIL>", datef, "
              AND INNLESING.VERSJONFRA<=", datef, "
              AND INNLESING.VERSJONTIL>", datef, sep = "")
  fb <- RODBC::sqlQuery(globs$dbh, sqlt, stringsAsFactors = FALSE)
  invisible(fb)
}

LagTabellFraFil <- function(filbesk, FGP, batchdate = SettKHBatchDate(), globs = SettGlobs(), versjonert = FALSE, dumps = list()) {
  is_kh_debug()
  
  klokke <- proc.time()
  # INNLESING
  filn <- filbesk$filn
  cat("\n#################\nLAGER TABELL FRA FIL:\n", filn, "\n")
  LestFil <- LesFil(filbesk, batchdate = batchdate, globs = globs, dumps = dumps)
  ok <- LestFil$ok
  DF <- LestFil$DF
  kolorgs <- getOption("khfunctions.kolorgs")
  
  if (ok == 1) {
    
    # Omdoep kolonnenavn.
    # NB: for oversiktelighet i parameterfila gjoeres dette baade foer og etter reshape
    # Dvs: kolonnenavn generert i reshape tillates aa avvike fra standardnavn, disse endres etterpaa
    # Valdiering skjer ved siste endring
    # Finn kolonner spesifisert i filbesk
    HarCols <- filbesk[kolorgs[grepl("^[^-<]", filbesk[kolorgs])]]
    HarCols <- HarCols[HarCols %in% names(DF)]
    # Sett standard kolonnenavn
    names(DF) <- plyr::mapvalues(names(DF), HarCols, names(HarCols))
    
    # EVT INNFYLLING AV TABULATOR N?R DENNE ER INNRYKKET
    if (is_not_empty(filbesk$FYLLTAB)) {
      TAB <- as.character(read.csv(text = filbesk$FYLLTAB, header = FALSE, stringsAsFactors = FALSE))
      if (all(TAB %in% names(DF))) {
        DF[, TAB][DF[, TAB] == ""] <- NA
        DF[, TAB] <- zoo::na.locf(DF[, TAB], na.rm = FALSE)
      } else {
        TilFilLogg(filbesk$KOBLID, "FYLLTABERR", paste("Kolonner", paste(TAB[!TAB %in% names(DF)], collapse = ","), " finnes ikke", sep = ""), batchdate = batchdate, globs = globs)
        ok <- 0
      }
    }
    
    # EVT KASTING AV KOLONNER FoeR RESHAPE (GJoeR melt LETTERE aa BRUKE)
    if(is_not_empty(filbesk$KASTKOLS)) {
      eval(parse(text = paste("DF<-DF[,-", filbesk$KASTKOLS, "]", sep = "")))
    }
    
    if ("RESHAPEpre" %in% names(dumps)) DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "RESHAPEpre", sep = "_"), globs = globs, format = dumps[["RESHAPEpre"]])
    
    # if (!(is.na(filbesk$RESHAPEid) || filbesk$RESHAPEid=='')){
    if (!(is.na(filbesk$RESHAPEvar) || filbesk$RESHAPEvar == "")) {
      rshpDF <- ReshapeTab(DF, filbesk, batchdate = batchdate, globs = globs)
      DF <- rshpDF$DF
      ok <- min(ok, rshpDF$ok)
      # cat("\nETTER RESHAPE\n#############################\n")
      # print(head(DF))
    }
    
    if ("RESHAPEpost" %in% names(dumps)) DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "RESHAPEpost", sep = "_"), globs = globs, format = dumps[["RESHAPEpost"]])
    
    TilFilLogg(filbesk$KOBLID, "RESHAPEh", DFHeadToString(DF), batchdate = batchdate, globs = globs)
  }
  
  if (ok == 1) {
    
    # Maa splitte evt kolonne fra MULTIHEAD
    if (!is.na(filbesk$MULTIHEAD)) {
      mhl <- LesMultiHead(filbesk$MULTIHEAD)
      DF[, mhl$colnames] <- stringr::str_split_fixed(DF[, mhl$varname], mhl$sep, 2)
    }
    
    if ("RSYNT2pre" %in% names(dumps)) DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "RSYNT2pre", sep = "_"), globs = globs, format = dumps[["RSYNT2pre"]])
    DF <- do_special_handling(dt = DF, code = filbesk$RSYNT2, batchdate = batchdate, stata_exe = globs$StataExe, DTout = FALSE)
    if ("RSYNT2post" %in% names(dumps)) DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "RSYNT2post", sep = "_"), globs = globs, format = dumps[["RSYNT2post"]])
  }
  
  if (ok == 1) {
    
    # Omdoep kolonnenavn, runde 2.
    
    # Finn kolonner spesifisert i filbesk
    HarCols <- filbesk[kolorgs[grepl("^[^-<]", filbesk[kolorgs])]]
    HarCols <- HarCols[HarCols %in% names(DF)]
    # Sett standard kolonnenavn
    names(DF) <- plyr::mapvalues(names(DF), HarCols, names(HarCols))
    
    # Finn kolonner med standardverdi ('<.*>' i filbesk)
    DefVCols <- kolorgs[grepl("^<.*>", filbesk[kolorgs])]
    DefV <- matrix(sub("^<(.*)>$", "\\1", filbesk[DefVCols]), nrow = 1)
    # Sett standardverdier (faar ikke til dette med enklere syntaks naar det kan vaere tuppel, virker kloenete)
    DF <- setNames(data.frame(DF, DefV, stringsAsFactors = FALSE), c(names(DF), DefVCols))
    
    # Sjekk for ikke-eksisterende/feilskrevet
    colerr <- ""
    if (!all(names(HarCols) %in% names(DF))) {
      colerr <- paste(colerr, "Kolonnene <", HarCols[!(names(HarCols) %in% names(DF))], "> finnes ikke\n")
      ok <- 0
    }
    
    # Sjekk at paakrevde kolonner finnes
    oblkols <- c("GEO", "AAR", "VAL1")
    if (!all(oblkols %in% names(DF))) {
      colerr <- paste(colerr, "KRITISK: Kolonnene <", oblkols[!(oblkols %in% names(DF))], "> finnes ikke\n")
      ok <- 0
    }
    if (ok == 0) {
      TilFilLogg(filbesk$KOBLID, "KOLNAVNERR", colerr, batchdate = batchdate, globs = globs)
    }
  }
  
  if (ok == 1) {
    # Merge GEO delt i to
    if (filbesk$GEOd2 != "-" & !is.na(filbesk$GEOd2)) {
      DF[, filbesk$GEOd2] <- gsub("^(\\d|\\d{3})$", "0\\1", DF[, filbesk$GEOd2])
      DF$GEO <- paste(DF$GEO, DF[, filbesk$GEOd2], sep = "")
    }
    
    # KAST USPESIFISERTE KOLONNER
    # Fiks for levekaar
    harlevekaar <- FALSE
    if("LEVEL" %in% names(DF) && "levekaar" %in% unique(DF$LEVEL)) harlevekaar <- TRUE
    
    DF <- DF[, names(DF)[names(DF) %in% kolorgs]]
  }
  
  TilFilLogg(filbesk$KOBLID, "INNLES_OK", ok, batchdate = batchdate, globs = globs)
  
  if (!is.na(filbesk$GRUNNKRETS) && filbesk$GRUNNKRETS == 1) {
    data.table::setDT(DF)
    data.table::setkeyv(DF, "GEO")
    data.table::setkeyv(globs$GkBHarm, "GK")
    DF <- globs$GkBHarm[DF]
    DF[is.na(Bydel2004), Bydel2004 := paste(substr(GK, 1, 4), "00", sep = "")]
    DF[, GK := NULL]
    data.table::setnames(DF, "Bydel2004", "GEO")
    tabkols <- names(DF)[!grepl("^VAL\\d$", names(DF))]
    valkols <- names(DF)[grepl("^VAL\\d$", names(DF))]
    data.table::setkeyv(DF, tabkols)
    lp <- paste("list(",
                paste(valkols, "=as.character(sum(as.numeric(", valkols, ")))",
                      sep = "", collapse = ","
                ),
                ")",
                sep = ""
    )
    DF <- as.data.frame(DF[, eval(parse(text = lp)), by = tabkols])
  }
  
  # SKILL EVT UT SOM EGEN FUNKSJON
  # Nullstill logg
  if ("KODEBOKpre" %in% names(dumps)) DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "KODEBOKpre", sep = "_"), globs = globs, format = dumps[["KODEBOKpre"]])
  
  RODBC::sqlQuery(globs$log, paste("DELETE * FROM KODEBOK_LOGG WHERE KOBLID=", filbesk$KOBLID, sep = ""))
  if (ok == 1) {
    colClass <- sapply(DF, class)
    if (any(colClass != "character")) {
      cat("Advarsel! Kolonnene ", names(DF)[colClass != "character"], " er ikke character (", colClass[colClass != "character"], ")\n", sep = "")
      ## DF[,colClass!="character"]<-as.character(DF[,colClass!="character"])
      
      ## The above code creates lots of duplicated columns so it's changed as below
      noneSTR <- names(colClass)[colClass != "character"]
      DF[noneSTR] <- lapply(DF[noneSTR], as.character)
    }
    DF[is.na(DF)] <- ""
    
    ## TABS
    ## KOPI_KOL will use the TAB1:TAB3 and defined in
    ## INNLESING tabel KOPI_KOL column with Existing_col=New_col
    ## New_col should be defined in one of the TABs
    allTabs <- c("TAB1", "TAB2", "TAB3")
    
    if (!is.na(filbesk$KOPI_KOL)) {
      message("Kopi kolonne er: ", filbesk$KOPI_KOL)
      spVal <- unlist(strsplit(filbesk$KOPI_KOL, "="))
      spVal <- trimws(spVal)
      
      if (isFALSE(spVal %in% names(filbesk))) {
        stop("Har ikke funnet kolonnenavn som: ")
      }
      
      spTab <- grep("<kopi_kol>", filbesk[allTabs], ignore.case = TRUE)
      
      if (length(spTab) == 0) {
        stop("Hvor skal kolonne ", spVal[1], " kopieres til?")
      }
      
      dfTab <- names(filbesk[allTabs][spTab])
      
      DF[dfTab] <- DF[spVal[1]]
    }
    
    
    # VASK AV TABx
    for (tab in allTabs) {
      if (tab %in% names(DF)) {
        tabKB <- setNames(as.data.frame(table(DF[, tab], useNA = "ifany"), stringsAsFactors = FALSE), c("ORG", "FREQ"))
        tabKB$KBOMK <- KBomkod(tabKB$ORG, type = tab, filbesk = filbesk, batchdate = batchdate, globs = globs)
        tabKB$OMK <- gsub("^-$", "XXXKASTXXX", tabKB$KBOMK) # Dirty tricks. Beskytter '-' mot uttrykket nedenfor, uten aa gjoere regexp unoedvendig komplisert
        tabKB$OMK <- gsub("[- ,\\/]", "_", tabKB$KBOMK)
        tabKB$OMK <- gsub("XXXKASTXXX", "-", tabKB$KBOMK)
        tabKB$OK <- 1
        SkrivKBLogg(KB = tabKB, type = tab, filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
        DF[, tab] <- plyr::mapvalues(DF[, tab], tabKB$ORG, tabKB$OMK, warn_missing = FALSE)
      }
    }
    
    ## GEO
    # RENSK GEO (Alle er legit inntil videre??? Eller kod til 9999???)
    if ("GEO" %in% names(DF)) {
      org <- setNames(as.data.frame(table(DF$GEO, useNA = "ifany"), stringsAsFactors = FALSE), c("ORG", "FREQ"))
      geo <- GEOvask(org, filbesk = filbesk, batchdate = batchdate, globs = globs, harlevekaar = harlevekaar)
      
      SkrivKBLogg(KB = geo, type = "GEO", filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
      TilFilLogg(filbesk$KOBLID, "GEO_ok", ifelse(0 %in% geo$OK, 0, 1), batchdate = batchdate, globs = globs)
      
      DF$GEOniv <- plyr::mapvalues(DF$GEO, geo$ORG, geo$GEOniv, warn_missing = FALSE)
      DF$FYLKE <- plyr::mapvalues(DF$GEO, geo$ORG, geo$FYLKE, warn_missing = FALSE)
      DF$GEO <- plyr::mapvalues(DF$GEO, geo$ORG, geo$OMK, warn_missing = FALSE) # NB: rekkefoelge har betydning
    }
    # RENSK ALDER
    # Sett intervall for alder ALLE
    if ("ALDER" %in% names(DF)) {
      DF$ALDER <- gsub(" \\Wr\\b", " år", DF$ALDER, perl = TRUE) # Problem med codebook i dbf
      
      org <- setNames(as.data.frame(table(DF$ALDER, useNA = "ifany"), stringsAsFactors = FALSE), c("ORG", "FREQ"))
      alder <- ALDERvask(org, FGP = FGP, filbesk = filbesk, batchdate = batchdate, globs = globs)
      
      # Kast der ALDEr koder til "-" (maa ta det her og ikek generelle under pga intervall)
      DF <- subset(DF, !ALDER %in% subset(alder, OMK == "-")$ORG)
      
      SkrivKBLogg(KB = alder, type = "ALDER", filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
      TilFilLogg(filbesk$KOBLID, "ALDER_ok", ifelse(getOption("khfunctions.alder_illegal") %in% alder$OMK, 0, 1), batchdate = batchdate, globs = globs)
      DF$ALDERl <- as.integer(plyr::mapvalues(DF$ALDER, alder$ORG, alder$LO, warn_missing = FALSE))
      DF$ALDERh <- as.integer(plyr::mapvalues(DF$ALDER, alder$ORG, alder$HI, warn_missing = FALSE))
      # DF$ALDERl<-as.numeric(plyr::mapvalues(DF$ALDER,alder$ORG,alder$LO,warn_missing = FALSE))
      # DF$ALDERh<-as.numeric(plyr::mapvalues(DF$ALDER,alder$ORG,alder$HI,warn_missing = FALSE))
    }
    
    
    # RENSK KJ?NN
    if ("KJONN" %in% names(DF)) {
      org <- setNames(as.data.frame(table(DF$KJONN, useNA = "ifany"), stringsAsFactors = FALSE), c("ORG", "FREQ"))
      kjonn <- KJONNvask(org, filbesk = filbesk, batchdate = batchdate, globs = globs)
      
      SkrivKBLogg(KB = kjonn, type = "KJONN", filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
      TilFilLogg(filbesk$KOBLID, "KJONN_ok", ifelse(getOption("khfunctions.illegal") %in% kjonn$OMK, 0, 1), batchdate = batchdate, globs = globs)
      
      DF$KJONN <- as.integer(plyr::mapvalues(DF$KJONN, kjonn$ORG, kjonn$OMK, warn_missing = FALSE))
    }
    
    # AAR TIL INTERVALL
    if ("AAR" %in% names(DF)) {
      org <- setNames(as.data.frame(table(DF$AAR, useNA = "ifany"), stringsAsFactors = FALSE), c("ORG", "FREQ"))
      
      aar <- AARvask(org, filbesk = filbesk, batchdate = batchdate, globs = globs)
      
      SkrivKBLogg(KB = aar, type = "AAR", filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
      TilFilLogg(filbesk$KOBLID, "AAR_ok", ifelse(getOption("khfunctions.aar_illegal") %in% aar$OMK, 0, 1), batchdate = batchdate, globs = globs)
      
      # Kast der AAR koder til "-" (maa ta det her og ikek generelle under pga intervall)
      DF <- subset(DF, !AAR %in% subset(aar, OMK == "-")$ORG)
      
      DF$AARl <- as.integer(plyr::mapvalues(DF$AAR, aar$ORG, aar$LO, warn_missing = FALSE))
      DF$AARh <- as.integer(plyr::mapvalues(DF$AAR, aar$ORG, aar$HI, warn_missing = FALSE))
    }
    
    
    # RENSK UTDANN
    if ("UTDANN" %in% names(DF)) {
      org <- setNames(as.data.frame(table(DF$UTDANN, useNA = "ifany"), stringsAsFactors = FALSE), c("ORG", "FREQ"))
      utdann <- UTDANNvask(org, filbesk = filbesk, batchdate = batchdate, globs = globs)
      
      SkrivKBLogg(KB = utdann, type = "UTDANN", filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
      TilFilLogg(filbesk$KOBLID, "UTDANN_ok", ifelse(getOption("khfunctions.illegal") %in% utdann$OMK, 0, 1), batchdate = batchdate, globs = globs)
      
      DF$UTDANN <- as.integer(plyr::mapvalues(DF$UTDANN, utdann$ORG, utdann$OMK, warn_missing = FALSE))
    }
    
    
    # RENSK INNVKAT
    if ("INNVKAT" %in% names(DF)) {
      org <- setNames(as.data.frame(table(DF$INNVKAT, useNA = "ifany"), stringsAsFactors = FALSE), c("ORG", "FREQ"))
      innvkat <- INNVKATvask(org, filbesk = filbesk, batchdate = batchdate, globs = globs)
      
      SkrivKBLogg(KB = innvkat, type = "INNVKAT", filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
      TilFilLogg(filbesk$KOBLID, "INNVKAT_ok", ifelse(getOption("khfunctions.illegal") %in% innvkat$OMK, 0, 1), batchdate = batchdate, globs = globs)
      
      DF$INNVKAT <- as.integer(plyr::mapvalues(DF$INNVKAT, innvkat$ORG, innvkat$OMK, warn_missing = FALSE))
    }
    
    
    # RENSK LANDBAK
    if ("LANDBAK" %in% names(DF)) {
      org <- setNames(as.data.frame(table(DF$LANDBAK, useNA = "ifany"), stringsAsFactors = FALSE), c("ORG", "FREQ"))
      landbak <- LANDBAKvask(org, filbesk = filbesk, batchdate = batchdate, globs = globs)
      
      SkrivKBLogg(KB = landbak, type = "LANDBAK", filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
      TilFilLogg(filbesk$KOBLID, "LANDBAK_ok", ifelse(getOption("khfunctions.illegal") %in% landbak$OMK, 0, 1), batchdate = batchdate, globs = globs)
      
      DF$LANDBAK <- as.integer(plyr::mapvalues(DF$LANDBAK, landbak$ORG, landbak$OMK, warn_missing = FALSE))
    }
    
    if ("KODEBOKpost" %in% names(dumps)) DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "KODEBOKpost", sep = "_"), globs = globs, format = dumps[["KODEBOKpost"]])
    
    # DROPP ALLE MED '-' I TABULERING (merk: AAR og ALDER maatte tas over pga intervall)
    DF <- subset(DF, rowSums(DF[, names(DF) %in% getOption("khfunctions.taborgs")] == "-") == 0)
    
    # VASK VERDIER. Litt annen prosess, bruker KB, men tabulerer bare ikke-numeriske.
    # Setter numerisk, med flagg for type NA
    for (val in c("VAL1", "VAL2", "VAL3")) {
      # Bedre, men funker ikke i forhold til logg
      # for (val in names(DF)[grepl("VAL\\d+$",names(DF))]){
      if (val %in% names(DF)) {
        DF[is.na(DF[, val]), val] <- ""
        
        valKB <- KBomkod(DF[, val], type = val, valsubs = TRUE, filbesk = filbesk, batchdate = batchdate, globs = globs)
        valKB <- KBomkod(DF[[val]], type = val, valsubs = TRUE, filbesk = filbesk, batchdate = batchdate, globs = globs)
        valKBut <- valKB$subsant
        
        valok <- 1
        valf <- paste(val, ".f", sep = "")
        vala <- paste(val, ".a", sep = "")
        valomk <- paste(val, "omk", sep = "")
        
        
        # Lag omkodet verdi med numerisk. Ikke numerisk blir foreloepig NA
        suppressWarnings(DF[, valomk] <- as.numeric(valKB$omk))
        DF[, valf] <- 0
        DF[, vala] <- 1
        DF[valKB$omk == ".." & DF[, val] != valKB$omk, valf] <- 1
        DF[valKB$omk == "." & DF[, val] != valKB$omk, valf] <- 2
        DF[valKB$omk == ":" & DF[, val] != valKB$omk, valf] <- 3
        
        
        # Behandle (resterende) ikke-numeriske
        nonNum <- which(is.na(DF[, valomk]) & DF[, val] == valKB$omk)
        if (length(nonNum) > 0) {
          # Kodebok
          valKB <- setNames(as.data.frame(table(DF[nonNum, val], useNA = "ifany"), stringsAsFactors = FALSE), c("ORG", "FREQ"))
          valKB$KBOMK <- KBomkod(valKB$ORG, type = val, filbesk = filbesk, batchdate = batchdate, globs = globs)
          valKB$OMK <- valKB$KBOMK
          
          
          # Gjoer nytt forsoek paa numerisk konvertering etter omkoding
          kbNUM <- suppressWarnings(as.numeric(valKB$OMK))
          
          # Legitime
          valKB$OK <- 0
          Num2 <- which(!is.na(kbNUM))
          valKB$OK[Num2] <- 1
          valKB$OK[valKB$OMK %in% c(".", "..", ":")] <- 1
          if (0 %in% valKB$OK) {
            valok <- 0
          }
          valKBut <- rbind(valKBut, valKB)
          
          # if(valok==0){print(valKB)}
          
          # Internt, regnbart format med numerisk flagg i "VAL1f" etc
          # ".." = 1, "." = 2, ":" = 3
          valKB$kbNUM <- kbNUM
          valKB$FLAG <- 0
          valKB$FLAG[valKB$OMK == ".."] <- 1
          valKB$FLAG[valKB$OMK == "."] <- 2
          valKB$FLAG[valKB$OMK == ":"] <- 3
          valKB$FLAG[valKB$OK == 0] <- 8
          # valKB$kbNUM[valKB$FLAG>0]<-0
          
          # if(valok==0){print(valKB)}
          DF[nonNum, valomk] <- as.numeric(plyr::mapvalues(DF[nonNum, val], valKB$ORG, valKB$kbNUM, warn_missing = FALSE))
          # DF[nonNum,valomk]<-suppressWarnings(as.numeric(plyr::mapvalues(DF[nonNum,val],valKB$ORG,valKB$kbNUM,warn_missing = FALSE)))
          DF[nonNum, valf] <- as.integer(plyr::mapvalues(DF[nonNum, val], valKB[, "ORG"], valKB[, "FLAG"], warn_missing = FALSE))
        }
        
        if (nrow(valKBut) > 0) {
          SkrivKBLogg(KB = valKBut, type = val, filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
        }
        
        
        DF[, val] <- NULL
        DF <- setNames(DF, plyr::mapvalues(names(DF), valomk, val))
        
        # DEVELOP20191219
        
        reskaler <- as.numeric(filbesk[[eval(paste("SKALA", "_", val, sep = ""))]])
        
        if (!(reskaler == 1 | is.na(reskaler))) {
          DF[, val] <- DF[, val] * filbesk[[eval(paste("SKALA", "_", val, sep = ""))]]
        }
      }
      
      TilFilLogg(filbesk$KOBLID, paste(val, "OK", sep = "_"), valok, batchdate = batchdate, globs = globs)
    }
    
    default.stringsAsFactors <- TRUE
    Kols <- c(globs$DefDesign$DesignKolsFA[globs$DefDesign$DesignKolsFA %in% names(DF)], names(DF)[grepl("^VAL\\d+(\\.(f|a)|)$", names(DF))])
    
    # print(filbesk)
    # kAN KRaeSJE VED UKJENT KOLNAVN!
    # print(FGP)
    DF <- DF[, Kols]
    
    # Kast rader for inaktive GEO med alle VAL==NA (maa gjoeres fordi alle kommunekoder gir utrapportert tall fra STATBANK og 0/NA er ikke noeytralt for ikke-sumerbare kolonner, jfr MEDIANINNT)
    # Merk at ekte NA settes inn igjen naar det rektangulariseres paa aktive kommuner ved kubeproduksjon
    GeoFra <- setNames(globs$GeoKoder$FRA, globs$GeoKoder$GEO)
    GeoTil <- setNames(globs$GeoKoder$TIL, globs$GeoKoder$GEO)
    valkols <- get_value_columns(names(DF))
    # Skjoenner ikke hvorfor dette ikke funker
    
    DF2 <- DF[!((unlist(GeoTil[DF$GEO]) <= DF$AARl | unlist(GeoFra[DF$GEO]) >= DF$AARh) & rowSums(is.na(data.frame(DF[, valkols]))) == length(valkols)), ]
    DF <- DF2
    
    
    # Aggreger ned. Unntaksvis der filene er "ucollapset"
    # etter f.eks omkoding av alder til aldersgrupper
    # Om ikke dette gjoeres blir det masse dubletter
    if (!is.na(filbesk$AGGERGER_DF) & filbesk$AGGERGER_DF == 1) {
      print("SKAL COLLAPSE")
      print(dim(DF))
      DF <- KHaggreger(DF, globs = globs)
      print(dim(DF))
    }
    
    
    DF$KOBLID <- filbesk$KOBLID
    DF$ROW <- 1:nrow(DF)
    
    TilFilLogg(filbesk$KOBLID, "FINALh", DFHeadToString(DF), batchdate = batchdate, globs = globs)
  }
  TilFilLogg(filbesk$KOBLID, "TidLagTab", (proc.time() - klokke)[3], batchdate = batchdate, globs = globs)
  
  if (versjonert == TRUE) {
    SVcloneRecord(globs$log, "INNLES_LOGG", filbesk$KOBLID)
    SVcloneRecord(globs$log, "KODEBOK_LOGG", filbesk$KOBLID)
    # SVcloneRecord(globs$log,"KODEBOK_LOGG",filbesk$KOBLID)
  }
  
  if (ok == 0) {
    DF <- data.frame()
    # DF<-DF[0,] #Fungerer ikke mht class, som kan vaere feil
  }
  
  return(DF)
}

#' LesFil (kb)
#'
#' @param filbesk 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
#' @param dumps 
LesFil <- function(filbesk, batchdate = SettKHBatchDate(), globs = SettGlobs(), dumps = character()) {
  is_kh_debug()
  
  klokke <- proc.time()
  DF <- data.frame()
  ok <- 1
  filn <- filbesk$filn
  format <- filbesk$FORMAT
  opt <- filbesk$INNLESARG
  
  
  # Initier log
  RODBC::sqlQuery(globs$log, paste("DELETE * FROM INNLES_LOGG WHERE KOBLID=", filbesk$KOBLID, "AND SV='S'", sep = ""))
  RODBC::sqlQuery(globs$log, paste("INSERT INTO INNLES_LOGG ( KOBLID,BATCH, SV, FILGRUPPE) SELECT =", filbesk$KOBLID, ",'", batchdate, "', 'S','", FinnFilGruppeFraKoblid(filbesk$KOBLID, globs = globs), "'", sep = ""))
  
  # Sjekk om fil eksisterer
  if (file.access(filn, mode = 0) == -1) {
    TilFilLogg(filbesk$KOBLID, "FILNAVNERR", paste("KRITISK FEIL: ", filn, " finnes ikke", sep = ""), batchdate = batchdate, globs = globs)
    ok <- 0
  } else if (file.access(filn, mode = 4) == -1) {
    TilFilLogg(filbesk$KOBLID, "FILNAVNERR", paste("KRITISK FEIL: ", filn, " finnes, men lar seg ikke lese", sep = ""), batchdate = batchdate, globs = globs)
    ok <- 0
  } else {
    default.stringsAsFactors <- FALSE
    
    format <- toupper(format)
    formats <- c("CSV", "XLS", "XLSX", "SPSS", "DBF", "SAS", "HTML")
    if (!format %in% formats) {
      ok <- 0
      TilFilLogg(filbesk$KOBLID, "INNLESARGerr", paste("FORMAT ", format, " ikke kjent, kjenner bare (", paste(formats, collapse = ","), ")", sep = ""), batchdate = batchdate, globs = globs)
    } else {
      # LES INN FIL
      # Skreddersydd feilstyring
      innleserr <- ""
      if (format == "XLS" || format == "XLSX") {
        expr <- paste("Xls2R.KH(filn", ifelse(is.na(opt), "", paste(",", opt, sep = "")), ",globs=globs)", sep = "")
        xls <- eval(parse(text = expr))
        DF <- xls$DF
        ok <- xls$ok
        innleserr <- xls$err
      } else {
        # Feilstyring fra eksterne rutiner med try()
        if (format == "CSV") {
          expr <- paste("KHCsvread(filn", ifelse(is.na(opt), "", paste(",", opt, sep = "")), ")", sep = "")
          INNLES <- try(eval(parse(text = expr)), silent = TRUE)
        } else if (format == "SPSS") {
          INNLES <- try(as.data.frame(foreign::read.spss(file = filn, use.value.labels = FALSE, max.value.labels = 0), stringsAsFactors = FALSE), silent = TRUE)
          # ALternativ metode: T<-spss.get(file=fil)
        } else if (format == "DBF") {
          # DEV sl? av Field name: '***NULL***' changed to: 'X...NULL...'
          INNLES <- try(suppressMessages(foreign::read.dbf(file = filn, as.is = TRUE)), silent = TRUE)
        } else if (format == "SAS") {
          INNLES <- try(sas7bdat::read.sas7bdat(file = filn), silent = TRUE)
        } else if (format == "HTML") {
          INNLES <- try(eval(parse(text = paste("DF<-XML::readHTMLTable(doc=filn,as.data.frame = TRUE,stringsAsFactors=FALSE", ifelse(is.na(opt), "", paste(",", opt, sep = "")), ")", sep = ""))), silent = TRUE)
        }
        if ("try-error" %in% class(INNLES)) {
          innleserr <- INNLES
          ok <- 0
        } else {
          DF <- INNLES
        }
      }
      # Maa sikre at data.frame, noen filer kan vaere bare en skalar (jfr ENPERSON)
      DF <- as.data.frame(DF, stringsAsFactors = FALSE)
      if (ok == 0) {
        TilFilLogg(filbesk$KOBLID, "INNLESARGerr", innleserr, batchdate = batchdate, globs = globs)
      } else {
        # PRINT INNLES
        TilFilLogg(filbesk$KOBLID, "INNLESh", DFHeadToString(DF), batchdate = batchdate, globs = globs)
      }
    }
  }
  
  # Fortsett hvis lest inn er ok
  if (ok == 1) {
    # Gjoer om innlest CSV-aktig til tabell
    
    if (format %in% c("CSV", "XLS", "XLSX")) {
      eval(parse(text = paste("DF<-cSVmod(DF,filbesk,", ifelse(is.na(opt), "", paste(",", opt, sep = "")), ",globs=globs)", sep = "")))
    }
    
    # Sett header manuelt
    # IKKE robust for feil parameter
    if (!is.na(filbesk$MANHEADER)) {
      mh <- unlist(stringr::str_split(filbesk$MANHEADER, "="))
      mh[1] <- gsub("\\[|\\]", "", mh[1])
      
      ## Use old colnames to specify for new colnames with index or regex
      mhi <- tryCatch({
        as.numeric(unlist(strsplit(mh[1], ",")))
      },
      warning = function(w){
        .colXX <- trimws(unlist(strsplit(mh[1], ",")))
        vapply(.colXX, function(x) grep(x, names(DF)), numeric(1))
      },
      error = function(err){
        .colXX <- trimws(unlist(strsplit(mh[1], ",")))
        .varsDF <- sapply(.colXX, function(x) grep(x, names(DF), value = TRUE))
        message("Columnames in the dataset to rename:")
        print(.varsDF)
        stop("Check MANHEADER! Columnames to rename must be unique: [", trimws( mh[1] ), "] =", mh[2])
      })
      
      eval(parse(text = paste("mhs<-", mh[2], sep = "")))
      names(DF)[mhi] <- mhs
      
      # Skjoenner ikke helt hvorfor ikke denne enkler funker:
      # eval(parse(text=paste("names(DF)",filbesk$MANHEADER,sep="")))
    }
    
    # Fix header
    names(DF) <- gsub("^\\s", "", names(DF))
    names(DF) <- gsub("\\s$", "", names(DF))
    names(DF)[names(DF) == ""] <- paste("C", which(names(DF) == ""), sep = "")
    
    
    # DEV dette b?r v?re un?dvendig '' skal v?re lest inn som NA
    # DF[DF==""]<-NA
    # des<-lapply(DF,class)
    # if(length(des[des=="factor"])>0){
    #  cat("FACTOR i DF <",paste(names(des[des=="factor"]),collapse="><"),">, det er ugreit\n",sep="")
    # }
    
    TilFilLogg(filbesk$KOBLID, "modINNLESh", DFHeadToString(DF), batchdate = batchdate, globs = globs)
    
    # EVT SPESIALBEHANDLING
    # Add special variables used in RSYNT1 STATA code
    if ("RSYNT1pre" %in% names(dumps)) DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "RSYNT1pre", sep = "_"), globs = globs, format = dumps[["RSYNT1pre"]])
    DF$filgruppe <- filbesk$FILGRUPPE
    DF$delid <- filbesk$DELID
    DF$tab1_innles <- filbesk$TAB1
    DF <- do_special_handling(dt = DF, code = filbesk$RSYNT1, batchdate = batchdate, stata_exe = globs$StataExe, DTout = FALSE)
    DF[c("filgruppe", "delid", "tab1_innles")] <- NULL
    if ("RSYNT1post" %in% names(dumps)) DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "RSYNT1post", sep = "_"), globs = globs, format = dumps[["RSYNT1post"]])
  }
  
  # sink(file=paste(getOption("khfunctions.root"),"/hoder.txt",sep=""),append=TRUE)
  # cat("\n#################\nFIL: ")
  # cat(filn)
  # cat("\n")
  # print(head(T))
  # sink()
  
  ## These variables are needed only in RSYNT1 for Stata
  DF[c("filgruppe", "delid", "tab1_innles")] <- NULL
  
  TilFilLogg(filbesk$KOBLID, "TidLesFil", (proc.time() - klokke)[3], batchdate = batchdate, globs = globs)
  
  default.stringsAsFactors <- TRUE
  return(list(DF = DF, ok = ok))
}

#' KHCsvread (kb)
#'
#' @param filn 
#' @param header 
#' @param skip 
#' @param colClasses 
#' @param sep 
#' @param quote 
#' @param dec 
#' @param fill 
#' @param encoding 
#' @param blank.lines.skip 
#' @param na.strings 
#' @param brukfread 
#' @param ... 
KHCsvread <- function(filn, header = FALSE, skip = 0, colClasses = "character", sep = ";", quote = "\"", dec = ".", fill = FALSE, encoding = "unknown", blank.lines.skip = FALSE, na.strings = c("NA"), brukfread = TRUE, ...) {
  is_kh_debug()
  
  if (!(quote == "\"" && dec == "." && fill == FALSE && encoding == "unknown")) {
    brukfread <- FALSE
  }
  if (brukfread == TRUE) {
    csvT <- as.data.frame(data.table::fread(filn, header = FALSE, skip = 0, colClasses = "character", sep = sep, na.strings = na.strings))
  } else {
    csvT <- as.data.frame(read.csv(filn, header = FALSE, skip = 0, colClasses = "character", sep = sep, quote = quote, dec = dec, fill = TRUE, encoding = encoding, blank.lines.skip = FALSE, na.strings = na.strings))
  }
  return(csvT)
}

#' cSVmod (kb)
#'
#' @param DF 
#' @param filbesk 
#' @param header 
#' @param skip 
#' @param slettRader 
#' @param sisteRad 
#' @param TomRadSlutt 
#' @param FjernTommeRader 
#' @param FjernTommeKol 
#' @param globs global parameters, defaults to SettGlobs
#' @param ... 
cSVmod <- function(DF, filbesk, header = TRUE, skip = 0, slettRader = integer(0), sisteRad = -1, TomRadSlutt = FALSE, FjernTommeRader = FALSE, FjernTommeKol = TRUE, globs = SettGlobs(), ...) {
  # Ved bruk av undertabeller med titler som ikke staar i egen kolonne
  # Lager egen kolonne av undertitler som blir ekta TAB
  # Ikke saa veldig elegant, men funker for de faar tilfellene der dette trengs og som ellers ville trengt haandsoem
  # Syntaks UNDERTABLOK er TAB:kolonne:kommasep liste undertitler:kommasep liste/skalar offset av disse (dvs antall raders forrykking)
  is_kh_debug()
  
  if (!is.na(filbesk$UNDERTABLOK)) {
    utl <- unlist(stringr::str_split(filbesk$UNDERTABLOK, ":"))
    loks <- as.numeric(unlist(stringr::str_split(utl[3], ",")))
    offsets <- as.numeric(unlist(stringr::str_split(utl[4], ",")))
    nytab <- character(nrow(DF))
    nytab[loks + offsets] <- DF[loks, as.numeric(utl[2])]
    nytab[nytab == ""] <- NA
    nytab <- zoo::na.locf(nytab, na.rm = FALSE)
    DF <- cbind(DF, nytab, stringsAsFactors = FALSE)
  }
  
  if (length(slettRader) > 0) {
    DF <- DF[-slettRader, ]
  }
  if (skip > 0) {
    DF <- DF[-(1:skip), ]
  }
  if (sisteRad > 0) {
    DF <- DF[1:(sisteRad - skip - length(slettRader)), ]
  }
  
  if (TomRadSlutt == TRUE) {
    tomr <- which(rowSums(is.na(DF) | DF == "") == ncol(DF))
    if (!is.na(tomr[1])) {
      DF <- DF[1:(tomr[1] - 1), ]
    }
  }
  if (FjernTommeRader == TRUE) {
    DF <- DF[rowSums(is.na(DF) | DF == "") != ncol(DF), ]
  }
  if (FjernTommeKol == TRUE) {
    DF <- DF[, colSums(is.na(DF) | DF == "") != nrow(DF)]
  }
  # Maa sikre at data.frame, noen filer kan vaere bare en skalar (jfr ENPERSON)
  DF <- as.data.frame(DF, stringsAsFactors = FALSE)
  
  # Sett header. Default er vanlige Excel-kolonnenavn
  
  names(DF) <- excelcols()[1:length(names(DF))]
  
  # Bruk av flernivaa header.
  # Ikke saerlig elegant syntaks, men prinsippet er rett fram
  # Disse pastes (evt) sammen til en header
  # Etter reshape splittes kolonneraden (som naa har blitt en kolonne)
  # i sine respektive kolonner
  # Kan ogsaa vaere pastet sammen originalt
  # Syntaks gir radnummer for de ulike leddene i multihead "c(TABNAVN1=rad1,TABNAVN2=rad2,...)
  if (!is.na(filbesk$MULTIHEAD)) {
    # Prossesser parameterstreng for multihead, gir liste med relevante deler
    mhl <- LesMultiHead(filbesk$MULTIHEAD)
    # Juster radnummerering for skip
    mhl$rader <- mhl$rader - skip
    headers <- DF[mhl$rader, ]
    headers[headers == ""] <- NA
    # Fyll inn ved "sparse" utfylling, slik som ved "innrykket" tabulering i kolonner
    headers <- zoo::na.locf(t(headers), na.rm = FALSE)
    # Paste sammen
    headstr <- apply(headers, 1, paste, collapse = mhl$sep)
    # Sett nye kolonnenavn for de som dekkes av headstr,
    # resten beholder sine standard ("excel") genererte navn
    nonempty <- as.vector(which(headstr != ""))
    names(DF)[nonempty] <- headstr[nonempty]
    # Dropp linjer brukt til header
    DF <- DF[-(1:length(mhl$rader)), ]
  } else if (header == TRUE) {
    if (nrow(DF) > 1) {
      # Bruk defaultnavn i celler der header mangler
      nonempty <- as.vector(which(DF[1, ] != ""))
      names(DF)[nonempty] <- DF[1, nonempty]
      DF <- DF[-1, ]
    } else {
      print("*******************ADVARSEL: Kan ikke sette header fra fil med bare en rad. Skal vel ha opsjon 'header=FALSE'")
    }
  }
  if (!is.na(filbesk$UNDERTABLOK)) {
    names(DF)[ncol(DF)] <- gsub("^(.*?):.*", "\\1", filbesk$UNDERTABLOK)
  }
  names(DF) <- gsub("^ *| *$", "", names(DF))
  # names(DF)<-gsub("[ ,./()+-]","_",names(DF))   #Skal navn fikses? Argumenter for og mot. Valgt: Nei!
  return(DF)
}

#' Xls2R.KH (kb/ybk)
#'
#' @param xlsfil 
#' @param ark 
#' @param globs global parameters, defaults to SettGlobs
#' @param brukfread 
#' @param na.strings 
#' @param ryddOpp 
#' @param ... 
Xls2R.KH <- function(xlsfil, ark = "", globs = SettGlobs(), brukfread = TRUE, na.strings = c("NA"), ryddOpp = 1, ...) {
  is_kh_debug()
  
  err <- ""
  ok <- 1
  DF <- data.frame()
  # step 1: Validate sheetname with fuzzy match
  # rdbh<-RODBC::odbcConnectExcel2007(xlsfil)
  # rdbh<-RODBC::odbcConnectExcel(xlsfil)
  # tables<-RODBC::sqlTables(rdbh)$TABLE_NAME
  # close(rdbh)
  
  tables <- readxl::excel_sheets(xlsfil)
  
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
    INNLES <- try(as.data.frame(readxl::read_excel(xlsfil, sheet = ark, col_names = FALSE, col_types = "text", skip = 0, na = na.strings)))
    if ("try-error" %in% class(INNLES)) {
      err <- INNLES
      ok <- 0
    } else {
      DF <- setNames(INNLES, excelcols()[1:ncol(INNLES)])
      ## ## Finner ut hvis hele kolonne er missing
      ## colMISS = sapply(DF, function(x) all(is.na(x)))
      ## missUT = attributes(colMISS[colMISS==1])$names
      ## DF[missUT] = NULL
    }
  }
  return(list(DF = DF, ok = ok, err = err))
}

#' FinnFilgruppeParametre (kb)
#'
#' @param gruppe 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
FinnFilgruppeParametre <- function(gruppe, batchdate = SettKHBatchDate(), globs = SettGlobs()) {
  is_kh_debug()
  
  dbh <- globs$dbh
  datef <- FormatSqlBatchdate(batchdate)
  FGPaktiv <- as.integer(RODBC::sqlQuery(globs$dbh, paste("SELECT count(*) FROM FILGRUPPER WHERE FILGRUPPE='", gruppe, "'",
                                                          "AND VERSJONFRA<=", datef, " AND VERSJONTIL>", datef, "
                                        ",
                                                          sep = ""
  ), as.is = TRUE))
  FGPfinnes <- as.integer(RODBC::sqlQuery(globs$dbh, paste("SELECT count(*) FROM FILGRUPPER WHERE FILGRUPPE='", gruppe, "'", sep = ""), as.is = TRUE))
  resultat <- list()
  if(FGPfinnes == 0) stop(paste("Filgruppe", gruppe, "finnes ikke. Droppes."))
  if(FGPaktiv == 0) stop(paste("Filgruppe", gruppe, "finnes, men er satt inaktiv"))
  FGP <- as.list(RODBC::sqlQuery(globs$dbh, paste("SELECT * FROM FILGRUPPER WHERE FILGRUPPE='", gruppe, "'",
                                                  "AND VERSJONFRA<=", datef, " AND VERSJONTIL>", datef, "
                                        ",
                                                  sep = ""
  ), as.is = TRUE))
  # Sette endelig default alder ALLE
  # Default er 0_ALDinf    der ALDinf er global parameter i HOVEDPARAMETRE
  amin <- getOption("khfunctions.amin")
  amax <- getOption("khfunctions.amax")
  # Evt egen def for filgruppe fra ALDER_ALLE i tabell FILGRUPPER
  if (!is.na(FGP$ALDER_ALLE)) {
    if (grepl("^\\d+_(\\d+|)$", FGP$ALDER_ALLE)) {
      alle_aldre <- unlist(strsplit(FGP$ALDER_ALLE, "_"))
      if (length(alle_aldre) == 1) {
        amin <- as.numeric(alle_aldre[1])
      } else if (length(alle_aldre) == 2) {
        amin <- as.numeric(alle_aldre[1])
        amax <- as.numeric(alle_aldre[2])
      }
    } else {
      cat("FEIL!!!!!! Feil format FGP$ALDER_ALLE", FGP$ALDER_ALLE, "\n")
    }
  }
  
  vals <- list()
  for (valf in names(FGP)[grepl("^VAL\\d+navn$", names(FGP))]) {
    val <- gsub("(VAL\\d+)navn", "\\1", valf)
    valn <- ifelse(is.na(FGP[[valf]]) || FGP[[valf]] == "", val, FGP[[valf]])
    valmissf <- paste(val, "miss", sep = "")
    valmiss <- ifelse(is.na(FGP[[valmissf]]) || FGP[[valmissf]] == "", "0", FGP[[valmissf]])
    valsumf <- paste(val, "sumbar", sep = "")
    valsum <- ifelse(is.na(FGP[[valsumf]]) || FGP[[valsumf]] == "", "0", FGP[[valsumf]])
    vals[[valn]] <- list(miss = valmiss, sumbar = valsum)
  }
  resultat <- c(FGP, list(vals = vals, amin = amin, amax = amax))
  
  gc()
  return(c(resultat))
}

#' SjekkDuplikater (kb)
#'
#' @param FG 
#' @param filgruppe 
#' @param FullResult 
#' @param batchdate 
#' @param versjonert 
#' @param globs global parameters, defaults to SettGlobs
SjekkDuplikater <- function(FG, filgruppe, FullResult = FALSE, batchdate = SettKHBatchDate(), versjonert = FALSE, globs = SettGlobs()) {
  is_kh_debug()
  
  HarDuplikater <- 0
  if (identical(class(FG), "data.frame")) {
    FG <- data.table::data.table(FG)
  }
  orgkeys <- data.table::key(FG)
  tabkols <- globs$DefDesign$DesignKolsFA
  tabkols <- tabkols[tabkols %in% names(FG)]
  valkols <- get_value_columns(names(FG))
  setkeym(FG, tabkols)
  
  dubi <- duplicated(FG)
  
  DUB <- data.table::data.table()
  result <- c(ANTdNO = 0, fANTV1 = 0, ANTdNOp = 0, fANTV1p = 0, ANTdNOg = 0, fANTV1g = 0)
  # dubi<-duplicated(FG[,tabkols,with=FALSE])
  if (any(dubi)) {
    HarDuplikater <- 1
    DUB <- FG[dubi, ]
    eval(parse(text = paste(
      "DUB[,dNO:=rank(", paste(valkols, collapse = ","), "),by=tabkols]",
      sep = ""
    )))
    DUB[, antV := length(unique(dNO)), by = tabkols]
    DUB[, antK := length(unique(KOBLID)), by = tabkols]
    
    
    # Positive verdier
    eval(parse(text = paste(
      "DUBp<-subset(DUB,", paste(valkols, "!=0", sep = "", collapse = " | "), ")",
      sep = ""
    )))
    if (nrow(DUBp) > 0) {
      eval(parse(text = paste(
        "DUBp[,dNOp:=rank(", paste(valkols, collapse = ","), "),by=tabkols]",
        sep = ""
      )))
      DUBp[, antVp := length(unique(dNOp)), by = tabkols]
      DUBp[, antKp := length(unique(KOBLID)), by = tabkols]
      data.table::setkeyv(DUB, names(DUB))
      data.table::setkeyv(DUBp, names(DUB))
      DUB <- DUBp[DUB]
      DUB[is.na(dNOp), dNOp := 0]
      DUB[is.na(antVp), antVp := 0]
      DUB[is.na(antKp), antKp := 0]
    } else {
      DUB[, dNOp := 0]
      DUB[, antVp := 0]
      DUB[, antKp := 0]
    }
    
    # Hold ##99-geokoder utenom. Her blir det lagd dubeltter naar to illegitime KNR blir samme ##99 etc
    DUBg <- subset(DUB, !grepl("99$", GEO))
    if (nrow(DUBg) > 0) {
      eval(parse(text = paste(
        "DUBg[,dNOg:=rank(", paste(valkols, collapse = ","), "),by=tabkols]",
        sep = ""
      )))
      DUBg[, antVg := length(unique(dNOg)), by = tabkols]
      DUBg[, antKg := length(unique(KOBLID)), by = tabkols]
      data.table::setkeyv(DUB, names(DUB))
      data.table::setkeyv(DUBg, names(DUB))
      DUB <- DUBg[DUB]
      DUB[is.na(dNOg), dNOg := 0]
      DUB[is.na(antVg), antVg := 0]
      DUB[is.na(antKg), antKg := 0]
    } else {
      DUB[, dNOg := 0]
      DUB[, antVg := 0]
      DUB[, antKg := 0]
    }
    
    ANTdNO <- nrow(DUB)
    fANTV1 <- nrow(subset(DUB, antV > 1))
    ANTdNOp <- nrow(subset(DUB, dNOp > 0))
    fANTV1p <- nrow(subset(DUB, antVp > 1))
    ANTdNOg <- nrow(subset(DUB, dNOg > 0))
    fANTV1g <- nrow(subset(DUB, antVg > 1))
    result <- c(ANTdNO = ANTdNO, fANTV1 = fANTV1, ANTdNOp = ANTdNOp, fANTV1p = fANTV1p, ANTdNOg = ANTdNOg, fANTV1g = fANTV1g)
    
    # Skriv dubletter til logg
    RODBC::sqlQuery(globs$log, paste("DELETE * FROM DUBLETT WHERE FILGRUPPE='", filgruppe, "' AND SV='S'", sep = ""))
    # Legg til resterende kolonner
    # Maa ha ok kolonnenavn til database
    data.table::setnames(DUB, names(DUB), gsub("^(VAL\\d+)\\.f$", "\\1f", names(DUB)))
    
    tmp <- RODBC::sqlQuery(globs$log, "SELECT * FROM DUBLETT WHERE KOBLID=-1")
    tmp2 <- tmp
    tmp[1:nrow(DUB), ] <- NA
    tmp[, intersect(names(tmp), names(DUB))] <- DUB[, intersect(names(tmp), names(DUB)), with = FALSE]
    tmp$FILGRUPPE <- filgruppe
    tmp$BATCHID <- batchdate
    tmp$SV <- "S"
    if (nrow(DUB) < 1000) {
      RODBC::sqlSave(globs$log, tmp, "DUBLETT", rownames = FALSE, append = TRUE)
      if (versjonert == TRUE) {
        tmp$SV <- "V"
        RODBC::sqlSave(globs$log, tmp, "DUBLETT", rownames = FALSE, append = TRUE)
      }
    }
  }
  setkeym(FG, orgkeys)
  if (FullResult == TRUE) {
    return(list(DUB = DUB, ANT = result))
  } else {
    return(result)
  }
}

#' ReshapeTab (kb)
#'
#' @param DELF 
#' @param filbesk 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
#'
#' @return
#' @export
#'
#' @examples
ReshapeTab <- function(DELF, filbesk, batchdate = SettKHBatchDate(), globs = SettGlobs()) {
  # Reshape av DELF basert paa parametre i filbesk
  is_kh_debug()
  
  ok <- 1
  if (!(is.na(filbesk$RESHAPEid) || filbesk$RESHAPEid == "")) {
    idvars <- eval(parse(text = paste("c(", filbesk$RESHAPEid, ")")))
  } else {
    idvars <- NULL
  }
  mevars <- NULL
  if (!(is.na(filbesk$RESHAPEmeas) || filbesk$RESHAPEmeas == "")) {
    mevars <- eval(parse(text = paste("c(", filbesk$RESHAPEmeas, ")")))
  }
  varname <- "variable"
  valname <- "value"
  # varname maa tas fra MULTIHEAD om denne brukes
  if (!is.na(filbesk$MULTIHEAD)) {
    varname <- LesMultiHead(filbesk$MULTIHEAD)$varname
  } else if (!(is.na(filbesk$RESHAPEvar) || filbesk$RESHAPEvar == "")) {
    varname <- as.character(filbesk$RESHAPEvar)
  }
  if (!(is.na(filbesk$RESHAPEval) || filbesk$RESHAPEval == "")) {
    valname <- as.character(filbesk$RESHAPEval)
  }
  
  if (all(idvars %in% names(DELF)) & (is.null(mevars) | all(mevars %in% names(DELF)))) {
    DELF[, idvars] <- sapply(DELF[, idvars], as.character) # Maa vaere av samme type for at ikke reshape skal kraesje
    if (!is.null(mevars)) {
      DELF <- data.table::melt(as.data.table(DELF), id.vars = idvars, measure.vars = mevars, variable.name = varname, value.name = valname, na.rm = FALSE)
    } else {
      DELF <- data.table::melt(as.data.table(DELF), id.vars = idvars, variable.name = varname, value.name = valname, na.rm = FALSE)
    }
    data.table::setDF(DELF)
    DELF[, varname] <- as.character(DELF[, varname]) # Kan ha blitt factor, og det gir kroell senere
  } else {
    rshperr <- ""
    if (!all(idvars %in% names(DELF))) {
      rshperr <- paste(rshperr, "Ukjente idvars <", paste(idvars[!idvars %in% names(DELF)], ">."))
    }
    if (!is.null(mevars) & !all(mevars %in% names(DELF))) {
      rshperr <- paste(rshperr, "Ukjente mevars <", paste(mevars[!mevars %in% names(DELF)], ">."))
    }
    TilFilLogg(filbesk$KOBLID, "RESHAPEERR", rshperr, batchdate = batchdate, globs = globs)
    ok <- 0
  }
  
  
  return(list(DF = DELF, ok = ok))
}

#' KBomkod (kb)
#'
#' @param org 
#' @param type 
#' @param filbesk 
#' @param valsubs 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
KBomkod <- function(org, type, filbesk, valsubs = FALSE, batchdate = NULL, globs = SettGlobs()) {
  is_kh_debug()
  
  datef <- format(Sys.time(), "#%Y-%m-%d#")
  if (!is.null(batchdate)) {
    datef <- FormatSqlBatchdate(batchdate)
  }
  omk <- org
  kbf <- paste(type, "kb", sep = "")
  sql <- paste("SELECT TYPE, ORGKODE, NYKODE FROM KODEBOK WHERE
             FELTTYPE='", type,
               "' AND FILGRUPPE='", filbesk$FILGRUPPE,
               "' AND (DELID='", filbesk$DELID, "' OR DELID='FELLES')",
               " AND VERSJONFRA<=", datef,
               " AND VERSJONTIL>", datef,
               sep = ""
  )
  kbok <- RODBC::sqlQuery(globs$dbh, sql, as.is = TRUE)
  kbok[is.na(kbok)] <- ""
  subsant <- data.frame(ORG = character(0), KBOMK = character(0), OMK = character(0), FREQ = integer(0), OK = integer(0))
  if (nrow(kbok) > 0) {
    KBsubs <- subset(kbok, TYPE == "SUB") # Regulaeruttrykk
    KB <- subset(kbok, TYPE == "KB") # Oppslagsliste
    i <- 1
    while (i <= nrow(KBsubs)) {
      KBsub <- KBsubs[i, ]
      if (valsubs == TRUE) {
        subsant <- rbind(subsant, data.frame(ORG = KBsub$ORGKODE, KBOMK = paste("<", KBsub$NYKODE, ">", sep = ""), OMK = paste("<", KBsub$NYKODE, ">", sep = ""), FREQ = length(grepl(KBsub$ORGKODE, omk, perl = TRUE)), OK = 1))
      }
      # omk<-sub(eval(parse(text=KBsub$ORGKODE)),eval(parse(text=KBsub$NYKODE)),omk)
      omk <- sub(KBsub$ORGKODE, KBsub$NYKODE, omk, perl = TRUE)
      i <- i + 1
    }
    if (valsubs == TRUE) {
      # Ta bare numeriske fra
      KB <- KB[!is.na(suppressWarnings(as.numeric(KB$ORGKODE))), ]
      if (nrow(KB) > 0) {
        freq <- table(omk)
        freq <- freq[KB$ORGKODE]
        if (!is.na(freq)) {
          subsant <- rbind(subsant, data.frame(ORG = KB$ORGKODE, KBOMK = KB$NYKODE, OMK = KB$NYKODE, FREQ = freq, OK = 1))
          # tmp2<-as.data.frame(table(DF$GEO,useNA="ifany"),stringsAsFactors=FALSE)
          omk <- plyr::mapvalues(omk, KB$ORGKODE, KB$NYKODE, warn_missing = FALSE)
        }
      }
    } else {
      omk <- plyr::mapvalues(omk, KB$ORGKODE, KB$NYKODE, warn_missing = FALSE)
    }
  }
  
  if (valsubs == FALSE) {
    return(omk)
  } else {
    return(list(omk = omk, subsant = subsant))
  }
}

#' GEOvask (kb)
#'
#' @param geo 
#' @param filbesk 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
GEOvask <- function(geo, filbesk = data.frame(), batchdate = SettKHBatchDate(), globs = SettGlobs(), harlevekaar = F) {
  is_kh_debug()
  
  if (nrow(filbesk) == 0) {
    geo <- setNames(as.data.frame(geo, stringsAsFactors = FALSE), c("GEO"))
    geo$KBOMK <- geo[, 1]
  } else {
    geo$KBOMK <- KBomkod(geo$ORG, type = "GEO", filbesk = filbesk, batchdate = batchdate, globs = globs)
    if (!is.na(filbesk$TKNR)) {
      suppressWarnings(geo$KBOMK[geo$ORG == geo$KBOMK] <- plyr::mapvalues(geo$ORG[geo$ORG == geo$KBOMK], globs$TKNR$ORGKODE, globs$TKNR$NYKODE, warn_missing = FALSE))
    }
  }
  geo$OMK <- geo$KBOMK
  geo$OK <- 1
  # Litt dirty her, sprintf funker d?rlig p? Windows: sprintf("%04s","214") -> " 0214"
  # M? bruke sprintf("%04s",as.numeric("0214")) for ? f? "0214", det blir for dumt
  geo$OMK <- sub("^\\s*", "", geo$OMK, ignore.case = TRUE)
  geo$OMK <- sub("\\s*$", "", geo$OMK, ignore.case = TRUE)
  geo$OMK <- sub("^0{1,2}(( hele|) landet| *$)", "0", geo$OMK, ignore.case = TRUE)
  geo$OMK <- sub("^(Hele +|)landet( i alt|) *$", "0", geo$OMK, ignore.case = TRUE)
  geo$OMK <- sub("^Fylke (\\d{1,2})$", "\\1", geo$OMK)
  geo$OMK <- sub("^(\\d{8})a{0,1}( +[A-ZÆØÅ].*| *$)", "\\1", geo$OMK)
  geo$OMK <- sub("^(\\d{7})a{0,1}( +[A-ZÆØÅ].*| *$)", "0\\1", geo$OMK)
  geo$OMK <- sub("^(\\d{6})a{0,1}( +[A-ZÆØÅ].*| *$)", "\\1", geo$OMK)
  geo$OMK <- sub("^(\\d{5})a{0,1}( +[A-ZÆØÅ].*| *$)", "0\\1", geo$OMK)
  geo$OMK <- sub("^(\\d{4})( +[A-ZÆØÅ].*| *$)", "\\1", geo$OMK)
  geo$OMK <- sub("^(\\d{3})( +[A-ZÆØÅ].*| *$)", "0\\1", geo$OMK)
  geo$OMK <- sub("^([012][1-9]|10|20|88|99)( +[A-ZÆØÅ].*| *$)", "\\1", geo$OMK)
  geo$OMK <- sub("^([1-9])( +[A-ZÆØÅ].*| *$)", "0\\1", geo$OMK)
  
  geo$OMK <- sub("^(\\d{4})xx*", "\\1", geo$OMK, ignore.case = TRUE)
  
  # Kode fra navn
  # Maa bli mer avansert for aa bli robust. Koder naa 1 til flere (Nes, etc)
  UGeo <- data.frame(NAVN = geo$OMK[!grepl("^\\d+$", geo$OMK)])
  if (nrow(UGeo) > 0) {
    GeoNavn <- RODBC::sqlQuery(globs$dbh, "SELECT * from GeoNavn", as.is = TRUE)
    omk <- sqldf::sqldf("SELECT GEO, UGeo.NAVN FROM UGeo INNER JOIN GeoNavn ON UGeo.NAVN=GeoNavn.NAVN")
    geo$OMK <- plyr::mapvalues(geo$OMK, omk$NAVN, omk$GEO, warn_missing = FALSE)
  }
  
  if (grepl("4", filbesk$SONER)) {
    geo$OMK[nchar(geo$OMK) == 4] <- paste(geo$OMK[nchar(geo$OMK) == 4], "00", sep = "")
  }
  
  # Finn ukjente koder. Sett til ukjent (99) under fylke eller by om mulig, ellers
  # TMP<-globs$GeoKoder
  # ukjent<-sqldf("SELECT OMK FROM geo LEFT JOIN TMP ON geo.OMK=TMP.GEO WHERE TMP.ID Is NULL")
  # print(head(globs$GeoKoder))
  # print(geo[1:50,])
  # print(which(!(geo[,"OMK"] %in% globs$GeoKoder$GEO)))
  ukjent <- geo$OMK[!(geo$OMK %in% c(globs$GeoKoder$GEO, "-"))]
  
  
  ukjent99 <- ukjent
  ukjent99 <- sub("^\\d{2}$", 99, ukjent99) # Ukjent fylke
  ukjent99 <- gsub("^(\\d{2})\\d{2}$", paste("\\1", "99", sep = ""), ukjent99) # Ukjent kommune
  ukjent99 <- sub("^(\\d{2})(\\d{2})00$", paste("\\1", "9900", sep = ""), ukjent99) # Ukjent bydel
  ukjent99 <- sub("^(\\d{4})([1-9]\\d|0[1-9])$", paste("\\1", "99", sep = ""), ukjent99) # Ukjent bydel
  ukjent99 <- sub("^(\\d{6})([1-9]\\d|0[1-9])$", paste("\\1", "99", sep = ""), ukjent99)
  
  # Sjekk om legitime 99-ukjente
  ukjent <- ukjent[ukjent99 %in% globs$GeoKoder$GEO]
  ukjent99 <- ukjent99[ukjent99 %in% globs$GeoKoder$GEO]
  geo$OMK <- plyr::mapvalues(geo$OMK, ukjent, ukjent99, warn_missing = FALSE)
  
  ukjent <- geo$OMK[!(geo$OMK %in% c(globs$GeoKoder$GEO, "-"))]
  heltukjent <- ukjent
  heltukjent[nchar(ukjent) == 9] <- 99999999
  heltukjent[nchar(ukjent) == 6] <- 999999
  heltukjent[nchar(ukjent) == 4] <- 9999
  heltukjent[nchar(ukjent) == 2] <- 99
  geo$OMK <- plyr::mapvalues(geo$OMK, ukjent, heltukjent, warn_missing = FALSE)
  
  # Sett GEOniv
  geo$GEOniv <- as.character(NA)
  geo$GEOniv[nchar(geo$OMK) == 8] <- ifelse(harlevekaar, "V", "G")
  if (grepl("6", filbesk$SONER)) {
    geo$GEOniv[nchar(geo$OMK) == 6] <- "S"
  } else {
    geo$GEOniv[nchar(geo$OMK) == 6] <- "B"
    geo$OMK[nchar(geo$OMK) == 6] <- gsub("^(\\d{4})00$", paste("\\1", "99", sep = ""), geo$OMK[nchar(geo$OMK) == 6])
  }
  geo$GEOniv[nchar(geo$OMK) == 4] <- "K"
  # geo$GEOniv[nchar(geo$OMK)==2 & !geo$OMK %in% c(51:54)]<-"F"
  geo$GEOniv[nchar(geo$OMK) == 2 & !geo$OMK %in% c(81:84)] <- "F"
  # geo$GEOniv[geo$OMK %in% c(51:54)]<-"H"
  geo$GEOniv[geo$OMK %in% c(81:84)] <- "H"
  geo$GEOniv[geo$OMK == 0] <- "L"
  geo$GEOniv[geo$OMK == "-"] <- "-"
  geo$GEOniv[is.na(geo$GEOniv)] <- "U"
  
  # Ekte ulegit
  geo$OK[geo$GEOniv == "-"] <- 1
  # DEVELOP: bare et GEOniv, sett ukjent paa dette nivaaet
  # Fil har bare kommunedata -> bruker 8888
  if (sum(c("G", "B", "F", "L") %in% geo$GEOniv) == 0) {
    geo$OMK[geo$OK == 0] <- "8888"
    geo$GEOniv[geo$OK == 0] <- "K"
  } else {
    geo$OMK[geo$OK == 0] <- getOption("khfunctions.geo_illegal")
  }
  # Sett fylke
  geo$FYLKE <- NA
  subfylke <- which(geo$GEOniv %in% c("G", "V", "S", "K", "F", "B"))
  geo$FYLKE[subfylke] <- substr(geo$OMK[subfylke], 1, 2)
  geo$FYLKE[geo$GEOniv %in% c("H", "L")] <- "00"
  
  return(geo)
}

#' ALDERvask (kb)
#'
#' @param alder 
#' @param filbesk 
#' @param FGP 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
ALDERvask <- function(alder, filbesk = data.frame(), FGP = list(amin = 0, amax = 120), batchdate = SettKHBatchDate(), globs = SettGlobs()) {
  is_kh_debug()
  
  amax <- FGP$amax
  amin <- FGP$amin
  if (nrow(filbesk) == 0) {
    alder <- setNames(as.data.frame(alder, stringsAsFactors = FALSE), c("ALDER"))
    alder$KBOMK <- alder[, 1]
  } else {
    alder$KBOMK <- KBomkod(alder$ORG, type = "ALDER", filbesk = filbesk, batchdate = batchdate, globs = globs)
  }
  alder$OMK <- alder$KBOMK
  alder$OK <- 1
  # alder$OMK<-sub("^ *(\\d+) *[\\_\\-] *(\\d+) *(.r|) *, *totalt$","\\1_\\2",alder$OMK,ignore.case = TRUE)
  alder$OMK <- sub("_år$", " år", alder$OMK)
  alder$OMK <- sub("(.+?),* *totalt *$", "\\1", alder$OMK, ignore.case = TRUE)
  alder$OMK <- sub("^ *(\\d+) *[-_] *(\\d+)( +år| *$)", "\\1_\\2", alder$OMK, ignore.case = TRUE)
  alder$OMK <- sub("^ *(\\d+) *- *high( +år| *$)", "\\1_", alder$OMK, ignore.case = TRUE)
  alder$OMK <- sub("^ *low *- *(\\d+)( +år| *$)", "_\\1", alder$OMK, ignore.case = TRUE)
  alder$OMK <- sub("^ *(\\d+) *\\+( +år| *$)", "\\1_", alder$OMK, ignore.case = TRUE)
  alder$OMK <- sub("^ *(\\d+) +år +\\+ *$", "\\1_", alder$OMK, ignore.case = TRUE)
  alder$OMK <- sub("^ *(\\d+) *-( +år| *$)", "_\\1", alder$OMK, ignore.case = TRUE)
  alder$OMK <- sub("^ *(\\d+) *år *-$", "_\\1", alder$OMK, ignore.case = TRUE)
  alder$OMK <- sub("^ *- *(\\d+)( +år| *$)", "_\\1", alder$OMK, ignore.case = TRUE)
  alder$OMK <- sub("^ *(\\d+) år (og|eller) eldre", "\\1_", alder$OMK, ignore.case = TRUE)
  alder$OMK <- sub("^ *over (\\d+)( å?r| *$)", "\\1_", alder$OMK, ignore.case = TRUE)
  # alder$OMK<-sub("^ *under (\\d+)( +år| *$)","_\\1",alder$OMK,ignore.case = TRUE)  # Dette blri galt, maa erstatte med "_(\\1-1)", men faar ikke det til. Maa bruke kdoebok
  alder$OMK <- sub("^ *(\\d+)( ?r|) *(og|eller) (yngre|under)", "_\\1", alder$OMK, ignore.case = TRUE)
  alder$OMK <- sub("^ *(\\d+) *( +år| *$)", "\\1_\\1", alder$OMK, ignore.case = TRUE)
  alder$OMK <- sub("^ *(Alle( *aldre.*|)|Totalt{0,1}|I alt) *$", paste(amin, "_", amax, sep = ""), alder$OMK, ignore.case = TRUE)
  alder$OMK <- sub("^ *(Ukjent|Uoppgitt|Ikke kjent) *$", getOption("khfunctions.alder_ukjent"), alder$OMK, ignore.case = TRUE)
  
  # alder$OMK[is.na(alder$OMK)]<-"999_999"
  alder$OMK <- sub("^(\\d+)_$", paste("\\1_", amax, sep = ""), alder$OMK)
  alder$OMK <- sub("^_(\\d+)$", paste(amin, "_\\1", sep = ""), alder$OMK)
  
  # Ukjent????????
  # !Maa ha to amax, en for ukjent som er hoeyere, se ogsaa ulest under!!!
  
  okformat <- grepl("^\\d+_\\d+$|^-$", alder$OMK)
  # Ugyldig verdi/ukjent kode
  alder$OMK[!okformat] <- getOption("khfunctions.alder_illegal")
  alder$OK[!okformat] <- 0
  
  # Sett intervall
  alder[, c("LO", "HI")] <- suppressWarnings(matrix(as.integer(stringr::str_split_fixed(alder$OMK, "_", 2)), ncol = 2))
  # Ugyldig intervall
  alder$OMK[alder$HI < alder$LO] <- getOption("khfunctions.alder_illegal")
  alder$OMK[alder$HI > 130 & !(alder$OMK %in% c(getOption("khfunctions.alder_illegal"), getOption("khfunctions.alder_ukjent")))] <- getOption("khfunctions.alder_illegal")
  alder[, c("LO", "HI")] <- suppressWarnings(matrix(as.integer(stringr::str_split_fixed(alder$OMK, "_", 2)), ncol = 2))
  return(alder)
}

#' KJONNvask (kb)
#'
#' @param kjonn 
#' @param filbesk 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
KJONNvask <- function(kjonn, filbesk = data.frame(), batchdate = SettKHBatchDate(), globs = SettGlobs()) {
  is_kh_debug()
  
  if (nrow(filbesk) == 0) {
    kjonn <- setNames(as.data.frame(kjonn, stringsAsFactors = FALSE), c("KJONN"))
    kjonn <- kjonn[, 1]
  } else {
    kjonn$KBOMK <- KBomkod(kjonn$ORG, type = "KJONN", filbesk = filbesk, batchdate = batchdate, globs = globs)
  }
  kjonn$OK <- 1
  kjonn$OMK <- kjonn$KBOMK
  kjonn$OMK <- sub("^ *(M|Menn|Mann|gutt(er|)|g|1) *$", "1", kjonn$OMK, ignore.case = TRUE)
  kjonn$OMK <- sub("^ *(K|F|Kvinner|Kvinne|jente(r|)|j|2) *$", "2", kjonn$OMK, ignore.case = TRUE)
  kjonn$OMK <- sub("^ *(Tot(alt{0,1}|)|Begge([ \\._]*kjønn|)|Alle|A|0|M\\+K) *$", "0", kjonn$OMK, ignore.case = TRUE)
  kjonn$OMK <- sub("^ *(Uspesifisert|Uoppgitt|Ikke spesifisert|Ikke oppgitt|Ukjent|) *$", "9", kjonn$OMK, ignore.case = TRUE)
  # kjonn$OMK[is.na(kjonn$ORG)]<-9
  
  # Ugyldig verdi/ukjent kode
  kjonn$OMK[!(kjonn$OMK %in% c(0, 1, 2, 9, "-"))] <- getOption("khfunctions.illegal")
  kjonn$OK[!(kjonn$OMK %in% c(0, 1, 2, 9, "-"))] <- 0
  
  return(kjonn)
}

#' UTDANNvask
#'
#' @param utdann 
#' @param filbesk 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
#' @param regexp 
UTDANNvask <- function(utdann, filbesk = data.frame(), batchdate = SettKHBatchDate(), globs = SettGlobs(), regexp = FALSE) {
  is_kh_debug()
  
  if (nrow(filbesk) == 0) {
    utdann <- setNames(as.data.frame(utdann, stringsAsFactors = FALSE), c("UTDANN"))
    utdann$KBOMK <- utdann[, 1]
  } else {
    utdann$KBOMK <- KBomkod(utdann$ORG, type = "UTDANN", filbesk = filbesk, batchdate = batchdate, globs = globs)
  }
  utdann$OK <- 1
  utdann$OMK <- utdann$KBOMK
  utdann$OMK <- sub("^0([0-4])$", "\\1", utdann$OMK, ignore.case = TRUE)
  if (regexp == TRUE) {
    utdann$OMK <- sub("^ *(grunnskole) *$", "1", utdann$OMK, ignore.case = TRUE)
    utdann$OMK <- sub("^ *(videregående( skole|)) *$", "2", utdann$OMK, ignore.case = TRUE)
    utdann$OMK <- sub("^ *(Universitet.*) *$", "3", utdann$OMK, ignore.case = TRUE)
    utdann$OMK <- sub("^ *(anne[nt]|ingen|uopgitt|ukjent) *$", "4", utdann$OMK, ignore.case = TRUE)
  }
  utdann$OMK <- sub("^ *(alle) *$", "0", utdann$OMK, ignore.case = TRUE)
  
  # Ugyldig verdi/ukjent kode
  utdann$OMK[!(utdann$OMK %in% c(0, 1, 2, 3, 4, "-"))] <- getOption("khfunctions.illegal")
  utdann$OK[!(utdann$OMK %in% c(0, 1, 2, 3, 4, "-"))] <- 0
  return(utdann)
}

#' INNVKATvask
#'
#' @param innvkat 
#' @param filbesk 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
#' @param regexp 
INNVKATvask <- function(innvkat, filbesk = data.frame(), batchdate = SettKHBatchDate(), globs = SettGlobs(), regexp = FALSE) {
  is_kh_debug()
  
  if (nrow(filbesk) == 0) {
    innvkat <- setNames(as.data.frame(innvkat, stringsAsFactors = FALSE), c("INNVKAT"))
    innvkat$KBOMK <- innvkat[, 1]
  } else {
    innvkat$KBOMK <- KBomkod(innvkat$ORG, type = "INNVKAT", filbesk = filbesk, batchdate = batchdate, globs = globs)
  }
  innvkat$OK <- 1
  innvkat$OMK <- innvkat$KBOMK
  if (regexp == TRUE) {
    innvkat$OMK <- sub("^ *(ugift|ug) *$", "1", innvkat$OMK, ignore.case = TRUE)
    innvkat$OMK <- sub("^ *(gift|g) *$", "2", innvkat$OMK, ignore.case = TRUE)
    innvkat$OMK <- sub("^ *(enke.*|e) *$", "3", innvkat$OMK, ignore.case = TRUE)
    innvkat$OMK <- sub("^ *(skilt|separert|s|skilt\\/separert) *$", "4", innvkat$OMK, ignore.case = TRUE)
    innvkat$OMK <- sub("^ *(annen) *$", "5", innvkat$OMK, ignore.case = TRUE)
    innvkat$OMK <- sub("^ *(ukjent|uoppgitt) *$", "9", innvkat$OMK, ignore.case = TRUE)
  }
  innvkat$OMK <- sub("^ *(alle) *$", "0", innvkat$OMK, ignore.case = TRUE)
  
  # Ugyldig verdi/ukjent kode
  innvkat$OMK[!(innvkat$OMK %in% c(0, 1, 2, 3, 4, 5, 9, 20, "-"))] <- getOption("khfunctions.illegal")
  innvkat$OK[!(innvkat$OMK %in% c(0, 1, 2, 3, 4, 5, 9, 20, "-"))] <- 0
  
  return(innvkat)
}

#' LANDBAKvask
#'
#' @param landbak 
#' @param filbesk 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
#' @param regexp 
LANDBAKvask <- function(landbak, filbesk = data.frame(), batchdate = SettKHBatchDate(), globs = SettGlobs(), regexp = FALSE) {
  is_kh_debug()
  
  if (nrow(filbesk) == 0) {
    landbak <- setNames(as.data.frame(landbak, stringsAsFactors = FALSE), c("LANDBAK"))
    landbak$KBOMK <- landbak[, 1]
  } else {
    landbak$KBOMK <- KBomkod(landbak$ORG, type = "LANDBAK", filbesk = filbesk, batchdate = batchdate, globs = globs)
  }
  landbak$OK <- 1
  landbak$OMK <- landbak$KBOMK
  if (regexp == TRUE) {
    landbak$OMK <- sub("^ *(Europa.*) *$", "1", landbak$OMK, ignore.case = TRUE)
    landbak$OMK <- sub("^ *(Afrika) *$", "2", landbak$OMK, ignore.case = TRUE)
    landbak$OMK <- sub("^ *(Asia.*) *$", "3", landbak$OMK, ignore.case = TRUE)
    landbak$OMK <- sub("^ *(Nord[ -]{1,3}Amerika) *$", "4", landbak$OMK, ignore.case = TRUE)
    landbak$OMK <- sub("^ *(Sør.*Amerika) *$", "5", landbak$OMK, ignore.case = TRUE)
    landbak$OMK <- sub("^ *(Oseania) *$", "6", landbak$OMK, ignore.case = TRUE)
    landbak$OMK <- sub("^ *(Statsløse) *$", "7", landbak$OMK, ignore.case = TRUE)
    landbak$OMK <- sub("^ *(Uoppgitt|Ukjent) *$", "8", landbak$OMK, ignore.case = TRUE)
    landbak$OMK <- sub("^ *(Andre) *$", "9", landbak$OMK, ignore.case = TRUE)
  }
  landbak$OMK <- sub("^ *(Alle) *$", "0", landbak$OMK, ignore.case = TRUE)
  
  # Ugyldig verdi/ukjent kode
  landbak$OMK[!(landbak$OMK %in% c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 20, "-"))] <- getOption("khfunctions.illegal")
  landbak$OK[!(landbak$OMK %in% c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 20, "-"))] <- 0
  
  return(landbak)
}

#' AARvask
#'
#' @param aar 
#' @param filbesk 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
AARvask <- function(aar, filbesk = data.frame(), batchdate = SettKHBatchDate(), globs = SettGlobs()) {
  is_kh_debug()
  
  if (nrow(filbesk) == 0) {
    aar <- setNames(as.data.frame(aar, stringsAsFactors = FALSE), c("AAR"))
    aar$KBOMK <- aar[, 1]
  } else {
    aar$KBOMK <- KBomkod(aar$ORG, type = "AAR", filbesk = filbesk, batchdate = batchdate, globs = globs)
  }
  aar$OMK <- aar$KBOMK
  aar$OK <- 1
  
  aar$OMK <- sub("^Høsten ", "", aar$OMK)
  aar$OMK <- sub("^(\\d+) *[_-] *(\\d+)$", "\\1_\\2", aar$OMK)
  aar$OMK <- sub("^ *(\\d+) *$", "\\1_\\1", aar$OMK)
  
  # Ugyldig verdi/ukjent kode
  okformat <- grepl("^\\d+_\\d+$|^-$", aar$OMK)
  aar$OMK[!okformat] <- getOption("khfunctions.aar_illegal")
  aar$OK[!okformat] <- 0
  
  # Sett intervall
  aar[, c("LO", "HI")] <- suppressMessages(matrix(as.integer(stringr::str_split_fixed(aar$OMK, "_", 2)), ncol = 2))
  # Ugyldig intervall
  aar$OMK[aar$HI < aar$LO] <- getOption("khfunctions.aar_illegal")
  aar[, c("LO", "HI")] <- suppressMessages(matrix(as.integer(stringr::str_split_fixed(aar$OMK, "_", 2)), ncol = 2))
  return(aar)
}

#' @title excelcols
#' @return default excel headers
excelcols <- function(){
  single <- LETTERS
  double <- sapply(single, paste0, single)
  triple <- sapply(double, paste0, single)
  c(single, double, triple)
}

#   # Diagnostisering og rapportering paa hele filgruppa under ett
#   
#   # Lå opprinnelig før eksterne kolonnenavn ble satt. 
#   
#   if (nrow(Filgruppe) > 0 & diagnose == 1) {
#     # Finn og rapporter duplikater
#     HarDuplikater <- SjekkDuplikater(Filgruppe, batchdate = batchdate, filgruppe = gruppe, versjonert = versjonert, globs = globs)
#     RODBC::sqlQuery(globs$dbh, paste("UPDATE FILGRUPPER SET DUPLIKATER='", HarDuplikater, "' WHERE FILGRUPPE='", gruppe, "'", sep = ""))
#     
#     # Sjekk design
#     FGd <- FinnDesign(Filgruppe, FGP = FGP, globs = globs)
#     
#     # Er ubalansert?
#     subset(FGd$Design, HAR != 1)
#     
#     FGdT <- FGd$Design
#     RODBC::sqlQuery(globs$log, paste("DELETE * FROM DESIGN WHERE FILGRUPPE='", gruppe, "' AND SV='S'", sep = ""))
#     # Legg til resterende kolonner
#     tmp <- RODBC::sqlQuery(globs$log, "SELECT * FROM DESIGN WHERE FILGRUPPE=''")
#     tmp[1:nrow(FGdT), ] <- NA
#     tmp[, names(FGdT)] <- FGdT
#     tmp$FILGRUPPE <- gruppe
#     tmp$BATCH <- batchdate
#     tmp$SV <- "S"
#     RODBC::sqlSave(globs$log, tmp, "DESIGN", rownames = FALSE, append = TRUE)
#     if (versjonert == TRUE) {
#       tmp$SV <- "V"
#       RODBC::sqlSave(globs$log, tmp, "DESIGN", rownames = FALSE, append = TRUE)
#     }
#   }
# }

#' LagFlereFilgrupper (kb)
#'
#' @param filgrupper 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
#' @param printR 
#' @param printCSV 
#' @param printSTATA 
#' @param versjonert 
LagFlereFilgrupper <- function(filgrupper = character(0), batchdate = SettKHBatchDate(), globs = SettGlobs(), printR = TRUE, printSTATA = FALSE, versjonert = FALSE) {
  is_kh_debug()
  
  # SKall rundt LagFilGruppe, lager og lagrer evt til fil
  # Default er aa ta alle grupper, ellers angis oensket batch i filgrupper-argumentet
  if (length(filgrupper) == 0) {
    # filgrupper<-as.matrix(RODBC::sqlQuery(globs$dbh,"SELECT DISTINCT Filgruppe from INNLESING WHERE Bruk=1",as.is=TRUE))
    filgrupper <- as.matrix(RODBC::sqlQuery(globs$dbh, "SELECT DISTINCT Filgruppe from FILGRUPPER", as.is = TRUE))
  }
  cat("BATCH:", batchdate, "\n")
  # HOVEDLOOP
  for (gruppe in filgrupper) {
    FG <- LagFilgruppe(gruppe, batchdate = batchdate, globs = globs, versjonert = versjonert)
  }
}