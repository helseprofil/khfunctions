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