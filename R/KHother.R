# Functions for purposes other than data processing (comparing etc), potentially deprecated

#' SammelignAarganger
#'
#' @param globs 
#' @param aar1 
#' @param aar2 
#' @param modus 
SammelignAarganger <- function(globs = FinnGlobs(), aar1 = globs$KHaargang, aar2 = globs$KHaargang - 1, modus = "KH") {
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
  
  globs <- SettKubedirs(globs, modus = modus)
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
        
        # MÅ BAREBERE NED KOLONNNER OG EVT OMDØPE
        
        utfil <- paste("F:/Prosjekter/Kommunehelsa/PRODUKSJON/VALIDERING/NESSTAR_KUBER/KH2016v2015/", paret$KUBE_NAVN1, "_", paret$VERSJON1, "v", paret$VERSJON2, ".csv", sep = "")
        cat(paste("Skriver ut", utfil, "\n"))
        write.table(KOMP, file = utfil, sep = ";", row.names = FALSE)
      } else {
        cat("!!!!!! ", paret$VERSJON1, "har ulike kolonner og kan ikke matches")
      }
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
  tabs <- unique(c(globs$FriskvikTabs, tabs))
  
  
  tabs1 <- intersect(names(KUBE1), tabs)
  tabs2 <- intersect(names(KUBE2), tabs)
  
  print(tabs1)
  print(tabs2)
  KHglobs$DefDesign$DesignKols
  
  ## Folder to keep the output if not allready there
  currYr <- KHglobs$KHaargang - 1
  nextYr <- KHglobs$KHaargang
  foldName <- paste0("Batch", currYr, "vs", nextYr)
  
  validDir <- file.path(defpath, "VALIDERING/NESSTAR_KUBER", foldName)
  
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
    
    # MÅ BAREBERE NED KOLONNNER OG EVT OMDØPE
    
    fileName <- paste0(KUBENAVN, ".csv")
    utfil <- file.path(validDir, fileName)
    cat(paste("Skriver ut", utfil, "\n"))
    ## write.table(KOMP,file=utfil,sep=";",row.names=FALSE)
    data.table::fwrite(KOMP, file = utfil, sep = ";")
  } else {
    stop("!!!!!! tabellene har ulike kolonner og kan ikke matches")
  }
}

#' FinnKubeT (kb)
#'
#' @param fila 
#' @param batch 
#' @param globs 
FinnKubeT <- function(fila, batch = NA, globs = FinnGlobs()) {
  is_kh_debug()
  
  if (is.na(batch)) {
    filn <- paste(globs$path, "/", globs$KubeDirNy, fila, ".rds", sep = "")
  } else {
    filn <- paste(globs$path, "/", globs$KubeDirDat, "/R/", fila, "_", batch, ".rds", sep = "")
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

#' KonverterRMappe (kb)
#'
#' @param Rmappe 
#' @param Utmappe 
#' @param Format 
#' @param globs 
KonverterRMappe <- function(Rmappe, Utmappe, Format = "csv", globs = FinnFlobs) {
  is_kh_debug()
  
  print(paste(globs$path, Rmappe, sep = "/"))
  setwd(paste(globs$path, Rmappe, sep = "/"))
  filer <- list.files()
  for (fil in filer[grepl("\\.rds$", filer)]) {
    filn <- gsub("(.*)\\.rds$", "\\1", fil)
    TABELL <- readRDS(fil)
    cat("Eksporterer ", filn, "\n")
    if (tolower(Format) == "csv") {
      filen <- paste(globs$path, "/", Utmappe, filn, ".csv", sep = "")
      write.table(TABELL, file = filen, sep = ";", row.names = FALSE)
      cat("CSV til", filen, "\n")
    } else if (tolower(Format) == "stata") {
      foreign::write.dta(TABELL, file = paste(globs$path, "/", Utmappe, filn, ".dta", sep = ""))
    }
  }
}

#' KonverterKUBER (kb)
#'
#' @param Format 
#' @param versjonert 
#' @param globs 
KonverterKUBER <- function(Format = "CSV", versjonert = FALSE, globs = FinnGlobs()) {
  is_kh_debug()
  
  # Rmappe<-paste(globs$path,"/",globs$KubeDirNy,sep="")
  Rmappe <- globs$KubeDirNy
  if (versjonert == TRUE) {
    Rmappe <- paste0(globs$KubeDirDat, "/R/")
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
  
  # Rmappe<-paste(globs$path,"/",globs$KubeDirNy,sep="")
  Rmappe <- globs$StablaDirNy
  if (versjonert == TRUE) {
    Rmappe <- globs$StablaDirDat
  }
  Utmappe <- gsub("(.*/)R(/.*)", paste("\\1", Format, "\\2", sep = ""), Rmappe)
  KonverterRMappe(Rmappe = Rmappe, Utmappe = Utmappe, Format = Format, globs = globs)
}

#' KH2014v2015 (kb)
#'
#' @param kube 
#' @param batchdate 
#' @param globs 
#' @param echo 
KH2014v2015 <- function(kube, batchdate = SettKHBatchDate(), globs = FinnGlobs(), echo = FALSE) {
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
  if (echo == TRUE) {
    print("F2014:")
    print(F2014)
    print("F2015:")
    print(F2015)
  }
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
    try(KH2014v2015(kube, globs = globs, echo = FALSE))
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

#' FinnDatertKube (kb)
#'
#' @param KUBEid 
#' @param batch 
#' @param silent 
#' @param hist 
FinnDatertKube <- function(KUBEid, batch = NA, silent = FALSE, hist = 0) {
  is_kh_debug()
  
  globs <- FinnGlobs()
  path <- paste(globs$path, "/", globs$KubeDirDat, "/R/", sep = "")
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
    cat("Kan ikke sammenligne KUBER når følgende kolonner ikker er i begge:", tabdiff, "\n")
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

#' CompNyOgKlarAlle (kb)
#'
#' @param globs 
CompNyOgKlarAlle <- function(globs = FinnGlobs()) {
  is_kh_debug()
  
  batchdate <- SettKHBatchDate()
  KUBER <- sqlQuery(globs$dbh, "SELECT KUBE_NAVN FROM KUBER", as.is = TRUE)
  utfil <- paste(globs$path, "/", globs$KubeDir, "/LOGG/CompNyOgKlar_", batchdate, ".txt", sep = "")
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