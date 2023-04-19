## All functions only used in LagKUBE

#' SettFilInfoKUBE (kb)
#' 
#' Read ACCESS, extracts information needed in LagKUBE
#'
#' @return
#' KUBEdscr: 
#' - ACCESS::KUBER, KUBENAVN = KUBEid
#' TNPdscr: 
#' - ACCESS::TNP_PROD, TNP_NAVN = KUBEdscr$TNP
#' filer: 
#' - TELLERFIL (+ NEVNERFIL) from TNPdscr
#' PredFilter: 
#' - uses SettPredFilter(KUBEdscr$REFVERDI)
#' STNPdscr: 
#' - ACCESS::TNP_PROD, TNP_NAVN = TNPdscr::STANDARDTNFIL
#' FGPs: 
#' - ACCESS::FILGRUPPE, for all files in filer
#' FilDesL: 
#' - Design of files in filer
#' tmpBUFFER
SettFilInfoKUBE <- function(KUBEid, batchdate = SettKHBatchDate(), versjonert = FALSE, globs = FinnGlobs()) {
  is_kh_debug()
  
  datef <- format(strptime(batchdate, "%Y-%m-%d-%H-%M"), "#%Y-%m-%d#")
  KUBEdscr <- as.list(sqlQuery(globs$dbh, paste("SELECT * FROM KUBER WHERE KUBE_NAVN='", KUBEid, "' AND VERSJONFRA<=", datef, " AND VERSJONTIL>", datef, sep = ""), as.is = TRUE))
  if ((is.na(KUBEdscr$TNP) | KUBEdscr$TNP == "")) {
    ok <- 0
    err <- "Feltet TNP ikke satt!"
  } else {
    TNPdscr <- sqlQuery(globs$dbh, paste("SELECT * FROM TNP_PROD WHERE TNP_NAVN='", KUBEdscr$TNP, "' AND VERSJONFRA<=", datef, " AND VERSJONTIL>", datef, sep = ""), as.is = TRUE)
  }
  
  filer <- character(0)
  if ((is.na(TNPdscr$TELLERFIL) | TNPdscr$TELLERFIL == "")) {
    ok <- 0
    err <- "Feltet TELLERFIL ikke satt!"
  } else {
    filer["T"] <- TNPdscr$TELLERFIL
  }
  if (!(is.na(TNPdscr$NEVNERFIL) | TNPdscr$NEVNERFIL == "")) {
    filer["N"] <- TNPdscr$NEVNERFIL
  }
  
  # Evt ekstrafiler med info for standardisering
  if (KUBEdscr$REFVERDI_VP == "P") {
    if (!(is.na(TNPdscr$PREDNEVNERFIL) | TNPdscr$PREDNEVNERFIL == "")) {
      filer["PN"] <- gsub("^(.*):(.*)", "\\1", TNPdscr$PREDNEVNERFIL)
    } else if (!is.na(TNPdscr$NEVNERFIL)) {
      filer["PN"] <- TNPdscr$NEVNERFIL
    } else {
      filer["PN"] <- TNPdscr$TELLERFIL
    }
    if (!(is.na(TNPdscr$STANDARDTNFIL) | TNPdscr$STANDARDTNFIL == "")) {
      STNPdscr <- sqlQuery(globs$dbh, paste("SELECT * FROM TNP_PROD WHERE TNP_NAVN='", TNPdscr$STANDARDTNFIL, "' AND VERSJONFRA<=", datef, " AND VERSJONTIL>", datef, sep = ""), as.is = TRUE)
      if ((is.na(STNPdscr$TELLERFIL) | STNPdscr$TELLERFIL == "")) {
        ok <- 0
        err <- "Feltet TELLERFIL ikke satt!"
      } else {
        filer["ST"] <- STNPdscr$TELLERFIL
      }
      if (!(is.na(STNPdscr$NEVNERFIL) | STNPdscr$NEVNERFIL == "")) {
        filer["SN"] <- STNPdscr$NEVNERFIL
      } else {
        filer["SN"] <- STNPdscr$TELLERFIL
      }
    } else {
      STNPdscr <- TNPdscr
      filer["ST"] <- filer["T"]
      if (!is.na(filer["N"])) {
        filer["SN"] <- filer["N"]
      }
    }
  } else {
    STNPdscr <- list()
  }
  # Denne er ikke så veldig robust for feilspesifkasjon, men den brukes ikke annet til de enkleste tilfellene
  PredFilter <- SettPredFilter(KUBEdscr$REFVERDI, globs = globs)
  
  # Sett Tab-filter
  # For å redusere ressursbruk er det viktig at lange lister med unødvendige ETAB blir barert bort tidlig
  TabConds <- character(0)
  TabFSubTT <- ""
  for (tab in names(KUBEdscr)[grepl("^TAB\\d+$", names(KUBEdscr))]) {
    if (!(is.na(KUBEdscr[[tab]]) || KUBEdscr[[tab]] == "")) {
      tablist <- KUBEdscr[[tab]]
      tab0 <- paste(tab, "_0", sep = "")
      if (!(is.null(KUBEdscr[[tab0]]) || is.na(KUBEdscr[[tab0]]) || KUBEdscr[[tab0]] == "")) {
        tablist <- KUBEdscr[[tab0]]
      }
      minus <- grepl("^-\\[", tablist)
      tablist <- gsub("^-\\[(.*)\\]$", "\\1", tablist)
      tablist <- paste("\"", gsub(",", "\",\"", tablist), "\"", sep = "")
      tabcond <- paste("(", tab, " %in% c(", tablist, "))", sep = "")
      if (minus) {
        tabcond <- paste("!", tabcond, sep = "")
      }
      TabConds <- c(TabConds, tabcond)
    }
    TabFSubTT <- paste(TabConds, collapse = " & ")
  }
  
  FGPs <- list()
  FilDesL <- list()
  tmpBUFFER <- character(0)
  for (fil in unique(filer)) {
    TabFSub <- ifelse(fil == filer["T"], TabFSubTT, "")
    FILinfo <- KlargjorFil(fil, TabFSub = TabFSub, KUBEid = KUBEid, versjonert = versjonert, FILbatch = NA, batchdate = batchdate, globs = globs)
    FGPs[[fil]] <- FILinfo$FGP
    FilDesL[[fil]] <- FILinfo$FILd
    
    if (FILinfo$TilBuffer == 1) {
      tmpBUFFER <- c(tmpBUFFER, fil)
    }
    # FilDesL[[fil]]<-FinnDesign(FinnFilT(fil,batch=batchdate,globs=globs),FGP=FGPs[[fil]],globs=globs)
  }
  return(list(KUBEdscr = KUBEdscr, TNPdscr = TNPdscr, filer = filer, PredFilter = PredFilter, STNPdscr = STNPdscr, FGPs = FGPs, FilDesL = FilDesL, tmpBUFFER = tmpBUFFER))
}

#' SettPredFilter (kb)
#'
#' @param refvstr 
#' @param FGP 
#' @param globs 
#'
#' @return
#' @export
#'
#' @examples
SettPredFilter <- function(refvstr, FGP = list(amin = 0, amax = 120), globs = FinnGlobs()) {
  
  # Bør nok konsolidere SettPredFilter og SettNaboAnoSpec, bør være greit å gjøre dette
  
  is_kh_debug()
  PredFilter <- list()
  Pcols <- character(0)

  # D-develop
  D_develop_predtype <- "IND"
  if (grepl("AAR", refvstr)) {
    D_develop_predtype <- "DIR"
  }
  
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
        } else if (grepl(paste("(^|\\&) *", delN, "l{0,1} *== *'*(.*?)'* *($|\\&)", sep = ""), refvstr)) {
          intval1 <- as.integer(gsub(paste("(^|.*\\&) *", delN, "l{0,1} *== *'*(.*?)'* *($|\\&.*)", sep = ""), "\\2", refvstr))
          intval <- c(intval1, intval1)
          # Gammelt: kunne ha f.eks. AAR='2012_2014'. Dette blir for dillete mot annen bruk, må da ha "AARl='2012' & AARh='2014'"
          # intval<-as.integer(unlist(str_split(gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),"\\2",refvstr),"_")))
          # if (length(intval)==1){intval<-c(intval,intval)}
          
          # Gammelt, feil?
          # val<-gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),"\\2",refvstr)
          # refvstr<-gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),
          #          paste("\\1", paste(delN,"l",sep=""),"==\\2 &"," \\1 ",paste(delN,"l",sep=""),"==\\2"," \\3",sep=""),refvstr)
          # refvstr<-gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),
          #              paste("\\1",paste(delN,"l",sep=""),"==",intval[1]," & ",paste(delN,"h",sep=""),"==",intval[2],"\\3",sep=""),refvstr)
          
          # Litt shaky her. For AAR kan man ikke sette 'AARl=y & AARh=y' fordi det vil krasje med AARs intervall ved snitt
          # Derfor bare 'AARl=y'
          refvstr <- gsub(paste(delN, "=", sep = ""), paste(delN, "l=", sep = ""), refvstr)
          Pcols <- c(Pcols, globs$DefDesign$DelKolsF[[del]])
          PredFilter[[del]] <- eval(parse(text = paste("data.frame(", delN, "l=", intval[1], ",", delN, "h=", intval[2], ",stringsAsFactors=FALSE)", sep = "")))
        }
      }
    }
  }
  return(list(Design = PredFilter, PfiltStr = refvstr, Pkols = Pcols, D_develop_predtype = D_develop_predtype))
}

KlargjorFil <- function(FilVers, TabFSub = "", rolle = "", KUBEid = "", versjonert = FALSE, FILbatch = NA, batchdate = SettKHBatchDate(), GeoHarmDefault = 1, globs = FinnGlobs()) {
  is_kh_debug()
  TilBuffer <- 0
  if (!exists("BUFFER")) {
    .GlobalEnv$BUFFER <- list()
  }
  datef <- format(strptime(batchdate, "%Y-%m-%d-%H-%M"), "#%Y-%m-%d#")
  
  FilterDscr <- as.list(sqlQuery(globs$dbh, paste("SELECT * FROM FILFILTRE WHERE FILVERSJON='", FilVers, "' AND VERSJONFRA<=", datef, " AND VERSJONTIL>", datef, sep = ""), as.is = TRUE))
  
  # Har oppsatt filter
  if (length(FilterDscr$FILVERSJON) > 0) {
    FGP <- FinnFilgruppeParametre(FilterDscr$ORGFIL, batchdate = batchdate, globs = globs)
    if (is.null(BUFFER[[FilVers]])) {
      if (!is.na(FilterDscr$SUBSET)) {
        if (FilterDscr$SUBSET != "") {
          if (TabFSub != "") {
            TabFSub <- paste(TabFSub, FilterDscr$SUBSET, sep = " & ")
          } else {
            TabFSub <- FilterDscr$SUBSET
          }
        }
      }
      
      FILn <- FinnFil(FilterDscr$ORGFIL, batch = FILbatch, versjonert = versjonert)
      FIL <- FILn$FT
      sqlQuery(globs$log, paste("INSERT INTO KUBEBATCH (KUBEBATCH,FILBATCH) SELECT '", KUBEid, "_", batchdate, "','", FilterDscr$ORGFIL, "_", FILn$batch, "'", sep = ""))
      if (TabFSub != "") {
        # print("ASKJDLKJASLDKJL  TabFSub")
        cat("Filtrer med tab-filter, før er dim(FIL)", dim(FIL))
        FIL <- eval(parse(text = paste("subset(FIL,", TabFSub, ")", sep = "")))
        cat(" og etter", dim(FIL), "\n")
      }
      
      orgkols <- copy(names(FIL))
      if (grepl("\\S", FilterDscr$KOLLAPSdeler)) {
        cat("Før aggregering er dim(FIL)", dim(FIL))
        tabkols <- FinnTabKols(names(FIL))
        setkeyv(FIL, tabkols)
        kolldel <- unlist(str_split(FilterDscr$KOLLAPSdeler, ","))
        kolldelN <- unlist(globs$DefDesign$DelKolsF[kolldel])
        # FIL[,(kolldelN):=NULL]
        FIL[, (kolldelN) := KHglobs$TotalKoder[kolldel]]
        FIL <- FIL[, lapply(.SD, sum), by = tabkols]
        FIL[, (kolldelN) := KHglobs$TotalKoder[kolldel]]
        FIL <- FIL[, orgkols, with = FALSE]
        cat(" og etter", dim(FIL), "\n")
      }
      
      if (!(is.na(FilterDscr$NYEKOL_KOL_preRAD) | FilterDscr$NYEKOL_KOL_preRAD == "")) {
        FIL <- LeggTilNyeVerdiKolonner(FIL, FilterDscr$NYEKOL_KOL_preRAD, slettInf = TRUE)
      }
      Filter <- SettFilterDesign(FilterDscr, bruk0 = FALSE, FGP = FGP, globs = globs)
      if (length(Filter) > 0) {
        FIL <- OmkodFil(FIL, FinnRedesign(FinnDesign(FIL), list(Parts = Filter)), globs = globs, echo = 1)
      }
      
      if (FilterDscr$GEOHARM == 1) {
        rektiser <- ifelse(FilterDscr$REKTISER == 1, 1, 0)
        FIL <- GeoHarm(FIL, vals = FGP$vals, rektiser = rektiser, batchdate = batchdate, globs = globs)
      }
      if (!(is.na(FilterDscr$NYETAB) | FilterDscr$NYETAB == "")) {
        FIL <- AggregerRader(FIL, FilterDscr$NYETAB, FGP = FGP)
      }
      
      if (grepl("\\S", FilterDscr$NYEKOL_RAD)) {
        FIL <- LeggTilSumFraRader(FIL, FilterDscr$NYEKOL_RAD, FGP = FGP, globs = globs)
      }
      if (!(is.na(FilterDscr$NYEKOL_KOL) | FilterDscr$NYEKOL_KOL == "")) {
        FIL <- LeggTilNyeVerdiKolonner(FIL, FilterDscr$NYEKOL_KOL, slettInf = TRUE)
      }
      
      if (!(is.na(FilterDscr$NYKOLSmerge) | FilterDscr$NYKOLSmerge == "")) {
        NY <- eval(parse(text = FilterDscr$NYKOLSmerge))
        tabK <- intersect(FinnTabKols(names(NY)), FinnTabKols(names(FIL)))
        setkeyv(NY, tabK)
        setkeyv(FIL, tabK)
        FIL <- NY[FIL]
      }
      
      # FF_RSYNT1
      if (!(is.na(FilterDscr$FF_RSYNT1) | FilterDscr$FF_RSYNT1 == "")) {
        FilterDscr$FF_RSYNT1 <- gsub("\\\r", "\\\n", FilterDscr$FF_RSYNT1)
        rsynt1err <- try(eval(parse(text = FilterDscr$FF_RSYNT1)), silent = TRUE)
        print("***AD HOC MANIPULERING\n")
        if (class(rsynt1err) == "try-error") {
          print(rsynt1err)
        }
      }
      
      .GlobalEnv$BUFFER[[FilVers]] <- FIL
      TilBuffer <- 1
    }
    # Bruk ferdig lagret versjon
    else {
      FIL <- copy(BUFFER[[FilVers]])
      print(FilVers)
      # print(BUFFERbatch)
      if (versjonert == TRUE) {
        # sqlQuery(globs$log,paste("INSERT INTO KUBEBATCH (KUBEBATCH,FILBATCH) SELECT '",KUBEid,"_",batchdate,"','",FilterDscr$ORGFIL,"_",BUFFERbatch[[FilVers]],"'",sep=""))
      }
    }
  }
  # Har ikke oppsatt filter, bruk rå
  else {
    FILn <- FinnFil(FilVers, versjonert = versjonert, batch = FILbatch)
    FIL <- FILn$FT
    # sqlQuery(globs$log,paste("INSERT INTO KUBEBATCH (KUBEBATCH,FILBATCH) SELECT '",KUBEid,"_",batchdate,"','",FilVers,"_",FILn$batch,"'",sep=""))
    
    if (TabFSub != "") {
      cat("Filtrer med tab-filter, før er dim(FIL)", dim(FIL))
      FIL <- eval(parse(text = paste("subset(FIL,", TabFSub, ")", sep = "")))
      cat(" og etter", dim(FIL), "\n")
    }
    FGP <- FinnFilgruppeParametre(FilVers, batchdate = batchdate, globs = globs)
    if (GeoHarmDefault == 1) {
      FIL <- GeoHarm(FIL, vals = FGP$vals, rektiser = FALSE, batchdate = batchdate, globs = globs)
    }
    .GlobalEnv$BUFFER[[FilVers]] <- FIL
    # .GlobalEnv$BUFFERbatch[[FilVers]]<-FILn$batch
    TilBuffer <- 1
  }
  
  
  FILd <- FinnDesign(FIL, FGP = FGP, globs = globs)
  gc()
  return(list(FIL = FIL, FGP = FGP, FILd = FILd, TilBuffer = TilBuffer))
}

#' LagTNTabell (kb)
#'
#' @param filer 
#' @param FilDesL 
#' @param FGPs 
#' @param TNPdscr 
#' @param TT 
#' @param NN 
#' @param Design 
#' @param KUBEdscr 
#' @param rapport 
#' @param globs 
LagTNtabell <- function(filer, FilDesL, FGPs, TNPdscr, TT = "T", NN = "N", Design = NULL, KUBEdscr = NULL, rapport = list(), globs = FinnGlobs()) {
  is_kh_debug()
  KUBEd <- list()
  
  # Finn initiellt design før evt lesing av KUBEdscr, dette for å kunne godta tomme angivelser der (gir default fra InitDes)
  if (is.null(Design)) {
    if (is.na(filer[NN])) {
      InitDes <- FilDesL[[filer[TT]]]
    } else {
      FTab <- FinnFellesTab(FilDesL[[filer[TT]]], FilDesL[[filer[NN]]], globs = globs)
      InitDes <- FTab$FDes
      # Må legge til deler som evt bare er i den ene slik at disse blir del av KUBEd
      for (del in setdiff(names(FilDesL[[filer[TT]]]$Part), names(FilDesL[[filer[NN]]]$Part))) {
        InitDes$Part[[del]] <- FilDesL[[filer[TT]]]$Part[[del]]
      }
      for (del in setdiff(names(FilDesL[[filer[NN]]]$Part), names(FilDesL[[filer[TT]]]$Part))) {
        InitDes$Part[[del]] <- FilDesL[[filer[NN]]]$Part[[del]]
      }
    }
  } else {
    InitDes <- Design
  }
  
  KUBEd <- list()
  if (!is.null(KUBEdscr)) {
    KUBEd <- FinnKubeDesignB(KUBEdscr, InitDes, FGP = FGPs[[filer[TT]]], globs = globs)
    TNdes <- list(Part = KUBEd$TMP)
  } else {
    TNdes <- InitDes
  }
  rektangularisert <- 0
  geoharmonisert <- 1 # Bygg ut til at de ikke trenger være geoharm fra KlargjørFil
  
  RDT <- FinnRedesign(FilDesL[[filer[TT]]], TNdes, globs = globs)
  if (nrow(RDT$Udekk) > 0) {
    KHerr("UDEKKA i RDT")
  }
  cat("***Lager TF fra", filer[TT], "\n")
  TF <- OmkodFil(FinnFilT(filer[TT]), RDT, globs = globs, echo = 1)
  
  # TF<-GeoHarm(TF,vals=FGPs[[filer[TT]]]$vals,globs=globs) #Trengs ikke om KUBEd, da tas rektisering i
  if (!is.na(filer[NN])) {
    RDN <- FinnRedesign(FilDesL[[filer[NN]]], TNdes, globs = globs)
    if (nrow(RDN$Udekk) > 0) {
      KHerr("UDEKKA i RDN")
    }
    cat("Lager NF fra", filer[NN], "\n")
    NF <- OmkodFil(FinnFilT(filer[NN]), RDN, globs = globs, echo = 1)
    
    # NF<-GeoHarm(NF,vals=FGPs[[filer[NN]]]$vals,globs=globs)
  }
  
  # Hvis TN er hoved i KUBE, brukes full rektangularisering
  
  if (length(KUBEd) > 0) {
    KubeDRekt <- RektangulariserKUBE(names(TF), KUBEd$TMP, globs = globs)
    
    kast <- unique(setdiff(TF$GEO, KubeDRekt$GEO))
    kastTell <- kast[!grepl("99$", kast)]
    if (length(kastTell) > 0) {
      cat("############################### ADVARSEL!!!!!!!!!!!!!!!!! ####################################################\n")
      cat("GEO ", paste(kastTell, collapse = ","), " kastes ved rektangulariseing!!\n")
      cat("Dessuten kastes ", length(setdiff(kast, kastTell)), "99-koder!!\n")
      print(TF[GEO %in% kast])
      cat("##############################################################################################################\n")
    } else if (length(setdiff(kast, kastTell)) > 0) {
      cat("Kaster ", length(setdiff(kast, kastTell)), "99-koder ved rektangulerisering.\n")
    }
    
    setkeyv(KubeDRekt, intersect(names(KubeDRekt), names(TF)))
    setkeyv(TF, intersect(names(KubeDRekt), names(TF)))
    TF <- TF[KubeDRekt]
    cat("REktangularisering TF, dim(KUBEd)", dim(KubeDRekt), "dim(TF)", dim(TF), "\n")
    TF <- SettMergeNAs(TF, FGPs[[filer[TT]]]$vals)
    
    if (!is.na(filer[NN])) {
      setkeyv(KubeDRekt, intersect(names(KubeDRekt), names(NF)))
      setkeyv(NF, intersect(names(KubeDRekt), names(NF)))
      NF <- NF[KubeDRekt]
      cat("REktangularisering NF dim(NF)", dim(NF), "\n")
      NF <- SettMergeNAs(NF, FGPs[[filer[NN]]]$vals)
      setkeyv(TF, intersect(names(TF), names(NF)))
      setkeyv(NF, intersect(names(TF), names(NF)))
      TNF <- TF[NF]
    } else {
      TNF <- TF
    }
    
    rektangularisert <- 1
    cat("--TNF ferdig merget med KUBEd, dim(TNF)", dim(TNF), "\n")
  } else if (!is.na(filer[NN])) {
    # DEVELOP HER 20160122
    # kolsT<-unlist(globs$DefDesign$DelKolsF[FilDesL[[filer[TT]]]$OmkDeler])
    # kolsN<-unlist(globs$DefDesign$DelKolsF[FilDesL[[filer[NN]]]$OmkDeler])
    kolsT <- FilDesL[[filer[TT]]]$KolNavn
    kolsN <- FilDesL[[filer[NN]]]$KolNavn
    kols <- intersect(kolsT, kolsN)
    # print(names(FGPs[[filer[TT]]]))
    # kols<-intersect(FinnTabKols(names(filer[TT])),FinnTabKols(names(filer[NN])))
    # print(FilDesL[[filer[TT]]])
    setkeyv(TF, kols)
    setkeyv(NF, kols)
    # print(kols)
    
    # Når KubeD=NULL er det ikke nødvendig å fange implisitt 0 i teller selv om nevner finnes,
    # derfor bare join TF->NF
    cat("TNF merges TNF<-NF[TF]\n")
    
    TNF <- NF[TF]
    TNF <- SettMergeNAs(TNF, c(FGPs[[filer[TT]]]$vals, FGPs[[filer[NN]]]$vals))
    cat("--TNF ferdig merget TNF<-NF[TF] gir dim(TF)", dim(TF), ", dim(NF)", dim(NF), ", og dim(TNF)", dim(TNF), "\n")
  } else {
    TNF <- TF
    cat("--TNF ferdig, har ikke nevner, så TNF<-TF\n")
  }
  
  # Evt prossesering av nye kolonner etter merge/design
  if (!(is.na(TNPdscr$NYEKOL_RAD) | TNPdscr$NYEKOL_RAD == "")) {
    FGPtnf <- FGPs[[filer[TT]]]
    FGPtnf$vals <- c(FGPs[[filer[TT]]]$vals, FGPs[[filer[NN]]]$vals)
    TNF <- LeggTilSumFraRader(TNF, TNPdscr$NYEKOL_RAD, FGP = FGPs[[filer[TT]]], globs = globs)
  }
  tabkosl <- FinnTabKols(names(TNF))
  
  if (!(is.na(TNPdscr$NYEKOL_KOL) | TNPdscr$NYEKOL_KOL == "")) {
    TNF <- LeggTilNyeVerdiKolonner(TNF, TNPdscr$NYEKOL_KOL, slettInf = TRUE)
  }
  
  dimorg <- dim(TNF)
  TNF <- FiltrerTab(TNF, KUBEd$MAIN, globs = globs)
  if (!identical(dimorg, dim(TNF))) {
    cat("Siste filtrering av TNF, hadde dim(TNF)", dimorg, "fik dim(TNF)", dim(TNF), "\n")
  }
  
  # Siste felles trinn for alle
  # SETT TELLER OG NEVNER navn
  TNnames <- names(TNF)
  TNnames <- gsub(paste("^", TNPdscr$TELLERKOL, "(\\.f|\\.a|)$", sep = ""), "TELLER\\1", TNnames)
  TNnames <- gsub(paste("^", TNPdscr$NEVNERKOL, "(\\.f|\\.a|)$", sep = ""), "NEVNER\\1", TNnames)
  # NEVNERKOL=='-' gir TELLER->MALTALL ??
  setnames(TNF, names(TNF), TNnames)
  
  cat("---Ferdig i LagTNtabell\n")
  
  return(list(TNF = TNF, KUBEd = KUBEd))
}

#' RektangulariserKUBE (kb)
#' 
#' Used in LagTNTabell 
#' 
#' Can be more efficient with collapse package?
#'
#' @param orgnames 
#' @param KubeD 
#' @param vals 
#' @param batchdate 
#' @param globs 
#' @param GEOstdAAR
RektangulariserKUBE <- function(orgnames, KubeD, vals = list(), batchdate = SettKHBatchDate(), globs = FinnGlobs(), GEOstdAAR = globs$KHaargang) {
  is_kh_debug()
  delDFstr <- character(0)
  delkolsA <- character(0)
  for (del in names(KubeD)) {
    delkols <- globs$DefDesign$DelKols[[del]]
    if (all(delkols %in% orgnames)) {
      delkolsA <- c(delkolsA, delkols)
      delDFstr <- c(delDFstr, paste("as.data.frame(KubeD[[\"", del, "\"]])", sep = ""))
    }
  }
  delerliste <- paste(delDFstr, collapse = ",")
  DELER <- data.table(eval(parse(text = paste("expand.grid.df(", delerliste, ")", sep = ""))))
  DELER <- DELER[, delkolsA, with = FALSE]
  REKT <- data.table()
  # Switch for TYP=="O" ??
  for (Gn in KubeD[["Gn"]][["GEOniv"]]) {
    GEOK <- subset(globs$GeoKoder, FRA <= GEOstdAAR & TIL > GEOstdAAR & GEOniv == Gn)
    
    # FYLKE
    subfylke <- which(GEOK$GEOniv %in% c("G", "S", "K", "F", "B"))
    GEOK$FYLKE <- NA
    GEOK$FYLKE[subfylke] <- substr(GEOK$GEO[subfylke], 1, 2)
    GEOK$FYLKE[GEOK$GEOniv %in% c("H", "L")] <- "00"
    DELERg <- subset(DELER, GEOniv == Gn)
    REKT <- rbind(data.table(expand.grid.df(data.frame(DELERg), data.frame(GEO = GEOK$GEO, FYLKE = GEOK$FYLKE))), REKT)
  }
  return(REKT)
}

#' FiltrerTab (kb)
#'
#' @param FT 
#' @param KubeD 
#' @param globs 
FiltrerTab <- function(FT, KubeD, globs = FinnGlobs()) {
  is_kh_debug()
  
  orgkols <- names(FT)
  for (del in names(KubeD)) {
    tKOLS <- globs$DefDesign$DelKols[[del]]
    if (all(tKOLS %in% names(FT))) {
      KubeD[[del]] <- KubeD[[del]][, tKOLS, with = FALSE] # Burde være unødvendig, men noen ganger har HAR-kolonner blitt med
      setkeyv(FT, tKOLS)
      setkeyv(KubeD[[del]], tKOLS)
      FT <- FT[KubeD[[del]], nomatch = 0]
    }
  }
  return(FT)
}

#' FinnKubeDesignB (kb)
#'
#' @param KUBEdscr 
#' @param ORGd 
#' @param FGP 
#' @param globs 
FinnKubeDesignB <- function(KUBEdscr, ORGd, FGP = list(amin = 0, amax = 120), globs = FinnGlobs()) {
  is_kh_debug()
  
  KubeD <- list(
    TMP = FinnKubeDesign(KUBEdscr, ORGd, bruk0 = TRUE, FGP = FGP, globs = globs),
    MAIN = FinnKubeDesign(KUBEdscr, ORGd, bruk0 = FALSE, FGP = FGP, globs = globs)
  )
}

#' FinnKubeDesign (kb)
#'
#' @param KUBEdscr 
#' @param ORGd 
#' @param bruk0 
#' @param FGP 
#' @param globs 
FinnKubeDesign <- function(KUBEdscr, ORGd, bruk0 = TRUE, FGP = list(amin = 0, amax = 120), globs = FinnGlobs()) {
  is_kh_debug()
  
  Deler <- list()
  for (del in names(globs$DefDesign$DelKolN)) {
    # for (del in names(unlist(globs$DefDesign$DelKolN[ORGd$OmkDeler]))){
    # if (del %in% names(ORGd$Part) | grepl("^T\\d$",del)){
    if (del %in% names(ORGd$Part)) {
      # Les liste
      koldel <- globs$DefDesign$DelKolN[del]
      koldel0 <- paste(koldel, "_0", sep = "")
      
      if (bruk0 == TRUE && !is.null(KUBEdscr[[koldel0]]) && !is.na(KUBEdscr[[koldel0]]) && KUBEdscr[[koldel0]] != "") {
        delListStr <- KUBEdscr[[koldel0]]
      } else {
        delListStr <- KUBEdscr[[koldel]]
      }
      if (!(is.null(delListStr) || is.na(delListStr) || delListStr == "")) {
        minus <- grepl("^-\\[", delListStr)
        delListStr <- gsub("^-\\[(.*)\\]$", "\\1", delListStr)
        delListA <- unlist(str_split(delListStr, ","))
        if (globs$DefDesign$DelType[del] == "INT") {
          if (del == "A") {
            delListA <- gsub("ALLE", paste(FGP$amin, "_", FGP$amax, sep = ""), delListA)
            delListA <- gsub("^_(\\d+)", paste(FGP$amin, "_\\1", sep = ""), delListA)
            delListA <- gsub("(\\d+)_$", paste("\\1_", FGP$amax, sep = ""), delListA)
          }
          delListA <- gsub("^(\\d+)$", "\\1_\\1", delListA)
          delListA <- as.data.table(matrix(as.integer(str_split_fixed(delListA, "_", 2)), ncol = 2))
        } else if (globs$DefDesign$DelFormat[del] == "integer") {
          delListA <- as.integer(delListA)
        } else if (globs$DefDesign$DelFormat[del] == "numeric") {
          delListA <- as.numeric(delListA)
        }
        listDT <- setnames(as.data.table(delListA), globs$DefDesign$DelKols[[del]])
        if (minus == TRUE) {
          setkeyv(listDT, key(ORGd$Part[[del]]))
          Deler[[del]] <- ORGd$Part[[del]][!listDT, ]
        } else {
          Deler[[del]] <- listDT
        }
      } else if (globs$DefDesign$DelType[del] == "INT") {
        delN <- globs$DefDesign$DelKolN[del]
        start <- KUBEdscr[[paste(delN, "_START", sep = "")]]
        stopp <- KUBEdscr[[paste(delN, "_STOP", sep = "")]]
        if (!(is.null(start) | is.null(stopp))) {
          if (stopp >= start) {
            Deler[[del]] <- subset(ORGd$Part[[del]], eval(parse(text = paste(delN, "l>=", start, " & ", delN, "h<=", stopp, sep = ""))))
          } else {
            cat("FEIL!!!!!!!! kan ikke ha start ", start, "> stopp ", stopp, "\n")
          }
        } else {
          Deler[[del]] <- ORGd$Part[[del]]
        }
      } else {
        Deler[[del]] <- ORGd$Part[[del]]
      }
    }
  }
  return(Deler)
}

#' FinnFellesTab (kb)
#'
#' @param DF1 
#' @param DF2 
#' @param globs 
#' @param echo 
FinnFellesTab <- function(DF1, DF2, globs = FinnGlobs(), echo = 0) {
  # Diff<-union(setdiff(names(DF1$Part),names(DF2$Part)),setdiff(names(DF2$Part),names(DF1$Part)))
  is_kh_debug()
  
  cat("Starter i FinnFellesTab.")
  FTabs <- list()
  for (del in intersect(names(DF1$Part), names(DF2$Part))) {
    FTabs[[del]] <- unique(rbind(DF1$Part[[del]], DF2$Part[[del]]))
  }
  RD1 <- FinnRedesign(DF1, list(Parts = FTabs))
  RD2 <- FinnRedesign(DF2, list(Parts = FTabs))
  omktabs <- names(RD1$FULL)[grepl("_omk$", names(RD1$FULL))]
  setkeyv(RD1$FULL, omktabs)
  setkeyv(RD2$FULL, omktabs)
  Dekk1 <- unique(RD1$FULL[, omktabs, with = FALSE])
  Dekk2 <- unique(RD2$FULL[, omktabs, with = FALSE])
  Dekk12 <- Dekk1[Dekk2, nomatch = 0]
  setnames(Dekk12, names(Dekk12), gsub("_omk$", "", names(Dekk12)))
  FDes <- FinnDesign(Dekk12)
  cat(" Ferdig i FinnFellesTab\n")
  gc()
  return(list(Dekk = Dekk12, FDes = FDes))
}

#' SettFilterDesign (kb)
#'
#' @param KUBEdscr 
#' @param OrgParts 
#' @param bruk0 
#' @param FGP 
#' @param globs 
SettFilterDesign <- function(KUBEdscr, OrgParts = list(), bruk0 = TRUE, FGP = list(amin = 0, amax = 120), globs = FinnGlobs()) {
  is_kh_debug()
  
  Deler <- list()
  for (del in names(globs$DefDesign$DelKolN)) {
    # for (del in names(unlist(globs$DefDesign$DelKolN[ORGd$OmkDeler]))){
    # if (del %in% names(ORGd$Part) | grepl("^T\\d$",del)){
    # Les liste
    koldel <- globs$DefDesign$DelKolN[del]
    koldel0 <- paste(koldel, "_0", sep = "")
    
    if (bruk0 == TRUE && !is.null(KUBEdscr[[koldel0]]) && !is.na(KUBEdscr[[koldel0]]) && KUBEdscr[[koldel0]] != "") {
      delListStr <- KUBEdscr[[koldel0]]
    } else {
      delListStr <- KUBEdscr[[koldel]]
    }
    if (!(is.null(delListStr) || is.na(delListStr) || delListStr == "")) {
      delListStr <- gsub("^ *| *$", "", delListStr)
      minus <- grepl("^-\\[", delListStr)
      delListStr <- gsub("^-\\[(.*)\\]$", "\\1", delListStr)
      delListA <- unlist(str_split(delListStr, ","))
      if (globs$DefDesign$DelType[del] == "INT") {
        if (del == "A") {
          delListA <- gsub("ALLE", paste(FGP$amin, "_", FGP$amax, sep = ""), delListA)
          delListA <- gsub("^_(\\d+)", paste(FGP$amin, "_\\1", sep = ""), delListA)
          delListA <- gsub("(\\d+)_$", paste("\\1_", FGP$amax, sep = ""), delListA)
        }
        delListA <- gsub("^(\\d+)$", "\\1_\\1", delListA)
        delListA <- as.data.table(matrix(as.integer(str_split_fixed(delListA, "_", 2)), ncol = 2))
      } else if (globs$DefDesign$DelFormat[del] == "integer") {
        delListA <- as.integer(delListA)
      } else if (globs$DefDesign$DelFormat[del] == "numeric") {
        delListA <- as.numeric(delListA)
      }
      listDT <- setnames(as.data.table(delListA), globs$DefDesign$DelKols[[del]])
      if (minus == TRUE) {
        if (!is.null(OrgParts[[del]])) {
          setkeyv(listDT, names(listDT))
          setkeyv(OrgParts[[del]], names(listDT))
          Deler[[del]] <- OrgParts[[del]][!listDT, ]
        } else {
          print("**********************KAN IKKE BRUKE -[liste] i SettFilterDesign når ikke OrgParts")
        }
      } else {
        Deler[[del]] <- listDT
      }
    } else if (globs$DefDesign$DelType[del] == "INT") {
      start <- KUBEdscr[[paste(koldel, "_START", sep = "")]]
      stopp <- KUBEdscr[[paste(koldel, "_STOP", sep = "")]]
      if (!(is.null(start) | is.null(stopp))) {
        if (!(is.na(start) | is.na(stopp))) {
          if (!(start == "" | stopp == "")) {
            if (stopp >= start) {
              if (!is.null(OrgParts[[del]])) {
                Deler[[del]] <- subset(OrgParts[[del]], eval(parse(text = paste(koldel, "l>=", start, " & ", koldel, "h<=", stopp, sep = ""))))
              } else {
                # Deler[[del]]<-setNames(as.data.frame(cbind(start:stopp,start:stopp)),paste(koldel,c("l","h"),sep=""))
              }
            } else {
              cat("FEIL!!!!!!!! kan ikke ha start ", start, "> stopp ", stopp, "\n")
            }
          }
        }
      }
    }
  }
  return(Deler)
}

#' FinnRedesign (kb)
#'
#' @param DesFRA 
#' @param DesTIL 
#' @param SkalAggregeresOpp 
#' @param ReturnerFullFull 
#' @param globs 
#' @param prios 
#' @param KB 
#' @param IntervallHull 
#' @param AggPri 
#' @param echo 
FinnRedesign <- function(DesFRA, DesTIL, SkalAggregeresOpp = character(), ReturnerFullFull = FALSE, globs = FinnGlobs(), prios = globs$DefDesign, KB = globs$KB, IntervallHull = globs$DefDesign$IntervallHull, AggPri = globs$DefDesign$AggPri, echo = 0) {
  is_kh_debug()
  
  # Merk assymtri mellom DesFRA og DesTIL.
  # For DesTIL brukes bare DesTil$Part og DesTIL$OmkDesign.
  # DesFRA må derfor komme fra FinnDesign med alle egenskaper satt der, mens DesTIL kan være enklere og satt andre steder

  #   #Deler i DesFra som ikke er i DesTil må legegs til i DesTil (full kryss mot Part[del])
  #   #Merk at deler i DesTil som ikke er i DesFra går greit (all omkoding er indirekte "betinget" på disse)
  #   kryssdeler<-names(DesFRA$Part)[!(names(DesFRA$Part) %in% names(DesTIL$Part))]
  #   if (length(kryssdeler)>0){
  #     DesTIL<-ModifiserDesign(DesFRA$Part[kryssdeler],DesTIL,globs=globs)
  #   }
  #   Redesign<-list()
  
  # Sett partiell omkoding
  # For intervaller kalles FinnKodebokINtervaller, elllers hentes fast kodebok som utgnagspunkt
  # Disse kodebøkene (KJONN etc) filtreres til de omkodingene som er aktuelle (bør gjøres her for å begrense kombinatorikk, selv om dette kunne vært utsatt)
  # Dvs omkodinger til en TIL som ikke har alle nødvendige deler i FRA filtreres bort
  # Merk at noen deler i FRA ikke er obligatoriske (slik som KJONN=9 for omkoding til KJONN=0)
  
  # Rydd DesTIL$Design (Kan variere litt mht HAR avhengig av hvor kallet på denne funksjonen er gjort fra. Skal ha 1 har felt)
  if (is.null(DesTIL$Design)) {
    komblist <- paste("as.data.frame(DesTIL$Part[[\"", names(DesTIL$Part), "\"]])", sep = "", collapse = ",")
    ## FULL <- data.table(eval(parse(text = paste("expand.grid.df(", komblist, ")", sep = ""))))
    FULL <- do.call(expand.grid.df, DesTIL$Part)
    data.table::setDT(FULL)
    
    harkols <- names(FULL)[grepl("_HAR$", names(FULL))]
    if (length(harkols) > 0) {
      FULL[, (harkols) := NULL]
    }
  } else {
    FULL <- DesTIL$Design[HAR == 1, ]
    harkols <- names(FULL)[grepl("_HAR$|^HAR$", names(FULL))]
    FULL[, (harkols) := NULL]
  }
  setnames(FULL, names(FULL), paste(names(FULL), "_omk", sep = ""))
  
  TempFile <- file.path(tempdir(), paste0("full", SettKHBatchDate(), ".RDS"))
  saveRDS(FULL, TempFile)
  
  # Need to get the colnames before manipulation
  namesFULL <- names(FULL)
  gc()
  
  betKols <- setdiff(names(DesFRA$SKombs$bet), "HAR")
  if (length(betKols) > 0) {
    FULL <- expand.grid.df(FULL, DesFRA$SKombs$bet[, ..betKols])
    data.table::setDT(FULL)
  }
  for (del in DesFRA$UBeting) {
    if (is.null(DesTIL$Part[[del]])) {
      DesTIL$Part[[del]] <- copy(DesFRA$Part[[del]])
    }
  }
  
  gc()
  Parts <- list()
  for (del in names(KB)) {
    # if (del %in% names(DesTIL$Part)){
    
    if (del %in% names(DesTIL$Part) & del %in% names(DesFRA$Part)) {
      DesTIL$Part[[del]] <- copy(as.data.table(DesTIL$Part[[del]])) # Får noen rare warnings uten copy, bør debugge dette
      delH <- paste(del, "_HAR", sep = "")
      if (!delH %in% names(DesTIL)) {
        DesTIL$Part[[del]][, (delH) := 1]
      }
      KBD <- KB[[del]]
      kol <- globs$DefDesign$DelKolN[del]
      kolomk <- paste(kol, "_omk", sep = "")
      kols <- globs$DefDesign$DelKols[[del]]
      kolsomk <- paste(kols, "_omk", sep = "")
      
      # Sett 1-1 koding for T1,T2,.. dersom ikke annet gitt
      if (grepl("^T\\d$", del) & nrow(KBD) == 0) {
        tabN <- globs$DefDesign$DelKolN[del]
        tilTabs <- DesTIL$Part[[del]][, tabN, with = FALSE]
        KBD <- setNames(data.frame(tilTabs, tilTabs, 0, 1), c(tabN, paste(tabN, "_omk", sep = ""), paste(del, c("_pri", "_obl"), sep = "")))
        Parts[[del]] <- KBD
      }
      # Behandling av enkle kolonner
      if (globs$DefDesign$DelType[del] == "COL") {
        if (nrow(KBD) > 0) {
          # Filtrer bort TIL-koder i global-KB som ikke er i desTIL
          
          KBD <- KBD[KBD[, kolomk] %in% DesTIL$Part[[del]][[kol]], ]
          omkcols <- c(kolomk, paste(del, "_pri", sep = ""))
          kolsomkpri <- c(kolsomk, paste(del, "_pri", sep = ""))
          KBD <- data.table(KBD, key = omkcols)
          # Sett HAR og Dekk
          eval(parse(text = paste(
            "KBD[,", del, "_HAR:=as.integer(", kol, " %in% DesFRA$Part[[del]][[kol]])]",
            sep = ""
          )))
          eval(parse(text = paste(
            "KBD[,", del, "_Dekk:=as.integer(!any(", del, "_HAR==0 & ", del, "_obl==1)),by=kolsomkpri]",
            sep = ""
          )))
          # Kast omkodinger uten noen deler i FRA, behold de som dekkes helt og delvis
          eval(parse(text = paste(
            "KBD[,Kast:=!any(", del, "_HAR==1),by=kolsomkpri]",
            sep = ""
          )))
          KBD <- subset(KBD, Kast == FALSE)
          KBD$Kast <- NULL
          Parts[[del]] <- KBD
        }
        
        # Behandling av intervaller (to kolonner etc)
      } else if (globs$DefDesign$DelType[del] == "INT") {
        # Global KB kan inneholde (fil)spesifikke koden "ALLE", må erstatte denne med "amin_amax" og lage intervall
        # Merk: dette gjelder typisk bare tilfellene der ukjent alder og evt tilsvarende skal settes inn under "ALLE"
        Imin <- eval(parse(text = paste("min(DesFRA$Part[[del]][,", globs$DefDesign$DelKolN[[del]], "l])", sep = "")))
        Imax <- eval(parse(text = paste("max(DesFRA$Part[[del]][,", globs$DefDesign$DelKolN[[del]], "h])", sep = "")))
        alle <- paste(Imin, "_", Imax, sep = "")
        if (nrow(KBD) > 0) {
          KBD[, kol] <- gsub("^(ALLE)$", alle, KBD[, kol])
          KBD[, kolomk] <- gsub("^(ALLE)$", alle, KBD[, kolomk])
          # KBD[,globs$DefDesign$DelKols[[del]]]<-as.integer(str_split_fixed(KBD[,kol],"_",2))
          KBD[, globs$DefDesign$DelKols[[del]]] <- matrix(as.integer(str_split_fixed(KBD[, kol], "_", 2)), ncol = 2)
          KBD[, paste(globs$DefDesign$DelKols[[del]], "_omk", sep = "")] <- matrix(as.integer(str_split_fixed(KBD[, kolomk], "_", 2)), ncol = 2)
          # Kodebok ferdig mod
          
          # Filtrer KBD mot TIL!!
          # KBD<-KBD[KBD[,kolomk] %in% paste(DesTIL$Part[[del]][,kols,with=FALSE],sep="_"),]
          KBD <- KBD[KBD[, kolomk] %in% apply(DesTIL$Part[[del]][, kols, with = FALSE], 1, paste, collapse = "_"), ]
        }
        # Må fjerne "del_HAR" inn i omkodintervall, fjerner dessuten del_HAR==0 i TIL
        
        delkols <- KHglobs$DefDesign$DelKols[[del]]
        IntFra <- DesFRA$Part[[del]][, delkols, with = FALSE]
        # IntTil<-DesTIL$Part[[del]][DesTIL$Part[[del]][[paste(del,"_HAR",sep="")]]==1,delkols,with=FALSE]
        # Merk: eneste som ikke har del_HAR er udekkede intervaller mellom amin og amax.
        # Videre er disse bare med når TilDes er satt fra FinnDesign(FG), ikke når TilDes er fra Parts
        # Usikker på om det alltid er best å slippe disse gjennom.
        IntTil <- DesTIL$Part[[del]][, delkols, with = FALSE]
        # Fjerner spesialkoder (dvs uoppgitt etc i KB) før intervallomregning
        IntFra <- IntFra[!apply(IntFra[, kols, with = FALSE], 1, paste, collapse = "_") %in% globs$LegKoder[[del]]$KODE]
        IntTil <- IntTil[!apply(IntTil[, kols, with = FALSE], 1, paste, collapse = "_") %in% globs$LegKoder[[del]]$KODE]
        # print("aksdløkaslødkøalsdkøkø")
        # print(IntFra)
        # print(IntTil)
        KBInt <- FinnKodebokIntervaller(as.data.frame(IntFra), as.data.frame(IntTil), deln = del)
        
        KBInt[, paste(del, "_obl", sep = "")] <- 1
        # DEVELOP:   DETTE ER TENMMELIG AD HOC!!!!!!!!!!
        if (del == "A") {
          KBInt[KBInt$ALDERl >= 90, paste(del, "_obl", sep = "")] <- 0
        }
        
        KBInt[, paste(del, "_ok", sep = "")] <- NULL # Denne brukes bare ved filtrering rett fra KBint
        # Legg til spesialkoder igjen
        if (nrow(KBD) > 0) {
          KBD <- rbind(KBInt, KBD[, c(kols, kolsomk, paste(del, c("_pri", "_obl"), sep = ""))])
        } else {
          KBD <- KBInt
        }
        
        # Koble på "del_HAR"
        omkcols <- c(kolomk, paste(del, "_pri", sep = ""))
        KBD <- data.table(KBD, key = kols)
        KBD <- data.table(DesFRA$Part[[del]], key = kols)[KBD]
        har <- paste(del, "_HAR", sep = "")
        eval(parse(text = paste(
          "KBD[is.na(KBD[,", har, "]),", har, ":=0]",
          sep = ""
        )))
        
        KBD <- SettPartDekk(KBD, del = del, IntervallHull = IntervallHull, globs = globs)
        # setnames(KBD,"DEKKok",paste(del,"_Dekk",sep=""))
        
        # Kast omkodinger uten noen deler i FRA, behold de som dekkes helt og delvis
        # USIKKER på om dette er optimalt. Det må ikke kastes for mye for riktig bruk fra FinnFellesTab
        # Egentlig er det jo unødvenig å kaste noe som helst. Dette er mest for rapport/lesing av KBD
        kolsomkpri <- c(kolsomk, paste(del, "_pri", sep = ""))
        eval(parse(text = paste(
          "KBD[,Kast:=!any(", del, "_HAR==1 | ", del, "_obl==0),by=kolsomkpri]",
          sep = ""
        )))
        #         eval(parse(text=paste(
        #           "KBD[,Kast:=!any(",del,"_HAR==1),by=kolsomkpri]",sep=""
        #         )))
        
        KBD <- KBD[Kast == FALSE, ]
        KBD[, Kast := NULL]
        Parts[[del]] <- KBD
      }
    }
  }
  
  if (echo >= 1) {
    cat("Parts:\n")
    print(Parts)
  }
  
  gc()
  SKombs <- list()
  KBs <- list()
  Filters <- list()
  DelStatus <- list()
  
  # Må passe på rekkefølge (Ubeting til slutt), ellers kan det gå galt i FULL
  beting <- intersect(globs$DefDesign$AggPri[length(globs$DefDesign$AggPri):1], c(globs$DefDesign$BetingOmk, globs$DefDesign$BetingF))
  ubeting <- intersect(globs$DefDesign$AggPri[length(globs$DefDesign$AggPri):1], c(globs$DefDesign$UBeting))
  
  for (del in intersect(c(beting, ubeting), names(Parts))) {
    delkols <- globs$DefDesign$DelKols[[del]]
    if (length(DesFRA[["UBeting"]]) > 0) {
      if (del %in% DesFRA[["UBeting"]]) {
        kombn <- "bet"
      } else {
        kombn <- paste("bet", del, sep = "")
      }
      # Koble med DeSFRA
      setkeyv(Parts[[del]], delkols)
      setkeyv(DesFRA$SKombs[[kombn]], delkols)
      betD <- DesFRA$SKombs[[kombn]][Parts[[del]], allow.cartesian = TRUE]
      betD[is.na(HAR), HAR := 0]
      # Må kaste de som ikke har del_Dekk==1 hvis ikke kan de feilaktig
      # få del_Dekk==1 under dersom del er i beting, da vil en annen del i beting få NA og by=betcols går galt!)
      betD <- subset(betD, eval(parse(text = paste(del, "_Dekk==1", sep = ""))))
      # Sett (betinget) dekning
      betcols <- unlist(globs$DefDesign$DelKols[setdiff(DesFRA[["UBeting"]], del)])
      betD <- SettPartDekk(betD, del = del, har = "HAR", IntervallHull = IntervallHull, betcols = betcols, globs = globs)
    } else {
      betcols <- character()
      betD <- data.table(Parts[[del]])
    }
    if (echo >= 1) {
      cat("betD 1:\n", kombn, "\n")
      print(betD)
      print(komblist)
    }

    # Finn beste alternativ
    OmkCols <- names(betD)[grepl("_(omk)$", names(betD))]
    bycols <- c(OmkCols, betcols)
    if (del %in% SkalAggregeresOpp) {
      eval(parse(text = paste("betD[", del, "_Dekk==1,Bruk:=max(", del, "_pri),by=bycols]", sep = "")))
    } else {
      eval(parse(text = paste("betD[", del, "_Dekk==1,Bruk:=min(", del, "_pri),by=bycols]", sep = "")))
    }
    
    prid <- paste(del, "_pri", sep = "")
    KB <- betD[eval(parse(text = paste("Bruk==", prid, " & ", del, "_HAR==1", sep = "")))]
    SKombs[[del]] <- betD
    
    # Sjekk om del kan omkodes helt partielt (fra Part) eller om må betinge (dvs KB)
    
    # Finner om en omk_kode bruker flere versjoner av partiell omkoding (hver versjon fra Part har ulik prid)
    # Om en slik finnes beholdes KB, ellers fjernes overlødig betinging
    maxBet <- KB[, eval(parse(text = paste("list(NOPri=length(unique(", prid, ")))", sep = ""))), by = OmkCols][, max(NOPri)]
    # Utgått se KB<- over
    # KB<-KB[[del]][eval(parse(text=paste(del,"_HAR==1",sep=""))),]
    if (maxBet == 1) {
      # brukcols<-setdiff(names(KB),betcols)
      brukcols <- c(gsub("_omk$", "", OmkCols), OmkCols)
      setkeyv(KB, brukcols)
      KBs[[del]] <- unique(KB[, brukcols, with = FALSE])
      DelStatus[[del]] <- "P"
    } else {
      KB[, (names(KB)[grepl("(_obl|_{0,1}HAR|_Dekk|_pri|Bruk)$", names(KB))]) := NULL]
      KBs[[del]] <- KB
      DelStatus[[del]] <- "B"
    }
    
    if (del == "Y" & DelStatus[[del]] == "B") {
      KHerr("Har DelStatus[[Y]]==B, dette takles per nå ikke i FilOmkod og vil gi feil der!!!")
    }
    
    # Sett dekning i FULL
    # common<-intersect(names(FULL),names(KBs[[del]]))
    common <- intersect(names(FULL), names(KB))
    setkeyv(KB, common)
    setkeyv(FULL, common)
    FULL <- FULL[KB[, common, with = FALSE], nomatch = 0, allow.cartesian = TRUE]
    
    # if (D_develop_predtype=="DIR"){
    #   delkols<-KHglobs$DefDesign$DelKols[[del]]
    # }
    
    # Ignorer KB der det ikke foregår reell omkoding
    if (all(KBs[[del]][, delkols, with = FALSE] == KBs[[del]][, paste(delkols, "_omk", sep = ""), with = FALSE])) {
      Filters[[del]] <- KBs[[del]][, names(KBs[[del]])[!grepl("_omk$", names(KBs[[del]]))], with = FALSE]
      KBs[del] <- NULL
      DelStatus[[del]] <- "F"
    }
  }
  omkkols <- names(FULL)[grepl("_omk$", names(FULL))]
  setkeyv(FULL, omkkols)
  Dekk <- unique(FULL[, omkkols, with = FALSE])
  setnames(Dekk, names(Dekk), gsub("_omk$", "", names(Dekk)))
  
  setkeyv(FULL, namesFULL)
  Udekk <- handle_udekk(FULL, namesFULL, TempFile)
  
  gc()
  return(list(Parts = Parts, SKombs = SKombs, KBs = KBs, Filters = Filters, FULL = FULL, Dekk = Dekk, Udekk = Udekk, DelStatus = DelStatus))
}

#' FinnKodebokIntervaller (kb)
#'
#' @param FRA 
#' @param TIL 
#' @param storst 
#' @param delnavn 
#' @param echo 
FinnKodebokIntervaller <- function(FRA, TIL, storst = TRUE, delnavn = "INT", echo = 0) {
  is_kh_debug()
  
  # I tilfelle input er data.table
  FRA <- as.data.frame(FRA)
  TIL <- as.data.frame(TIL)
  # Bruk Intrevals-klassen
  utcolnavn <- c(names(FRA), paste(names(FRA), "_omk", sep = ""), paste(delnavn, "_pri", sep = ""))
  TILi <- Intervals(TIL, type = "Z")
  FRAi <- Intervals(FRA, type = "Z")
  if (storst == TRUE) {
    # Gir høyest prioritet til å bruke/beholde store intervaller
    sorter <- order(size(FRAi), decreasing = TRUE)
  } else {
    # Gir høyest prioritet til å bruke små intervaller og aggregger opp. Brukes ved aldersstandardisering
    sorter <- order(size(FRAi))
  }
  
  # sorter<-order(size(FRAi))
  FRAi <- FRAi[sorter]
  FRA <- FRA[sorter, ]
  # Finn kandidater, dvs inkluderte "underintrevaller"
  KAND <- interval_included(TILi, FRAi)
  if (class(KAND) == "matrix") { # Irriterende bug(?) i interval når TILi har dim 1 eller KAND er n*m
    # KAND<-list(KAND)
    KAND <- split(KAND, rep(1:ncol(KAND), each = nrow(KAND)))
  }
  if (echo == 1) {
    print(KAND)
  }
  
  # Finn intern overlapp i FRA
  OVLP <- interval_overlap(FRAi, FRAi)
  if (echo == 1) {
    print(OVLP)
  }
  
  # Initier tom kodebok
  KODEBOK0 <- as.data.frame(setNames(replicate(length(utcolnavn), integer(0), simplify = F), utcolnavn))
  KODEBOK <- KODEBOK0
  # Må loope over alle TIL. Kan dette vektoriseres? Kanskje ikke så mye å vinne?
  for (i in 1:nrow(TIL)) {
    result <- list("pri" = 0, "KODEBOK" = KODEBOK0)
    result <- FinnKodebokForEtIntervall(KAND[[i]], FRA, TIL[i, ], OVLP, 0, result, utcolnavn)
    # ???????? ok==1???????  Dette skal vel aldri skje????
    #     if (result$ok==0){
    #       result$KODEBOK<-rbind(result$KODEBOK,as.data.frame(setNames(list(NA_integer_,NA_integer_,TIL[i,1],TIL[i,2],0,1),utcolnavn)))
    #     }
    # print(result$KODEBOK)
    KODEBOK <- rbind(KODEBOK, result$KODEBOK)
  }
  return(KODEBOK)
}

#' FinnKodebokForEtIntervall (kb)
#'
#' @param Find 
#' @param FRA 
#' @param TILint 
#' @param OVLP 
#' @param letn 
#' @param result 
#' @param utcolnavn 
FinnKodebokForEtIntervall <- function(Find, FRA, TILint, OVLP, letn, result, utcolnavn) {
  is_kh_debug()
  
  if (DekkerInt(FRA[Find, ], TILint)) {
    jobb <- Find[Find > letn]
    if (length(jobb) > 0) {
      # Må unngå jobbing på alle esoteriske kombinasjoner ved mye overlap
      if (result$pri < 6) {
        letn <- jobb[1]
        # cat("Let videres med letn",letn,"\n")
        result <- FinnKodebokForEtIntervall(Find[!(Find %in% OVLP[[letn]]) | Find == letn], FRA, TILint, OVLP, letn, result, utcolnavn)
        # LEt videre uten letn
        result <- FinnKodebokForEtIntervall(Find[Find != letn], FRA, TILint, OVLP, letn, result, utcolnavn)
      }
    } else {
      result$KODEBOK <- rbind(result$KODEBOK, setNames(cbind(FRA[Find, ], TILint, result$pri), utcolnavn))
      result$pri <- result$pri + 1
    }
  } else {
    # Sett diff/residual
    if (nrow(FRA[Find, ]) > 0) {
      miss <- setNames(as.data.frame(interval_difference(Intervals(TILint, type = "Z"), Intervals(FRA[Find, ], type = "Z"))), names(FRA))
    } else {
      miss <- setNames(as.data.frame(TILint), names(FRA))
    }
    # Ta bare med dersom residual er ny
    miss <- miss[!apply(miss, 1, paste, collapse = "_") %in% apply(FRA, 1, paste, collapse = "_"), ]
    if (nrow(miss) > 0) {
      FRA <- rbind(FRA, miss)
      Find <- c(Find, nrow(FRA))
      OVLP <- interval_overlap(Intervals(FRA, type = "Z"), Intervals(FRA, type = "Z"))
      result <- FinnKodebokForEtIntervall(Find, FRA, TILint, OVLP, letn, result, utcolnavn)
    }
  }
  return(result)
}

#' handle_udekk (ybk)
#'
#' @param FULL 
#' @param namesFULL 
#' @param TempFile 
handle_udekk <- function(FULL, namesFULL, TempFile){
  is_kh_debug()
  Udekk <- readRDS(TempFile)
  setkeyv(Udekk, namesFULL)
  Udekk <- Udekk[!FULL, allow.cartesian = TRUE]
  setnames(Udekk, namesFULL, gsub("_omk$", "", namesFULL))
  return(Udekk)
}

#' SettPartDekk (kb)
#'
#' @param KB 
#' @param del 
#' @param har 
#' @param betcols 
#' @param globs 
#' @param IntervallHull 
SettPartDekk <- function(KB, del = "", har = paste(del, "_HAR", sep = ""), betcols = character(0), globs = FinnGlobs(), IntervallHull = globs$DefDesign$IntervallHull) {
  is_kh_debug()
  
  OmkPriCols <- names(KB)[grepl("_(omk|pri)$", names(KB))]
  delKolN <- globs$DefDesign$DelKolN[del]
  bycols <- c(OmkPriCols, betcols)
  
  if (del %in% names(IntervallHull)) {
    # haroblcond<-paste(del,"_HAR==1 & ",del,"_obl==1",sep="")
    haroblcond <- paste(har, "==1 | ", del, "_obl==0", sep = "")
    DekkInt <- paste("sum((", haroblcond, ")*(1+", delKolN, "h-", delKolN, "l))", sep = "")
    NTOT <- paste("sum(", haroblcond, ")", sep = "")
    oblcond <- paste(del, "_obl==1", sep = "")
    # TotInt<-paste("sum((",oblcond,")*(1+",delKolN,"h_omk-",delKolN,"l_omk))",sep="")
    TotInt <- paste("1+", delKolN, "h_omk-", delKolN, "l_omk", sep = "")
    NHAR <- paste("sum(", oblcond, ")", sep = "")
    # Sett hjelpevariablene
    eval(parse(text = paste(
      "KB[,c(\"DekkInt\",\"FRADELER\",\"NHAR\",\"TotInt\",\"NTOT\"):=list(", DekkInt, ",.N,", NTOT, ",", TotInt, ",", NHAR, "),by=bycols]",
      sep = ""
    )))
    # Sjekk om dekning tilfrestiller krav satt i
    eval(parse(text = paste(
      "KB[,\"", del, "_Dekk\":=as.integer(", IntervallHull[[del]], "),by=bycols]",
      sep = ""
    )))
    IntRapp <- 0
    if (IntRapp == 1 & del == "A" & "AARl" %in% names(KB)) {
      print("PARTDEKKK")
      print(IntervallHull[[del]])
      print(KB)
    }
    
    
    KB[, c("DekkInt", "NHAR", "TotInt", "NTOT", "FRADELER") := NULL]
    # Kast hjelpekolonnner
    # KB[,c("DekkInt","NHAR","TotInt","NTOT","DEKKok"):=NULL]
  } else {
    eval(parse(text = paste(
      "KB[,", del, "_Dekk:=as.integer(!any(", har, "==0 & ",
      del, "_obl==1)),by=bycols]",
      sep = ""
    )))
  }
  
  gc()
  return(KB)
}

#' FinnRedesignForFilter (kb)
#'
#' @param ORGd 
#' @param Filter 
#' @param globs 
#'
#' @return
#' @export
#'
#' @examples
FinnRedesignForFilter <- function(ORGd, Filter, globs = FinnGlobs()) {
  is_kh_debug()
  
  MODd <- Filter
  for (del in setdiff(names(ORGd$Part), names(Filter))) {
    MODd[[del]] <- copy(ORGd$Part[[del]])
  }
  return(FinnRedesign(ORGd, list(Part = MODd), globs = globs))
}

#' FinnDesignEtterFiltrering (kb)
#'
#' @param ORGd 
#' @param Filter 
#' @param FilterKols 
#' @param FGP 
#' @param globs 
FinnDesignEtterFiltrering <- function(ORGd, Filter, FilterKols = character(0), FGP = list(amin = 0, amax = 120), globs = FinnGlobs()) {
  is_kh_debug()
  
  FiltD <- FinnRedesignForFilter(ORGd, Filter, globs = globs)$Dekk
  FiltD <- FiltD[, setdiff(names(FiltD), FilterKols), with = FALSE]
  return(FinnDesign(FiltD, FGP = FGP, globs = globs))
}

#' OmkodFil (kb)
#'
#' @param FIL 
#' @param RD 
#' @param globs 
#' @param echo 
OmkodFil <- function(FIL, RD, globs = FinnGlobs(), echo = 0) {
  is_kh_debug()
  
  orgkols <- names(FIL)
  setDT(FIL)
  tabnames <- FinnTabKols(names(FIL))
  valkols <- FinnValKols(names(FIL))
  lp <- paste(valkols, "=sum(", valkols, "),",
              valkols, ".f=max(", valkols, ".f),",
              valkols, ".a=sum(", valkols, ".a*(!is.na(", valkols, ") & ", valkols, "!=0))",
              sep = "", collapse = ","
  )
  
  if (nrow(RD$FULL) > 0) {
    for (del in names(RD$Filters)) {
      setkeyv(FIL, names(RD$Filters[[del]]))
      setkeyv(RD$Filters[[del]], names(RD$Filters[[del]]))
      if (echo == 1) {
        cat("Filtrerer", del, "før dim(FIL)=", dim(FIL))
      }
      if (any(duplicated(RD$Filters[[del]]))) {
        print("CARTESIAN????")
        print(RD$Filters[[del]])
        print(RD$Filters[[del]][duplicated(RD$Filters[[del]]), ])
      }
      FIL <- FIL[RD$Filters[[del]], nomatch = 0]
      if (echo == 1) {
        cat(" og etter", dim(FIL), "\n")
      }
    }
    
    # NB! Rekkefølge er essensiell, dvs at ubeting kommer til slutt
    beting <- intersect(globs$DefDesign$AggPri[length(globs$DefDesign$AggPri):1], c(globs$DefDesign$BetingOmk, globs$DefDesign$BetingF))
    ubeting <- intersect(globs$DefDesign$AggPri[length(globs$DefDesign$AggPri):1], c(globs$DefDesign$UBeting))
    
    for (del in intersect(c(beting, ubeting), names(RD$KBs))) {
      orgtabs <- names(RD$KBs[[del]])[!grepl("_omk$", names(RD$KBs[[del]]))]
      omktabs <- names(RD$KBs[[del]])[grepl("_omk$", names(RD$KBs[[del]]))]
      bycols <- c(setdiff(tabnames, gsub("_omk", "", omktabs)), omktabs)
      
      # Sjekk type omkoding som trengs.
      # Dersom hver orgkode skal til mange omkkoder
      # er det uheldig å merge FIL[KB] om FIL er stor siden det lages mange kopier av orglinjer i FIL
      # i slike tilfeller kobles i stedet inn en loop over omkoding til hver omktab  (jfr laging av tiårssnitt i KREFT)
      
      setkeyv(RD$KBs[[del]], orgtabs)
      replikfaktor <- RD$KBs[[del]][, list(N = .N), by = orgtabs][, mean(N)]
      setkeyv(FIL, orgtabs)
      if (echo == 1) {
        cat("Omkoder", del, "dim(FIL) er ", dim(FIL), "originalt")
      }
      if (nrow(FIL) < 1000000 | replikfaktor < 4 | del == "Gn") {
        FIL <- FIL[RD$KBs[[del]], nomatch = 0, allow.cartesian = TRUE]
        # FIL<-FIL[RD$KBs[[del]],nomatch=0]
        if (echo == 1) {
          cat(" og", dim(FIL), "etter merge")
        }
        if (del == "Gn") {
          # Omkod geo
          FIL[GEOniv_omk == "K", GEO := substr(GEO, 0, 4)]
          FIL[GEOniv_omk == "F", GEO := FYLKE]
          FIL[GEOniv_omk == "L", c("GEO", "FYLKE") := list("0", "00")]
          # FIL[GEOniv_omk=="B" & GEOniv=="S" & grepl("^(0301|1103|1201|1601)",GEO),c("GEO","FYLKE"):=list(substr(GEO,0,6),substr(GEO,0,2))]
          # FIL[GEOniv_omk=="B" & GEOniv=="S" & !grepl("^(0301|1103|1201|1601)",GEO),c("GEO","FYLKE"):=list("999999","99")]
          FIL[GEOniv_omk == "B" & GEOniv == "S" & !grepl("^(0301|1103|1201|1601|4601|5001)", GEO), c("GEO", "FYLKE") := list("999999", "99")]
          # Dette er dårlig, bør endre til
          # FIL[GEOniv_omk=="B" & GEOniv=="S" & !GEO %in% globs$GeoKoder[GEOniv=="B"]$GEO,c("GEO","FYLKE"):=list("999999","99")]
          FIL[GEOniv_omk == "H" & GEOniv != "H", GEO := mapvalues(FYLKE, globs$HELSEREG$FYLKE, globs$HELSEREG$HELSEREG, warn_missing = FALSE)]
          # FIL[GEOniv_omk=="H" & GEOniv!="H",FYLKE:="00"]
          FIL[GEOniv_omk == "H", FYLKE := "00"]
        }
        setkeyv(FIL, bycols)
        lpl <- paste("list(", lp, ")", sep = "")
        FIL <- FIL[, eval(parse(text = lpl)), by = bycols]
        # Dette skulle vel vært bedre, men blir alt for tregt? når ikke bycols er key
        # FIL<-FIL[RD$KBs[[del]],nomatch=0,allow.cartesian=TRUE][, eval(parse(text=lp)), by=bycols]
      } else {
        KB <- copy(RD$KBs[[del]])
        setkeyv(KB, omktabs)
        OMKs <- unique(KB[, omktabs, with = FALSE])
        FILt <- FIL[0, ]
        for (i in 1:nrow(OMKs)) {
          OMK <- OMKs[i, ]
          print(OMK)
          KBt <- KB[OMK]
          setkeyv(KBt, orgtabs)
          FILd <- FIL[KBt, nomatch = 0, allow.cartesian = TRUE]
          setkeyv(FILd, bycols)
          lpt <- paste("list(", paste(gsub("_omk$", "", names(OMK)), OMK, sep = "=", collapse = ","), ",", lp, ")", sep = "")
          FILt <- rbind(FILt, FILd[, eval(parse(text = lpt)), by = bycols][, names(FILt), with = FALSE])
        }
        FIL <- FILt
      }
      if (echo == 1) {
        cat(" og til slutt", dim(FIL), "\n")
      }
      setnames(FIL, names(FIL), gsub("_omk$", "", names(FIL)))
    }
  }
  
  if (nrow(RD$Udekk) > 0) {
    UDekk <- copy(RD$Udekk)
    restkols <- setdiff(tabnames, names(UDekk))
    setkeyv(FIL, names(UDekk))
    setkeyv(UDekk, names(UDekk))
    FIL <- FIL[!UDekk, ]
    valkolsF <- unlist(lapply(valkols, function(x) {
      paste(x, c("", ".f", ".a"), sep = "")
    }))
    ## feil med recycling av := lest NEWS 1.12.2 data.table
    ## UDekk[,(valkolsF):=list(NA,9,0)]
    UDekk[, (valkols) := NA]
    valg_f <- grep(".f$", valkolsF, value = TRUE)
    UDekk[, (valg_f) := 9]
    valg_a <- grep(".a$", valkolsF, value = TRUE)
    UDekk[, (valg_a) := 0]
    if (length(restkols) > 0) {
      rest <- as.data.frame(unique(FIL[, restkols, with = FALSE]))
      UDekk <- data.table(expand.grid.df(rest, as.data.frame(UDekk)))
    }
    FIL <- rbind(FIL[, orgkols, with = FALSE], UDekk[, orgkols, with = FALSE])
    cat("UDEKKA:", nrow(RD$Udekk), "\n")
    # print(subset(RD$Udekk,GEOniv!="B"))
    print(RD$Udekk)
  }
  return(FIL)
}

#' GeoHarm (kb)
#' 
#' Helper function in KlargjorFil and LagTNtabell
#'
#' @param FIL 
#' @param vals 
#' @param rektiser 
#' @param FDesign 
#' @param batchdate 
#' @param globs 
#' @param GEOstdAAR 
GeoHarm <- function(FIL, vals = list(), rektiser = TRUE, FDesign = list(), batchdate = SettKHBatchDate(), globs = FinnGlobs(), GEOstdAAR = globs$KHaargang) {
  is_kh_debug()
  
  if (identical(class(FIL), "data.frame")) {
    FIL <- data.table(FIL)
  }
  keyorg <- key(FIL)
  geoomk <- globs$KnrHarm
  FIL$GEO <- mapvalues(FIL$GEO, geoomk$GEO, geoomk$GEO_omk, warn_missing = FALSE)
  FIL[, FYLKE := NULL]
  FIL <- KHaggreger(FIL, vals = vals, globs = globs)
  # Rektangulariser
  if (rektiser == TRUE) {
    REKT <- data.table()
    if (length(FDesign) == 0) {
      FDesign <- FinnDesign(FIL)
    }
    FDes <- FDesign$Design
    # Switch for TYP=="O" ??
    for (Gn in FDesign$Part[["Gn"]][["GEOniv"]]) {
      GEOK <- subset(globs$GeoKoder, FRA <= GEOstdAAR & TIL > GEOstdAAR & GEOniv == Gn)$GEO
      FDesG <- FDes[HAR == 1 & GEOniv == Gn, intersect(names(FIL), names(FDes)), with = FALSE]
      REKT <- rbind(data.table(expand.grid.df(data.frame(FDesG), data.frame(GEO = GEOK))), REKT)
    }
    setkeyv(REKT, names(REKT))
    setkeyv(FIL, names(REKT))
    FIL <- FIL[REKT]
    FIL <- SettMergeNAs(FIL, vals = vals)
  }
  
  
  FIL[, FYLKE := ifelse(GEOniv %in% c("H", "L"), "00", substr(GEO, 1, 2))]
  return(FIL)
}

#' SettMergeNAs (kb)
#' 
#' Helper function in LagKUBE, KlargjorFil, LagTNtabell, and GeoHarm
#'
#' @param FT 
#' @param valsdef 
#'
#' @return
#' @export
#'
#' @examples
SettMergeNAs <- function(FT, valsdef = list()) {
  is_kh_debug()
  
  vals <- gsub("^(.*)\\.f$", "\\1", names(FT)[grepl("^(.*)\\.f$", names(FT))])
  
  for (ValK in vals) {
    if (ValK %in% names(vals)) {
      if (valsdef[[ValK]][["miss"]] == "..") {
        valt <- c(0, 1, 1)
      } else if (valsdef[[ValK]][["miss"]] == ".") {
        valt <- c(0, 2, 1)
      } else if (valsdef[[ValK]][["miss"]] == ":") {
        valt <- c(0, 3, 1)
      } else if (!is.na(as.numeric(valsdef[[ValK]][["miss"]], warn_missing = FALSE))) {
        valt <- c(as.numeric(valsdef[[ValK]][["miss"]], warn_missing = FALSE), 0, 1)
      }
    } else {
      valt <- c(0, 0, 1) # Default er (implisitt 0). Merk at valsdef er tom bare for avledete kolonner, her er 0 naturlig default
    }
    # miss<-eval(parse(text=paste(
    #  "nrow(FT[is.na(",ValK,") & (",ValK,".f==0 | is.na(",ValK,".f)),])",sep=""
    # )))
    miss <- eval(parse(text = paste(
      "FT[is.na(", ValK, ") & (", ValK, ".f==0 | is.na(", ValK, ".f)),]",
      sep = ""
    )))
    if (nrow(miss) > 0) {
      cat("SettMergeNAs, setter inn", nrow(miss), "default for", ValK, "\n")
    }
    
    eval(parse(text = paste(
      "FT[is.na(", ValK, ") & (", ValK, ".f==0 | is.na(", ValK, ".f)),c(\"", ValK, "\",\"", ValK, ".f\",\"", ValK, ".a\"):=list(", paste(valt, collapse = ","), ")]",
      sep = ""
    )))
  }
  return(FT)
}

#' LeggTilSumFraRader (kb)
#'
#' Helper function in KlargjorFil and LagTNtabell
#' @param TNF 
#' @param NYdscr 
#' @param FGP 
#' @param globs 
LeggTilSumFraRader <- function(TNF, NYdscr, FGP = list(amin = 0, amax = 120), globs = FinnGlobs()) {
  is_kh_debug()
  
  if (!(is.na(NYdscr) | NYdscr == "")) {
    for (sumfra in unlist(str_split(NYdscr, ";"))) {
      # cat("SUMFRA: ",sumfra,"\n")
      if (grepl("^ *(.+?) *= *(.+?)\\{(.*)\\} *$", sumfra)) {
        nycol <- gsub("^ *(.+?) *= *(.+?)\\{(.*)\\} *$", "\\1", sumfra)
        gmlcol <- gsub("^ *(.+?) *= *(.+?)\\{(.*)\\} *$", "\\2", sumfra)
        expr <- gsub("^ *(.+?) *= *(.+?)\\{(.*)\\} *$", "\\3", sumfra)
        # cat("nycol:",nycol,"gmlcol:",gmlcol,"expr:",expr,"\n")
        NF <- EkstraherRadSummer(TNF, expr, FGP = FGP, globs = globs)
        gmlcols <- paste(gmlcol, c("", ".f", ".a"), sep = "")
        nycols <- paste(nycol, c("", ".f", ".a"), sep = "")
        setnames(NF, gmlcols, nycols)
        # print(NF)
        # Sy sammen
        commontabs <- globs$DefDesign$DesignKolsFA[globs$DefDesign$DesignKolsFA %in% names(NF)]
        
        # Er usikker på om hva som egentlig er best her.
        # Siden OmkodFraPart brukt i EkstraherRadSummer gir full rektulangusering kan man ha satt
        # deler i NF som er udekket i TNF. 1) Disse ønskes vel egentlig ikke med
        # men motsatt, 2) dersom TNF ha manglende GEO-koder som finnes i NF er det kanskje ønskelig å ha disse med
        # Jeg velger å sette venstre join TNF->NF slik at problem 1 faller bort
        # Så lenge herværende prosedyre bare kjøres etter at TNF er rektangularisert mht GEO faller også 2) bort
        # Dette gjelder i standard produskjonsløype (LagTnTabell, LagKUBE etc)
        
        setkeym(TNF, commontabs)
        setkeym(NF, commontabs)
        dimorg <- dim(TNF)
        TNF <- NF[, c(commontabs, nycols), with = FALSE][TNF]
        cat("LeggTilSumFraRader. Før er dim(TNF)", dimorg, "og dim(NF)", dim(NF), "etter er dim(TNF)", dim(TNF), "\n")
        # altså ikke
        # TNF<-merge(TNF,NF[,c(commontabs,nycols),with=FALSE],all=TRUE,by=commontabs)
        
        # TNF<-merge(TNF,NF[,c(commontabs,nycols),with=FALSE],all=TRUE,by=commontabs)
        TNF <- SettMergeNAs(TNF, list(gmlcol = FGP$vals, nycol = FGP$vals[gmlcol]))
        
        # print(TNF)
      } else {
        cat("FEIL!!!!!: NYEKOL_RAD har feil format:", NYdscr, "\n")
      }
    }
  }
  
  return(TNF)
}

#' EkstraherRadSummer (kb)
#'
#' @param FIL 
#' @param pstrorg 
#' @param FGP 
#' @param globs 
EkstraherRadSummer <- function(FIL, pstrorg, FGP = list(amin = 0, amax = 120), globs = FinnGlobs()) {
  is_kh_debug()
  
  setDT(FIL)
  amin <- FGP$amin
  amax <- FGP$amax
  
  # Modifiser syntaks pstrorg
  
  # Rydd i "feil syntaks"
  # NB: takler ikke "|" (or) i pstrorg
  
  pstrorg <- gsub("(?<!=)=(?!=)", "==", pstrorg, perl = TRUE)
  pstrorg <- gsub(" *== *(?=c\\()", " %in% ", pstrorg, perl = TRUE)
  
  # Standard "alle"-verdier
  pstrorg <- gsub("(^ *|& *)ALDER( *&| *$)", "ALDER==\\1\"ALLE\"\\2", pstrorg)
  pstrorg <- gsub("(^ *|& *)(KJONN|UTD|LAND|INNVKAT)( *&| *$)", "\\1\\2==0\\3", pstrorg)
  
  # Intervaller
  # Er det mulig å abstrahere her, dvs å ta alle "INT"-deler med samme syntaks???
  pstrorg <- gsub("ALDER *(={1,2}) *\"*ALLE\"*", paste("ALDERl==", amin, " & ALDERh==", amax, sep = ""), pstrorg)
  pstrorg <- gsub("ALDER *(={1,2}) *(\\d+)$", "ALDERl==\\2 & ALDERh==\\2", pstrorg)
  pstrorg <- gsub("AAR *(={1,2}) *(\\d+)$", "AARl==\\2 & AARh==\\2", pstrorg)
  
  # Klipp opp pstrorg
  
  # Finn kolonner involvert i pstrorg
  alletabs <- str_replace(unlist(str_split(pstrorg, " *& *")), "^(\\w*?) *(%in%.*|==.*| *$)", "\\1")
  
  # Fjern de som ikke er del av subset betingelse
  # subsetstr<-gsub("(?<=[&^]) *\\w+ *(?=[&$])","",pstrorg,perl=TRUE)
  # subsetstr<-gsub("&+","&",subsetstr)
  subsetstr <- gsub("^ *\\w+ *(&|$)|(^|&) *\\w+ *$", "", pstrorg, perl = TRUE)
  subsetstr <- gsub("& *\\w+ *&", "&", subsetstr, perl = TRUE)
  
  # Splitt i kolonnenavn og verdi
  subtabs <- str_replace(unlist(str_split(subsetstr, " *& *")), "^(\\w+) *(%in%.*|==.*)", "\\1")
  subvals <- str_replace(unlist(str_split(subsetstr, " *& *")), "^.*(%in% *|== *)(\\w+)", "\\2")
  subvals <- setNames(subvals, subtabs)
  # Filtrer til de som er aktuelle for omkoding
  subvals <- subvals[names(subvals) %in% globs$DefDesign$DesignKols]
  
  # Omkod disse
  if (length(subvals) > 0) {
    # For omkodbare kolonner må disse omkodes til søkte verdier (for generalitet må det omkodes selv om disse finnes)
    OmkParts <- list()
    for (del in names(globs$DefDesign$DelKols)) {
      if (all(globs$DefDesign$DelKols[[del]] %in% names(subvals))) {
        dvals <- subvals[globs$DefDesign$DelKols[[del]]]
        if (KHglobs$DefDesign$DelFormat[[del]] == "integer") {
          dvals <- setNames(as.integer(dvals), names(dvals))
        }
        OmkParts[[del]] <- setNames(data.frame(matrix(dvals, ncol = length(dvals))), names(dvals))
      } else if (any(globs$DefDesign$DelKols[[del]] %in% names(subvals))) {
        print("VARSKU HER!!!!!!!!!!!!!!! FEIL i EkstraherRadSummer!")
      }
    }
    # omk[,]<-as.numeric(omk[,])
    # omkD<-FinnDesign(omk,amin=amin,amax=amax,globs=globs)
    print("Til OmkodFil fra EkstraherRadSummer, dette kan fort gi udekt ved ubalansert design. Dette faller bort igjen ved NF[TNF")
    FIL <- OmkodFil(FIL, FinnRedesign(FinnDesign(FIL), list(Parts = OmkParts)), globs = globs, echo = 1)
  }
  if (subsetstr != "") {
    FIL <- eval(parse(text = paste("subset(FIL,", subsetstr, ")", sep = "")))
  }
  # cat("ALLEtabs: ",alletabs," names(FIL): ",names(FIL), "SETT: ",names(FIL)[!names(FIL) %in% alletabs],"\n")
  # print(alletabs)
  # print(head(FIL[,names(FIL)[!names(FIL) %in% alletabs],with=FALSE]))
  FIL <- KHaggreger(FIL[, !names(FIL) %in% alletabs, with = FALSE], globs = globs)
  return(FIL)
}

#' AggregerRader (kb)
#'
#' @param FG 
#' @param nyeexpr 
#' @param FGP 
AggregerRader <- function(FG, nyeexpr, FGP) {
  is_kh_debug()
  
  if (!(is.na(nyeexpr) || nyeexpr == "")) {
    nytabs <- unlist(str_split(nyeexpr, ";"))
    for (nytab in nytabs) {
      # PARSING av syntaks
      nylab <- gsub("^\\[(.*?)\\]=.*", "\\1", nytab)
      subexp <- gsub(".*\\]=\\{(.*)\\}", "\\1", nytab)
      if (grepl("%in%|==", subexp)) {
        tab <- gsub("^ *(.*) *(%in%|==).*", "\\1", subexp)
      } else {
        tab <- gsub("^ *(.*) *$", "\\1", subexp)
        subexp <- TRUE
      }
      if (!tab %in% names(FG)) {
        tabE <- names(FGP)[which(FGP == tab)]
        subexp <- gsub("^ *tab(.*)", paste(tabE, "\\1", sep = ""), subexp)
        tab <- tabE
      }
      FG2 <- eval(parse(text = paste("subset(FG,", subexp, ")", sep = "")))
      FG2 <- KHaggreger(FG2[, setdiff(names(FG), tab), with = FALSE])
      FG2[, eval(parse(text = paste(tab, ":='", nylab, "'", sep = "")))]
      FG <- rbind(FG, FG2[, names(FG), with = FALSE])
    }
  }
  return(FG)
}

#' OmkodFilFraPart (kb)
#'
#' @param Fil 
#' @param Part 
#' @param FGP 
#' @param rapport 
#' @param globs 
#' @param echo 
OmkodFilFraPart <- function(Fil, Part, FGP = list(amin = 0, amax = 120), rapport = list(), globs = FinnGlobs(), echo = 0) {
  is_kh_debug()
  
  rapport["KALL"] <- "OmkodFilFraPart"
  Dorg <- FinnDesign(Fil, FGP = FGP, globs = globs)
  Dmod <- ModifiserDesign(Part, Dorg, globs = globs)
  RD <- FinnRedesign(Dorg, Dmod, globs = globs, echo = echo)
  return(OmkodFil(Fil, RD, rapport = rapport, globs = globs))
}

#' ModifiserDesign (kb)
#'
#' @param Nytt 
#' @param Org 
#' @param globs 
ModifiserDesign <- function(Nytt, Org = list(), globs = FinnGlobs()) {
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
  
  if (any(grepl("_HAR", names(Org$OmkDesign)))) {
    cat("************************************************************\n*\n*  OBSN NOE RART MED ModifiserDesign HAR\n*\n*********************************\n")
  }
  
  omkDeler <- intersect(names(Nytt), c(globs$DefDesign$UBeting, globs$DefDesign$BetingOmk))
  if (length(omkDeler) > 0) {
    NyKols <- unlist(globs$DefDesign$DelKols[omkDeler])
    NyKols <- intersect(names(Org$OmkDesign), NyKols)
    OmkDesignGmlKols <- setdiff(names(Org$OmkDesign), c(NyKols, "HAR"))
    delerlist <- paste("as.data.frame(Nytt[[\"", names(Nytt), "\"]])", sep = "", collapse = ",")
    # Skal beholdes
    if (length(OmkDesignGmlKols) > 0) {
      OmkDesGml <- Org$OmkDesign[, c(OmkDesignGmlKols, "HAR"), with = FALSE]
      setkeyv(OmkDesGml, OmkDesignGmlKols)
      OmkDesGml <- OmkDesGml[, list(HAR = max(HAR)), by = OmkDesignGmlKols]
      delerlist <- paste(delerlist, ",as.data.frame(OmkDesGml)", sep = "")
    }
    OmkDesNy <- data.table(eval(parse(text = paste("expand.grid.df(", delerlist, ")", sep = ""))))
    if (length(OmkDesignGmlKols) == 0) {
      OmkDesNy[, HAR := 1]
    }
    OmkKols <- globs$DefDesign$DesignKols[globs$DefDesign$DesignKols %in% names(OmkDesNy)]
    setkeym(OmkDesNy, OmkKols)
    Org[["OmkDesign"]] <- OmkDesNy
  }
  # Merk, det gir bare mening å bruke denne for å lage et TIL-design, da trengs ikke de følgende delene
  # Om det modifiserte designet skal brukes som et FRA-design må også disse endres. Det er en klønete operasjon (og som vel knapt er veldefinert)
  # Kan altså IKKE bruke FinnFellesTab(Org,ModifiserDesign(PredFilter,Org))
  
  Org[["Design"]] <- NULL
  Org[["SKombs"]] <- NULL
  Org[["FKombs"]] <- NULL
  
  return(Org)
}

#' FinnSumOverAar (kb)
#'
#' @param KUBE 
#' @param per 
#' @param FyllMiss 
#' @param AntYMiss 
#' @param na.rm 
#' @param report_lpsvars 
#' @param globs 
FinnSumOverAar <- function(KUBE, per = 0, FyllMiss = FALSE, AntYMiss = 0, na.rm = FALSE, report_lpsvars = TRUE, globs = FinnGlobs()) {
  is_kh_debug()
  UT <- KUBE[0, ]
  tabs <- setdiff(FinnTabKols(names(KUBE)), c("AARl", "AARh"))
  valkols <- FinnValKols(names(KUBE))
  # Utrykk for KH-aggregering (med hjelpestørrelses for snitt)
  if (na.rm == FALSE) {
    lpv <- paste(valkols, "=sum(", valkols, ",na.rm=", na.rm, "),",
                 valkols, ".f=max(", valkols, ".f),",
                 valkols, ".a=sum(", valkols, ".a*(!is.na(", valkols, ") & ", valkols, "!=0))",
                 sep = "", collapse = ","
    )
  }
  if (na.rm == TRUE) {
    lpv <- paste(valkols, "=sum(", valkols, ",na.rm=", na.rm, "),",
                 valkols, ".f=0,",
                 valkols, ".a=sum(", valkols, ".a*(!is.na(", valkols, ") & ", valkols, "!=0)),",
                 valkols, ".fn1=sum(", valkols, ".f %in% 1:2),",
                 valkols, ".fn3=sum(", valkols, ".f==3),",
                 valkols, ".fn9=sum(", valkols, ".f==9),",
                 valkols, ".n=sum(", valkols, ".f==0)",
                 # valkols,".n=sum(as.numeric(!is.na(",valkols,")))",
                 sep = "", collapse = ","
    )
    lpsvars <- unlist(lapply(valkols, function(x) {
      paste(x, c(".fn1", ".fn3", ".fn9", ".n"), sep = "")
    }))
    UT[, (lpsvars) := NA_integer_]
  }
  
  aara <- unique(KUBE$AARh)
  if (FyllMiss == TRUE) {
    aara <- (min(aara) + per - 1):max(aara)
  } else {
    aara <- intersect((min(aara) + per - 1):max(aara), aara)
  }
  cat("Finner", per, "-års sum for ")
  for (aar in aara) {
    cat(aar, " ")
    lp <- paste("list(AARl=", aar - per + 1, ",AARh=", aar, ",", lpv, ")", sep = "")
    UT <- rbind(UT, KUBE[AARh %in% c((aar - per + 1):aar), eval(parse(text = lp)), by = tabs][, names(UT), with = FALSE])
  }
  cat("\n")
  for (valkol in valkols) {
    eval(parse(text = paste("UT[", valkol, ".f>0,", valkol, ":=list(NA)]", sep = "")))
  }
  if (na.rm == TRUE) {
    if (AntYMiss <= per) {
      for (valkol in valkols) {
        eval(parse(text = paste("UT[", valkol, ".fn9>", AntYMiss, ",c(\"", valkol, "\",\"", valkol, ".f\"):=list(NA,9)]", sep = "")))
      }
    }
  }
  f9s <- names(UT)[grepl(".f9$", names(UT))]
  if (length(f9s) > 0) {
    UT[, (f9s) := NULL]
  }
  if (na.rm == TRUE & report_lpsvars == FALSE) {
    UT[, (lpsvars) := NULL]
  }
  return(UT)
}

#' AnonymiserNaboer (kb)
#' 
#' Potentially deprecated?
#'
#' @param FG 
#' @param ovkatstr 
#' @param FGP 
#' @param D_develop_predtype 
#' @param globs
AnonymiserNaboer <- function(FG, ovkatstr, FGP = list(amin = 0, amax = 120), D_develop_predtype = "IND", globs = FinnGlobs()) {
  is_kh_debug()
  FG <- copy(FG)
  AoverkSpecs <- SettNaboAnoSpec(ovkatstr, FGP = FGP, globs = globs)
  
  vals <- FinnValKols(names(FG))
  # FinnValKolsF funker ikke riktig!!!! Både pga nye flag slik som fn9 og pga verdikolonner uten .f (MEISskala) etc
  # Må utbedres gjennomgripende, men kan ikke gjøre dette nå derfor bare denne ad hoc løsninga
  if (D_develop_predtype == "IND") {
    alletabs <- setdiff(names(FG), FinnValKolsF(names(FG)))
  } else {
    alletabs <- intersect(c("GEO", "GEOniv", "FYLKE", "AARl", "AARh", "ALDERl", "ALDERh", "KJONN", "TAB1", "TAB2", "UTDANN", "INNVKAT", "LANDBAK"), names(FG))
  }
  for (ovkSpec in AoverkSpecs) {
    FGt <- FG[eval(parse(text = ovkSpec$subcond)), ]
    FGr <- FG[!eval(parse(text = ovkSpec$subcond)), ]
    overkats <- ovkSpec$overkat
    for (val in vals) {
      eval(parse(text = paste(
        "FGt[,", val, ".na:=0]",
        sep = ""
      )))
    }
    for (i in 1:length(overkats)) {
      kombs <- combn(names(overkats), i)
      for (j in 1:ncol(kombs)) {
        substrs <- character(0)
        overtabs <- character(0)
        for (del in kombs[, j]) {
          substrs <- c(substrs, overkats[[del]]$over)
          overtabs <- c(overtabs, overkats[[del]]$kols)
        }
        substr <- paste("(", substrs, ")", sep = "", collapse = " | ")
        for (val in vals) {
          bycols <- setdiff(alletabs, overtabs)
          eval(parse(text = paste(
            "FGt[!(", substr, "),", val, ".na:=ifelse((", val, ".na==1 | any(", val, ".f %in% 3:4)),1,0),by=bycols]",
            sep = ""
          )))
        }
        # FG[substr,VAL.na:=ifelse(any(VAL.f==3),1,0),by=setdiff(alletabs,overtabs)]
      }
    }
    
    for (val in vals) {
      eval(parse(text = paste(
        "FGt[", val, ".na==1,", val, ".f:=4]",
        sep = ""
      )))
      eval(parse(text = paste(
        "FGt[", val, ".na==1,", val, ":=NA]",
        sep = ""
      )))
      eval(parse(text = paste(
        "FGt[,", val, ".na:=NULL]",
        sep = ""
      )))
    }
    
    FG <- rbind(FGt, FGr)
  }
  return(FG)
}

#' SettNaboAnoSpec (kb)
#'
#' @param ovkatspec 
#' @param FGP 
#' @param globs 
SettNaboAnoSpec <- function(ovkatspec, FGP = list(amin = 0, amax = 120), globs = FinnGlobs()) {
  is_kh_debug()
  Foverkat <- list()
  if (!(is.null(ovkatspec) || is.na(ovkatspec))) {
    specs <- unlist(str_split(ovkatspec, ";"))
    i <- 1
    for (spec in specs) {
      if (grepl("\\[(.*?)\\]=\\[.*\\]", spec)) {
        subcond <- gsub("^\\[(.*?)\\]=\\[.*\\]", "\\1", spec)
        subcond <- paste("(", subcond, ")", sep = "")
        ovkatstr <- gsub("^\\[(.*?)\\]=\\[(.*)\\]", "\\2", spec)
      } else {
        subcond <- "TRUE"
        ovkatstr <- spec
      }
      
      overkat <- list()
      ovkatstr <- gsub("([^=]+)=([^=]+)", "\\1==\\2", ovkatstr)
      ovkatstr <- gsub("(.*)ALDER=='*ALLE'*(.*)", paste("\\1", "ALDER==", FGP$amin, "_", FGP$amax, "\\2", sep = ""), ovkatstr)
      ovkatstr <- gsub("(.*)ALDER=='*(\\d+)_('| )(.*)", paste("\\1", "ALDER==\\2_", FGP$amax, "\\3\\4", sep = ""), ovkatstr)
      for (del in names(globs$DefDesign$DelKolN)) {
        delN <- globs$DefDesign$DelKolN[del]
        if (globs$DefDesign$DelType[del] == "COL") {
          if (grepl(paste("(^|\\&) *", delN, " *== *'*(.*?)'* *(\\&|$)", sep = ""), ovkatstr)) {
            over <- gsub(paste(".*(^|\\&) *(", delN, " *== *'*.*?'*) *(\\&|$).*", sep = ""), "\\2", ovkatstr)
            overkat[[del]] <- list(over = over, kols = delN)
          }
        } else if (globs$DefDesign$DelType[del] == "INT") {
          if (grepl(paste("(^|\\&) *", delN, "l *== *'*(.*?)'* *($|\\&)", sep = ""), ovkatstr) &&
              grepl(paste("(^|\\&) *", delN, "h *== *'*(.*?)'* *($|\\&)", sep = ""), ovkatstr)) {
            overl <- gsub(paste(".*(^|\\&) *(", delN, "l *== *'*.*?)'* *($|\\&).*", sep = ""), "\\2", ovkatstr)
            overh <- gsub(paste(".*(^|\\&) *(", delN, "h *== *'*.*?)'* *($|\\&).*", sep = ""), "\\2", ovkatstr)
            overkat[[del]] <- list(over = paste(overl, overh, sep = " & "), kols = paste(delN, c("l", "h"), sep = ""))
          } else if (grepl(paste("(^|\\&) *", delN, " *== *'*(.*?)'* *($|\\&)", sep = ""), ovkatstr)) {
            intval <- unlist(str_split(gsub(paste("(^|.*\\&) *", delN, " *== *'*(.*?)'* *($|\\&.*)", sep = ""), "\\2", ovkatstr), "_"))
            if (length(intval) == 1) {
              intval <- c(intval, intval)
            }
            over <- paste(paste(delN, "l", sep = ""), "==", intval[1], " & ", paste(delN, "h", sep = ""), "==", intval[2], sep = "")
            overkat[[del]] <- list(over = over, kols = paste(delN, c("l", "h"), sep = ""))
          }
        }
      }
      Foverkat[[i]] <- list(subcond = subcond, overkat = overkat)
      i <- i + 1
    }
  }
  return(Foverkat)
}

#' DekkerInt (kb)
#'
#' @param FRA 
#' @param TIL 
DekkerInt <- function(FRA, TIL) {
  is_kh_debug()
  
  # Sjekker at hele intervallet TIL[,1]-TIL[,2] dekkes av intervallene i FRA
  # Bryr seg ikke om overlapp her, det gjøres andre steder
  # Kompakt, men effektiv syntaks
  return(all(TIL[, 1]:TIL[, 2] %in% unlist(mapply(seq, FRA[, 1], FRA[, 2]))))
}

#' DFHeadToString (kb)
#'
#' @param innDF 
#' @param topn 
DFHeadToString <- function(innDF, topn = 10) {
  is_kh_debug()
  
  # Bruker data.table print for summary
  DT <- data.table(innDF)
  optdef <- getOption("width") # Sett bred output
  options(width = 250)
  head <- paste(capture.output(print(print(DT, topn = topn))), collapse = "\n")
  head <- sub("NULL$", "", head)
  options(width = optdef)
  return(head)
}

## Stata prikking do file
#' do_stata_prikk (ybk)
#' 
#' Function to censor the data using the STATA method (JRM)#'
do_stata_prikk <- function(dt, spc, batchdate, globs, test = FALSE){
  is_kh_debug()
  
  stataVar <- c("Stata_PRIKK_T", "Stata_PRIKK_N", "Stata_STATTOL_T")
  s_prikk <- sum(sapply(spc[, ..stataVar], get_col), na.rm = TRUE)
  
  RprikkVar <- c("PRIKK_T", "PRIKK_N", "STATTOL_T")
  r_prikk <- sum(sapply(spc[, ..RprikkVar], get_col), na.rm = TRUE)
  
  # Check that R prikk should be empty if Stata prikk should be used
  warn_prikk(r_prikk, s_prikk)
  RES <- NULL
  
  if (s_prikk > 0){
    ## synt <- 'include "F:\\Forskningsprosjekter\\PDB 2455 - Helseprofiler og til_\\PRODUKSJON\\BIN\\Z_Statasnutter\\Rsynt_Postprosess_naboprikking_del_1_LESEFERD_INNV.do'
    sfile <- paste(globs[["path"]], globs[["KubeStataPrikkFil"]], sep = "/")
    synt <- paste0('include "', sfile, '"')
    
    RES <- KjorStataSkript(dt, script = synt, tableTYP = "DT", batchdate = batchdate, globs = globs)
    dt <- RES$TABLE
  } else {
    RES[["feil"]] <- ""
  }
  
  if (RES$feil != "") {
    stop("Noe gikk galt i kjøring av STATA \n", RES$feil)
  }
  
  return(dt)
}

#' kube_spec (ybk)
#' 
#' Saves ACCESS specs + list of dimensions to be used in STATA censoring
kube_spec <- function(spec, dims){
  is_kh_debug()
  
  rootDir <- file.path(fs::path_home(), "helseprofil")
  if (!fs::dir_exists(rootDir))
    fs::dir_create(rootDir)
  
  specDF <- as.data.table(spec)
  varStata <- grep("^Stata", names(specDF), value = TRUE)
  varSpec <- c("KUBE_NAVN", varStata)
  varDF <- specDF[, .SD, .SDcols = varSpec]
  varDF[, DIMS := list(dims)]
  fileSpec <- file.path(rootDir, "kubespec.csv")
  data.table::fwrite(varDF, fileSpec, sep = ";", sep2 = c("", " ", ""))
  message("Create Stata spec in ", fileSpec)
  return(specDF)
}

#' find_dims (vl)
#' 
#' Helper function for kube_spec, finding dimensions in KUBE
find_dims <- function(dt, spec){
  is_kh_debug()
  # List standarddims
  standarddims <- c("GEO",
                    "AAR",
                    "ALDER",
                    "KJONN",
                    "UTDANN",
                    "INNVKAT",
                    "LANDBAK")
  
  # Extract everything written in TAB1, TAB2, and TAB3 in the files involved
  tabdims <- vector()
  for(i in 1:length(spec)){
    tabdims <- c(tabdims, 
                 unlist(spec[[i]][c("TAB1", "TAB2", "TAB3")], use.names = F))
  }
  
  # Remove NA from tabdims, combine with standarddims
  tabdims <- tabdims[!is.na(tabdims)]
  alldims <- c(standarddims, tabdims)
  # Extract column names from dt included in dimension list
  names(dt)[names(dt) %in% alldims]
}

#' LagAlleFriskvikIndikatorerForKube (kb)
#'
#' @param KUBEid 
#' @param globs 
#' @param modus 
#' @param aargang 
#' @param ...
LagAlleFriskvikIndikatorerForKube <- function(KUBEid, globs = FinnGlobs(), modus = globs$HOVEDMODUS, aargang = format(Sys.time(), "%Y"), ...) {
  
  is_kh_debug()
  # indikatorer<-unlist(sqlQuery(globs$dbh,paste("SELECT INDIKATOR FROM ",friskvikTAB,aargang," WHERE KUBE_NAVN='",KUBEid,"'",sep=""),as.is=TRUE))
  indikatorer <- sqlQuery(globs$dbh, paste("SELECT INDIKATOR, ID FROM FRISKVIK WHERE AARGANG=", aargang, "AND KUBE_NAVN='", KUBEid, "'", sep = ""), as.is = TRUE)
  
  if (dim(indikatorer)[1] > 0) {
    for (i in 1:dim(indikatorer)[1]) {
      cat("Lager Friskvikfil for ", indikatorer[i, 1], "\n")
      LagFriskvikIndikator(id = indikatorer[i, 2], aargang = aargang, modus = modus, globs = globs, ...)
    }
  }
}

#' LagFriskvikIndikator (kb)
#'
#' @param id 
#' @param KUBE 
#' @param FGP 
#' @param versjonert 
#' @param aargang 
#' @param batchdate 
#' @param globs 
#' @param modus
LagFriskvikIndikator <- function(id, KUBE = data.table(), FGP = list(amin = 0, amax = 120), versjonert = FALSE, aargang = format(Sys.time(), "%Y"), batchdate = SettKHBatchDate(), globs = FinnGlobs(), modus = globs$HOVEDMODUS) {
  
  is_kh_debug()
  # FVdscr<-sqlQuery(globs$dbh,paste("SELECT * FROM ",aargang," WHERE INDIKATOR='",indikator,"' AND VERSJONFRA<=",datef," AND VERSJONTIL>",datef,sep=""),as.is=TRUE)
  FVdscr <- sqlQuery(globs$dbh, paste("SELECT * FROM FRISKVIK WHERE ID=", id, sep = ""), as.is = TRUE)
  
  moduser <- unlist(str_split(FVdscr$MODUS, ""))
  
  
  ## FHP and Oppveksprofile (OVP) folders specification
  profile <- FVdscr$PROFILTYPE
  
  switch(profile,
         "FHP" = {
           setDir_K <- globs$FriskVDir_K
           setDir_B <- globs$FriskVDir_B
           setDir_F <- globs$FriskVDir_F
         },
         "OVP" = {
           setDir_K <- globs$ovpDir_K
           setDir_B <- globs$ovpDir_B
           setDir_F <- globs$ovpDir_F
         }
  )
  
  for (modus in moduser) {
    if (modus %in% c("K", "F", "B")) {
      FriskVDir <- ""
      GEOfilter <- character(0)
      
      if (modus == "K") {
        FriskVDir <- setDir_K
        GEOfilter <- c("K", "F", "L")
      } else if (modus == "B") {
        FriskVDir <- setDir_B
        GEOfilter <- c("B", "K", "F", "L")
      } else if (modus == "F") {
        FriskVDir <- setDir_F
        GEOfilter <- c("F", "L")
      }
      
      # FILTRER RADER
      filterA <- "(GEOniv %in% GEOfilter)"
      if (grepl("\\S", FVdscr$ALDER) & FVdscr$ALDER != "-") {
        FVdscr$ALDER <- gsub("^(\\d+)$", "\\1_\\1", FVdscr$ALDER)
        FVdscr$ALDER <- gsub("^(\\d+)_$", paste("\\1_", FGP$amax, sep = ""), FVdscr$ALDER)
        FVdscr$ALDER <- gsub("^_(\\d+)$", paste(FGP$amin, "_\\1", sep = ""), FVdscr$ALDER)
        FVdscr$ALDER <- gsub("^ALLE$", paste(FGP$amin, "_", FGP$amax, sep = ""), FVdscr$ALDER)
        filterA <- c(filterA, paste("ALDER=='", FVdscr$ALDER, "'", sep = ""))
      }
      for (tab in c("AARh", "KJONN", "INNVKAT", "UTDANN", "LANDBAK")) {
        if (grepl("\\S", FVdscr[[tab]]) & FVdscr[[tab]] != "-") {
          filterA <- c(filterA, paste(tab, "==", FVdscr[[tab]], sep = ""))
        }
      }
      if (grepl("\\S", FVdscr$EKSTRA_TAB) & FVdscr$EKSTRA_TAB != "-") {
        filterA <- c(filterA, FVdscr$EKSTRA_TAB)
        KUBE$ETAB <- FVdscr$EKSTRA_TAB
      }
      filter <- paste(filterA, collapse = " & ")
      FRISKVIK <- subset(KUBE, eval(parse(text = filter)))
      
      defrows <- nrow(subset(KHglobs$GeoKoder, FRA <= aargang & TIL > aargang & TYP == "O" & GEOniv %in% GEOfilter))
      if (nrow(FRISKVIK) != defrows) {
        KHerr(paste("FEIL I FRISKVIKFILTER", filter, "GIR bare", nrow(FRISKVIK), "/", defrows, "rader!"))
      }
      
      # SISTE RYDD KOLONNER (bare for TabKols)
      MissKol <- setdiff(c("GEO", "AAR", "KJONN", "ALDER", "UTDANN", "INNVKAT", "LANDBAK", "ETAB"), names(FRISKVIK))
      if (length(MissKol) > 0) {
        FRISKVIK[, (MissKol) := NA]
      }
      
      MissKol2 <- setdiff(c(globs$FriskvikTabs, globs$FriskvikVals), names(FRISKVIK))
      if (length(MissKol2) > 0) {
        KHerr(paste("FEIL: Kolonnene", MissKol2, "mangler i Friskvik!"))
        FRISKVIK[, (MissKol2) := NA]
      }
      
      if (grepl("\\S", FVdscr$ALTERNATIV_MALTALL)) {
        FRISKVIK$MALTALL <- FRISKVIK[[FVdscr$ALTERNATIV_MALTALL]]
        kastkols <- setdiff(globs$FriskvikVals, "MALTALL")
        FRISKVIK[, (kastkols) := NA]
      }
      
      # ALLTID prikk MEIS dersom SPVFLAGG > 0. Denne skal ut i profiler, og kan ikke være uprikket. 
      FRISKVIK[SPVFLAGG > 0, MEIS := NA]
      
      FRISKVIK <- FRISKVIK[, mget(c(globs$FriskvikTabs, globs$FriskvikVals))]
      
      versjonert <- TRUE
      # SKRIV UT
      if (versjonert == TRUE) {
        setPath <- paste(globs$path, "/", FriskVDir, aargang, "/csv/", sep = "")
        
        ## Check path if doesn't exist so create
        if (!fs::dir_exists(setPath)) fs::dir_create(setPath)
        
        utfiln <- paste0(setPath, FVdscr$INDIKATOR, "_", batchdate, ".csv")
        cat("-->> FRISKVIK EKSPORT:", utfiln, "\n")
        data.table::fwrite(FRISKVIK, utfiln, sep = ";", row.names = FALSE)
      }
    } else {
      cat("ADVARSEL!!!!!!!! modus ", modus, "i FRISKVIK støttes ikke\n")
    }
  }
}

#' LagQCkube (vl)
#' 
#' Saves QC kube containing standard columns defined in globs, 
#' and extra cols existing in the specific KUBE
LagQCKube <- function(KUBEid,
                      KUBE,
                      kubedims,
                      kubevals,
                      batchdate = batchdate,
                      globs = globs){
  QC <- copy(KUBE)
  qccols <- c(globs$QCTabs,setdiff(kubedims, globs$QCTabs),
              globs$QCVals,setdiff(kubevals, globs$QCVals),
              "SPVFLAGG")
  qcmisscols <- setdiff(qccols, names(QC))
  if (length(qcmisscols > 0)) {
    QC[, (qcmisscols) := NA]
  }
  QC <- QC[, ..qccols]
  
  return(QC)
}
