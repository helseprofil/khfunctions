LagTabellFraFil <- function(filbesk, FGP, batchdate = SettKHBatchDate(), diagnose = 0, globs = FinnGlobs(), versjonert = FALSE, echo = FALSE, dumps = list()) {
  is_kh_debug()
  
  klokke <- proc.time()
  # INNLESING
  filn <- filbesk$filn
  cat("\n#################\nLAGER TABELL FRA FIL:\n", filn, "\n")
  LestFil <- LesFil(filbesk, batchdate = batchdate, globs = globs, dumps = dumps)
  ok <- LestFil$ok
  DF <- LestFil$DF
  
  if (echo == TRUE) {
    cat("\nETTER INNLES\n#############################\n")
    print(head(DF))
  }
  
  #   #EVT SPESIALBEHANDLING
  #   if (!is.na(filbesk$RSYNT1)){
  #     filbesk$RSYNT1<-gsub("\\\r","\\\n",filbesk$RSYNT1)
  #     eval(parse(text=filbesk$RSYNT1))
  #   }
  # cat("\nETTER INNLES\n#############################\n")
  
  if (ok == 1) {
    
    # Omdøp kolonnenavn.
    # NB: for oversiktelighet i parameterfila gjøres dette både før og etter reshape
    # Dvs: kolonnenavn generert i reshape tillates å avvike fra standardnavn, disse endres etterpå
    # Valdiering skjer ved siste endring
    kolorgs <- globs$kolorgs
    # Finn kolonner spesifisert i filbesk
    HarCols <- filbesk[kolorgs[grepl("^[^-<]", filbesk[kolorgs])]]
    HarCols <- HarCols[HarCols %in% names(DF)]
    # Sett standard kolonnenavn
    names(DF) <- mapvalues(names(DF), HarCols, names(HarCols))
    
    # EVT INNFYLLING AV TABULATOR N?R DENNE ER INNRYKKET
    if (!is.na(filbesk$FYLLTAB)) {
      TAB <- as.character(read.csv(text = filbesk$FYLLTAB, header = FALSE, stringsAsFactors = FALSE))
      if (all(TAB %in% names(DF))) {
        DF[, TAB][DF[, TAB] == ""] <- NA
        DF[, TAB] <- na.locf(DF[, TAB], na.rm = FALSE)
      } else {
        TilFilLogg(filbesk$KOBLID, "FYLLTABERR", paste("Kolonner", paste(TAB[!TAB %in% names(DF)], collapse = ","), " finnes ikke", sep = ""), batchdate = batchdate, globs = globs)
        ok <- 0
      }
    }
    
    # EVT KASTING AV KOLONNER FØR RESHAPE (GJØR melt LETTERE Å BRUKE)
    if (!is.na(filbesk$KASTKOLS)) {
      eval(parse(text = paste("DF<-DF[,-", filbesk$KASTKOLS, "]", sep = "")))
    }
    
    if ("RESHAPEpre" %in% names(dumps)) {
      for (format in dumps[["RESHAPEpre"]]) {
        DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "RESHAPEpre", sep = "_"), globs = globs, format = format)
      }
    }
    
    # if (!(is.na(filbesk$RESHAPEid) || filbesk$RESHAPEid=='')){
    if (!(is.na(filbesk$RESHAPEvar) || filbesk$RESHAPEvar == "")) {
      rshpDF <- ReshapeTab(DF, filbesk, batchdate = batchdate, globs = globs)
      DF <- rshpDF$DF
      ok <- min(ok, rshpDF$ok)
      # cat("\nETTER RESHAPE\n#############################\n")
      # print(head(DF))
    }
    
    if ("RESHAPEpost" %in% names(dumps)) {
      for (format in dumps[["RESHAPEpost"]]) {
        DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "RESHAPEpost", sep = "_"), globs = globs, format = format)
      }
    }
    
    
    TilFilLogg(filbesk$KOBLID, "RESHAPEh", DFHeadToString(DF), batchdate = batchdate, globs = globs)
  }
  
  if (ok == 1) {
    
    # Må splitte evt kolonne fra MULTIHEAD
    if (!is.na(filbesk$MULTIHEAD)) {
      mhl <- LesMultiHead(filbesk$MULTIHEAD)
      DF[, mhl$colnames] <- str_split_fixed(DF[, mhl$varname], mhl$sep, 2)
    }
    
    if ("RSYNT2pre" %in% names(dumps)) {
      for (format in dumps[["RSYNT2pre"]]) {
        DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "RSYNT2pre", sep = "_"), globs = globs, format = format)
      }
    }
    
    # EVT SPESIALBEHANDLING
    if (!is.na(filbesk$RSYNT2)) {
      synt <- gsub("\\\r", "\\\n", filbesk$RSYNT2)
      error <- ""
      ok <- 1
      if (grepl("<STATA>", synt)) {
        synt <- gsub("<STATA>[ \n]*(.*)", "\\1", synt)
        RES <- KjorStataSkript(DF, synt, batchdate = batchdate, globs = globs)
        if (RES$feil != "") {
          stop("Noe gikk galt i kjøring av STATA \n", RES$feil)
          ok <- 0
        } else {
          DF <- RES$TABLE
        }
      } else {
        rsynterr <- try(eval(parse(text = synt)), silent = TRUE)
        if ("try-error" %in% class(rsynterr)) {
          ok <- 0
          error <- rsynterr
        }
      }
      if (ok == 0) {
        print(error)
        TilFilLogg(filbesk$KOBLID, "RSYNT2ERR", error, batchdate = batchdate, globs = globs)
      }
    }
    
    if ("RSYNT2post" %in% names(dumps)) {
      for (format in dumps[["RSYNT2post"]]) {
        DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "RSYNT2post", sep = "_"), globs = globs, format = format)
      }
    }
  }
  
  if (ok == 1) {
    
    # Omdøp kolonnenavn, runde 2.
    
    # Finn kolonner spesifisert i filbesk
    HarCols <- filbesk[kolorgs[grepl("^[^-<]", filbesk[kolorgs])]]
    HarCols <- HarCols[HarCols %in% names(DF)]
    # Sett standard kolonnenavn
    names(DF) <- mapvalues(names(DF), HarCols, names(HarCols))
    
    # Finn kolonner med standardverdi ('<.*>' i filbesk)
    DefVCols <- kolorgs[grepl("^<.*>", filbesk[kolorgs])]
    DefV <- matrix(sub("^<(.*)>$", "\\1", filbesk[DefVCols]), nrow = 1)
    # Sett standardverdier (får ikke til dette med enklere syntaks når det kan være tuppel, virker klønete)
    DF <- setNames(data.frame(DF, DefV, stringsAsFactors = FALSE), c(names(DF), DefVCols))
    
    # Sjekk for ikke-eksisterende/feilskrevet
    colerr <- ""
    if (!all(names(HarCols) %in% names(DF))) {
      colerr <- paste(colerr, "Kolonnene <", HarCols[!(names(HarCols) %in% names(DF))], "> finnes ikke\n")
      ok <- 0
    }
    
    # Sjekk at påkrevde kolonner finnes
    oblkols <- c("GEO", "AAR", "VAL1")
    if (!all(oblkols %in% names(DF))) {
      colerr <- paste(colerr, "KRITISK: Kolonnene <", oblkols[!(oblkols %in% names(DF))], "> finnes ikke\n")
      ok <- 0
    }
    if (ok == 0) {
      TilFilLogg(filbesk$KOBLID, "KOLNAVNERR", colerr, batchdate = batchdate, globs = globs)
    }
  }
  
  if (echo == TRUE) {
    cat("\nETTER TRINN2\n#############################\n")
    print(head(DF))
  }
  
  
  if (ok == 1) {
    # Merge GEO delt i to
    if (filbesk$GEOd2 != "-" & !is.na(filbesk$GEOd2)) {
      DF[, filbesk$GEOd2] <- gsub("^(\\d|\\d{3})$", "0\\1", DF[, filbesk$GEOd2])
      DF$GEO <- paste(DF$GEO, DF[, filbesk$GEOd2], sep = "")
    }
    
    # KAST USPESIFISERTE KOLONNER
    DF <- DF[, names(DF)[names(DF) %in% kolorgs]]
  }
  
  if (echo == TRUE) {
    cat("\nETTER TRINN3\n#############################\n")
    print(head(DF))
  }
  TilFilLogg(filbesk$KOBLID, "INNLES_OK", ok, batchdate = batchdate, globs = globs)
  
  
  if (!is.na(filbesk$GRUNNKRETS) && filbesk$GRUNNKRETS == 1) {
    setDT(DF)
    setkeyv(DF, "GEO")
    setkeyv(globs$GkBHarm, "GK")
    DF <- globs$GkBHarm[DF]
    DF[is.na(Bydel2004), Bydel2004 := paste(substr(GK, 1, 4), "00", sep = "")]
    DF[, GK := NULL]
    setnames(DF, "Bydel2004", "GEO")
    tabkols <- names(DF)[!grepl("^VAL\\d$", names(DF))]
    valkols <- names(DF)[grepl("^VAL\\d$", names(DF))]
    setkeyv(DF, tabkols)
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
  if ("KODEBOKpre" %in% names(dumps)) {
    for (format in dumps[["KODEBOKpre"]]) {
      DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "KODEBOKpre", sep = "_"), globs = globs, format = format)
    }
  }
  sqlQuery(globs$log, paste("DELETE * FROM KODEBOK_LOGG WHERE KOBLID=", filbesk$KOBLID, sep = ""))
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
        tabKB$OMK <- gsub("^-$", "XXXKASTXXX", tabKB$KBOMK) # Dirty tricks. Beskytter '-' mot uttrykket nedenfor, uten å gjøre regexp unødvendig komplisert
        tabKB$OMK <- gsub("[- ,\\/]", "_", tabKB$KBOMK)
        tabKB$OMK <- gsub("XXXKASTXXX", "-", tabKB$KBOMK)
        tabKB$OK <- 1
        SkrivKBLogg(KB = tabKB, type = tab, filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
        DF[, tab] <- mapvalues(DF[, tab], tabKB$ORG, tabKB$OMK, warn_missing = FALSE)
      }
    }
    
    ## GEO
    # RENSK GEO (Alle er legit inntil videre??? Eller kod til 9999???)
    if ("GEO" %in% names(DF)) {
      org <- setNames(as.data.frame(table(DF$GEO, useNA = "ifany"), stringsAsFactors = FALSE), c("ORG", "FREQ"))
      geo <- GEOvask(org, filbesk = filbesk, batchdate = batchdate, globs = globs)
      
      SkrivKBLogg(KB = geo, type = "GEO", filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
      TilFilLogg(filbesk$KOBLID, "GEO_ok", ifelse(0 %in% geo$OK, 0, 1), batchdate = batchdate, globs = globs)
      
      DF$GEOniv <- mapvalues(DF$GEO, geo$ORG, geo$GEOniv, warn_missing = FALSE)
      DF$FYLKE <- mapvalues(DF$GEO, geo$ORG, geo$FYLKE, warn_missing = FALSE)
      DF$GEO <- mapvalues(DF$GEO, geo$ORG, geo$OMK, warn_missing = FALSE) # NB: rekkefølge har betydning
    }
    # RENSK ALDER
    # Sett intervall for alder ALLE
    if ("ALDER" %in% names(DF)) {
      DF$ALDER <- gsub(" \\Wr\\b", " år", DF$ALDER, perl = TRUE) # Problem med codebook i dbf
      
      org <- setNames(as.data.frame(table(DF$ALDER, useNA = "ifany"), stringsAsFactors = FALSE), c("ORG", "FREQ"))
      alder <- ALDERvask(org, FGP = FGP, filbesk = filbesk, batchdate = batchdate, globs = globs)
      
      # Kast der ALDEr koder til "-" (må ta det her og ikek generelle under pga intervall)
      DF <- subset(DF, !ALDER %in% subset(alder, OMK == "-")$ORG)
      
      SkrivKBLogg(KB = alder, type = "ALDER", filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
      TilFilLogg(filbesk$KOBLID, "ALDER_ok", ifelse(globs$alder_illeg %in% alder$OMK, 0, 1), batchdate = batchdate, globs = globs)
      DF$ALDERl <- as.integer(mapvalues(DF$ALDER, alder$ORG, alder$LO, warn_missing = FALSE))
      DF$ALDERh <- as.integer(mapvalues(DF$ALDER, alder$ORG, alder$HI, warn_missing = FALSE))
      # DF$ALDERl<-as.numeric(mapvalues(DF$ALDER,alder$ORG,alder$LO,warn_missing = FALSE))
      # DF$ALDERh<-as.numeric(mapvalues(DF$ALDER,alder$ORG,alder$HI,warn_missing = FALSE))
    }
    
    
    # RENSK KJ?NN
    if ("KJONN" %in% names(DF)) {
      org <- setNames(as.data.frame(table(DF$KJONN, useNA = "ifany"), stringsAsFactors = FALSE), c("ORG", "FREQ"))
      kjonn <- KJONNvask(org, filbesk = filbesk, batchdate = batchdate, globs = globs)
      
      SkrivKBLogg(KB = kjonn, type = "KJONN", filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
      TilFilLogg(filbesk$KOBLID, "KJONN_ok", ifelse(globs$kjonn_illeg %in% kjonn$OMK, 0, 1), batchdate = batchdate, globs = globs)
      
      DF$KJONN <- as.integer(mapvalues(DF$KJONN, kjonn$ORG, kjonn$OMK, warn_missing = FALSE))
    }
    
    # AAR TIL INTERVALL
    if ("AAR" %in% names(DF)) {
      org <- setNames(as.data.frame(table(DF$AAR, useNA = "ifany"), stringsAsFactors = FALSE), c("ORG", "FREQ"))
      
      aar <- AARvask(org, filbesk = filbesk, batchdate = batchdate, globs = globs)
      
      SkrivKBLogg(KB = aar, type = "AAR", filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
      TilFilLogg(filbesk$KOBLID, "AAR_ok", ifelse(globs$aar_illeg %in% aar$OMK, 0, 1), batchdate = batchdate, globs = globs)
      
      # Kast der AAR koder til "-" (må ta det her og ikek generelle under pga intervall)
      DF <- subset(DF, !AAR %in% subset(aar, OMK == "-")$ORG)
      
      DF$AARl <- as.integer(mapvalues(DF$AAR, aar$ORG, aar$LO, warn_missing = FALSE))
      DF$AARh <- as.integer(mapvalues(DF$AAR, aar$ORG, aar$HI, warn_missing = FALSE))
    }
    
    
    # RENSK UTDANN
    if ("UTDANN" %in% names(DF)) {
      org <- setNames(as.data.frame(table(DF$UTDANN, useNA = "ifany"), stringsAsFactors = FALSE), c("ORG", "FREQ"))
      utdann <- UTDANNvask(org, filbesk = filbesk, batchdate = batchdate, globs = globs)
      
      SkrivKBLogg(KB = utdann, type = "UTDANN", filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
      TilFilLogg(filbesk$KOBLID, "UTDANN_ok", ifelse(globs$utdann_illeg %in% utdann$OMK, 0, 1), batchdate = batchdate, globs = globs)
      
      DF$UTDANN <- as.integer(mapvalues(DF$UTDANN, utdann$ORG, utdann$OMK, warn_missing = FALSE))
    }
    
    
    # RENSK INNVKAT
    if ("INNVKAT" %in% names(DF)) {
      org <- setNames(as.data.frame(table(DF$INNVKAT, useNA = "ifany"), stringsAsFactors = FALSE), c("ORG", "FREQ"))
      innvkat <- INNVKATvask(org, filbesk = filbesk, batchdate = batchdate, globs = globs)
      
      SkrivKBLogg(KB = innvkat, type = "INNVKAT", filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
      TilFilLogg(filbesk$KOBLID, "INNVKAT_ok", ifelse(globs$innvkat_illeg %in% innvkat$OMK, 0, 1), batchdate = batchdate, globs = globs)
      
      DF$INNVKAT <- as.integer(mapvalues(DF$INNVKAT, innvkat$ORG, innvkat$OMK, warn_missing = FALSE))
    }
    
    
    # RENSK LANDBAK
    if ("LANDBAK" %in% names(DF)) {
      org <- setNames(as.data.frame(table(DF$LANDBAK, useNA = "ifany"), stringsAsFactors = FALSE), c("ORG", "FREQ"))
      landbak <- LANDBAKvask(org, filbesk = filbesk, batchdate = batchdate, globs = globs)
      
      SkrivKBLogg(KB = landbak, type = "LANDBAK", filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
      TilFilLogg(filbesk$KOBLID, "LANDBAK_ok", ifelse(globs$landbak_illeg %in% landbak$OMK, 0, 1), batchdate = batchdate, globs = globs)
      
      DF$LANDBAK <- as.integer(mapvalues(DF$LANDBAK, landbak$ORG, landbak$OMK, warn_missing = FALSE))
    }
    
    if ("KODEBOKpost" %in% names(dumps)) {
      for (format in dumps[["KODEBOKpost"]]) {
        DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "KODEBOKpost", sep = "_"), globs = globs, format = format)
      }
    }
    
    # DROPP ALLE MED '-' I TABULERING (merk: AAR og ALDER måtte tas over pga intervall)
    DF <- subset(DF, rowSums(DF[, names(DF) %in% globs$taborgs] == "-") == 0)
    
    # VASK VERDIER. Litt annen prosess, bruker KB, men tabulerer bare ikke-numeriske.
    # Setter numerisk, med flagg for type NA
    for (val in c("VAL1", "VAL2", "VAL3")) {
      # Bedre, men funker ikke i forhold til logg
      # for (val in names(DF)[grepl("VAL\\d+$",names(DF))]){
      if (val %in% names(DF)) {
        DF[is.na(DF[, val]), val] <- ""
        
        valKB <- KBomkod(DF[, val], type = val, valsubs = TRUE, filbesk = filbesk, batchdate = batchdate, globs = globs)
        valKBut <- valKB$subsant
        
        valok <- 1
        valf <- paste(val, ".f", sep = "")
        vala <- paste(val, ".a", sep = "")
        valomk <- paste(val, "omk", sep = "")
        
        
        # Lag omkodet verdi med numerisk. Ikke numerisk blir foreløpig NA
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
          
          
          # Gjør nytt forsøk på numerisk konvertering etter omkoding
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
          DF[nonNum, valomk] <- as.numeric(mapvalues(DF[nonNum, val], valKB$ORG, valKB$kbNUM, warn_missing = FALSE))
          # DF[nonNum,valomk]<-suppressWarnings(as.numeric(mapvalues(DF[nonNum,val],valKB$ORG,valKB$kbNUM,warn_missing = FALSE)))
          DF[nonNum, valf] <- as.integer(mapvalues(DF[nonNum, val], valKB[, "ORG"], valKB[, "FLAG"], warn_missing = FALSE))
        }
        
        if (nrow(valKBut) > 0) {
          SkrivKBLogg(KB = valKBut, type = val, filbesk = filbesk, FGP$FILGRUPPE, batchdate = batchdate, globs = globs)
        }
        
        
        DF[, val] <- NULL
        DF <- setNames(DF, mapvalues(names(DF), valomk, val))
        
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
    if (echo == TRUE) {
      print(Kols)
      cat("Nest siste trinn\n#########################\n")
    }
    
    # print(filbesk)
    # kAN KRÆSJE VED UKJENT KOLNAVN!
    # print(FGP)
    DF <- DF[, Kols]
    
    # Kast rader for inaktive GEO med alle VAL==NA (må gjøres fordi alle kommunekoder gir utrapportert tall fra STATBANK og 0/NA er ikke nøytralt for ikke-sumerbare kolonner, jfr MEDIANINNT)
    # Merk at ekte NA settes inn igjen når det rektangulariseres på aktive kommuner ved kubeproduksjon
    GeoFra <- setNames(globs$GeoKoder$FRA, globs$GeoKoder$GEO)
    GeoTil <- setNames(globs$GeoKoder$TIL, globs$GeoKoder$GEO)
    valkols <- FinnValKols(names(DF))
    # Skjønner ikke hvorfor dette ikke funker
    
    DF2 <- DF[!((unlist(GeoTil[DF$GEO]) <= DF$AARl | unlist(GeoFra[DF$GEO]) >= DF$AARh) & rowSums(is.na(data.frame(DF[, valkols]))) == length(valkols)), ]
    DF <- DF2
    
    
    # Aggreger ned. Unntaksvis der filene er "ucollapset"
    # etter f.eks omkoding av alder til aldersgrupper
    # Om ikke dette gjøres blir det masse dubletter
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
    # DF<-DF[0,] #Fungerer ikke mht class, som kan være feil
  }
  
  return(DF)
}

#' LesFil (kb)
#'
#' @param filbesk 
#' @param batchdate 
#' @param globs 
#' @param dumps 
LesFil <- function(filbesk, batchdate = SettKHBatchDate(), globs = FinnGlobs(), dumps = character()) {
  is_kh_debug()
  
  klokke <- proc.time()
  DF <- data.frame()
  ok <- 1
  filn <- filbesk$filn
  format <- filbesk$FORMAT
  opt <- filbesk$INNLESARG
  
  
  # Initier log
  sqlQuery(globs$log, paste("DELETE * FROM INNLES_LOGG WHERE KOBLID=", filbesk$KOBLID, "AND SV='S'", sep = ""))
  sqlQuery(globs$log, paste("INSERT INTO INNLES_LOGG ( KOBLID,BATCH, SV, FILGRUPPE) SELECT =", filbesk$KOBLID, ",'", batchdate, "', 'S','", FinnFilGruppeFraKoblid(filbesk$KOBLID), "'", sep = ""))
  
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
          INNLES <- try(as.data.frame(read.spss(file = filn, use.value.labels = FALSE, max.value.labels = 0), stringsAsFactors = FALSE), silent = TRUE)
          # ALternativ metode: T<-spss.get(file=fil)
        } else if (format == "DBF") {
          # DEV sl? av Field name: '***NULL***' changed to: 'X...NULL...'
          INNLES <- try(suppressMessages(read.dbf(file = filn, as.is = TRUE)), silent = TRUE)
        } else if (format == "SAS") {
          INNLES <- try(read.sas7bdat(file = filn), silent = TRUE)
        } else if (format == "HTML") {
          INNLES <- try(eval(parse(text = paste("DF<-readHTMLTable(doc=filn,as.data.frame = TRUE,stringsAsFactors=FALSE", ifelse(is.na(opt), "", paste(",", opt, sep = "")), ")", sep = ""))), silent = TRUE)
        }
        if ("try-error" %in% class(INNLES)) {
          innleserr <- INNLES
          ok <- 0
        } else {
          DF <- INNLES
        }
      }
      # Må sikre at data.frame, noen filer kan være bare en skalar (jfr ENPERSON)
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
    # Gjør om innlest CSV-aktig til tabell
    
    if (format %in% c("CSV", "XLS", "XLSX")) {
      eval(parse(text = paste("DF<-cSVmod(DF,filbesk,", ifelse(is.na(opt), "", paste(",", opt, sep = "")), ",globs=globs)", sep = "")))
    }
    
    # Sett header manuelt
    # IKKE robust for feil parameter
    if (!is.na(filbesk$MANHEADER)) {
      mh <- unlist(str_split(filbesk$MANHEADER, "="))
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
      
      # Skjønner ikke helt hvorfor ikke denne enkler funker:
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
    
    if ("RSYNT1pre" %in% names(dumps)) {
      for (format in dumps[["RSYNT1pre"]]) {
        
        if(format == "STATA"){
          # Add special variables used in RSYNT1 STATA code
          DF$filgruppe <- filbesk$FILGRUPPE
          DF$delid <- filbesk$DELID
          DF$tab1_innles <- filbesk$TAB1
        }
        
        DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "RSYNT1pre", sep = "_"), globs = globs, format = format)
        
        if(format == "STATA"){
          # Delete special variables 
          DF[c("filgruppe", "delid", "tab1_innles")] <- NULL
        }
      }
    }
    
    # EVT SPESIALBEHANDLING
    if (!is.na(filbesk$RSYNT1)) {
      synt <- gsub("\\\r", "\\\n", filbesk$RSYNT1)
      error <- ""
      ok <- 1
      if (grepl("<STATA>", synt)) {
        synt <- gsub("<STATA>[ \n]*(.*)", "\\1", synt)
        
        ## These variables are to be use in Stata process (request from Jørgen)
        ## They will be deleted when Stata RSYNT1 is completed below
        DF$filgruppe <- filbesk$FILGRUPPE
        DF$delid <- filbesk$DELID
        DF$tab1_innles <- filbesk$TAB1
        
        RES <- KjorStataSkript(DF, synt, batchdate = batchdate, globs = globs)
        if (RES$feil != "") {
          stop("Noe gikk galt i kjøring av STATA \n", RES$feil)
          ok <- 0
        } else {
          DF <- RES$TABLE
        }
      } else {
        rsynterr <- try(eval(parse(text = synt)), silent = TRUE)
        if ("try-error" %in% class(rsynterr)) {
          ok <- 0
          error <- rsynterr
        }
      }
      if (ok == 0) {
        print(error)
        TilFilLogg(filbesk$KOBLID, "RSYNT1ERR", error, batchdate = batchdate, globs = globs)
      }
    }
  }
  
  if ("RSYNT1post" %in% names(dumps)) {
    for (format in dumps[["RSYNT1post"]]) {
      DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "RSYNT1post", sep = "_"), globs = globs, format = format)
    }
  }
  
  # sink(file=paste(globs$path,"/hoder.txt",sep=""),append=TRUE)
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
    csvT <- as.data.frame(fread(filn, header = FALSE, skip = 0, colClasses = "character", sep = sep, na.strings = na.strings))
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
#' @param globs 
#' @param ... 
cSVmod <- function(DF, filbesk, header = TRUE, skip = 0, slettRader = integer(0), sisteRad = -1, TomRadSlutt = FALSE, FjernTommeRader = FALSE, FjernTommeKol = TRUE, globs = FinnGlobs(), ...) {
  # Ved bruk av undertabeller med titler som ikke står i egen kolonne
  # Lager egen kolonne av undertitler som blir ekta TAB
  # Ikke så veldig elegant, men funker for de får tilfellene der dette trengs og som ellers ville trengt håndsøm
  # Syntaks UNDERTABLOK er TAB:kolonne:kommasep liste undertitler:kommasep liste/skalar offset av disse (dvs antall raders forrykking)
  is_kh_debug()
  
  if (!is.na(filbesk$UNDERTABLOK)) {
    utl <- unlist(str_split(filbesk$UNDERTABLOK, ":"))
    loks <- as.numeric(unlist(str_split(utl[3], ",")))
    offsets <- as.numeric(unlist(str_split(utl[4], ",")))
    nytab <- character(nrow(DF))
    nytab[loks + offsets] <- DF[loks, as.numeric(utl[2])]
    nytab[nytab == ""] <- NA
    nytab <- na.locf(nytab, na.rm = FALSE)
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
  # Må sikre at data.frame, noen filer kan være bare en skalar (jfr ENPERSON)
  DF <- as.data.frame(DF, stringsAsFactors = FALSE)
  
  # Sett header. Default er vanlige Excel-kolonnenavn
  names(DF) <- globs$XLScols[1:length(names(DF))]
  
  # Bruk av flernivå header.
  # Ikke særlig elegant syntaks, men prinsippet er rett fram
  # Disse pastes (evt) sammen til en header
  # Etter reshape splittes kolonneraden (som nå har blitt en kolonne)
  # i sine respektive kolonner
  # Kan også være pastet sammen originalt
  # Syntaks gir radnummer for de ulike leddene i multihead "c(TABNAVN1=rad1,TABNAVN2=rad2,...)
  if (!is.na(filbesk$MULTIHEAD)) {
    # Prossesser parameterstreng for multihead, gir liste med relevante deler
    mhl <- LesMultiHead(filbesk$MULTIHEAD)
    # Juster radnummerering for skip
    mhl$rader <- mhl$rader - skip
    headers <- DF[mhl$rader, ]
    headers[headers == ""] <- NA
    # Fyll inn ved "sparse" utfylling, slik som ved "innrykket" tabulering i kolonner
    headers <- na.locf(t(headers), na.rm = FALSE)
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
#' @param globs 
#' @param brukfread 
#' @param na.strings 
#' @param ryddOpp 
#' @param ... 
Xls2R.KH <- function(xlsfil, ark = "", globs = FinnGlobs(), brukfread = TRUE, na.strings = c("NA"), ryddOpp = 1, ...) {
  is_kh_debug()
  
  err <- ""
  ok <- 1
  DF <- data.frame()
  # step 1: Validate sheetname with fuzzy match
  # rdbh<-odbcConnectExcel2007(xlsfil)
  # rdbh<-odbcConnectExcel(xlsfil)
  # tables<-sqlTables(rdbh)$TABLE_NAME
  # close(rdbh)
  
  tables <- excel_sheets(xlsfil)
  
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
    INNLES <- try(as.data.frame(read_excel(xlsfil, sheet = ark, col_names = FALSE, col_types = "text", skip = 0, na = na.strings)))
    if ("try-error" %in% class(INNLES)) {
      err <- INNLES
      ok <- 0
    } else {
      DF <- setNames(INNLES, globs$XLScols[1:ncol(INNLES)])
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
#' @param globs 
FinnFilgruppeParametre <- function(gruppe, batchdate = SettKHBatchDate(), globs = FinnGlobs()) {
  is_kh_debug()
  
  dbh <- globs$dbh
  datef <- format(strptime(batchdate, "%Y-%m-%d-%H-%M"), "#%Y-%m-%d#")
  FGPaktiv <- as.integer(sqlQuery(globs$dbh, paste("SELECT count(*) FROM FILGRUPPER WHERE FILGRUPPE='", gruppe, "'",
                                                   "AND VERSJONFRA<=", datef, " AND VERSJONTIL>", datef, "
                                        ",
                                                   sep = ""
  ), as.is = TRUE))
  FGPfinnes <- as.integer(sqlQuery(globs$dbh, paste("SELECT count(*) FROM FILGRUPPER WHERE FILGRUPPE='", gruppe, "'", sep = ""), as.is = TRUE))
  ok <- 0
  resultat <- list()
  if (FGPfinnes == 0) {
    KHerr(paste("Filgruppe", gruppe, "finnes ikke. Droppes."))
  } else if (FGPaktiv == 0) {
    KHerr(paste("Filgruppe", gruppe, "finnes, men er satt inaktiv. Droppes."))
  } else {
    ok <- 1
    FGP <- as.list(sqlQuery(globs$dbh, paste("SELECT * FROM FILGRUPPER WHERE FILGRUPPE='", gruppe, "'",
                                             "AND VERSJONFRA<=", datef, " AND VERSJONTIL>", datef, "
                                          ",
                                             sep = ""
    ), as.is = TRUE))
    # Sette endelig default alder ALLE
    # Default er 0_ALDinf    der ALDinf er global parameter i HOVEDPARAMETRE
    amin <- 0
    amax <- as.numeric(sqlQuery(dbh, "SELECT ALDinf FROM HOVEDPARAMETREg")[1])
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
  }
  
  gc()
  return(c(resultat, list(ok = ok)))
}

#' FinnFilBeskGruppe (kb)
#'
#' @param filgruppe 
#' @param batchdate 
#' @param globs 
#' @param test 
#' @param testID 
FinnFilBeskGruppe <- function(filgruppe, batchdate = NULL, globs = FinnGlobs(), test = runtest, testID = testfiles) {
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
              WHERE INNLESING.FILGRUPPE='", filgruppe, "'
              AND ORIGINALFILER.IBRUKFRA<=", datef, "
              AND ORIGINALFILER.IBRUKTIL>", datef, "
              AND INNLESING.VERSJONFRA<=", datef, "
              AND INNLESING.VERSJONTIL>", datef, sep = "")
  fb <- sqlQuery(globs$dbh, sqlt, stringsAsFactors = FALSE)
  
  ## Picking up files path that is refered to in ORIGINALFILER
  
  if (test) {
    fb <- subset(fb, KOBLID %in% testID)
  }
  
  invisible(fb)
}

#' SjekkDuplikater (kb)
#'
#' @param FG 
#' @param filgruppe 
#' @param FullResult 
#' @param echo 
#' @param batchdate 
#' @param versjonert 
#' @param globs 
SjekkDuplikater <- function(FG, filgruppe, FullResult = FALSE, echo = 0, batchdate = SettKHBatchDate(), versjonert = FALSE, globs = FinnGlobs()) {
  is_kh_debug()
  
  HarDuplikater <- 0
  if (identical(class(FG), "data.frame")) {
    FG <- data.table(FG)
  }
  orgkeys <- key(FG)
  tabkols <- globs$DefDesign$DesignKolsFA
  tabkols <- tabkols[tabkols %in% names(FG)]
  valkols <- FinnValKols(names(FG))
  setkeym(FG, tabkols)
  
  dubi <- duplicated(FG)
  
  DUB <- data.table()
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
      setkeyv(DUB, names(DUB))
      setkeyv(DUBp, names(DUB))
      DUB <- DUBp[DUB]
      DUB[is.na(dNOp), dNOp := 0]
      DUB[is.na(antVp), antVp := 0]
      DUB[is.na(antKp), antKp := 0]
    } else {
      DUB[, dNOp := 0]
      DUB[, antVp := 0]
      DUB[, antKp := 0]
    }
    
    # Hold ##99-geokoder utenom. Her blir det lagd dubeltter når to illegitime KNR blir samme ##99 etc
    DUBg <- subset(DUB, !grepl("99$", GEO))
    if (nrow(DUBg) > 0) {
      eval(parse(text = paste(
        "DUBg[,dNOg:=rank(", paste(valkols, collapse = ","), "),by=tabkols]",
        sep = ""
      )))
      DUBg[, antVg := length(unique(dNOg)), by = tabkols]
      DUBg[, antKg := length(unique(KOBLID)), by = tabkols]
      setkeyv(DUB, names(DUB))
      setkeyv(DUBg, names(DUB))
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
    sqlQuery(globs$log, paste("DELETE * FROM DUBLETT WHERE FILGRUPPE='", filgruppe, "' AND SV='S'", sep = ""))
    # Legg til resterende kolonner
    # Må ha ok kolonnenavn til database
    setnames(DUB, names(DUB), gsub("^(VAL\\d+)\\.f$", "\\1f", names(DUB)))
    
    tmp <- sqlQuery(globs$log, "SELECT * FROM DUBLETT WHERE KOBLID=-1")
    tmp2 <- tmp
    tmp[1:nrow(DUB), ] <- NA
    tmp[, intersect(names(tmp), names(DUB))] <- DUB[, intersect(names(tmp), names(DUB)), with = FALSE]
    tmp$FILGRUPPE <- filgruppe
    tmp$BATCHID <- batchdate
    tmp$SV <- "S"
    if (echo >= 1) {
      cat("HAR", nrow(DUB), "duplikater (", nrow(subset(DUB, dNOg > 0)), "), det kan gjerne ta", nrow(DUB) / 100, "sek  å skrive ut logg\n")
    }
    if (echo >= 2) {
      print(DUB)
    }
    if (nrow(DUB) < 1000) {
      sqlSave(globs$log, tmp, "DUBLETT", rownames = FALSE, append = TRUE)
      if (echo >= 1) {
        cat("Ferdig dublogg\n")
      }
      if (versjonert == TRUE) {
        tmp$SV <- "V"
        sqlSave(globs$log, tmp, "DUBLETT", rownames = FALSE, append = TRUE)
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
#' @param globs 
#'
#' @return
#' @export
#'
#' @examples
ReshapeTab <- function(DELF, filbesk, batchdate = SettKHBatchDate(), globs = FinnGlobs()) {
  # Reshape av DELF basert på parametre i filbesk
  is_kh_debug()
  
  ok <- 1
  if (!(is.na(filbesk$RESHAPEid) || filbesk$RESHAPEid == "")) {
    idvars <- eval(parse(text = paste("c(", filbesk$RESHAPEid, ")")))
  } else {
    idvars <- NULL
  }
  # idvars<-SettCols(filbesk$RESHAPEid,names(T))
  # mevars<-SettCols(filbesk$RESHAPEmeas,names(T))
  mevars <- NULL
  if (!(is.na(filbesk$RESHAPEmeas) || filbesk$RESHAPEmeas == "")) {
    mevars <- eval(parse(text = paste("c(", filbesk$RESHAPEmeas, ")")))
  }
  varname <- "variable"
  valname <- "value"
  # varname må tas fra MULTIHEAD om denne brukes
  if (!is.na(filbesk$MULTIHEAD)) {
    varname <- LesMultiHead(filbesk$MULTIHEAD)$varname
  } else if (!(is.na(filbesk$RESHAPEvar) || filbesk$RESHAPEvar == "")) {
    varname <- as.character(filbesk$RESHAPEvar)
  }
  if (!(is.na(filbesk$RESHAPEval) || filbesk$RESHAPEval == "")) {
    valname <- as.character(filbesk$RESHAPEval)
  }
  
  if (all(idvars %in% names(DELF)) & (is.null(mevars) | all(mevars %in% names(DELF)))) {
    DELF[, idvars] <- sapply(DELF[, idvars], as.character) # Må være av samme type for at ikke reshape skal kræsje
    if (!is.null(mevars)) {
      DELF <- data.table::melt(as.data.table(DELF), id.vars = idvars, measure.vars = mevars, variable.name = varname, value.name = valname, na.rm = FALSE)
    } else {
      DELF <- data.table::melt(as.data.table(DELF), id.vars = idvars, variable.name = varname, value.name = valname, na.rm = FALSE)
    }
    data.table::setDF(DELF)
    DELF[, varname] <- as.character(DELF[, varname]) # Kan ha blitt factor, og det gir krøll senere
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

# Necessary to create this object?
subsant <- data.frame(ORG = character(0), KBOMK = character(0), OMK = character(0), FREQ = integer(0), OK = integer(0))

#' KBomkod (kb)
#'
#' @param org 
#' @param type 
#' @param filbesk 
#' @param valsubs 
#' @param batchdate 
#' @param globs 
KBomkod <- function(org, type, filbesk, valsubs = FALSE, batchdate = NULL, globs = FinnGlobs()) {
  is_kh_debug()
  
  datef <- format(Sys.time(), "#%Y-%m-%d#")
  if (!is.null(batchdate)) {
    datef <- format(strptime(batchdate, "%Y-%m-%d-%H-%M"), "#%Y-%m-%d#")
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
  kbok <- sqlQuery(globs$dbh, sql, as.is = TRUE)
  kbok[is.na(kbok)] <- ""
  subsant <- data.frame(ORG = character(0), KBOMK = character(0), OMK = character(0), FREQ = integer(0), OK = integer(0))
  if (nrow(kbok) > 0) {
    KBsubs <- subset(kbok, TYPE == "SUB") # Regulæruttrykk
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
          omk <- mapvalues(omk, KB$ORGKODE, KB$NYKODE, warn_missing = FALSE)
        }
      }
    } else {
      omk <- mapvalues(omk, KB$ORGKODE, KB$NYKODE, warn_missing = FALSE)
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
#' @param globs 
GEOvask <- function(geo, filbesk = data.frame(), batchdate = SettKHBatchDate(), globs = FinnGlobs()) {
  is_kh_debug()
  
  if (nrow(filbesk) == 0) {
    geo <- setNames(as.data.frame(geo, stringsAsFactors = FALSE), c("GEO"))
    geo$KBOMK <- geo[, 1]
  } else {
    geo$KBOMK <- KBomkod(geo$ORG, type = "GEO", filbesk = filbesk, batchdate = batchdate, globs = globs)
    if (!is.na(filbesk$TKNR)) {
      suppressWarnings(geo$KBOMK[geo$ORG == geo$KBOMK] <- mapvalues(geo$ORG[geo$ORG == geo$KBOMK], globs$TKNR$ORGKODE, globs$TKNR$NYKODE, warn_missing = FALSE))
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
  # Må bli mer avansert for å bli robust. Koder nå 1 til flere (Nes, etc)
  UGeo <- data.frame(NAVN = geo$OMK[!grepl("^\\d+$", geo$OMK)])
  if (nrow(UGeo) > 0) {
    GeoNavn <- sqlQuery(globs$dbh, "SELECT * from GeoNavn", as.is = TRUE)
    omk <- sqldf("SELECT GEO, UGeo.NAVN FROM UGeo INNER JOIN GeoNavn ON UGeo.NAVN=GeoNavn.NAVN")
    geo$OMK <- mapvalues(geo$OMK, omk$NAVN, omk$GEO, warn_missing = FALSE)
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
  
  # Sjekk om legitime 99-ukjente
  ukjent <- ukjent[ukjent99 %in% globs$GeoKoder$GEO]
  ukjent99 <- ukjent99[ukjent99 %in% globs$GeoKoder$GEO]
  geo$OMK <- mapvalues(geo$OMK, ukjent, ukjent99, warn_missing = FALSE)
  
  ukjent <- geo$OMK[!(geo$OMK %in% c(globs$GeoKoder$GEO, "-"))]
  heltukjent <- ukjent
  heltukjent[nchar(ukjent) == 6] <- 999999
  heltukjent[nchar(ukjent) == 4] <- 9999
  heltukjent[nchar(ukjent) == 2] <- 99
  geo$OMK <- mapvalues(geo$OMK, ukjent, heltukjent, warn_missing = FALSE)
  
  # Sett GEOniv
  geo$GEOniv <- as.character(NA)
  geo$GEOniv[nchar(geo$OMK) == 8] <- "G"
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
  # DEVELOP: bare et GEOniv, sett ukjent på dette nivået
  # Fil har bare kommunedata -> bruker 8888
  if (sum(c("G", "B", "F", "L") %in% geo$GEOniv) == 0) {
    geo$OMK[geo$OK == 0] <- "8888"
    geo$GEOniv[geo$OK == 0] <- "K"
  } else {
    geo$OMK[geo$OK == 0] <- globs$geo_illeg
  }
  # Sett fylke
  geo$FYLKE <- NA
  subfylke <- which(geo$GEOniv %in% c("G", "S", "K", "F", "B"))
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
#' @param globs 
ALDERvask <- function(alder, filbesk = data.frame(), FGP = list(amin = 0, amax = 120), batchdate = SettKHBatchDate(), globs = FinnGlobs()) {
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
  # alder$OMK<-sub("^ *under (\\d+)( +år| *$)","_\\1",alder$OMK,ignore.case = TRUE)  # Dette blri galt, må erstatte med "_(\\1-1)", men får ikke det til. Må bruke kdoebok
  alder$OMK <- sub("^ *(\\d+)( ?r|) *(og|eller) (yngre|under)", "_\\1", alder$OMK, ignore.case = TRUE)
  alder$OMK <- sub("^ *(\\d+) *( +år| *$)", "\\1_\\1", alder$OMK, ignore.case = TRUE)
  alder$OMK <- sub("^ *(Alle( *aldre.*|)|Totalt{0,1}|I alt) *$", paste(amin, "_", amax, sep = ""), alder$OMK, ignore.case = TRUE)
  alder$OMK <- sub("^ *(Ukjent|Uoppgitt|Ikke kjent) *$", globs$alder_ukjent, alder$OMK, ignore.case = TRUE)
  
  # alder$OMK[is.na(alder$OMK)]<-"999_999"
  alder$OMK <- sub("^(\\d+)_$", paste("\\1_", amax, sep = ""), alder$OMK)
  alder$OMK <- sub("^_(\\d+)$", paste(amin, "_\\1", sep = ""), alder$OMK)
  
  # Ukjent????????
  # !Må ha to amax, en for ukjent som er høyere, se også ulest under!!!
  
  okformat <- grepl("^\\d+_\\d+$|^-$", alder$OMK)
  # Ugyldig verdi/ukjent kode
  alder$OMK[!okformat] <- globs$alder_illeg
  alder$OK[!okformat] <- 0
  
  # Sett intervall
  alder[, c("LO", "HI")] <- suppressWarnings(matrix(as.integer(str_split_fixed(alder$OMK, "_", 2)), ncol = 2))
  # Ugyldig intervall
  alder$OMK[alder$HI < alder$LO] <- globs$alder_illeg
  alder$OMK[alder$HI > 130 & !(alder$OMK %in% c(globs$alder_illeg, globs$alder_ukjent))] <- globs$alder_illeg
  alder[, c("LO", "HI")] <- suppressWarnings(matrix(as.integer(str_split_fixed(alder$OMK, "_", 2)), ncol = 2))
  return(alder)
}

#' KJONNvask (kb)
#'
#' @param kjonn 
#' @param filbesk 
#' @param batchdate 
#' @param globs 
KJONNvask <- function(kjonn, filbesk = data.frame(), batchdate = SettKHBatchDate(), globs = FinnGlobs()) {
  is_kh_debug()
  
  if (nrow(filbesk) == 0) {
    kjonn <- setNames(as.data.frame(kjonn, stringsAsFactors = FALSE), c("KJONN"))
    kjonn$KBOMK <- kjonn[, 1]
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
  kjonn$OMK[!(kjonn$OMK %in% c(0, 1, 2, 9, "-"))] <- globs$kjonn_illeg
  kjonn$OK[!(kjonn$OMK %in% c(0, 1, 2, 9, "-"))] <- 0
  
  return(kjonn)
}

#' UTDANNvask
#'
#' @param utdann 
#' @param filbesk 
#' @param batchdate 
#' @param globs 
#' @param regexp 
UTDANNvask <- function(utdann, filbesk = data.frame(), batchdate = SettKHBatchDate(), globs = FinnGlobs(), regexp = FALSE) {
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
  utdann$OMK[!(utdann$OMK %in% c(0, 1, 2, 3, 4, "-"))] <- globs$utdann_illeg
  utdann$OK[!(utdann$OMK %in% c(0, 1, 2, 3, 4, "-"))] <- 0
  return(utdann)
}

#' INNVKATvask
#'
#' @param innvkat 
#' @param filbesk 
#' @param batchdate 
#' @param globs 
#' @param regexp 
INNVKATvask <- function(innvkat, filbesk = data.frame(), batchdate = SettKHBatchDate(), globs = FinnGlobs(), regexp = FALSE) {
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
  innvkat$OMK[!(innvkat$OMK %in% c(0, 1, 2, 3, 4, 5, 9, 20, "-"))] <- globs$innvkat_illeg
  innvkat$OK[!(innvkat$OMK %in% c(0, 1, 2, 3, 4, 5, 9, 20, "-"))] <- 0
  
  return(innvkat)
}

#' LANDBAKvask
#'
#' @param landbak 
#' @param filbesk 
#' @param batchdate 
#' @param globs 
#' @param regexp 
LANDBAKvask <- function(landbak, filbesk = data.frame(), batchdate = SettKHBatchDate(), globs = FinnGlobs(), regexp = FALSE) {
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
  landbak$OMK[!(landbak$OMK %in% c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 20, "-"))] <- globs$landbak_illeg
  landbak$OK[!(landbak$OMK %in% c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 20, "-"))] <- 0
  
  return(landbak)
}

#' AARvask
#'
#' @param aar 
#' @param filbesk 
#' @param batchdate 
#' @param globs 
AARvask <- function(aar, filbesk = data.frame(), batchdate = SettKHBatchDate(), globs = FinnGlobs()) {
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
  aar$OMK[!okformat] <- globs$aar_illeg
  aar$OK[!okformat] <- 0
  
  # Sett intervall
  aar[, c("LO", "HI")] <- suppressMessages(matrix(as.integer(str_split_fixed(aar$OMK, "_", 2)), ncol = 2))
  # Ugyldig intervall
  aar$OMK[aar$HI < aar$LO] <- globs$aar_illeg
  aar[, c("LO", "HI")] <- suppressMessages(matrix(as.integer(str_split_fixed(aar$OMK, "_", 2)), ncol = 2))
  return(aar)
}