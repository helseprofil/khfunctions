# Funksjoner fra master

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
FinnRedesign_master <- function(DesFRA, DesTIL, SkalAggregeresOpp = character(), ReturnerFullFull = FALSE, globs = FinnGlobs(), prios = globs$DefDesign, KB = globs$KB, IntervallHull = globs$DefDesign$IntervallHull, AggPri = globs$DefDesign$AggPri, echo = 0) {
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
    ## FULL <- data.table::data.table(eval(parse(text = paste("expand.grid.df(", komblist, ")", sep = ""))))
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
  data.table::setnames(FULL, names(FULL), paste(names(FULL), "_omk", sep = ""))
  
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
      DesTIL$Part[[del]] <- data.table::copy(DesFRA$Part[[del]])
    }
  }
  
  gc()
  Parts <- list()
  for (del in names(KB)) {
    # if (del %in% names(DesTIL$Part)){
    
    if (del %in% names(DesTIL$Part) & del %in% names(DesFRA$Part)) {
      DesTIL$Part[[del]] <- data.table::copy(data.table::as.data.table(DesTIL$Part[[del]])) # Får noen rare warnings uten copy, bør debugge dette
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
          KBD <- data.table::data.table(KBD, key = omkcols)
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
          # KBD[,globs$DefDesign$DelKols[[del]]]<-as.integer(stringr::str_split_fixed(KBD[,kol],"_",2))
          KBD[, globs$DefDesign$DelKols[[del]]] <- matrix(as.integer(stringr::str_split_fixed(KBD[, kol], "_", 2)), ncol = 2)
          KBD[, paste(globs$DefDesign$DelKols[[del]], "_omk", sep = "")] <- matrix(as.integer(stringr::str_split_fixed(KBD[, kolomk], "_", 2)), ncol = 2)
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
        KBInt <- FinnKodebokIntervaller_master(as.data.frame(IntFra), as.data.frame(IntTil), deln = del)
        
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
        KBD <- data.table::data.table(KBD, key = kols)
        KBD <- data.table::data.table(DesFRA$Part[[del]], key = kols)[KBD]
        har <- paste(del, "_HAR", sep = "")
        eval(parse(text = paste(
          "KBD[is.na(KBD[,", har, "]),", har, ":=0]",
          sep = ""
        )))
        
        KBD <- SettPartDekk_master(KBD, del = del, IntervallHull = IntervallHull, globs = globs)
        # data.table::setnames(KBD,"DEKKok",paste(del,"_Dekk",sep=""))
        
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
      data.table::setkeyv(Parts[[del]], delkols)
      data.table::setkeyv(DesFRA$SKombs[[kombn]], delkols)
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
      betD <- data.table::data.table(Parts[[del]])
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
      data.table::setkeyv(KB, brukcols)
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
    data.table::setkeyv(KB, common)
    data.table::setkeyv(FULL, common)
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
  data.table::setkeyv(FULL, omkkols)
  Dekk <- unique(FULL[, omkkols, with = FALSE])
  data.table::setnames(Dekk, names(Dekk), gsub("_omk$", "", names(Dekk)))
  
  data.table::setkeyv(FULL, namesFULL)
  Udekk <- handle_udekk_master(FULL, namesFULL, TempFile)
  
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
FinnKodebokIntervaller_master <- function(FRA, TIL, storst = TRUE, delnavn = "INT", echo = 0) {
  is_kh_debug()
  
  # I tilfelle input er data.table
  FRA <- as.data.frame(FRA)
  TIL <- as.data.frame(TIL)
  # Bruk Intrevals-klassen
  utcolnavn <- c(names(FRA), paste(names(FRA), "_omk", sep = ""), paste(delnavn, "_pri", sep = ""))
  TILi <- intervals::Intervals(TIL, type = "Z")
  FRAi <- intervals::Intervals(FRA, type = "Z")
  if (storst == TRUE) {
    # Gir høyest prioritet til å bruke/beholde store intervaller
    sorter <- order(intervals::size(FRAi), decreasing = TRUE)
  } else {
    # Gir høyest prioritet til å bruke små intervaller og aggregger opp. Brukes ved aldersstandardisering
    sorter <- order(intervals::size(FRAi))
  }
  
  # sorter<-order(size(FRAi))
  FRAi <- FRAi[sorter]
  FRA <- FRA[sorter, ]
  # Finn kandidater, dvs inkluderte "underintrevaller"
  KAND <- intervals::interval_included(TILi, FRAi)
  if ("matrix" %in% class(KAND)) { # Irriterende bug(?) i interval når TILi har dim 1 eller KAND er n*m
    # KAND<-list(KAND)
    KAND <- split(KAND, rep(1:ncol(KAND), each = nrow(KAND)))
  }
  if (echo == 1) {
    print(KAND)
  }
  
  # Finn intern overlapp i FRA
  OVLP <- intervals::interval_overlap(FRAi, FRAi)
  if (echo == 1) {
    print(OVLP)
  }
  
  # Initier tom kodebok
  KODEBOK0 <- as.data.frame(setNames(replicate(length(utcolnavn), integer(0), simplify = F), utcolnavn))
  KODEBOK <- KODEBOK0
  # Må loope over alle TIL. Kan dette vektoriseres? Kanskje ikke så mye å vinne?
  for (i in 1:nrow(TIL)) {
    result <- list("pri" = 0, "KODEBOK" = KODEBOK0)
    result <- FinnKodebokForEtIntervall_master(KAND[[i]], FRA, TIL[i, ], OVLP, 0, result, utcolnavn)
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
FinnKodebokForEtIntervall_master <- function(Find, FRA, TILint, OVLP, letn, result, utcolnavn) {
  is_kh_debug()
  
  if (DekkerInt(FRA[Find, ], TILint)) {
    jobb <- Find[Find > letn]
    if (length(jobb) > 0) {
      # Må unngå jobbing på alle esoteriske kombinasjoner ved mye overlap
      if (result$pri < 6) {
        letn <- jobb[1]
        # cat("Let videres med letn",letn,"\n")
        result <- FinnKodebokForEtIntervall_master(Find[!(Find %in% OVLP[[letn]]) | Find == letn], FRA, TILint, OVLP, letn, result, utcolnavn)
        # LEt videre uten letn
        result <- FinnKodebokForEtIntervall_master(Find[Find != letn], FRA, TILint, OVLP, letn, result, utcolnavn)
      }
    } else {
      result$KODEBOK <- rbind(result$KODEBOK, setNames(cbind(FRA[Find, ], TILint, result$pri), utcolnavn))
      result$pri <- result$pri + 1
    }
  } else {
    # Sett diff/residual
    if (nrow(FRA[Find, ]) > 0) {
      miss <- setNames(as.data.frame(intervals::interval_difference(Intervals(TILint, type = "Z"), Intervals(FRA[Find, ], type = "Z"))), names(FRA))
    } else {
      miss <- setNames(as.data.frame(TILint), names(FRA))
    }
    # Ta bare med dersom residual er ny
    miss <- miss[!apply(miss, 1, paste, collapse = "_") %in% apply(FRA, 1, paste, collapse = "_"), ]
    if (nrow(miss) > 0) {
      FRA <- rbind(FRA, miss)
      Find <- c(Find, nrow(FRA))
      OVLP <- intervals::interval_overlap(intervals::Intervals(FRA, type = "Z"), intervals::Intervals(FRA, type = "Z"))
      result <- FinnKodebokForEtIntervall_master(Find, FRA, TILint, OVLP, letn, result, utcolnavn)
    }
  }
  return(result)
}

#' handle_udekk (ybk)
#'
#' @param FULL 
#' @param namesFULL 
#' @param TempFile 
handle_udekk_master <- function(FULL, namesFULL, TempFile){
  is_kh_debug()
  Udekk <- readRDS(TempFile)
  data.table::setkeyv(Udekk, namesFULL)
  Udekk <- Udekk[!FULL, allow.cartesian = TRUE]
  data.table::setnames(Udekk, namesFULL, gsub("_omk$", "", namesFULL))
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
SettPartDekk_master <- function(KB, del = "", har = paste(del, "_HAR", sep = ""), betcols = character(0), globs = FinnGlobs(), IntervallHull = globs$DefDesign$IntervallHull) {
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