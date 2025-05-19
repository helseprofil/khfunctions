#' SettFilterDesign (kb)
#'
#' @param KUBEdscr 
#' @param OrgParts 
#' @param bruk0 
#' @param FGP 
#' @param globs global parameters, defaults to SettGlobs
SettFilterDesign <- function(KUBEdscr, OrgParts = list(), bruk0 = TRUE, FGP = list(amin = 0, amax = 120), parameters) {
  Deler <- list()
  for (del in names(parameters$DefDesign$DelKolN)) {
    # for (del in names(unlist(globs$DefDesign$DelKolN[ORGd$OmkDeler]))){
    # if (del %in% names(ORGd$Part) | grepl("^T\\d$",del)){
    # Les liste
    koldel <- parameters$DefDesign$DelKolN[del]
    koldel0 <- paste0(koldel, "_0")
    
    if (bruk0 == TRUE && !is.null(KUBEdscr[[koldel0]]) && !is.na(KUBEdscr[[koldel0]]) && KUBEdscr[[koldel0]] != "") {
      delListStr <- KUBEdscr[[koldel0]]
    } else {
      delListStr <- KUBEdscr[[koldel]]
    }
    if (!(is.null(delListStr) || is.na(delListStr) || delListStr == "")) {
      delListStr <- gsub("^ *| *$", "", delListStr)
      minus <- grepl("^-\\[", delListStr)
      delListStr <- gsub("^-\\[(.*)\\]$", "\\1", delListStr)
      delListA <- unlist(stringr::str_split(delListStr, ","))
      if (parameters$DefDesign$DelType[del] == "INT") {
        if (del == "A") {
          delListA <- gsub("ALLE", paste0(FGP$amin, "_", FGP$amax), delListA)
          delListA <- gsub("^_(\\d+)", paste0(FGP$amin, "_\\1"), delListA)
          delListA <- gsub("(\\d+)_$", paste0("\\1_", FGP$amax), delListA)
        }
        delListA <- gsub("^(\\d+)$", "\\1_\\1", delListA)
        delListA <- data.table::as.data.table(matrix(as.integer(stringr::str_split_fixed(delListA, "_", 2)), ncol = 2))
      } else if (parameters$DefDesign$DelFormat[del] == "integer") {
        delListA <- as.integer(delListA)
      } else if (parameters$DefDesign$DelFormat[del] == "numeric") {
        delListA <- as.numeric(delListA)
      }
      listDT <- data.table::setnames(data.table::as.data.table(delListA), parameters$DefDesign$DelKols[[del]])
      if (minus == TRUE) {
        if (!is.null(OrgParts[[del]])) {
          data.table::setkeyv(listDT, names(listDT))
          data.table::setkeyv(OrgParts[[del]], names(listDT))
          Deler[[del]] <- OrgParts[[del]][!listDT, ]
        } else {
          print("**********************KAN IKKE BRUKE -[liste] i SettFilterDesign naar ikke OrgParts")
        }
      } else {
        Deler[[del]] <- listDT
      }
    } else if (parameters$DefDesign$DelType[del] == "INT") {
      start <- KUBEdscr[[paste0(koldel, "_START")]]
      stopp <- KUBEdscr[[paste0(koldel, "_STOP")]]
      if (!(is.null(start) | is.null(stopp))) {
        if (!(is.na(start) | is.na(stopp))) {
          if (!(start == "" | stopp == "")) {
            if (stopp >= start) {
              if (!is.null(OrgParts[[del]])) {
                Deler[[del]] <- subset(OrgParts[[del]], eval(parse(text = paste0(koldel, "l>=", start, " & ", koldel, "h<=", stopp))))
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

#' @title FinnRedesign (kb)
#'
#' @param fradesign 
#' @param tildesign 
#' @param SkalAggregeresOpp skal noen deler evt aggregeres opp?
#' @param parameters global parameters
FinnRedesign <- function(fradesign, tildesign, SkalAggregeresOpp = character(), parameters) {
  KB = parameters$KB
  AggPri = parameters$DefDesign$AggPri
  # Merk assymtri mellom fradesign og tildesign.
  # For tildesign brukes bare tildesign$Part og tildesign$OmkDesign.
  # fradesign maa derfor komme fra FinnDesign med alle egenskaper satt der, mens tildesign kan vaere enklere og satt andre steder
  
  # Deler i fradesign som ikke er i tildesign maa legegs til i tildesign (full kryss mot Part[del])
  # Merk at deler i tildesign som ikke er i fradesign gaar greit (all omkoding er indirekte "betinget" paa disse)
  
  # Sett partiell omkoding
  # For intervaller kalles FinnKodebokINtervaller, elllers hentes fast kodebok som utgnagspunkt
  # Disse kodeboekene (KJONN etc) filtreres til de omkodingene som er aktuelle (boer gjoeres her for aa begrense kombinatorikk, selv om dette kunne vaert utsatt)
  # Dvs omkodinger til en TIL som ikke har alle noedvendige deler i FRA filtreres bort
  # Merk at noen deler i FRA ikke er obligatoriske (slik som KJONN=9 for omkoding til KJONN=0)
  
  # Rydd tildesign$Design (Kan variere litt mht HAR avhengig av hvor kallet paa denne funksjonen er gjort fra. Skal ha 1 har felt)
  if (is.null(tildesign$Design)) {
    FULL <- do.call(expand.grid.dt, tildesign$Part)
    FULL[, names(.SD) := NULL, .SDcols = grep("_HAR$", names(FULL), value = T)]
  } else {
    FULL <- tildesign$Design[HAR == 1, ]
    FULL[, names(.SD) := NULL, .SDcols = grep("_HAR$|^HAR$", names(FULL), value = T)]
  }
  data.table::setnames(FULL, names(FULL), paste0(names(FULL), "_omk"))
  
  TempFile <- file.path(tempdir(), paste0("full", SettKHBatchDate(), ".RDS"))
  saveRDS(FULL, TempFile)
  namesFULL <- names(FULL) # Need to get the original colnames before manipulation
  
  betKols <- setdiff(names(fradesign$SKombs$bet), "HAR")
  if (length(betKols) > 0) {
    FULL <- expand.grid.dt(FULL, fradesign$SKombs$bet[, ..betKols])
  }
  for (del in fradesign$UBeting) {
    if (is.null(tildesign$Part[[del]])) {
      tildesign$Part[[del]] <- data.table::copy(fradesign$Part[[del]])
    }
  }
  
  Parts <- list()
  # DEV: Denne delen kan hentes ut til en egen funksjon
  for (del in names(KB)) {
    if (del %in% names(tildesign$Part) & del %in% names(fradesign$Part)) {
      d <- data.table::copy(data.table::setDT(tildesign$Part[[del]])) # Faar noen rare warnings uten copy, boer debugge dette
      delH <- paste0(del, "_HAR")
      delP <- paste0(del, "_pri")
      delO <- paste0(del, "_obl")
      delD <- paste0(del, "_Dekk")
      if (!delH %in% names(d)) {
        d[, (delH) := 1]
      }
      KBD <- data.table::as.data.table(KB[[del]])
      kol <- as.character(parameters$DefDesign$DelKolN[del])
      kolomk <- paste0(kol, "_omk")
      kolomkpri <- c(kolomk, paste0(del, "_pri"))
      kols <- parameters$DefDesign$DelKols[[del]]
      kolsomk <- paste0(kols, "_omk")
      kolsomkpri <- c(kolsomk, paste0(del, "_pri"))
      
      # Sett 1-1 koding for T1,T2,.. dersom ikke annet gitt
      if (grepl("^T\\d$", del) & nrow(KBD) == 0) {
        tilTabs <- d[, ..kol]
        KBD <- data.table::setnames(data.table::data.table(tilTabs, tilTabs, 0, 1), c(kol, kolomk, delP, delO))
        Parts[[del]] <- KBD
      }
      
      if (parameters$DefDesign$DelType[del] == "COL") {
        if (nrow(KBD) > 0) {
          # Filtrer bort TIL-koder i global-KB som ikke er i tildesign
          KBD <- KBD[get(kolomk) %in% d[[kol]]]
          data.table::setkeyv(KBD, kolomkpri)
          KBD[, (delH) := as.integer(0L)]
          KBD[get(kol) %in% fradesign$Part[[del]][[kol]], (delH) := 1L]
          KBD[, (delD) := as.integer(!any(get(delH) == 0 & get(delO) == 1)), by = kolsomkpri]
          # Kast omkodinger uten noen deler i FRA, behold de som dekkes helt og delvis
          # Setter keep her, kaster til slutt
          KBD[, keep := as.integer(any(get(delH) == 1)), by = kolsomkpri]
        }
      } else if (parameters$DefDesign$DelType[del] == "INT") {
        # Global KB kan inneholde (fil)spesifikke koden "ALLE", maa erstatte denne med "amin_amax" og lage intervall
        # Merk: dette gjelder typisk bare tilfellene der ukjent alder og evt tilsvarende skal settes inn under "ALLE"
        Imin <- min(fradesign$Part[[del]][[paste0(parameters$DefDesign$DelKolN[[del]], "l")]])
        Imax <- max(fradesign$Part[[del]][[paste0(parameters$DefDesign$DelKolN[[del]], "h")]])
        alle <- paste0(Imin, "_", Imax)
        if (nrow(KBD) > 0) {
          KBD[, names(.SD) := lapply(.SD, function(x) gsub("^(ALLE)$", alle, x)), .SDcols = c(kol, kolomk)]
          KBD[, (kols) := data.table::tstrsplit(get(kol), "_", fixed = TRUE)]
          KBD[, (kolsomk) := data.table::tstrsplit(get(kolomk), "_", fixed = TRUE)]
          # Filtrer KBD mot TIL!!
          KBD <- KBD[get(kolomk) %in% apply(d[, ..kols], 1, paste, collapse = "_"), ]
        }
        # Maa fjerne "del_HAR" inn i omkodintervall, fjerner dessuten del_HAR==0 i TIL
        # Merk: eneste som ikke har del_HAR er udekkede intervaller mellom amin og amax.
        # Videre er disse bare med naar TilDes er satt fra FinnDesign(FG), ikke naar TilDes er fra Parts
        # Usikker paa om det alltid er best aa slippe disse gjennom.
        
        IntFra <- fradesign$Part[[del]][, ..kols]
        IntTil <- d[, ..kols]
        # Fjerner spesialkoder (dvs uoppgitt etc i KB) foer intervallomregning
        IntFra <- IntFra[!apply(IntFra[, ..kols], 1, paste, collapse = "_") %in% parameters$LegKoder[[del]]$KODE]
        IntTil <- IntTil[!apply(IntTil[, ..kols], 1, paste, collapse = "_") %in% parameters$LegKoder[[del]]$KODE]
        KBInt <- FinnKodebokIntervaller(IntFra, IntTil, delnavn = del)
        KBInt[, (delO) := 1]
        
        # DEVELOP:   DETTE ER TENMMELIG AD HOC!!!!!!!!!!
        if (del == "A") {
          KBInt[KBInt$ALDERl >= 90, (delO) := 0]
        }
        
        KBInt[[paste0(del, "_ok")]] <- NULL # Denne brukes bare ved filtrering rett fra KBint
        outnames <- names(KBInt)
        # Legg til spesialkoder igjen
        if (nrow(KBD) > 0) {
          KBD <- data.table::rbindlist(list(KBInt, KBD[, ..outnames]))
          KBD[, names(.SD) := lapply(.SD, as.integer), .SDcols = outnames]
        } else {
          KBD <- KBInt
        }
        
        # Koble paa "del_HAR"
        data.table::setkeyv(KBD, kols)
        KBD <- collapse::join(fradesign$Part[[del]], KBD, how = "r", verbose = 0)
        KBD[is.na(get(delH)), (delH) := 0]
        KBD <- SettPartDekk(KBD, del = del, har = delH, parameters = parameters)
        
        # Kast omkodinger uten noen deler i FRA, behold de som dekkes helt og delvis
        # USIKKER paa om dette er optimalt. Det maa ikke kastes for mye for riktig bruk fra FinnFellesTab
        # Egentlig er det jo unoedvenig aa kaste noe som helst. Dette er mest for rapport/lesing av KBD
        # Setter keep her, kaster til slutt
        KBD[, keep := as.integer(any(get(delH) == 1) | get(delO) == 0), by = kolsomkpri]
      }
      
      KBD <- KBD[keep == 1][, keep := NULL]
      Parts[[del]] <- KBD
    }
  }
  
  gc()
  SKombs <- list()
  KBs <- list()
  Filters <- list()
  DelStatus <- list()
  
  # Maa passe paa rekkefoelge (Ubeting til slutt), ellers kan det gaa galt i FULL
  beting <- intersect(parameters$DefDesign$AggPri[length(parameters$DefDesign$AggPri):1], c(parameters$DefDesign$BetingOmk, parameters$DefDesign$BetingF))
  ubeting <- intersect(parameters$DefDesign$AggPri[length(parameters$DefDesign$AggPri):1], c(parameters$DefDesign$UBeting))
  
  for (del in intersect(c(beting, ubeting), names(Parts))) {
    kols <- parameters$DefDesign$DelKols[[del]]
    kolsomk <- paste0(kols, "_omk")
    delD <- paste0(del, "_Dekk")
    delP <- paste0(del, "_pri")
    delH <- paste0(del, "_HAR")
    if (length(fradesign[["UBeting"]]) > 0) {
      if (del %in% fradesign[["UBeting"]]) {
        kombn <- "bet"
      } else {
        kombn <- paste0("bet", del)
      }
      # Koble med fradesign
      data.table::setkeyv(Parts[[del]], kols)
      data.table::setkeyv(fradesign$SKombs[[kombn]], kols)
      betD <- collapse::join(fradesign$SKombs[[kombn]], Parts[[del]], how = "right", multiple = T, verbose = F)
      betD[is.na(HAR), HAR := 0]
      # Maa kaste de som ikke har del_Dekk==1 hvis ikke kan de feilaktig
      # faa del_Dekk==1 under dersom del er i beting, da vil en annen del i beting faa NA og by=betcols gaar galt!)
      betD <- subset(betD, eval(parse(text = paste0(del, "_Dekk==1"))))
      betD <- betD[get(delD) == 1]
      # Sett (betinget) dekning
      betcols <- unlist(parameters$DefDesign$DelKols[setdiff(fradesign[["UBeting"]], del)])
      betD <- SettPartDekk(betD, del = del, har = "HAR", betcols = betcols, parameters = parameters)
    } else {
      betcols <- character()
      betD <- data.table::copy(Parts[[del]])
    }
    
    # Finn beste alternativ
    bycols <- c(kolsomk, betcols)
    if (del %in% SkalAggregeresOpp) {
      betD[get(delD) == 1, Bruk := max(get(delP)), by = bycols]
    } else {
      betD[get(delD) == 1, Bruk := min(get(delP)), by = bycols]
    }
    
    KB <- betD[Bruk == get(delP) & get(delH) == 1]
    SKombs[[del]] <- betD
    
    # Sjekk om del kan omkodes helt partielt (fra Part) eller om maa betinge (dvs KB)
    
    # Finner om en omk_kode bruker flere versjoner av partiell omkoding (hver versjon fra Part har ulik prid)
    # Om en slik finnes beholdes KB, ellers fjernes overloedig betinging
    maxBet <- KB[, .(NOPri = length(unique(get(delP)))), by = kolsomk][, max(NOPri)]
    if (maxBet == 1) {
      brukcols <- c(gsub("_omk$", "", kolsomk), kolsomk)
      data.table::setkeyv(KB, brukcols)
      KBs[[del]] <- unique(KB[, ..brukcols])
      DelStatus[[del]] <- "P"
    } else {
      brukcols <- names(KB)[!grepl("(_obl|_{0,1}HAR|_Dekk|_pri|Bruk)$", names(KB))]
      KBs[[del]] <- KB[, ..brukcols]
      DelStatus[[del]] <- "B"
    }
    
    if (del == "Y" & DelStatus[[del]] == "B") {
      KHerr("Har DelStatus[[Y]]==B, dette takles per naa ikke i FilOmkod og vil gi feil der!!!")
    }
    
    # Sett dekning i FULL
    common <- intersect(names(FULL), names(KB))
    data.table::setkeyv(KB, common)
    data.table::setkeyv(FULL, common)
    FULL <- FULL[KB[, ..common], nomatch = NULL, allow.cartesian = TRUE]
    
    # Ignorer KB der det ikke foregaar reell omkoding
    if (all(KBs[[del]][, ..kols] == KBs[[del]][, ..kolsomk])) {
      # Filters[[del]] <- KBs[[del]][, names(KBs[[del]])[!grepl("_omk$", names(KBs[[del]]))], with = FALSE]
      Filters[[del]] <- KBs[[del]][, ..kols]
      KBs[del] <- NULL
      DelStatus[[del]] <- "F"
    }
  }
  omkkols <- names(FULL)[grepl("_omk$", names(FULL))]
  data.table::setkeyv(FULL, omkkols)
  Dekk <- unique(FULL[, ..omkkols])
  data.table::setnames(Dekk, names(Dekk), gsub("_omk$", "", names(Dekk)))
  
  data.table::setkeyv(FULL, namesFULL)
  Udekk <- handle_udekk(FULL, namesFULL, TempFile)
  
  gc()
  return(list(Parts = Parts, SKombs = SKombs, KBs = KBs, Filters = Filters, FULL = FULL, Dekk = Dekk, Udekk = Udekk, DelStatus = DelStatus))
}

#' FinnKodebokIntervaller (kb)
#'
#' @param FRA 
#' @param TIL 
#' @param delnavn 
FinnKodebokIntervaller <- function(FRA, TIL, delnavn = "INT") {
  # is_kh_debug()
  
  # I tilfelle input er data.table
  # FRA <- as.data.frame(FRA)
  # TIL <- as.data.frame(TIL)
  # Bruk Intrevals-klassen
  utcolnavn <- c(names(FRA), paste0(names(FRA), "_omk"), paste0(delnavn, "_pri"))
  TILi <- intervals::Intervals(TIL, type = "Z")
  FRAi <- intervals::Intervals(FRA, type = "Z")
  sorter <- order(intervals::size(FRAi), decreasing = TRUE)
  FRAi <- FRAi[sorter]
  FRA <- FRA[sorter]
  # Finn kandidater, dvs inkluderte "underintrevaller"
  KAND <- intervals::interval_included(TILi, FRAi)
  if ("matrix" %in% class(KAND)) { # Irriterende bug(?) i interval naar TILi har dim 1 eller KAND er n*m
    # KAND<-list(KAND)
    KAND <- split(KAND, rep(1:ncol(KAND), each = nrow(KAND)))
  }
  
  # Finn intern overlapp i FRA
  OVLP <- intervals::interval_overlap(FRAi, FRAi)
  
  # Initier tom kodebok
  KODEBOK0 <- data.table::setDT(setNames(replicate(length(utcolnavn), integer(0), simplify = F), utcolnavn))
  KODEBOK <- KODEBOK0
  # Maa loope over alle TIL. Kan dette vektoriseres? Kanskje ikke saa mye aa vinne?
  for (i in 1:nrow(TIL)) {
    result <- list("pri" = 0, "KODEBOK" = KODEBOK0)
    result <- FinnKodebokForEtIntervall(KAND[[i]], FRA, TIL[i], OVLP, 0, result, utcolnavn)
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
  # is_kh_debug()
  
  if (DekkerInt(FRA[Find], TILint)) {
    jobb <- Find[Find > letn]
    if (length(jobb) > 0) {
      # Maa unngaa jobbing paa alle esoteriske kombinasjoner ved mye overlap
      if (result$pri < 6) {
        letn <- jobb[1]
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
      miss <- setNames(as.data.frame(intervals::interval_difference(intervals::Intervals(TILint, type = "Z"), intervals::Intervals(FRA[Find, ], type = "Z"))), names(FRA))
    } else {
      miss <- setNames(as.data.frame(TILint), names(FRA))
    }
    # Ta bare med dersom residual er ny
    miss <- miss[!apply(miss, 1, paste, collapse = "_") %in% apply(FRA, 1, paste, collapse = "_"), ]
    if (nrow(miss) > 0) {
      FRA <- rbind(FRA, miss)
      Find <- c(Find, nrow(FRA))
      OVLP <- intervals::interval_overlap(intervals::Intervals(FRA, type = "Z"), intervals::Intervals(FRA, type = "Z"))
      result <- FinnKodebokForEtIntervall(Find, FRA, TILint, OVLP, letn, result, utcolnavn)
    }
  }
  return(result)
}

#' DekkerInt (kb)
#'
#' @param FRA 
#' @param TIL 
DekkerInt <- function(FRA, TIL) {
  # is_kh_debug()
  
  # Sjekker at hele intervallet TIL[,1]-TIL[,2] dekkes av intervallene i FRA
  # Bryr seg ikke om overlapp her, det gjoeres andre steder
  # Kompakt, men effektiv syntaks
  all(TIL[[1]]:TIL[[2]] %in% unlist(mapply(seq, FRA[[1]], FRA[[2]])))
}

#' handle_udekk (ybk)
#'
#' @param FULL 
#' @param namesFULL 
#' @param TempFile 
handle_udekk <- function(FULL, namesFULL, TempFile){
  # is_kh_debug()
  Udekk <- readRDS(TempFile)
  data.table::setkeyv(Udekk, namesFULL)
  Udekk <- Udekk[!FULL, allow.cartesian = TRUE]
  data.table::setnames(Udekk, namesFULL, gsub("_omk$", "", namesFULL))
  return(Udekk)
}

#' @title SettPartDekk (kb)
#'
#' @param KB 
#' @param del 
#' @param har 
#' @param betcols 
#' @param parameters global parameters
#' @param IntervallHull 
#' SettPartDekk(KBD, del = del, har = delH, IntervallHull = IntervallHull, globs = globs)
SettPartDekk <- function(KB, del = "", har = paste0(del, "_HAR"), betcols = character(0), parameters) {
  
  IntervallHull = parameters$DefDesign$IntervallHull
  
  kol <- as.character(parameters$DefDesign$DelKolN[del])
  kolsomkpri <- names(KB)[grepl("_(omk|pri)$", names(KB))]
  bycols <- c(kolsomkpri, betcols)
  kolL <- paste0(kol, "l")
  kolLomk <- paste0(kolL, "_omk")
  kolH <- paste0(kol, "h")
  kolHomk <- paste0(kolH, "_omk")
  delD <- paste0(del, "_Dekk")
  delO <- paste0(del, "_obl")
  
  if (del %in% names(IntervallHull)) {
    KB[, let(DekkInt = sum((get(har) == 1 | get(delO) == 0) * (1+get(kolH) - get(kolL))),
             FRADELER = .N,
             NHAR = sum(get(har) == 1 | get(delO) == 0),
             TotInt = 1+get(kolHomk)-get(kolLomk),
             NTOT = sum(get(delO) == 1)),
       by = bycols]
    
    sjekkintervall <- rlang::parse_expr(IntervallHull[[del]])
    KB[, (delD) := as.integer(eval(sjekkintervall)), by = bycols]
    KB[, c("DekkInt", "NHAR", "TotInt", "NTOT", "FRADELER") := NULL]
  } else {
    KB[, (delD) := as.integer(!any(get(har) == 0 & get(delO) == 1)), by = bycols]
  }
  
  gc()
  return(KB)
}

#' @title FinnRedesignForFilter (kb)
#' @keywords internal
#' @noRd
FinnRedesignForFilter <- function(ORGd, Filter, parameters) {
  MODd <- Filter
  for (del in setdiff(names(ORGd$Part), names(Filter))) {
    MODd[[del]] <- data.table::copy(ORGd$Part[[del]])
  }
  return(FinnRedesign(ORGd, list(Part = MODd), parameters = parameters))
}

#' AggregerRader (kb)
#'
#' @param FG 
#' @param nyeexpr 
#' @param FGP 
AggregerRader <- function(FG, nyeexpr, FGP) {
  # is_kh_debug()
  
  if (!(is.na(nyeexpr) || nyeexpr == "")) {
    nytabs <- unlist(stringr::str_split(nyeexpr, ";"))
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
        subexp <- gsub("^ *tab(.*)", paste0(tabE, "\\1"), subexp)
        tab <- tabE
      }
      FG2 <- eval(parse(text = paste0("subset(FG,", subexp, ")")))
      FG2 <- KHaggreger(FG2[, setdiff(names(FG), tab), with = FALSE], globs = globs)
      FG2[, eval(parse(text = paste0(tab, ":='", nylab, "'")))]
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
#' @param globs global parameters, defaults to SettGlobs
OmkodFilFraPart <- function(Fil, Part, FGP = list(amin = 0, amax = 120), parameters) {
  # is_kh_debug()
  
  Dorg <- FinnDesign(Fil, FGP = FGP, parameters = parameters)
  Dmod <- ModifiserDesign(Part, Dorg, parameters = parameters)
  RD <- FinnRedesign(Dorg, Dmod, parameters = parameters)
  return(OmkodFil(Fil, RD, parameters = parameters))
}

#' ModifiserDesign (kb)
#'
#' @param Nytt 
#' @param Org 
#' @param globs global parameters, defaults to SettGlobs
ModifiserDesign <- function(Nytt, Org = list(), parameters) {
  # is_kh_debug()
  
  Nkombs <- 1
  for (del in names(Org$Part)) {
    Nkombs <- Nkombs * nrow(Org$Part[[del]])
  }
  
  
  for (del in names(Nytt)) {
    delT <- data.table::as.data.table(Nytt[[del]])
    delT[, paste0(del, "_HAR")] <- 1
    Org$Part[[del]] <- delT
    Nkombs <- Nkombs * nrow(delT)
  }
  
  if (any(grepl("_HAR", names(Org$OmkDesign)))) {
    cat("************************************************************\n*\n*  OBSN NOE RART MED ModifiserDesign HAR\n*\n*********************************\n")
  }
  
  omkDeler <- intersect(names(Nytt), c(parameters$DefDesign$UBeting, parameters$DefDesign$BetingOmk))
  if (length(omkDeler) > 0) {
    NyKols <- unlist(parameters$DefDesign$DelKols[omkDeler])
    NyKols <- intersect(names(Org$OmkDesign), NyKols)
    OmkDesignGmlKols <- setdiff(names(Org$OmkDesign), c(NyKols, "HAR"))
    delerlist <- paste0("as.data.frame(Nytt[[\"", names(Nytt), "\"]])", collapse = ",")
    # Skal beholdes
    if (length(OmkDesignGmlKols) > 0) {
      OmkDesGml <- Org$OmkDesign[, c(OmkDesignGmlKols, "HAR"), with = FALSE]
      data.table::setkeyv(OmkDesGml, OmkDesignGmlKols)
      OmkDesGml <- OmkDesGml[, list(HAR = max(HAR)), by = OmkDesignGmlKols]
      delerlist <- paste0(delerlist, ",as.data.frame(OmkDesGml)")
    }
    OmkDesNy <- data.table::data.table(eval(parse(text = paste0("expand.grid.df(", delerlist, ")"))))
    if (length(OmkDesignGmlKols) == 0) {
      OmkDesNy[, HAR := 1]
    }
    OmkKols <- parameters$DefDesign$DesignKols[parameters$DefDesign$DesignKols %in% names(OmkDesNy)]
    setkeym(OmkDesNy, OmkKols)
    Org[["OmkDesign"]] <- OmkDesNy
  }
  # Merk, det gir bare mening aa bruke denne for aa lage et TIL-design, da trengs ikke de foelgende delene
  # Om det modifiserte designet skal brukes som et FRA-design maa ogsaa disse endres. Det er en kloenete operasjon (og som vel knapt er veldefinert)
  # Kan altsaa IKKE bruke FinnFellesTab(Org,ModifiserDesign(PredFilter,Org))
  
  Org[["Design"]] <- NULL
  Org[["SKombs"]] <- NULL
  Org[["FKombs"]] <- NULL
  
  return(Org)
}

#' SettNaboAnoSpec (kb)
#'
#' @param ovkatspec 
#' @param FGP 
#' @param globs global parameters, defaults to SettGlobs
SettNaboAnoSpec <- function(ovkatspec, FGP = list(amin = 0, amax = 120), globs = get_global_parameters()) {
  # is_kh_debug()
  Foverkat <- list()
  if (!(is.null(ovkatspec) || is.na(ovkatspec))) {
    specs <- unlist(stringr::str_split(ovkatspec, ";"))
    i <- 1
    for (spec in specs) {
      if (grepl("\\[(.*?)\\]=\\[.*\\]", spec)) {
        subcond <- gsub("^\\[(.*?)\\]=\\[.*\\]", "\\1", spec)
        subcond <- paste0("(", subcond, ")")
        ovkatstr <- gsub("^\\[(.*?)\\]=\\[(.*)\\]", "\\2", spec)
      } else {
        subcond <- "TRUE"
        ovkatstr <- spec
      }
      
      overkat <- list()
      ovkatstr <- gsub("([^=]+)=([^=]+)", "\\1==\\2", ovkatstr)
      ovkatstr <- gsub("(.*)ALDER=='*ALLE'*(.*)", paste0("\\1", "ALDER==", FGP$amin, "_", FGP$amax, "\\2"), ovkatstr)
      ovkatstr <- gsub("(.*)ALDER=='*(\\d+)_('| )(.*)", paste0("\\1", "ALDER==\\2_", FGP$amax, "\\3\\4"), ovkatstr)
      for (del in names(globs$DefDesign$DelKolN)) {
        delN <- globs$DefDesign$DelKolN[del]
        if (globs$DefDesign$DelType[del] == "COL") {
          if (grepl(paste0("(^|\\&) *", delN, " *== *'*(.*?)'* *(\\&|$)"), ovkatstr)) {
            over <- gsub(paste0(".*(^|\\&) *(", delN, " *== *'*.*?'*) *(\\&|$).*"), "\\2", ovkatstr)
            overkat[[del]] <- list(over = over, kols = delN)
          }
        } else if (globs$DefDesign$DelType[del] == "INT") {
          if (grepl(paste0("(^|\\&) *", delN, "l *== *'*(.*?)'* *($|\\&)"), ovkatstr) &&
              grepl(paste0("(^|\\&) *", delN, "h *== *'*(.*?)'* *($|\\&)"), ovkatstr)) {
            overl <- gsub(paste0(".*(^|\\&) *(", delN, "l *== *'*.*?)'* *($|\\&).*"), "\\2", ovkatstr)
            overh <- gsub(paste0(".*(^|\\&) *(", delN, "h *== *'*.*?)'* *($|\\&).*"), "\\2", ovkatstr)
            overkat[[del]] <- list(over = paste(overl, overh, sep = " & "), kols = paste0(delN, c("l", "h")))
          } else if (grepl(paste0("(^|\\&) *", delN, " *== *'*(.*?)'* *($|\\&)"), ovkatstr)) {
            intval <- unlist(stringr::str_split(gsub(paste0("(^|.*\\&) *", delN, " *== *'*(.*?)'* *($|\\&.*)"), "\\2", ovkatstr), "_"))
            if (length(intval) == 1) {
              intval <- c(intval, intval)
            }
            over <- paste0(paste0(delN, "l"), "==", intval[1], " & ", paste0(delN, "h"), "==", intval[2])
            overkat[[del]] <- list(over = over, kols = paste0(delN, c("l", "h")))
          }
        }
      }
      Foverkat[[i]] <- list(subcond = subcond, overkat = overkat)
      i <- i + 1
    }
  }
  return(Foverkat)
}



#' DFHeadToString (kb)
#'
#' @param innDF 
#' @param topn 
DFHeadToString <- function(innDF, topn = 10) {
  # is_kh_debug()
  
  # Bruker data.table print for summary
  DT <- data.table::data.table(innDF)
  optdef <- getOption("width") # Sett bred output
  options(width = 250)
  head <- paste(capture.output(print(print(DT, topn = topn))), collapse = "\n")
  head <- sub("NULL$", "", head)
  options(width = optdef)
  return(head)
}

#' kube_spec (ybk)
#' 
#' Saves ACCESS specs + list of dimensions to be used in STATA censoring
save_kubespec_csv <- function(spec, dims = NULL, geonaboprikk = NULL, geoprikktriangel = NULL){
  # is_kh_debug()
  
  rootDir <- file.path(fs::path_home(), "helseprofil")
  if (!fs::dir_exists(rootDir))
    fs::dir_create(rootDir)
  
  specDF <- data.table::as.data.table(spec)
  varStata <- grep("^Stata", names(specDF), value = TRUE)
  varSpec <- c("KUBE_NAVN", varStata)
  varDF <- specDF[, .SD, .SDcols = varSpec]
  if(!is.null(dims)) varDF[, DIMS := list(dims)]
  if(!is.null(geonaboprikk)) varDF[, GEOnaboprikk := as.character(geonaboprikk)]
  if(!is.null(geoprikktriangel)) varDF[, names(geoprikktriangel) := geoprikktriangel]
  fileSpec <- file.path(rootDir, "kubespec.csv")
  data.table::fwrite(varDF, fileSpec, sep = ";", sep2 = c("", " ", ""))
  return(invisible(specDF))
}

get_geonaboprikk_triangles <- function(geoniv){
  data.table::data.table("Stata_naboprGeo_LF" = paste0("niva1", getOption("khfunctions.geoprikk")$LF),
                         "Stata_naboprGeo_FK" = paste0("niva2", getOption("khfunctions.geoprikk")$FK),
                         "Stata_naboprGeo_KB" = paste0("niva3", getOption("khfunctions.geoprikk")$KB))
}

#' find_dims (vl)
#' 
#' Helper function for kube_spec, finding dimensions in KUBE
find_dims_for_stataprikk <- function(dt, etabs){
  alldims <- c(getOption("khfunctions.khtabs"), etabs$tabnames)
  alldims[alldims %in% names(dt)]
}

#' LagAlleFriskvikIndikatorerForKube (kb)
#'
#' @param KUBEid 
#' @param KUBE 
#' @param FGP
#' @param modus 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
#' @param ...
LagAlleFriskvikIndikatorerForKube <- function(KUBEid, 
                                              KUBE, 
                                              aargang,
                                              FGP, 
                                              modus, 
                                              batchdate, 
                                              globs = get_global_parameters(),  
                                              ...) {
  
  if(is_empty(modus)) stop("KUBER::MODUS er ikke satt for kuben")
  indikatorer <- sqlQuery(globs$dbh, paste0("SELECT INDIKATOR, ID FROM FRISKVIK WHERE AARGANG=", aargang, "AND KUBE_NAVN='", KUBEid, "'"), as.is = TRUE)
  
  if (dim(indikatorer)[1] > 0) {
    for (i in 1:dim(indikatorer)[1]) {
      cat("Lager Friskvikfil for ", indikatorer[i, 1], "\n")
      LagFriskvikIndikator(id = indikatorer[i, 2], KUBE = KUBE, FGP = FGP, modus = modus, aargang = aargang, batchdate = batchdate, globs = globs, ...)
    }
  }
}

#' LagFriskvikIndikator (kb)
#'
#' @param id 
#' @param KUBE 
#' @param FGP 
#' @param modus
#' @param aargang 
#' @param batchdate 
#' @param globs global parameters, defaults to SettGlobs
LagFriskvikIndikator <- function(id, 
                                 KUBE = data.table(), 
                                 FGP = list(amin = 0, amax = 120), 
                                 modus, 
                                 aargang,
                                 batchdate = SettKHBatchDate(), 
                                 globs = get_global_parameters()) {
  
  FVdscr <- sqlQuery(globs$dbh, paste0("SELECT * FROM FRISKVIK WHERE ID=", id), as.is = TRUE)
  
  moduser <- unlist(stringr::str_split(FVdscr$MODUS, ""))
  
  
  ## FHP and Oppveksprofile (OVP) folders specification
  profile <- FVdscr$PROFILTYPE
  
  switch(profile,
         "FHP" = {
           setDir_K <- getOption("khfunctions.fhpK")
           setDir_B <- getOption("khfunctions.fhpB")
           setDir_F <- getOption("khfunctions.fhpF")
         },
         "OVP" = {
           setDir_K <- getOption("khfunctions.ovpK")
           setDir_B <- getOption("khfunctions.ovpB")
           setDir_F <- getOption("khfunctions.ovpF")
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
        FVdscr$ALDER <- gsub("^(\\d+)_$", paste0("\\1_", FGP$amax), FVdscr$ALDER)
        FVdscr$ALDER <- gsub("^_(\\d+)$", paste0(FGP$amin, "_\\1"), FVdscr$ALDER)
        FVdscr$ALDER <- gsub("^ALLE$", paste0(FGP$amin, "_", FGP$amax), FVdscr$ALDER)
        filterA <- c(filterA, paste0("ALDER=='", FVdscr$ALDER, "'"))
      }
      for (tab in c("AARh", "KJONN", "INNVKAT", "UTDANN", "LANDBAK")) {
        if (grepl("\\S", FVdscr[[tab]]) & FVdscr[[tab]] != "-") {
          filterA <- c(filterA, paste0(tab, "==", FVdscr[[tab]]))
        }
      }
      if (grepl("\\S", FVdscr$EKSTRA_TAB) & FVdscr$EKSTRA_TAB != "-") {
        filterA <- c(filterA, FVdscr$EKSTRA_TAB)
        KUBE$ETAB <- FVdscr$EKSTRA_TAB
      }
      filter <- paste(filterA, collapse = " & ")
      FRISKVIK <- subset(KUBE, eval(parse(text = filter)))
      
      defrows <- nrow(subset(globs$GeoKoder, FRA <= aargang & TIL > aargang & TYP == "O" & GEOniv %in% GEOfilter))
      if (nrow(FRISKVIK) != defrows) {
        KHerr(paste("FEIL I FRISKVIKFILTER", filter, "GIR bare", nrow(FRISKVIK), "/", defrows, "rader!"))
      }
      
      # SISTE RYDD KOLONNER (bare for TabKols)
      MissKol <- setdiff(c("GEO", "AAR", "KJONN", "ALDER", "UTDANN", "INNVKAT", "LANDBAK", "ETAB"), names(FRISKVIK))
      if (length(MissKol) > 0) {
        FRISKVIK[, (MissKol) := NA]
      }
      
      MissKol2 <- setdiff(c(getOption("khfunctions.profiltabs"), getOption("khfunctions.profilvals")), names(FRISKVIK))
      if (length(MissKol2) > 0) {
        KHerr(paste("FEIL: Kolonnene", MissKol2, "mangler i Friskvik!"))
        FRISKVIK[, (MissKol2) := NA]
      }
      
      if (grepl("\\S", FVdscr$ALTERNATIV_MALTALL)) {
        FRISKVIK$MALTALL <- FRISKVIK[[FVdscr$ALTERNATIV_MALTALL]]
        kastkols <- setdiff(getOption("khfunctions.profilvals"), "MALTALL")
        FRISKVIK[, (kastkols) := NA]
      }
      
      # ALLTID prikk MEIS dersom SPVFLAGG > 0. Denne skal ut i profiler, og kan ikke vaere uprikket. 
      FRISKVIK[SPVFLAGG > 0, MEIS := NA]
      
      FRISKVIK <- FRISKVIK[, mget(c(getOption("khfunctions.profiltabs"), getOption("khfunctions.profilvals")))]
      
      setPath <- file.path(getOption("khfunctions.root"), 
                           getOption("khfunctions.kubedir"), FriskVDir, aargang, "csv")
      
      ## Check path if doesn't exist so create
      if (!fs::dir_exists(setPath)) fs::dir_create(setPath)
      
      utfiln <- file.path(setPath, paste0(FVdscr$INDIKATOR, "_", batchdate, ".csv"))
      cat("-->> FRISKVIK EKSPORT:", utfiln, "\n")
      data.table::fwrite(FRISKVIK, utfiln, sep = ";", row.names = FALSE)
    } else {
      cat("ADVARSEL!!!!!!!! modus ", modus, "i FRISKVIK stoettes ikke\n")
    }
  }
}

#' LagQCKube (vl)
#' 
#' Adds uncensored columns sumTELLER/sumNEVNER/RATE.n to the ALLVISkube
#'
#' @param allvis Censored ALLVIs kube
#' @param uprikk Uncensored KUBE
#' @param allvistabs Dimensions included in ALLVIS kube
#' @param allvisvals All columns 
LagQCKube <- function(allvis,
                      allvistabs,
                      kube){
  # is_kh_debug()
  qcvals <- getOption("khfunctions.qcvals")
  QC <- data.table::copy(allvis)
  uprikk <- data.table::copy(kube)[, mget(c(allvistabs, qcvals))]
  data.table::setnames(uprikk, qcvals, paste0(qcvals, "_uprikk"))
  
  QC <- QC[uprikk, on = allvistabs]
  
  return(QC[])
}

#' @title GetAccessSpecs (vl)
#'
#' @param kuber 
#' @param tnp 
#' @param filgrupper 
#' @param STPNdscr 
#' @return
#' @export
save_access_specs <- function(parameters){
  specs <- data.table::rbindlist(list(melt_access_spec(parameters$CUBEinformation, name = "KUBER"),
                                      melt_access_spec(parameters$TNPinformation, name = "TNP_PROD")))
  if(parameters$CUBEinformation$REFVERDI_VP == "P") specs <- data.table::rbindlist(list(specs, melt_access_spec(parameters$STNPinformation, name = "STANDARD TNP")))
  
  for(i in names(parameters$fileinformation)){
    fgp <- parameters$fileinformation[[i]]
    end = which(names(fgp) == "vals")-1
    specs <- data.table::rbindlist(list(specs, melt_access_spec(fgp[1:end], name = paste0("FILGRUPPER: ", i))))
    if(i %in% parameters$FILFILTRE$FILVERSJON){
      specs <- data.table::rbindlist(list(specs, melt_access_spec(parameters$FILFILTRE[FILVERSJON == i], name = paste0("FILFILTRE: ", i))))
    }
  }
  
  if(length(parameters$friskvik$INDIKATOR) > 0){
    for(i in parameters$friskvik$ID){
      specs <- data.table::rbindlist(list(specs, melt_access_spec(parameters$friskvik[ID == i], name = paste0("FRISKVIK:ID-", i))))
    }
  }
  
  file <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kubedir"), getOption("khfunctions.kube.specs"), paste0("spec_", parameters$cube_name, "_", parameters$batchdate, ".csv"))
  data.table::fwrite(specs, file = file, sep = ";")
}

#' @title melt_access_spec
#' @description
#' helper function for save_access_specs
#' @noRd
melt_access_spec <- function(dscr, name = NULL){
  if(is.null(name)){
    name <- deparse(substitute(dscr))
  }
  d <- data.table::as.data.table(dscr)
  d[, names(d) := lapply(.SD, as.character)]
  d <- data.table::melt(d, measure.vars = names(d), variable.name = "Kolonne", value.name = "Innhold")
  d[, Tabell := name]
  data.table::setcolorder(d, "Tabell")
}

#' KHaggreger (kb)
#'
#' @param FIL 
#' @param vals 
#' @param snitt 
#' @param globs global parameters, defaults to SettGlobs
KHaggreger <- function(FIL, vals = list(), parameters) {
  # is_kh_debug()
  
  orgclass <- class(FIL)
  orgcols <- names(FIL)
  if (identical(orgclass, "data.frame")) {
    FIL <- data.table::setDT(FIL)
  }
  orgkeys <- data.table::key(FIL)
  tabnames <- names(FIL)[names(FIL) %in% parameters$DefDesign$DesignKolsFA]
  valkols <- get_value_columns(names(FIL))
  if(!identical(data.table::key(FIL), tabnames)) data.table::setkeyv(FIL, tabnames)
  
  FIL[, names(.SD) := lapply(.SD, sum), .SDcols = valkols, by = tabnames]
  FIL[, names(.SD) := lapply(.SD, max), .SDcols = paste0(valkols, ".f"), by = tabnames]
  colorder <- tabnames
  for(val in valkols){
    FIL[is.na(get(val)) | get(val) == 0, paste0(val, ".a") := 0]
    colorder <- c(colorder, paste0(val, c("", ".f", ".a")))
  }
  FIL[, names(.SD) := lapply(.SD, sum), .SDcols = paste0(valkols, ".a"), by = tabnames]
  data.table::setcolorder(FIL, colorder)
  
  vals <- vals[valkols]
  usumbar <- valkols[unlist(lapply(vals[valkols], function(x) {
    x$sumbar == 0
  }))]
  for (val in valkols) {
    if (!is.null(vals[[val]]) && vals[[val]]$sumbar == 0) {
      eval(parse(text = paste(
        "FIL[", val, ".a>1,c(\"", val, "\",\"", val, ".f\"):=list(NA,2)]",
        sep = ""
      )))
    }
  }
  if(!identical(data.table::key(FIL), orgkeys)) data.table::setkeyv(FIL, tabnames)
  
  return(unique(FIL))
}
