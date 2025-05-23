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

#' handle_udekk (ybk)
#'
#' @param FULL 
#' @param namesFULL 
#' @param TempFile 
handle_udekk <- function(FULL, namesFULL, TempFile){
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

#' @title FinnKodebokIntervaller
#' @keywords internal
#' @noRd
FinnKodebokIntervaller <- function(FRA, TIL, delnavn = "INT") {
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

#' @title FinnKodebokForEtIntervall
#' @keywords internal
#' @noRd
FinnKodebokForEtIntervall <- function(Find, FRA, TILint, OVLP, letn, result, utcolnavn) {
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

#' @keywords internal
#' @noRd
DekkerInt <- function(FRA, TIL) {
  # Sjekker at hele intervallet TIL[,1]-TIL[,2] dekkes av intervallene i FRA
  # Bryr seg ikke om overlapp her, det gjoeres andre steder
  # Kompakt, men effektiv syntaks
  all(TIL[[1]]:TIL[[2]] %in% unlist(mapply(seq, FRA[[1]], FRA[[2]])))
}
