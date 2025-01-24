#' @title find_redesign
#'
#' @param fradesign 
#' @param tildesign 
#' @param SkalAggregeresOpp skal noen deler evt aggregeres opp?
#' @param globs global parameters, defaults to SettGlobs
find_redesign <- function(fradesign, tildesign, SkalAggregeresOpp = character(), globs = SettGlobs()) {
  is_kh_debug()
  KB = globs$KB
  IntervallHull = globs$DefDesign$IntervallHull
  AggPri = globs$DefDesign$AggPri
  
  FULL <- set_all_dimension_combinations_tildesign(tildesign = tildesign)
  
  TempFile <- file.path(tempdir(), paste0("full", SettKHBatchDate(), ".RDS"))
  saveRDS(FULL, TempFile)
  namesFULL <- names(FULL) # Need to get the original colnames before manipulation
  
  betKols <- setdiff(names(fradesign$SKombs$bet), "HAR")
  if (length(betKols) > 0) FULL <- expand.grid.dt(FULL, fradesign$SKombs$bet[, ..betKols])
  
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
      KBD <- as.data.table(KB[[del]])
      kol <- as.character(globs$DefDesign$DelKolN[del])
      kolomk <- paste0(kol, "_omk")
      kolomkpri <- c(kolomk, paste0(del, "_pri"))
      kols <- globs$DefDesign$DelKols[[del]]
      kolsomk <- paste0(kols, "_omk")
      kolsomkpri <- c(kolsomk, paste0(del, "_pri"))
      
      # Sett 1-1 koding for T1,T2,.. dersom ikke annet gitt
      if (grepl("^T\\d$", del) & nrow(KBD) == 0) {
        tilTabs <- d[, ..kol]
        KBD <- setnames(data.table::data.table(tilTabs, tilTabs, 0, 1), c(kol, kolomk, delP, delO))
        Parts[[del]] <- KBD
      }
      
      if (globs$DefDesign$DelType[del] == "COL") {
        if (nrow(KBD) > 0) {
          # Filtrer bort TIL-koder i global-KB som ikke er i tildesign
          KBD <- KBD[get(kolomk) %in% d[[kol]]]
          setkeyv(KBD, kolomkpri)
          KBD[, (delH) := as.integer(0L)]
          KBD[get(kol) %in% fradesign$Part[[del]][[kol]], (delH) := 1L]
          KBD[, (delD) := as.integer(!any(get(delH) == 0 & get(delO) == 1)), by = kolsomkpri]
          # Kast omkodinger uten noen deler i FRA, behold de som dekkes helt og delvis
          # Setter keep her, kaster til slutt
          KBD[, keep := as.integer(any(get(delH) == 1)), by = kolsomkpri]
        }
      } else if (globs$DefDesign$DelType[del] == "INT") {
        # Global KB kan inneholde (fil)spesifikke koden "ALLE", maa erstatte denne med "amin_amax" og lage intervall
        # Merk: dette gjelder typisk bare tilfellene der ukjent alder og evt tilsvarende skal settes inn under "ALLE"
        Imin <- min(fradesign$Part[[del]][[paste0(globs$DefDesign$DelKolN[[del]], "l")]])
        Imax <- max(fradesign$Part[[del]][[paste0(globs$DefDesign$DelKolN[[del]], "h")]])
        alle <- paste0(Imin, "_", Imax)
        if (nrow(KBD) > 0) {
          KBD[, names(.SD) := lapply(.SD, function(x) gsub("^(ALLE)$", alle, x)), .SDcols = c(kol, kolomk)]
          KBD[, (kols) := tstrsplit(get(kol), "_", fixed = TRUE)]
          KBD[, (kolsomk) := tstrsplit(get(kolomk), "_", fixed = TRUE)]
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
        IntFra <- IntFra[!apply(IntFra[, ..kols], 1, paste, collapse = "_") %in% globs$LegKoder[[del]]$KODE]
        IntTil <- IntTil[!apply(IntTil[, ..kols], 1, paste, collapse = "_") %in% globs$LegKoder[[del]]$KODE]
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
          KBD <- rbindlist(list(KBInt, KBD[, ..outnames]))
          KBD[, names(.SD) := lapply(.SD, as.integer), .SDcols = outnames]
        } else {
          KBD <- KBInt
        }
        
        # Koble paa "del_HAR"
        setkeyv(KBD, kols)
        KBD <- collapse::join(fradesign$Part[[del]], KBD, how = "r", verbose = 0)
        KBD[is.na(get(delH)), (delH) := 0]
        KBD <- SettPartDekk(KBD, del = del, har = delH, IntervallHull = IntervallHull, globs = globs)
        
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
  beting <- intersect(globs$DefDesign$AggPri[length(globs$DefDesign$AggPri):1], c(globs$DefDesign$BetingOmk, globs$DefDesign$BetingF))
  ubeting <- intersect(globs$DefDesign$AggPri[length(globs$DefDesign$AggPri):1], c(globs$DefDesign$UBeting))
  
  for (del in intersect(c(beting, ubeting), names(Parts))) {
    kols <- globs$DefDesign$DelKols[[del]]
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
      betcols <- unlist(globs$DefDesign$DelKols[setdiff(fradesign[["UBeting"]], del)])
      betD <- SettPartDekk(betD, del = del, har = "HAR", IntervallHull = IntervallHull, betcols = betcols, globs = globs)
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



#' @title find_all_dimension_combinations
#' @param design tildesign
#' @noRd
set_all_dimension_combinations_tildesign <- function(tildesign){
  ifelse(!is.null(tildesign$Design),
         FULL <- tildesign$Design[HAR == 1],
         FULL <- do.call(expand.grid.dt, tildesign$Part))
  
  HAR_columns <- grep("_HAR$|^HAR$", names(FULL), value = T)
  FULL[, (HAR_columns) := NULL]
  data.table::setnames(FULL, names(FULL), paste0(names(FULL), "_omk"))
  return(FULL)    
}
