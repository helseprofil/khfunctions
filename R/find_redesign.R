#' @title find_redesign
#' 
#' @description
#' WIP: replacement for finnredesign()
#' 
#' @return A list with information to redesign a file into a target design.
#'
#' @param orgdesign design of file, must be set with FinnDesign()
#' @param targetdesign target design, can be simpler than orgdesign as only uses $Part
#' @param aggregate aggregate parts? Used in standardization, default = character()
#' @param parameters global parameters
find_redesign <- function(orgdesign, targetdesign, aggregate = character(), parameters) {
  IntervallHull = parameters$DefDesign$IntervallHull
  AggPri = parameters$DefDesign$AggPri
  
  FULL <- get_all_dimension_combinations_targetdesign(targetdesign = targetdesign)
  namesFULL <- names(FULL) # Need to get the original colnames before manipulation
  TempFile <- file.path(tempdir(), paste0("full", SettKHBatchDate(), ".RDS"))
  saveRDS(FULL, TempFile)
  FULL <- add_betcols_to_full(dt = FULL, betcols = grep("HAR", names(orgdesign$SKombs$bet), value = T, invert = T))
  targetdesign <- add_missing_parts_from_orgdesign(targetdesign, orgdesign)
  
  out <- list()
  Parts <- set_redesign_parts(orgdesign = orgdesign, targetdesign = targetdesign, parameters = parameters)
  
  # TODO: Ekstrahere det under til en funksjon, legge alt direkte inn i out
  # Sjekke om alle tabellene er nødvendig å beregne/ta med ut 
  
  SKombs <- list()
  KBs <- list()
  Filters <- list()
  DelStatus <- list()
  
  # Maa passe paa rekkefoelge (Ubeting til slutt), ellers kan det gaa galt i FULL
  beting <- intersect(rev(AggPri), c(parameters$DefDesign$BetingOmk, parameters$DefDesign$BetingF))
  ubeting <- intersect(rev(AggPri), c(parameters$DefDesign$UBeting))
  partcols <- intersect(c(beting, ubeting), names(Parts))
  
  for(del in partcols) {
    kols <- parameters$DefDesign$DelKols[[del]]
    kolsomk <- paste0(kols, "_omk")
    delD <- paste0(del, "_Dekk")
    delP <- paste0(del, "_pri")
    delH <- paste0(del, "_HAR")
    if (length(orgdesign[["UBeting"]]) > 0) {
      if (del %in% orgdesign[["UBeting"]]) {
        kombn <- "bet"
      } else {
        kombn <- paste0("bet", del)
      }
      # Koble med orgdesign
      data.table::setkeyv(Parts[[del]], kols)
      data.table::setkeyv(orgdesign$SKombs[[kombn]], kols)
      betD <- collapse::join(orgdesign$SKombs[[kombn]], Parts[[del]], how = "right", multiple = T, verbose = F)
      betD[is.na(HAR), HAR := 0]
      # Maa kaste de som ikke har del_Dekk==1 hvis ikke kan de feilaktig
      # faa del_Dekk==1 under dersom del er i beting, da vil en annen del i beting faa NA og by=betcols gaar galt!)
      betD <- subset(betD, eval(parse(text = paste0(del, "_Dekk==1"))))
      betD <- betD[get(delD) == 1]
      # Sett (betinget) dekning
      betcols <- unlist(parameters$DefDesign$DelKols[setdiff(orgdesign[["UBeting"]], del)])
      betD <- SettPartDekk(betD, del = del, har = "HAR", betcols = betcols, parameters = parameters)
    } else {
      betcols <- character()
      betD <- data.table::copy(Parts[[del]])
    }
    
    # Finn beste alternativ
    bycols <- c(kolsomk, betcols)
    if (del %in% aggregate) {
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
#' @param targetdesign targetdesign
#' @keywords internal
#' @noRd
get_all_dimension_combinations_targetdesign <- function(targetdesign){
  if(!is.null(targetdesign$Design)){
    FULL <- targetdesign$Design[HAR == 1]
  } else {
    FULL <- do.call(expand.grid.dt, targetdesign$Part)
  }
  
  FULL[, (names(.SD)) := NULL, .SDcols = grep("_HAR$|^HAR$", names(FULL), value = T)]
  data.table::setnames(FULL, names(FULL), paste0(names(FULL), "_omk"))
  return(FULL)    
}

#' @keywords internal
#' @noRd
add_betcols_to_full <- function(dt, betcols){
  if(length(betcols) == 0) return(dt)
  return(expand.grid.dt(dt, orgdesign$SKombs$bet[, ..betcols]))
}

#' @keywords internal
#' @noRd
add_missing_parts_from_orgdesign <- function(target, org){
  for(del in org$UBeting){
    if(is.null(target$Part[[del]])) target$Part[[del]] <- data.table::copy(org$Part[[del]])
  }
  return(target)
}

set_redesign_parts <- function(orgdesign, targetdesign, parameters){
  out <- list()
  redesignparts <- intersect(names(orgdesign$Part), names(targetdesign$Part))
  
  for(part in redesignparts){
    partinfo <- get_part_info(part = part)
    target_part <- data.table::copy(targetdesign$Part[[part]])
    org_part <- data.table::copy(orgdesign$Part[[part]])
    if(!partinfo$har %in% names(target_part)) target_part[, partinfo$har := 1]
    cb_part <- get_codebook_part(codebook = parameters$KB, target_part = target_part, partinfo = partinfo)
    if(partinfo$type == "COL") cb_part <- format_codebook_col(cb_part = cb_part, partinfo = partinfo, target_part = target_part, org_part = org_part)
    if(partinfo$type == "INT") cb_part <- format_codebook_int(cb_part = cb_part, partinfo = partinfo, target_part = target_part, org_part = org_part)
    
    cb_part[, keep := as.integer(any(get(partinfo$har) == 1)), by = c(partinfo$colsomk, partinfo$pri)]
    cb_part <- cb_part[keep == 1][, keep := NULL]
    setkeyv(cb_part, partinfo$cols)
    out[[part]] <- cb_part
  }
  return(out)
}
  
get_part_info <- function(part){
  out <- list()
  out[["part"]] <- part
  out[["type"]] <- parameters$DefDesign$DelType[[part]]
  out[["name"]] <- parameters$DefDesign$DelKolN[[part]]
  out[["omk"]] <- paste0(out$name, "_omk")
  out[["pri"]] <- paste0(part, "_pri")
  out[["obl"]] <- paste0(part, "_obl")
  out[["har"]] <- paste0(part, "_HAR")
  out[["dekk"]] <- paste0(part, "_Dekk")
  out[["cols"]] <- parameters$DefDesign$DelKols[[part]]
  out[["colsomk"]] <- paste0(out$cols, "_omk")
  return(out)
}

get_codebook_part <- function(codebook, target_part, partinfo){
  out <- codebook[[partinfo$part]]
  if(grepl("^T\\d$", partinfo$part) & nrow(out) == 0){
    col <- target_part[[partinfo$name]]
    out <- data.table::data.table(col, col, 0, 1)
    data.table::setnames(out, unlist(partinfo[c("name", "omk", "pri", "obl")]))
  }
  return(out)
}

format_codebook_col <- function(cb_part, partinfo, target_part, org_part){
  cb_part <- cb_part[get(partinfo$omk) %in% unique(target_part[[partinfo$name]])]
  cb_part[, partinfo$har := 0]
  cb_part[get(partinfo$name) %in% org_part[[partinfo$name]], partinfo$har := 1]
  cb_part[, partinfo$dekk := as.integer(!any(get(partinfo$obl) == 1 & get(partinfo$har) == 0)), by = c(partinfo$omk, partinfo$pri)]
  return(cb_part)
}

int_string <- function(dt, cols){
  dt[, paste(get(cols[1]), get(cols[2]), sep = "_")]
}

format_codebook_int <- function(cb_part, partinfo, target_part, org_part){
  org_min <- min(org_part[[partinfo$cols[1]]])
  org_max <- max(org_part[[partinfo$cols[2]]])
  target_intervals <- int_string(dt = target_part, cols = partinfo$cols)
  if (nrow(cb_part) > 0) {
    cb_part[, names(.SD) := lapply(.SD, function(x) gsub("^ALLE$", paste0(org_min, "_", org_max), x, ignore.case = TRUE)), .SDcols = c(partinfo$name, partinfo$omk)]
    cb_part[, (partinfo$cols) := data.table::tstrsplit(get(partinfo$name), "_", fixed = TRUE)]
    cb_part[, (partinfo$colsomk) := data.table::tstrsplit(get(partinfo$omk), "_", fixed = TRUE)]
    cb_part <- cb_part[get(partinfo$omk) %in% target_intervals]
  }
  interval_from <- org_part[, .SD, .SDcols = partinfo$cols][!int_string(dt = org_part, cols = partinfo$cols) %in% parameters$LegKoder]
  interval_to <- target_part[, .SD, .SDcols = partinfo$cols][!int_string(dt = target_part, cols = partinfo$cols) %in% parameters$LegKoder]
  cb_intervals <- FinnKodebokIntervaller(interval_from, interval_to, delnavn = partinfo$part)
  cb_intervals[, (partinfo$obl) := 1]
  if(partinfo$part == "A") cb_intervals[cb_intervals$ALDERl >= 90, (partinfo$obl) := 0]
  
  outnames <- names(cb_intervals)
  cb_part <- data.table::rbindlist(list(cb_intervals, cb_part[, .SD, .SDcols = names(cb_intervals)]))
  cb_part[, names(.SD) := lapply(.SD, as.integer), .SDcols = names(cb_intervals)]
  cb_part <- collapse::join(org_part, cb_part, on = partinfo$cols, how = "r", verbose = 0, overid = 2) # bytt til "l" når alt fungerer
  cb_part[is.na(get(partinfo$har)), (partinfo$har) := 0]
  cb_part <- SettPartDekk(cb_part, del = partinfo$part, har = partinfo$har, parameters = parameters)
  return(cb_part)
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

