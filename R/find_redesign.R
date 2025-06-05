#' @title find_redesign
#' @return A list with information to redesign a file into a target design.
#' @param orgdesign design of file, must be set with FinnDesign()
#' @param targetdesign target design, can be simpler than orgdesign as only uses $Part
#' @param aggregate aggregate parts? Used in standardization, default = character()
#' @param parameters global parameters
find_redesign <- function(orgdesign, targetdesign, aggregate = character(), parameters) {
  FULL <- get_all_dimension_combinations_targetdesign(targetdesign = targetdesign)
  namesFULL <- names(FULL) # Need to get the original colnames before manipulation
  TempFile <- file.path(tempdir(), paste0("full", SettKHBatchDate(), ".RDS"))
  saveRDS(FULL, TempFile)
  FULL <- add_betcols_to_full(dt = FULL, orgdesign = orgdesign)
  targetdesign <- add_missing_parts_from_orgdesign(targetdesign, orgdesign)
  any_ubeting <- length(orgdesign$UBeting) > 0
  
  out <- setNames(replicate(8, list()), 
                  c("Parts", "SKombs", "KBs", "Filters", "FULL", "Dekk", "Udekk", "DelStatus"))
  out[["Parts"]] <- set_redesign_parts(orgdesign = orgdesign, targetdesign = targetdesign, parameters = parameters)
  partcols <- get_partcols_and_set_aggpri(Parts = out$Parts, parameters = parameters)
  
  for(part in partcols){
    partinfo <- get_part_info(part)
    partinfo[["kombname"]] <- ifelse(part %in% orgdesign$UBeting, "bet", paste0("bet", part))
    partinfo[["aggregate"]] <- aggregate
    if(any_ubeting) partinfo[["betcols"]] <- unlist(parameters$DefDesign$DelKols[setdiff(orgdesign$UBeting, part)]) 
    partdata <- data.table::copy(out$Parts[[part]])
    if(any_ubeting) partdata <- merge_partdata_orgdesign(partdata = partdata, orgdesign = orgdesign, partinfo = partinfo)
    set_partdata_bruk(partdata = partdata, partinfo = partinfo)
    cb_part <- partdata[Bruk == get(partinfo$pri) & get(partinfo$har) == 1]
    FULL <- merge_cbpart_to_full(full = FULL, cb_part = cb_part)
    
    out$SKombs[[part]] <- partdata
    cb_part <- filter_cbpart(cb_part = cb_part, partinfo = partinfo)
    
    filter <- all(cb_part$codebook[, .SD, .SDcols = partinfo$cols] == cb_part$codebook[, .SD, .SDcols = partinfo$colsomk])
    if(filter) cb_part <- switch_cbpart_to_filter(cb_part = cb_part, partinfo = partinfo)
    # if(part == "Y" & cb_part$status == "B") stop("Delstatus for AAR kan ikke være B"), tror ikke denne trengs da delstatus ikke brukes i omkodfil. Må sjekke. 
    
    out$KBs[[part]] <- cb_part$codebook
    out$DelStatus[[part]] <- cb_part$status
    out$Filters[[part]] <- cb_part$filter
  }
  
  out[["FULL"]] <- FULL
  out[["Dekk"]] <- get_dekk(full = FULL)
  out[["Udekk"]] <- handle_udekk(FULL, namesFULL, TempFile)
  gc()
  return(out)
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
add_betcols_to_full <- function(dt, orgdesign){
  betcols <-  grep("HAR", names(orgdesign$SKombs$bet), value = T, invert = T)
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

#' @keywords internal
#' @noRd
set_redesign_parts <- function(orgdesign, targetdesign, parameters){
  out <- list()
  redesignparts <- intersect(names(orgdesign$Part), names(targetdesign$Part))
  
  for(part in redesignparts){
    partinfo <- get_part_info(part = part)
    target_part <- data.table::copy(targetdesign$Part[[part]])
    org_part <- data.table::copy(orgdesign$Part[[part]])
    if(!partinfo$har %in% names(target_part)) target_part[, partinfo$har := 1]
    cb_part <- get_global_codebook_part(codebook = parameters$KB, target_part = target_part, partinfo = partinfo)
    if(partinfo$type == "COL") cb_part <- format_global_codebook_col(cb_part = cb_part, partinfo = partinfo, target_part = target_part, org_part = org_part)
    if(partinfo$type == "INT") cb_part <- format_global_codebook_int(cb_part = cb_part, partinfo = partinfo, target_part = target_part, org_part = org_part)
    
    cb_part[, keep := as.integer(any(get(partinfo$har) == 1)), keyby = c(partinfo$colsomk, partinfo$pri)]
    data.table::setkeyv(cb_part, partinfo$cols) # KUTT? 
    cb_part <- cb_part[keep == 1][, keep := NULL]
    out[[part]] <- cb_part
  }
  return(out)
}

#' @title get_global_codebook_part
#' @description
#' Fetch global codebook from parameters, read from ACCESS::KH_KODER
#' @keywords internal
#' @noRd
get_global_codebook_part <- function(codebook, target_part, partinfo){
  out <- codebook[[partinfo$part]]
  if(grepl("^T\\d$", partinfo$part) & nrow(out) == 0){
    col <- target_part[[partinfo$name]]
    out <- data.table::data.table(col, col, 0, 1)
    data.table::setnames(out, unlist(partinfo[c("name", "omk", "pri", "obl")]))
  }
  return(out)
}

#' @keywords internal
#' @noRd
format_global_codebook_col <- function(cb_part, partinfo, target_part, org_part){
  cb_part <- cb_part[get(partinfo$omk) %in% unique(target_part[[partinfo$name]])]
  cb_part[, partinfo$har := 0]
  cb_part[get(partinfo$name) %in% org_part[[partinfo$name]], partinfo$har := 1]
  cb_part[, partinfo$dekk := as.integer(!any(get(partinfo$obl) == 1 & get(partinfo$har) == 0)), by = c(partinfo$omk, partinfo$pri)]
  return(cb_part)
}

#' @keywords internal
#' @noRd
format_global_codebook_int <- function(cb_part, partinfo, target_part, org_part){
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

#' @keywords internal
#' @noRd
int_string <- function(dt, cols){
  dt[, paste(get(cols[1]), get(cols[2]), sep = "_")]
}

#' @title FinnKodebokIntervaller
#' @keywords internal
#' @noRd
FinnKodebokIntervaller <- function(FRA, TIL, delnavn = "INT") {
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



#' @description
#' Creates a list of parts to set recoding
#' @keywords internal
#' @noRd
get_partcols_and_set_aggpri <- function(Parts, parameters){
  AggPri <- parameters$DefDesign$AggPri
  # Maa passe paa rekkefoelge (Ubeting til slutt), ellers kan det gaa galt i FULL
  beting <- intersect(rev(AggPri), unlist(parameters$DefDesign[c("BetingOmk", "BetingF")], use.names = F))
  ubeting <- intersect(rev(AggPri), parameters$DefDesign$UBeting)
  return(intersect(c(beting, ubeting), names(Parts)))
}

#' @keywords internal
#' @noRd
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

#' @keywords internal
#' @noRd
merge_partdata_orgdesign <- function(partdata, orgdesign, partinfo){
  orgdata <- orgdesign$SKombs[[partinfo$kombname]]
  d <- collapse::join(orgdata, partdata, on = partinfo$cols, how = "right", multiple = T, verbose = F)[get(partinfo$dekk) == 1]
  data.table::setkeyv(d, c(partinfo$cols, partinfo$colsomk)) # KUTT
  d[is.na(HAR), HAR := 0]
  d <- SettPartDekk(d, del = partinfo$part, har = "HAR", betcols = partinfo$betcols, parameters = parameters)
  return(d)
}

#' @title SettPartDekk (kb)
#' @keywords internal
#' @noRd
SettPartDekk <- function(KB, del = "", har = paste0(del, "_HAR"), betcols = NULL, parameters) {
  
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

#' @description
#' Set Bruk column, used to generate the codebook for the part, by reference.
#' If part is aggregated, 'Bruk' is set to max(part_pri), otherwise it is set to min(part_pri). 
#' This is done by in strata of the recode-column as well as conditional columns (year and geoniv)
#' @keywords internal
#' @noRd
set_partdata_bruk <- function(partdata, partinfo){
  bycols = unlist(partinfo[c("colsomk", "betcols")], use.names = F)
  part_aggregate <- partinfo$part %in% partinfo$aggregate
  if(part_aggregate) partdata[get(partinfo$dekk) == 1, Bruk := max(get(partinfo$pri)), by = bycols]
  if(!part_aggregate) partdata[get(partinfo$dekk) == 1, Bruk := min(get(partinfo$pri)), by = bycols]
}

#' @keywords internal
#' @noRd
merge_cbpart_to_full <- function(full, cb_part){
  commoncols <- intersect(names(full), names(cb_part))
  full <- collapse::join(full, cb_part[, .SD, .SDcols = commoncols], 
                         on = commoncols, how = "inner", multiple = TRUE, overid = 2, verbose = 0)
  return(full)
}

#' @description
#' # Sjekk om del kan omkodes helt partielt (fra Part) eller om maa betinge (dvs KB)
# Finner om en omk_kode bruker flere versjoner av partiell omkoding (hver versjon fra Part har ulik prid)
# Om en slik finnes beholdes KB, ellers fjernes overloedig betinging
#' @keywords internal
#' @noRd
filter_cbpart <- function(cb_part, partinfo){
  out <- list()
  n_pri <- cb_part[, .(n_pri = length(unique(get(partinfo$pri)))), by = c(partinfo$colsomk)][, max(n_pri)]
  out[["status"]] <- ifelse(n_pri == 1, "P", "B") # omdøp til Partiell/Kodebok
  items <- c("cols", "colsomk")
  if(n_pri != 1) items <- c("betcols", items)  
  out[["codebook"]] <- unique(cb_part[, .SD, .SDcols = unlist(partinfo[items], use.names = F)])
  return(out)
}

#' @keywords internal
#' @noRd
switch_cbpart_to_filter <- function(cb_part, partinfo){
  cb_part$filter <- cb_part$codebook[, .SD, .SDcols = partinfo$cols]
  cb_part$codebook <- NULL
  cb_part$status <-  "F" #rename to filter
  return(cb_part)
}

#' @keywords internal
#' @noRd
get_dekk <- function(full){
  omkkols <- grep("_omk$", names(full), value = T)
  data.table::setkeyv(full, omkkols)
  Dekk <- unique(full[, ..omkkols])
  data.table::setnames(Dekk, names(Dekk), gsub("_omk$", "", names(Dekk)))
  return(Dekk)
}

#' handle_udekk (ybk)
#' @keywords internal
#' @noRd
handle_udekk <- function(FULL, namesFULL, TempFile){
  Udekk <- readRDS(TempFile)
  data.table::setkeyv(Udekk, namesFULL)
  data.table::setkeyv(FULL, namesFULL)
  Udekk <- Udekk[!FULL, allow.cartesian = TRUE]
  data.table::setnames(Udekk, namesFULL, gsub("_omk$", "", namesFULL))
  return(Udekk)
}
