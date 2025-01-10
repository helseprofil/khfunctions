# Helper functions used in the data processing, both in LagKUBE and LagFilgruppe

#' is_kh_debug (ybk)
#' 
#' Debugging all other functions in the project
#' To use, set show_functions or show_arguments = TRUE
is_kh_debug <- function(fun = show_functions, arg = show_arguments, show = FALSE){
  
  # If both are TRUE then show_arguments will be deactivated automatically.
  if (fun & arg)
    arg <- FALSE
  
  if (arg) {
    show = show_arguments
    out = sys.calls()[[1]]
  }
  
  if (fun) {
    show = show_functions
    out = sys.calls()[[1]][1]
  }
  
  if (show) {
    message("Execute: ", deparse(out))
  }
  
  invisible()
}

#' SettKHBatchDate
#' 
#' Used to set date tag in file names
SettKHBatchDate <- function() {
  format(Sys.time(), "%Y-%m-%d-%H-%M")
}

#' KHerr (kb)
KHerr <- function(error) {
  cat(
    "***************************************************************************\n",
    "*KHFEIL!! ", error, "\n***************************************************************************\n"
  )
}

#' DumpTabell (kb)
#' 
#' To save file dumps
#'
#' @param TABELL 
#' @param TABELLnavn 
#' @param globs 
#' @param format
DumpTabell <- function(TABELL, TABELLnavn, globs = SettGlobs(), format = NULL) {
  is_kh_debug()
  if(is.null(format)) format <- getOption("khfunctions.defdumpformat")
  
  if (format == "CSV") {
    write.table(TABELL, file = file.path(getOption("khfunctions.root"), getOption("khfunctions.dumpdir"), paste0(TABELLnavn, ".csv")), sep = ";", na = "", row.names = FALSE)
  } else if (format == "R") {
    .GlobalEnv$DUMPtabs[[TABELLnavn]] <- TABELL
    print(DUMPtabs)
  } else if (format == "STATA") {
    TABELL[TABELL == ""] <- " " # STATA stoetter ikke "empty-string"
    names(TABELL) <- gsub("^(\\d.*)$", "S_\\1", names(TABELL)) # STATA 14 taaler ikke numeriske kolonnenavn
    names(TABELL) <- gsub("^(.*)\\.([afn].*)$", "\\1_\\2", names(TABELL)) # Endre .a, .f, .n til _
    foreign::write.dta(TABELL, file.path(getOption("khfunctions.root"), getOption("khfunctions.dumpdir"), paste0(TABELLnavn, ".dta"), sep = ""))
  }
}

#' KjorStataScript (kb, ybk?)
#'
#' @param TABLE 
#' @param script 
#' @param tableTYP 
#' @param batchdate 
#' @param globs
KjorStataSkript <- function(TABLE, script, tableTYP = "DF", batchdate = SettKHBatchDate(), globs = SettGlobs()) {
  is_kh_debug()
  
  tmpdir <- file.path(fs::path_home(), "helseprofil", "STATAtmp")
  if(!fs::dir_exists(tmpdir)) fs::dir_create(tmpdir)
  wdOrg <- getwd()
  setwd(tmpdir)
  tmpdo <- paste("STATAtmp_", batchdate, ".do", sep = "")
  tmpdta <- paste("STATAtmp_", batchdate, ".dta", sep = "")
  tmplog <- paste("STATAtmp_", batchdate, ".log", sep = "")
  TABLE[TABLE == ""] <- " " # STATA stÃ¸tter ikke "empty-string"
  names(TABLE) <- gsub("^(\\d.*)$", "S_\\1", names(TABLE)) # STATA 14 tÃ¥ler ikke numeriske kolonnenavn
  names(TABLE) <- gsub("^(.*)\\.([afn].*)$", "\\1_\\2", names(TABLE)) # Endre .a, .f, .n og .fn1/3/9 til _
  haven::write_dta(TABLE, tmpdta)
  
  sink(tmpdo)
  cat("use ", tmpdta, "\n", sep = "")
  cat(script, "\n")

  if (globs$StataVers < 12) {
    cat("save ", tmpdta, ",  replace\n", sep = "")
  } else if (globs$StataVers %in% 12:13) {
    cat("saveold ", tmpdta, ", replace\n", sep = "")
  } else {
    cat("saveold ", tmpdta, ", version(11) replace\n", sep = "")
  }
  sink()
  statacall <- paste("\"", globs$StataExe, "\" /e do ", tmpdo, " \n", sep = "")
  system(statacall, intern = TRUE)
  log <- readLines(tmplog)
  feil <- ""
  if (log[length(log)] != "end of do-file") {
    log_start <- which(grepl(paste("do", tmpdo), log))
    feil <- paste(log[log_start:length(log)], collapse = "\n")
  } else {
    TABLE <- haven::read_dta(tmpdta, encoding = "UTF-8")
  }
  # Reverserer omforminger for aa kunne skrive til STATA
  TABLE[TABLE == " "] <- ""
  names(TABLE) <- gsub("^S_(\\d.*)$", "\\1", names(TABLE))
  names(TABLE) <- gsub("^(.*)_([afn].*)$", "\\1.\\2", names(TABLE)) # Endre _a, _f, _n og _fn1/3/9 til .
  # delete data file
  file.remove(tmpdta)
  setwd(wdOrg)
  if (tableTYP == "DT") {
    data.table::setDT(TABLE)
  }
  return(list(TABLE = TABLE, feil = feil))
}

#' warn_prikk
#' 
#' helper function in STATA censoring
warn_prikk <- function(r, s){
  is_kh_debug()
  
  if (r > 0 & s > 0){
    stop("You can't prikk for both R and Stata way. Choose either one!")
  }
  
  invisible()
}

#' get_value_columns
get_value_columns <- function(columnnames, full = FALSE) {
  valcols <- grep("^(.*?)\\.f$", columnnames, value = T)
  valcols <- gsub("\\.f$", "", valcols)
  if(full) valcols <- paste0(rep(valcols, each = 4), c("", ".f", ".a", ".n"))
  return(intersect(columnnames, valcols))
}

#' get_dimension_columns
get_dimension_columns <- function(columnnames) {
  notab <- c(get_value_columns(columnnames, full = TRUE), "KOBLID", "ROW")
  return(setdiff(columnnames, notab))
}

#' FinnDesign (kb)
#' 
#' Brukes i lagkube og lagfilgruppe
#'
#' @param FIL 
#' @param FGP 
#' @param globs 
FinnDesign <- function(FIL, FGP = list(amin = 0, amax = 120), globs = SettGlobs()) {
  is_kh_debug()
  
  if (identical(class(FIL), "data.frame")) {
    FIL <- data.table::data.table(FIL)
  }
  keyorg <- data.table::key(FIL)
  DelKols <- globs$DefDesign$DelKols
  UBeting <- globs$DefDesign$UBeting
  BetingOmk <- globs$DefDesign$BetingOmk
  BetingF <- globs$DefDesign$BetingF
  
  DesignKols <- globs$DefDesign$DesignKolsF[globs$DefDesign$DesignKolsF %in% names(FIL)]
  
  DesignKols <- globs$DefDesign$DesignKolsF[globs$DefDesign$DesignKolsF %in% names(FIL)]
  OmkKols <- globs$DefDesign$DesignKols[globs$DefDesign$DesignKols %in% names(FIL)]
  
  Design <- list()
  Design[["KolNavn"]] <- names(FIL)
  # Finn faktisk design
  setkeym(FIL, c(DesignKols))
  ObsDesign <- unique(FIL[, ..DesignKols])
  
  # Finn deler inneholdt i tabell
  Deler <- character()
  for (del in names(DelKols)) {
    if (all(DelKols[[del]] %in% DesignKols)) {
      Deler <- c(Deler, del)
    }
  }
  
  # Sett omkodingskombinasjoner
  Design[["UBeting"]] <- UBeting[UBeting %in% Deler]
  Design[["BetingOmk"]] <- BetingOmk[BetingOmk %in% Deler]
  Design[["BetingF"]] <- BetingF[BetingF %in% Deler]
  Alle <- c(Design[["UBeting"]], Design[["BetingOmk"]], Design[["BetingF"]])
  Design[["OmkDeler"]] <- c(Design[["UBeting"]], Design[["BetingOmk"]])
  
  # Sett alle partielle tabuleringer (Gn,Y,K,A,T1,T2,T3),
  for (del in Deler) {
    kols <- DelKols[[del]]
    data.table::setkeyv(ObsDesign, kols)
    # SETT HAR
    Design[["Part"]][[del]] <- data.table::data.table(setNames(cbind(unique(ObsDesign[, kols, with = FALSE]), 1), c(kols, paste(del, "_HAR", sep = ""))), key = kols)
  }
  
  # Fyll evt hull i aldersintervaller
  if ("A" %in% names(Design$Part)) {
    mangler <- intervals::interval_difference(Intervals(c(FGP$amin, FGP$amax), type = "Z"), Intervals(Design$Part$A[, DelKols$A, with = FALSE], type = "Z"))
    if (nrow(mangler) > 0) {
      mangler <- setNames(cbind(as.data.frame(mangler), 0), c("ALDERl", "ALDERh", "A_HAR"))
      Design[["Part"]][["A"]] <- rbind(Design[["Part"]][["A"]], mangler)
    }
  }
  
  # Finn fullt design, dvs kryssing av alle partielle.
  delerlist <- paste("as.data.frame(Design[[\"Part\"]][[\"", Alle, "\"]])", sep = "", collapse = ",")
  FullDesign <- data.table::data.table(eval(parse(text = paste("expand.grid.df(", delerlist, ")", sep = ""))))
  setkeym(ObsDesign, names(ObsDesign))
  setkeym(FullDesign, names(ObsDesign))
  # Sett HAR=1 om denne finnes i fakttisk design
  FullDesign[, HAR := 0]
  FullDesign[ObsDesign, HAR := 1]
  Design[["Design"]] <- FullDesign
  
  # Sett omkodingskombinasjone
  # Noen dimensjoner faar variere fritt (UBeting). Andre maa vaere fast for alle versjoner av UBeting
  # Def er at Gn og Y er frie, mens K og A maa vaere fast for hver Gn,Y kombinasjon
  Beting <- c("", Design[["BetingOmk"]], Design[["BetingF"]])
  komb <- Design[["UBeting"]]
  for (del in Beting) {
    if (del != "") {
      komb <- c(Design[["UBeting"]], del)
    }
    if (length(komb) > 0) {
      kols <- character(0)
      for (k in komb) {
        kols <- c(kols, DelKols[[k]])
      }
      data.table::setkeyv(ObsDesign, kols)
      data.table::setkeyv(FullDesign, kols)
      kombFull <- data.table::data.table(unique(FullDesign[, kols, with = FALSE]))
      kombObs <- data.table::data.table(unique(ObsDesign[, kols, with = FALSE]))
      kombFull[, HAR := 0]
      kombFull[kombObs, HAR := 1]
      kombn <- paste("bet", del, sep = "")
      Design[["SKombs"]][[kombn]] <- kombFull
    }
  }
  
  setkeym(ObsDesign, names(ObsDesign))
  setkeym(FIL, keyorg)
  gc()
  return(Design)
}

#' @title do_aggregate_file
#'
#' @param file 
#' @param valsumbardef 
#' @param globs 
#'
#' @returns
#' @export
#'
#' @examples
do_aggregate_file <- function(file, valsumbardef = list(), globs = SettGlobs()){
  if(!is(file, "data.table")) data.table::setDT(file)
  tabcols <- get_dimension_columns(names(file))
  valcols <- get_value_columns(names(file))
  colorder <- tabcols 
  if(!identical(key(file), tabcols)) setkeyv(file, tabcols)
  
  g <- collapse::GRP(file, tabcols)
  file[, names(.SD) := collapse::fsum(.SD, g = g, TRA = 2), .SDcols = valcols]
  file[, names(.SD) := collapse::fmax(.SD, g = g, TRA = 2), .SDcols = paste0(valcols, ".f")]
  for(val in valcols){
    file[is.na(get(val)) | get(val) == 0, paste0(val, ".a") := 0]
    colorder <- c(colorder, paste0(val, c("", ".f", ".a")))
  }
  file[, names(.SD) := collapse::fsum(.SD, g = g, TRA = 2), .SDcols = paste0(valcols, ".a")]
  # Remove if marked as not "sumbar"
  for(val in valcols){
    if(val %in% names(valsumbardef) && valsumbardef[[val]]$sumbar == 0){
      valA <- paste0(val, ".a")
      valF <- paste0(val, ".f")
      file[get(valA) > 1, c(val, valF) := list(NA, 2)]
    }
  }
  data.table::setcolorder(file, colorder)
  return(file)
}

#' KHaggreger (kb)
#'
#' @param FIL 
#' @param vals 
#' @param snitt 
#' @param globs 
KHaggreger <- function(FIL, vals = list(), globs = SettGlobs()) {
  is_kh_debug()
  
  orgclass <- class(FIL)
  orgcols <- names(FIL)
  if (identical(orgclass, "data.frame")) {
    FIL <- data.table::setDT(FIL)
  }
  orgkeys <- data.table::key(FIL)
  tabnames <- names(FIL)[names(FIL) %in% globs$DefDesign$DesignKolsFA]
  valkols <- get_value_columns(names(FIL))
  if(!identical(key(FIL), tabnames)) setkeyv(FIL, tabnames)
  
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
  if(!identical(key(FIL), orgkeys)) setkeyv(FIL, tabnames)

  return(FIL)
}

ht2 <- function(x, n = 3) {
  is_kh_debug()
  
  rbind(head(x, n), tail(x, n))
}

#' FinnFilGruppeFraKoblid (kb)
#'
#' @param koblid 
#' @param globs 
FinnFilGruppeFraKoblid <- function(koblid, globs = SettGlobs()) {
  is_kh_debug()
  
  return(as.character(sqlQuery(globs$dbh, paste("SELECT FILGRUPPE FROM ORGINNLESkobl WHERE KOBLID=", koblid, sep = ""), stringsAsFactors = FALSE)))
}

#' SkrivKBLogg (kb)
#'
#' @param KB 
#' @param type 
#' @param filbesk 
#' @param gruppe 
#' @param batchdate 
#' @param globs 
SkrivKBLogg <- function(KB, type, filbesk, gruppe, batchdate = SettKHBatchDate(), globs = SettGlobs()) {
  is_kh_debug()
  sqlQuery(globs$log, paste("DELETE * FROM KODEBOK_LOGG WHERE KOBLID=", filbesk$KOBLID, " AND TYPE='", type, "' AND SV='S'", sep = ""))
  sqlSave(globs$log, cbind(KOBLID = filbesk$KOBLID, FILGRUPPE = gruppe, FELTTYPE = type, SV = "S", KB[, c("ORG", "KBOMK", "OMK", "FREQ", "OK")], BATCHDATE = batchdate), "KODEBOK_LOGG", rownames = FALSE, append = TRUE)
}

#' SVcloneRecord (kb)
#'
#' @param dbh 
#' @param table 
#' @param koblid 
SVcloneRecord <- function(dbh, table, koblid) {
  is_kh_debug()
  design <- names(sqlQuery(dbh, paste("SELECT * FROM ", table, " WHERE KOBLID=-1", sep = "")))
  felt <- paste(design, collapse = ",")
  feltm <- sub("SV", "'V' AS SV", felt)
  sql <- paste(
    "INSERT INTO ", table, "(", felt, ")",
    "SELECT ", feltm, "FROM ", table,
    "WHERE KOBLID=", koblid, "AND SV='S'"
  )
  sqlQuery(dbh, sql)
}

#' readRDS_KH (kb)
#'
#' @param file 
#' @param IDKOLS 
#' @param ... 
readRDS_KH <- function(file, IDKOLS = FALSE, ...) {
  is_kh_debug()
  FIL <- readRDS(file, ...)
  if (IDKOLS == FALSE) {
    if ("KOBLID" %in% names(FIL)) {
      FIL$KOBLID <- NULL
    }
    if ("ROW" %in% names(FIL)) {
      FIL$ROW <- NULL
    }
  }
  return(FIL)
}

#' TilFilLogg (kb)
#'
#' @param koblid 
#' @param felt 
#' @param verdi 
#' @param batchdate 
#' @param globs 
TilFilLogg <- function(koblid, felt, verdi, batchdate = SettKHBatchDate(), globs = SettGlobs()) {
  is_kh_debug()
  # Sjekk om finnes rad for filid, eller lag ny
  if (nrow(sqlQuery(globs$log, paste("SELECT * FROM INNLES_LOGG WHERE KOBLID=", koblid, " AND SV='S' AND BATCH='", batchdate, "'", sep = ""))) == 0) {
    print("**************Hvorfor er jeg egentlig her?*********************'")
    sqlQuery(globs$log, paste("DELETE * FROM INNLES_LOGG WHERE KOBLID=", koblid, "AND SV='S'", sep = ""))
    upd <- paste("INSERT INTO INNLES_LOGG ( KOBLID, BATCH, SV, FILGRUPPE ) SELECT=", koblid, ",'", batchdate, "', 'S',", FinnFilGruppeFraKoblid(koblid, globs = globs), sep = "")
    sqlQuery(globs$log, upd)
  }
  if (is.character(verdi)) {
    verdi <- paste("'", verdi, "'", sep = "")
    verdi <- gsub("\\n", "' & Chr(13) & Chr(10) & '", verdi) # Veldig saer \n i Access!
  }
  upd <- paste("UPDATE INNLES_LOGG SET ", felt, "=", verdi, " WHERE KOBLID=", koblid, " AND SV='S' AND BATCH='", batchdate, "'", sep = "")
  tmp <- sqlQuery(globs$log, upd)
  # cat("********\n",tmp,"__________\n")
}

#' LesMultiHead (kb)
#'
#' @param mhstr 
LesMultiHead <- function(mhstr) {
  # Leser parameterstreng for multihead og gjoer om til relevante variable
  # Velger aa kalle paa denne funksjonen ved behov for samme inputstreng heller enn aa porssessere strengen en gang og sende bitene rundt
  # Finn evt angitt separator (trengs bare settes dersom det er snakk om en originalt pastet rad med annen seaprator enn "|"
  is_kh_debug()
  
  if (grepl("sep=\".\"", mhstr)) {
    sep <- sub(".*,sep=\"(.)\"", "\\1", mhstr)
    mhstr <- sub("(.*),sep=\".\"", "\\1", mhstr)
  } else {
    sep <- "&"
  }
  # Les inn rader som inneholder deler
  eval(parse(text = paste("mh<-c(", mhstr, ")")))
  colnames <- names(mh)
  # Sett paste av tabnavn. Denne blir senere splitta til kolonnenavn
  varname <- paste(names(mh), collapse = "_")
  # Fjern rader som er duplikater, dvs som allerede er pastet sammen originalt
  rader <- mh[!duplicated(mh)]
  return(list(rader = rader, sep = sep, colnames = colnames, varname = varname))
}

#' FinnFil (kb)
#'
#' @param FILID 
#' @param versjonert 
#' @param batch 
#' @param ROLLE 
#' @param TYP 
#' @param IDKOLS 
#' @param globs 
FinnFil <- function(FILID, versjonert = FALSE, batch = NA, ROLLE = "", TYP = "STABLAORG", IDKOLS = FALSE, globs = SettGlobs()) {
  is_kh_debug()
  
  FT <- data.frame()
  if (is.na(batch) & exists("BUFFER") && FILID %in% names(BUFFER)) {
    FT <- data.table::copy(BUFFER[[FILID]])
    cat("Hentet ", ROLLE, "FIL ", FILID, " fra BUFFER (", dim(FT)[1], " x ", dim(FT)[2], ")\n", sep = "")
  } else {
    if (!is.na(batch)) {
      filn <- file.path(getOption("khfunctions.root"), getOption("khfunctions.filegroups.dat"), paste0(FILID, "_", batch, ".rds"))
    } else if (versjonert == TRUE) {
      orgwd <- getwd()
      path <- file.path(getOption("khfunctions.root"), getOption("khfunctions.filegroups.dat"))
      # setwd(path)
      Filer <- unlist(list.files(path, include.dirs = FALSE))
      Filer <- Filer[grepl(paste("^", FILID, "_(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}).rds$", sep = ""), Filer)]
      if (length(Filer) > 0) {
        filn <- file.path(path, Filer[order(Filer)][length(Filer)])
        batch <- gsub(".*_(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}).rds$", "\\1", Filer[order(Filer)][length(Filer)])
      } else {
        filn <- file.path(path, paste0(FILID, ".rds"))
      }
      setwd(orgwd)
    } else {
      filn <- file.path(getOption("khfunctions.root"), getOption("khfunctions.filegroups.ny"), paste0(FILID, ".rds"))
      print(filn)
    }
    if (file.access(filn, mode = 0) == -1) {
      cat("KRITISK FEIL: ", filn, " finnes ikke\n")
    } else if (file.access(filn, mode = 4) == -1) {
      cat("KRITISK FEIL: ", filn, " finnes, men lar seg ikke lese\n")
    } else {
      FT <- readRDS_KH(filn, IDKOLS = IDKOLS)
      cat("Lest inn ", ROLLE, "FIL ", FILID, " (", dim(FT)[1], " x ", dim(FT)[2], "), batch=", batch, "\n", sep = "")
    }
  }
  return(list(FT = data.table::as.data.table(FT), batch = batch))
}

#' FinnFilT (kb)
#'
#' @param ... 
FinnFilT <- function(filid) {
  is_kh_debug()
  
  return(FinnFil(filid, globs = globs)$FT)
}

## Try to handle problem with "memory exhausted (limit reached?)" the solution above
#' expand.grid.df (ybk)
#'
#' @param ... 
expand.grid.df <- function(...) {
  is_kh_debug()
  
  DFs <- list(...)
  
  ddt <- lapply(DFs, function(x) is(x, "data.table"))
  dx <- which(ddt == 0)
  
  if (length(dx) > 0){
    for (i in dx){
      DFs[[i]] <- data.table::as.data.table(DFs[[i]])
    }
  }
  
  rows <- do.call(data.table::CJ, lapply(DFs, function(x) seq(nrow(x))))
  
  for (i in seq_along(DFs))
    names(DFs)[i] <- paste0("data", i)
  
  DFlength <- length(DFs)
  DFnames <- names(DFs)
  
  res <- DFs[[1L]][rows[[1L]]]
  DFs[[1L]] <- NULL
  for (i in seq_len(DFlength)[-1L]){
    x <- DFnames[i]
    res <- res[, c(.SD, DFs[[x]][rows[[i]]])]
    DFs[[x]] <- NULL
  }
  
  rm(DFs, rows)
  gc()
  data.table::setDT(res)
}

expand.grid.dt <- function(...){
  DFs <- list(...)
  for(i in seq_along(DFs)){
    if(!is(DFs[[i]], "data.table")){
      DFs[[i]] <- data.table::setDT(DFs[[i]])
    }
  }
  
  rows <- do.call(data.table::CJ, lapply(DFs, function(x) seq(nrow(x))))
  
  for (i in seq_along(DFs)){
    DFs[[i]] <- DFs[[i]][rows[[i]]]
  }
    
  res <- DFs[[1L]]
  for(i in 2:length(DFs)){
    res[, names(DFs[[i]]) := DFs[[i]]]
  }
    
  rm(DFs, rows)
  gc()
  return(res)
}

#' setkeym (kb)
#'
#' @param DTo 
#' @param keys 
setkeym <- function(DTo, keys) {
  is_kh_debug()
  
  # Foroesk paa aa speede opp naar setkeyv brukes for aa sikre key(DTo)=keys
  if (!("data.table" %in% class(DTo) && identical(key(DTo), keys))) {
    data.table::setDT(DTo)
    data.table::setkeyv(DTo, keys)
  }
}

#' godkjent (ybk)
#'
#' @param profil 
#' @param modus 
#' @param aar 
#' @param ... 
godkjent <- function(profil = c("FHP", "OVP"),
                     modus = c("K", "F", "B"),
                     aar = getOption("khfunctions.year"), ...) {
  is_kh_debug()
  
  profil <- match.arg(profil)
  modus <- match.arg(modus)
  
  # modusFolder <- switch(modus,
  #                       F = "NH",
  #                       "KH"
  # )
  
  bruker <- Sys.info()[["user"]]
  message(
    "\n********\n  Kopiering av filer for ",
    profil[1], " og geonivaa ", modus, " for ",
    aar, " begynner naa. Gjennomfoert av ", bruker, "\n********\n"
  )
  
  ## Get connection to DB
  mdb_file <- file.path(getOption("khfunctions.root"), getOption("khfunctions.db"))
  conn <- RODBC::odbcDriverConnect(paste0(
    "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
    mdb_file
  ))
  
  tblCols <- c("PROFILTYPE", "INDIKATOR", "KUBE_NAVN", "MODUS", "AARGANG")
  tblName <- "FRISKVIK"
  sqlFrisk <- glue::glue_sql("SELECT {`tblCols`*}
                      FROM {`tblName`}
                      WHERE {`tblCols[5]`} = {aar}", .con = DBI::ANSI())
  
  tbl_fsk <- RODBC::sqlQuery(conn, sqlFrisk)
  
  tblCols <- c("KUBE_NAVN", "DATOTAG_KUBE", "QC_OK")
  tblName <- paste0("KUBESTATUS_", aar)
  sqlKube <- glue::glue_sql("SELECT {`tblCols`*} from {`tblName`}", .con = DBI::ANSI())
  
  tbl_kube <- RODBC::sqlQuery(conn, sqlKube)
  
  invisible(sapply(list(tbl_fsk, tbl_kube), setDT))
  
  ## merge tabels
  rawAlle <- tbl_fsk[tbl_kube, on = "KUBE_NAVN"]
  
  ## filter data
  utTYP <- profil[1]
  
  tblAlle <- rawAlle[PROFILTYPE == utTYP, ] %>%
    .[MODUS == modus, ] %>%
    .[QC_OK == 1, ]
  
  
  ## Create filenames
  fileNames <- tblAlle[, filename := paste0(INDIKATOR, "_", DATOTAG_KUBE, ".csv")][["filename"]]
  
  ## Root folder where the file is
  pathRoot <- getOption("khfunctions.root")
  
  ## Path for Profile
  pathProfil <- switch(utTYP,
                       "FHP" = c(
                         getOption("khfunctions.fhpF"),
                         getOption("khfunctions.fhpK"),
                         getOption("khfunctions.fhpB")
                       ),
                       "OVP" = c(
                         getOption("khfunctions.ovpF"),
                         getOption("khfunctions.ovpK"),
                         getOption("khfunctions.ovpB")
                       )
  )
  
  ## Geolevels
  modeProfil <- c("F", "K", "B")
  indMode <- grep(modus, modeProfil, ignore.case = TRUE)
  
  ## Get correct path to profil
  pathDir <- pathProfil[indMode]
  
  ## Current date style to create folder
  batchdate <- SettKHBatchDate()
  
  fileRoot <- file.path(pathRoot, pathDir, aar)
  fileFrom <- file.path(fileRoot, "CSV")
  fileTo <- file.path(fileRoot, "GODKJENT", batchdate)
  
  ## Check if folder exists else create
  if (!fs::dir_exists(fileTo)) fs::dir_create(fileTo)
  
  ## Group files to those that succeed or fail
  fileOK <- list()
  fileKO <- list()
  
  for (i in fileNames) {
    outFile <- file.path(fileFrom, i)
    inFile <- file.path(fileTo, i)
    
    outMsg <- tryCatch(
      {
        fs::file_copy(outFile, inFile, overwrite = TRUE)
      },
      error = function(err) err
    )
    
    if (inherits(outMsg, "error")) {
      message("\n --> OPS! Finner ikke filen: ", i, "\n")
      fileKO[i] <- i
      next
    } else {
      message("Kopierer filen: ", i)
      fileOK[i] <- i
    }
  }
  
  message(
    "\n**********\n", " ",
    length(fileOK),
    " filer ble kopiert til ",
    fileTo, "\n"
  )
  
  message(
    "----------\n", " ",
    length(fileKO),
    " filer finnes ikke i ",
    fileFrom, "\n**********\n"
  )
}

#' @title usebranch
#' @description
#' use to test other branches, loads all functions from a specified branch
usebranch <- function(branch){
  rm(list = lsf.str(all.names = T))
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHmisc.R"), encoding = "latin1")
  KH_options()
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHpaths.R"), encoding = "latin1")
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHglobs.R"), encoding = "latin1")
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHfilgruppefunctions.R"), encoding = "latin1")
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHfilgruppe.R"), encoding = "latin1")
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHkubefunctions.R"), encoding = "latin1")
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHkube.R"), encoding = "latin1")
  # source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHother.R"), encoding = "latin1")
  cat("\nLoaded functions from branch: ", branch)
}

#' @title KH_options
#' @description
#' Reads config-khfunctions.yml and sets global options accordingly. Checks if any 
#' option is different from the config-file, and give the option to update. 
KH_options <- function(){
  # Set global options
  op <- options()
  optOrg <- orgdata:::is_globs("khfunctions")
  orgDT <- !(names(optOrg) %in% names(op))
  if(any(orgDT)) options(optOrg[orgDT])
  corrglobs <- orgdata:::is_correct_globs(optOrg)
  if(!isTRUE(corrglobs)){
    x <- utils::askYesNo("Options are not the same as in the config file, update options now?")
    if(isTRUE(x)){
      orgdata:::update_globs("khfunctions")
    }
  }
}

FormatSqlBatchdate <- function(batchdate){
  format(strptime(batchdate, "%Y-%m-%d-%H-%M"), "#%Y-%m-%d#")
}

#' @title usebranch
#' @description
#' use to test other branches, loads all functions from a specified branch
use_branch <- function(branch, debug = FALSE){
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  show_functions <<- debug
  show_arguments <<- debug
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHmisc.R"), encoding = "latin1")
  KH_options()
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHglobs.R"), encoding = "latin1")
  
  rfiles <- list_files_github(branch = branch)
  rfiles <- grep("KHmisc.R|KHglobs.R|KHsetup.R", rfiles, value = T, invert = T)
  for(file in rfiles){
    source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/", file), encoding = "latin1")
  }
  cat("\nLoaded functions from branch: ", branch)
}

list_files_github <- function(branch){
  req <- httr2::request(paste0("https://api.github.com/repos/helseprofil/khfunctions/git/trees/", branch, "?recursive=1"))
  response <- httr2::req_perform(req)
  files <- httr2::resp_body_json(response, simplifyDataFrame = TRUE)$tree$path
  files <- basename(grep("^R/", files, value = T))
  return(files)
}

#' @title check_if_system_available
#' @description
#' Checks if file exists, indicating that the system is already running.
#' If file doesn't exist, or if user overrides and force continue, the file
#' is generated and TRUE is returned indicating that the function may continue. 
#' If the file exists and the user does not override, FALSE is returned indicating 
#' that the system is busy and data processing stops.
#' 
#' The path to the file must be generated in the main function, with an on.exit call to delete
#' the file when the function finish or crash. This function checks if the file already exists, 
#' and generate the file if not (or overridden by user). 
#' 
#'
#' @param file 
#'
#' @returns
#' @export
#'
#' @examples
check_if_system_available <- function(file){
  continue <- TRUE
  if(file.exists(file)){
    force_continue <- utils::menu(choices = c("JA", "NEI"),
                            title = paste0("Det ser ut til at du allerede kjører en kube på denne maskinen.\n",
                                           "For å hindre feil ved dobbelkjøring tillates ikke parallellkjøring av kuber\n",
                                           "Dersom du ikke kjører noe parallellt kan du fortsette\n\n",
                                           "Vil du fortsette?"))
    if(force_continue == 2) continue <- FALSE
  }
  if(continue) fs::file_create(file)
  return(continue)
}
