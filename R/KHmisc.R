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
  is_kh_debug()
  
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
DumpTabell <- function(TABELL, TABELLnavn, globs = FinnGlobs(), format = globs$DefDumpFormat) {
  is_kh_debug()
  
  if (format == "CSV") {
    write.table(TABELL, file = paste(globs$path, "/", globs$DUMPdir, "/", TABELLnavn, ".csv", sep = ""), sep = ";", na = "", row.names = FALSE)
  } else if (format == "R") {
    .GlobalEnv$DUMPtabs[[TABELLnavn]] <- TABELL
    print(DUMPtabs)
  } else if (format == "STATA") {
    TABELL[TABELL == ""] <- " " # STATA støtter ikke "empty-string"
    names(TABELL) <- gsub("^(\\d.*)$", "S_\\1", names(TABELL)) # STATA 14 tåler ikke numeriske kolonnenavn
    names(TABELL) <- gsub("^(.*)\\.([afn])$", "\\1_\\2", names(TABELL)) # Endre .a, .f, .n til _
    foreign::write.dta(TABELL, paste(globs$path, "/", globs$DUMPdir, "/", TABELLnavn, ".dta", sep = ""))
  }
}

#' KjorStataScript (kb, ybk?)
#'
#' @param TABLE 
#' @param script 
#' @param tableTYP 
#' @param batchdate 
#' @param globs
KjorStataSkript <- function(TABLE, script, tableTYP = "DF", batchdate = SettKHBatchDate(), globs = FinnGlobs()) {
  is_kh_debug()
  
  tmpdir <- paste(globs$path, "/", globs$BUFFERdir, "/", sep = "")
  wdOrg <- getwd()
  setwd(tmpdir)
  tmpdo <- paste("STATAtmp_", batchdate, ".do", sep = "")
  tmpdta <- paste("STATAtmp_", batchdate, ".dta", sep = "")
  tmplog <- paste("STATAtmp_", batchdate, ".log", sep = "")
  TABLE[TABLE == ""] <- " " # STATA støtter ikke "empty-string"
  names(TABLE) <- gsub("^(\\d.*)$", "S_\\1", names(TABLE)) # STATA 14 tåler ikke numeriske kolonnenavn
  # DEVELOP: STATAPRIKK slå av neste linje om det ikke funker
  names(TABLE) <- gsub("^(.*)\\.([afn])$", "\\1_\\2", names(TABLE)) # Endre .a, .f, .n til _
  # DEVELOP: try(write.dta), if error then write.csv. Må da også sette tmpdta<-tmpcsv og evt opsjoner i STATAs use tmpdta
  foreign::write.dta(TABLE, tmpdta)
  # file.create(tmpdo,overwrite=TRUE,showWarnings=FALSE)
  sink(tmpdo)
  cat("use ", tmpdta, "\n", sep = "")
  cat(script, "\n")
  # cat("save ",tmpdta,", replace\n",sep="")
  if (globs$StataVers < 12) {
    cat("save ", tmpdta, ",  replace\n", sep = "")
  } else if (globs$StataVers %in% 12:13) {
    cat("saveold ", tmpdta, ", replace\n", sep = "")
  } else {
    cat("saveold ", tmpdta, ", version(11) replace\n", sep = "")
  }
  sink()
  # system(paste("\"",globs$StataExe,"\" /e do",tmpdo,"\n",sep=""),intern=TRUE)
  statacall <- paste("\"", globs$StataExe, "\" /e do ", tmpdo, " \n", sep = "")
  system(statacall, intern = TRUE)
  # system(paste("\"C:\\Program Files (x86)\\Stata11\\StataSE-64.exe\"","/e do",tmpdo,"\n"),intern=TRUE)
  # system(paste("StataSE-64 /e do",tmpdo,"\n"),intern=TRUE)
  log <- readLines(tmplog)
  feil <- ""
  if (log[length(log)] != "end of do-file") {
    log_start <- which(grepl(paste("do", tmpdo), log))
    feil <- paste(log[log_start:length(log)], collapse = "\n")
  } else {
    TABLE <- foreign::read.dta(tmpdta)
  }
  # Reverserer omforminger for å kunne skrive til STATA
  TABLE[TABLE == " "] <- ""
  names(TABLE) <- gsub("^S_(\\d.*)$", "\\1", names(TABLE))
  # DEVELOP: STATAPRIKK slå av neste linje om det ikke funker
  names(TABLE) <- gsub("^(.*)_([afn])$", "\\1.\\2", names(TABLE)) # Endre .a, .f, .n til _
  # file.remove(tmpdo,tmpdta,tmplog)
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

#' get_col (ybk)
#' 
#' helper function in STATA censoring
#' Easier to check with sum by converting valid col value to 1
get_col <- function(var, num = TRUE){
  is_kh_debug()
  
  if (is.na(var) || var == ""){
    var <- NA
  }
  
  if (num){
    var <- var_num(var)
  }
  
  if (!is.na(var) && num){
    var <- 1
  }
  
  return(var)
}

#' var_num (ybk)
#' 
#' Helper function for STATA censoring
#' Avoid warning message "NAs introduced by coercion" when using as.numeric
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
var_num <- function(x){
  is_kh_debug()
  
  v <- is.numeric(x)
  if (!v){
    x <- NA
  }
  
  return(x)
}

#' FinnValKols (kb)
#'
#' @param knames 
FinnValKols <- function(knames) {
  is_kh_debug()
  
  gsub("^(.*?)\\.f$", "\\1", knames[grepl("^(.*?)\\.f$", knames)])
}

#' FinnValKolsF (kb)
#'
FinnValKolsF <- function(knames) {
  is_kh_debug()
  
  vkolsN <- gsub("^(.*?)\\.f$", "\\1", knames[grepl("^(.*?)\\.f$", knames)])
  vkolsNF <- unlist(lapply(vkolsN, function(x) {
    paste(x, c("", ".f", ".a", ".n"), sep = "")
  }))
  return(intersect(knames, vkolsNF))
}

#' FinnTabKols (kb)
#'
FinnTabKols <- function(knames) {
  is_kh_debug()
  
  return(setdiff(knames, c(FinnValKolsF(knames), "KOBLID", "ROW")))
}


#' FinnTabKolsKUBE (kb)
#'
#' Finds column names not in NESSTARTUPPEL
FinnTabKolsKUBE <- function(allnames, globs = FinnGlobs()) {
  is_kh_debug()
  
  annet <- union(union(unlist(globs$NesstarOutputDef), FinnValKolsF(allnames)), c("NORMSMR", "SMRtmp"))
  return(setdiff(allnames, annet))
}

#' LeggTilNyeVerdiKolonner (kb)
#'
#' @param TNF 
#' @param NYEdscr 
#' @param slettInf 
#' @param postMA 
LeggTilNyeVerdiKolonner <- function(TNF, NYEdscr, slettInf = TRUE, postMA = FALSE) {
  is_kh_debug()
  
  TNF <- data.table::copy(TNF) # Får uønsket warning om self.reference under om ikke gjør slik
  data.table::setDT(TNF)
  valKols <- gsub("^(.+)\\.f$", "\\1", names(TNF)[grepl(".+\\.f$", names(TNF))])
  # FinnValKols(names(TNF))
  if (!(is.na(NYEdscr) | NYEdscr == "")) {
    for (nycolexpr in unlist(stringr::str_split(NYEdscr, ";"))) {
      nycol <- gsub("^(.*?)=(.*)$", "\\1", nycolexpr)
      expr <- gsub("^(.*?)=(.*)$", "\\2", nycolexpr)
      invKols <- valKols[sapply(valKols, FUN = function(x) {
        grepl(x, expr)
      })]
      eval(parse(text = paste(
        "TNF[,c(\"", paste(nycol, c("", ".f", ".a"), collapse = "\",\"", sep = ""), "\")
      :=list(", expr, ",pmax(", paste(invKols, ".f", collapse = ",", sep = ""), "),
                      pmax(", paste(invKols, ".a", collapse = ",", sep = ""), "))]",
        sep = ""
      )))
      if (postMA == TRUE) {
        eval(parse(text = paste(
          "TNF[,c(\"", paste(nycol, c(".n", ".fn1", ".fn3", ".fn9"), collapse = "\",\"", sep = ""), "\")
        :=list(1,0,0,0)]",
          sep = ""
        )))
      }
      if (slettInf == TRUE) {
        eval(parse(text = paste("suppressWarnings(",
                                "TNF[", nycol, "%in% c(Inf,NaN,NA),c(\"", paste(nycol, c("", ".f"), collapse = "\",\"", sep = ""), "\"):=list(NA,2)])",
                                sep = ""
        )))
      }
    }
  }
  return(TNF)
}

#' FinnDesign (kb)
#' 
#' Brukes i lagkube og lagfilgruppe
#'
#' @param FIL 
#' @param FGP 
#' @param globs 
#'
#' @return
#' @export
#'
#' @examples
FinnDesign <- function(FIL, FGP = list(amin = 0, amax = 120), globs = FinnGlobs()) {
  is_kh_debug()
  
  if (identical(class(FIL), "data.frame")) {
    FIL <- data.table::data.table(FIL)
  }
  keyorg <- data.table::key(FIL)
  # Sett defdesign
  DelKols <- globs$DefDesign$DelKols
  UBeting <- globs$DefDesign$UBeting
  BetingOmk <- globs$DefDesign$BetingOmk
  BetingF <- globs$DefDesign$BetingF
  
  DesignKols <- globs$DefDesign$DesignKolsF[globs$DefDesign$DesignKolsF %in% names(FIL)]
  
  DesignKols <- globs$DefDesign$DesignKolsF[globs$DefDesign$DesignKolsF %in% names(FIL)]
  OmkKols <- globs$DefDesign$DesignKols[globs$DefDesign$DesignKols %in% names(FIL)]
  
  # Initier tomt resultat
  Design <- list()
  Design[["KolNavn"]] <- names(FIL)
  # Finn faktisk design
  setkeym(FIL, c(DesignKols))
  ObsDesign <- unique(FIL[, DesignKols, with = FALSE])
  # print(unique(FIL[,c("ALDERl","ALDERh"),with=FALSE]))
  # print(subset(FIL,GEOniv==1 & AARl==2009 & TAB1=="Total"))
  
  # Finn deler inneholdt i tabell
  Deler <- character()
  for (del in names(DelKols)) {
    if (all(DelKols[[del]] %in% DesignKols)) {
      Deler <- c(Deler, del)
    }
  }
  # Sjekk for evt ugyldig med bare ALDERl etc?
  
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
  # Bør generaliserer til INT !!!
  if (globs$DefDesign$AMissAllow == TRUE) {
    if ("A" %in% names(Design$Part)) {
      mangler <- intervals::interval_difference(Intervals(c(FGP$amin, FGP$amax), type = "Z"), Intervals(Design$Part$A[, DelKols$A, with = FALSE], type = "Z"))
      if (nrow(mangler) > 0) {
        mangler <- setNames(cbind(as.data.frame(mangler), 0), c("ALDERl", "ALDERh", "A_HAR"))
        #         if (max(mangler$ALDERl)>=95){
        #           mangler[ALDERl==max(mangler$ALDERl),A_HAR]<-1
        #         }
        Design[["Part"]][["A"]] <- rbind(Design[["Part"]][["A"]], mangler)
      }
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
  
  # Utgått
  # Filtrer til bare den delen av designet som er aktuell for omkoding (dsv uten TAB1 etc)
  # setkeym(FullDesign,OmkKols)
  # Design[["OmkDesign"]]<-FullDesign[,list(HAR=max(HAR)),by=OmkKols]

  # Sett omkodingskombinasjone
  # Noen dimensjoner får variere fritt (UBeting). Andre må være fast for alle versjoner av UBeting
  # Def er at Gn og Y er frie, mens K og A må være fast for hver Gn,Y kombinasjon
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
  
  # Tilbakestill key
  setkeym(ObsDesign, names(ObsDesign))
  setkeym(FIL, keyorg)
  gc()
  return(Design)
}

#' KHaggreger (kb)
#'
#' @param FIL 
#' @param vals 
#' @param snitt 
#' @param globs 
KHaggreger <- function(FIL, vals = list(), snitt = FALSE, globs = FinnGlobs()) {
  is_kh_debug()
  
  orgclass <- class(FIL)
  orgcols <- names(FIL)
  if (identical(orgclass, "data.frame")) {
    FIL <- data.table::data.table(FIL)
  }
  orgkeys <- data.table::key(FIL)
  tabnames <- globs$DefDesign$DesignKolsFA[globs$DefDesign$DesignKolsFA %in% names(FIL)]
  # tabnames<-names(FIL)[!grepl("^VAL\\d+(f|)$",names(FIL))]
  valkols <- names(FIL)[!names(FIL) %in% tabnames]
  valkols <- valkols[!grepl("\\.(f|a)", valkols)]
  valkols <- valkols[!valkols %in% c("KOBLID", "ROW")]
  setkeym(FIL, tabnames) # Sjekk om key ok for å effektivisere?
  
  if (snitt == FALSE) {
    lp <- paste("list(",
                paste(valkols, "=sum(", valkols, "),",
                      valkols, ".f=max(", valkols, ".f),",
                      valkols, ".a=sum(", valkols, ".a*(!is.na(", valkols, ") & ", valkols, "!=0))",
                      sep = "", collapse = ","
                ),
                ")",
                sep = ""
    )
    FILa <- FIL[, eval(parse(text = lp)), by = tabnames]
  } else {
    # Sett også hjelpestørrelser for vurdering av snitt
    lp <- paste("list(",
                paste(valkols, "=sum(", valkols, ",na.rm=TRUE),",
                      valkols, ".f=max(", valkols, ".f),",
                      valkols, ".a=sum(", valkols, ".a*(!is.na(", valkols, ") & ", valkols, "!=0)),",
                      valkols, ".fn1=sum(", valkols, ".f==1),",
                      valkols, ".fn3=sum(", valkols, ".f>1),",
                      valkols, ".n=.N",
                      sep = "", collapse = ","
                ),
                ")",
                sep = ""
    )
    FILa <- FIL[, eval(parse(text = lp)), by = tabnames]
    # Anonymiser, trinn 1
    # Filtrer snitt som ikke skal brukes pga for mye anonymt
    anon_tot_tol <- 0.2
    lp <- paste("FILa[,':='(",
                paste(valkols, "=ifelse(", valkols, ".fn3/", valkols, ".n>=", anon_tot_tol, ",NA,", valkols, "),",
                      valkols, ".f=ifelse(", valkols, ".fn3/", valkols, ".n>=", anon_tot_tol, ",3,", valkols, ".f)",
                      sep = "", collapse = ","
                ),
                ")]",
                sep = ""
    )
    eval(parse(text = lp))
    
    FILa <- FILa[, c(orgcols, paste(valkols, ".n", sep = "")), with = FALSE]
  }
  vals <- vals[valkols]
  usumbar <- valkols[unlist(lapply(vals[valkols], function(x) {
    x$sumbar == 0
  }))]
  for (val in valkols) {
    if (!is.null(vals[[val]]) && vals[[val]]$sumbar == 0) {
      eval(parse(text = paste(
        "FILa[", val, ".a>1,c(\"", val, "\",\"", val, ".f\"):=list(NA,2)]",
        sep = ""
      )))
    }
  }
  setkeym(FIL, orgkeys)
  if (identical(orgclass, "data.frame")) {
    FIL <- data.frame(FIL)
  }
  return(FILa)
}

ht2 <- function(x, n = 3) {
  is_kh_debug()
  
  rbind(head(x, n), tail(x, n))
}

#' FinnFilGruppeFraKoblid (kb)
#'
#' @param koblid 
#' @param globs 
FinnFilGruppeFraKoblid <- function(koblid, globs = FinnGlobs()) {
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
SkrivKBLogg <- function(KB, type, filbesk, gruppe, batchdate = SettKHBatchDate(), globs = FinnGlobs()) {
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
TilFilLogg <- function(koblid, felt, verdi, batchdate = SettKHBatchDate(), globs = FinnGlobs()) {
  is_kh_debug()
  # Sjekk om finnes rad for filid, eller lag ny
  if (nrow(sqlQuery(globs$log, paste("SELECT * FROM INNLES_LOGG WHERE KOBLID=", koblid, " AND SV='S' AND BATCH='", batchdate, "'", sep = ""))) == 0) {
    print("**************Hvorfor er jeg egentlig her?*********************'")
    sqlQuery(globs$log, paste("DELETE * FROM INNLES_LOGG WHERE KOBLID=", koblid, "AND SV='S'", sep = ""))
    upd <- paste("INSERT INTO INNLES_LOGG ( KOBLID, BATCH, SV, FILGRUPPE ) SELECT=", koblid, ",'", batchdate, "', 'S',", FinnFilGruppeFraKoblid(koblid), sep = "")
    sqlQuery(globs$log, upd)
  }
  if (is.character(verdi)) {
    verdi <- paste("'", verdi, "'", sep = "")
    verdi <- gsub("\\n", "' & Chr(13) & Chr(10) & '", verdi) # Veldig sær \n i Access!
  }
  upd <- paste("UPDATE INNLES_LOGG SET ", felt, "=", verdi, " WHERE KOBLID=", koblid, " AND SV='S' AND BATCH='", batchdate, "'", sep = "")
  tmp <- sqlQuery(globs$log, upd)
  # cat("********\n",tmp,"__________\n")
}

#' LesMultiHead (kb)
#'
#' @param mhstr 
LesMultiHead <- function(mhstr) {
  # Leser parameterstreng for multihead og gjør om til relevante variable
  # Velger å kalle på denne funksjonen ved behov for samme inputstreng heller enn å porssessere strengen en gang og sende bitene rundt
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
FinnFil <- function(FILID, versjonert = FALSE, batch = NA, ROLLE = "", TYP = "STABLAORG", IDKOLS = FALSE, globs = FinnGlobs()) {
  is_kh_debug()
  
  FT <- data.frame()
  if (is.na(batch) & exists("BUFFER") && FILID %in% names(BUFFER)) {
    FT <- data.table::copy(BUFFER[[FILID]])
    cat("Hentet ", ROLLE, "FIL ", FILID, " fra BUFFER (", dim(FT)[1], " x ", dim(FT)[2], ")\n", sep = "")
  } else {
    if (!is.na(batch)) {
      filn <- paste(globs$path, "/", globs$StablaDirDat, "/", FILID, "_", batch, ".rds", sep = "")
    } else if (versjonert == TRUE) {
      orgwd <- getwd()
      path <- paste(globs$path, "/", globs$StablaDirDat, sep = "")
      setwd(path)
      Filer <- unlist(list.files(include.dirs = FALSE))
      Filer <- Filer[grepl(paste("^", FILID, "_(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}).rds$", sep = ""), Filer)]
      if (length(Filer) > 0) {
        filn <- paste(path, "/", Filer[order(Filer)][length(Filer)], sep = "")
        batch <- gsub(".*_(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}).rds$", "\\1", Filer[order(Filer)][length(Filer)])
      } else {
        filn <- paste(path, "/", FILID, ".rds", sep = "")
      }
      setwd(orgwd)
    } else {
      filn <- paste(globs$path, "/", globs$StablaDirNy, "/", FILID, ".rds", sep = "")
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
FinnFilT <- function(...) {
  is_kh_debug()
  
  return(FinnFil(...)$FT)
}

## expand.grid.df <- function(...) {
##   is_kh_debug()

##   # Hjelpefunksjon, se http://stackoverflow.com/questions/11693599/alternative-to-expand-grid-for-data-frames
##   # Finnes også en i reshape, men ikke i reshape2, så bruker ikke denne
##   # Skjønner ikke helt syntaksen, men funker utmerket
##   Reduce(function(...) merge(..., by = NULL), list(...))
## }

## Try to handle problem with "memory exhausted (limit reached?)" the solution above
#' expand.grid.df (kb/ybk)
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
  data.table::setDF(res)
}

#' setkeym (kb)
#'
#' @param DTo 
#' @param keys 
setkeym <- function(DTo, keys) {
  is_kh_debug()
  
  # Forøsk på å speede opp når setkeyv brukes for å sikre key(DTo)=keys
  if (!("data.table" %in% class(DTo) && identical(key(DTo), keys))) {
    data.table::setDT(DTo)
    data.table::setkeyv(DTo, keys)
  }
}

# used in access
#' YAlagVal (kb)
#'
#' @param FG 
#' @param YL 
#' @param AL 
#' @param vals 
#' @param globs 
YAlagVal <- function(FG, YL, AL, vals = FinnValKols(names(FG)), globs = FinnGlobs()) {
  is_kh_debug()
  
  data.table::setDT(FG)
  orgkols <- names(FG)
  ltag <- function(lag) {
    ltag <- ""
    if (lag > 0) {
      ltag <- paste("m", abs(lag), sep = "")
    } else if (lag < 0) {
      ltag <- paste("p", abs(lag), sep = "")
    }
    return(ltag)
  }
  FGl <- data.table::copy(FG)
  FGl[, c("lAARl", "lALDERl") := list(AARl + YL, ALDERl + AL)]
  FGl[, c("AARl", "AARh", "ALDERl", "ALDERh") := list(NULL)]
  data.table::setnames(FGl, c("lAARl", "lALDERl"), c("AARl", "ALDERl"))
  tabkols <- setdiff(names(FGl), FinnValKolsF(names(FG)))
  lvals <- paste("Y", ltag(YL), "_A", ltag(AL), "_", vals, c("", ".f", ".a"), sep = "")
  data.table::setnames(FGl, unlist(lapply(vals, function(x) {
    paste(x, c("", ".f", ".a"), sep = "")
  })), lvals)
  FGl <- FGl[, c(tabkols, lvals), with = FALSE]
  data.table::setkeyv(FG, tabkols)
  data.table::setkeyv(FGl, tabkols)
  return(FGl)
}

#' godkjent (ybk)
#'
#' @param profil 
#' @param modus 
#' @param aar 
#' @param ... 
godkjent <- function(profil = c("FHP", "OVP"),
                     modus = globglobs$KHgeoniv,
                     aar = globglobs$KHaar, ...) {
  is_kh_debug()
  
  profil <- match.arg(profil)
  
  modusFolder <- switch(modus,
                        F = "NH",
                        "KH"
  )
  
  bruker <- Sys.info()[["user"]]
  message(
    "\n********\n  Kopiering av filer for ",
    profil[1], " og geonivå ", modus, " for ",
    aar, " begynner nå. Gjennomført av ", bruker, "\n********\n"
  )
  
  ## Get connection to DB
  mdb_file <- file.path(defpath, globglobs$KHdbname)
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
  
  tblCols <- c("KUBE_NAVN", "VERSJON_PROFILAAR_GEO", "OK_PROFILAAR_GEO")
  tblName <- paste0(modusFolder, aar, "_KUBESTATUS")
  sqlKube <- glue::glue_sql("SELECT {`tblCols`*} from {`tblName`}", .con = DBI::ANSI())
  
  tbl_kube <- RODBC::sqlQuery(conn, sqlKube)
  
  invisible(sapply(list(tbl_fsk, tbl_kube), setDT))
  
  ## merge tabels
  rawAlle <- tbl_fsk[tbl_kube, on = "KUBE_NAVN"]
  
  ## filter data
  utTYP <- profil[1]
  
  tblAlle <- rawAlle[PROFILTYPE == utTYP, ] %>%
    .[MODUS == modus, ] %>%
    .[OK_PROFILAAR_GEO == 1, ]
  
  
  ## Create filenames
  fileNames <- tblAlle[, filename := paste0(INDIKATOR, "_", VERSJON_PROFILAAR_GEO, ".csv")][["filename"]]
  
  ## Root folder where the file is
  pathRoot <- defpath
  
  ## Path for Profile
  pathProfil <- switch(utTYP,
                       "FHP" = c(
                         globglobs$FriskVDir_F,
                         globglobs$FriskVDir_K,
                         globglobs$FriskVDir_B
                       ),
                       "OVP" = c(
                         globglobs$ovpDir_F,
                         globglobs$ovpDir_K,
                         globglobs$ovpDir_B
                       )
  )
  
  ## Geolevels
  modeProfil <- c("F", "K", "B")
  indMode <- grep(modus, modeProfil, ignore.case = TRUE)
  
  ## Get correct path to profil
  pathDir <- pathProfil[indMode]
  
  ## Current date style to create folder
  batchdate <- SettKHBatchDate()
  
  fileRoot <- paste0(pathRoot, "/", pathDir, "/", aar)
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

#' usebranch (VL)
#' 
#' use to test other branches, loads all functions from a specified branch
#'
#' @param branch 
#'
#' @return
#' @export
#'
#' @examples
usebranch <- function(branch){
  rm(list = lsf.str(all.names = T))
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHmisc.R"), encoding = "latin1")
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHpaths.R"), encoding = "latin1")
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHglobs.R"), encoding = "latin1")
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHfilgruppefunctions.R"), encoding = "latin1")
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHfilgruppe.R"), encoding = "latin1")
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHkubefunctions.R"), encoding = "latin1")
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHkube.R"), encoding = "latin1")
  source(paste0("https://raw.githubusercontent.com/helseprofil/khfunctions/", branch, "/R/KHother.R"), encoding = "latin1")
  cat("\nLoaded functions from branch: ", branch)
}
