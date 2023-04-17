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
    write.dta(TABELL, paste(globs$path, "/", globs$DUMPdir, "/", TABELLnavn, ".dta", sep = ""))
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
  write.dta(TABLE, tmpdta)
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
    TABLE <- read.dta(tmpdta)
  }
  # Reverserer omforminger for å kunne skrive til STATA
  TABLE[TABLE == " "] <- ""
  names(TABLE) <- gsub("^S_(\\d.*)$", "\\1", names(TABLE))
  # DEVELOP: STATAPRIKK slå av neste linje om det ikke funker
  names(TABLE) <- gsub("^(.*)_([afn])$", "\\1.\\2", names(TABLE)) # Endre .a, .f, .n til _
  # file.remove(tmpdo,tmpdta,tmplog)
  setwd(wdOrg)
  if (tableTYP == "DT") {
    setDT(TABLE)
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
  
  TNF <- copy(TNF) # Får uønsket warning om self.reference under om ikke gjør slik
  setDT(TNF)
  valKols <- gsub("^(.+)\\.f$", "\\1", names(TNF)[grepl(".+\\.f$", names(TNF))])
  # FinnValKols(names(TNF))
  if (!(is.na(NYEdscr) | NYEdscr == "")) {
    for (nycolexpr in unlist(str_split(NYEdscr, ";"))) {
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
    FIL <- data.table(FIL)
  }
  keyorg <- key(FIL)
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
    setkeyv(ObsDesign, kols)
    # SETT HAR
    Design[["Part"]][[del]] <- data.table(setNames(cbind(unique(ObsDesign[, kols, with = FALSE]), 1), c(kols, paste(del, "_HAR", sep = ""))), key = kols)
  }
  
  # Fyll evt hull i aldersintervaller
  # Bør generaliserer til INT !!!
  if (globs$DefDesign$AMissAllow == TRUE) {
    if ("A" %in% names(Design$Part)) {
      mangler <- interval_difference(Intervals(c(FGP$amin, FGP$amax), type = "Z"), Intervals(Design$Part$A[, DelKols$A, with = FALSE], type = "Z"))
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
  FullDesign <- data.table(eval(parse(text = paste("expand.grid.df(", delerlist, ")", sep = ""))))
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
      setkeyv(ObsDesign, kols)
      setkeyv(FullDesign, kols)
      kombFull <- data.table(unique(FullDesign[, kols, with = FALSE]))
      kombObs <- data.table(unique(ObsDesign[, kols, with = FALSE]))
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