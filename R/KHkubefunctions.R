#' SettFilInfoKUBE (kb)
#' 
#' Read ACCESS, extracts information needed in LagKUBE
#'
#' @return
#' KUBEdscr: 
#' - ACCESS::KUBER, KUBENAVN = KUBEid
#' TNPdscr: 
#' - ACCESS::TNP_PROD, TNP_NAVN = KUBEdscr$TNP
#' filer: 
#' - TELLERFIL (+ NEVNERFIL) from TNPdscr
#' PredFilter: 
#' - uses SettPredFilter(KUBEdscr$REFVERDI)
#' STNPdscr: 
#' - ACCESS::TNP_PROD, TNP_NAVN = TNPdscr::STANDARDTNFIL
#' FGPs: 
#' - ACCESS::FILGRUPPE, for all files in filer
#' FilDesL: 
#' - Design of files in filer
#' tmpBUFFER
SettFilInfoKUBE <- function(KUBEid, batchdate = SettKHBatchDate(), versjonert = FALSE, globs = FinnGlobs()) {
  is_kh_debug()
  
  datef <- format(strptime(batchdate, "%Y-%m-%d-%H-%M"), "#%Y-%m-%d#")
  KUBEdscr <- as.list(sqlQuery(globs$dbh, paste("SELECT * FROM KUBER WHERE KUBE_NAVN='", KUBEid, "' AND VERSJONFRA<=", datef, " AND VERSJONTIL>", datef, sep = ""), as.is = TRUE))
  if ((is.na(KUBEdscr$TNP) | KUBEdscr$TNP == "")) {
    ok <- 0
    err <- "Feltet TNP ikke satt!"
  } else {
    TNPdscr <- sqlQuery(globs$dbh, paste("SELECT * FROM TNP_PROD WHERE TNP_NAVN='", KUBEdscr$TNP, "' AND VERSJONFRA<=", datef, " AND VERSJONTIL>", datef, sep = ""), as.is = TRUE)
  }
  
  filer <- character(0)
  if ((is.na(TNPdscr$TELLERFIL) | TNPdscr$TELLERFIL == "")) {
    ok <- 0
    err <- "Feltet TELLERFIL ikke satt!"
  } else {
    filer["T"] <- TNPdscr$TELLERFIL
  }
  if (!(is.na(TNPdscr$NEVNERFIL) | TNPdscr$NEVNERFIL == "")) {
    filer["N"] <- TNPdscr$NEVNERFIL
  }
  
  # Evt ekstrafiler med info for standardisering
  if (KUBEdscr$REFVERDI_VP == "P") {
    if (!(is.na(TNPdscr$PREDNEVNERFIL) | TNPdscr$PREDNEVNERFIL == "")) {
      filer["PN"] <- gsub("^(.*):(.*)", "\\1", TNPdscr$PREDNEVNERFIL)
    } else if (!is.na(TNPdscr$NEVNERFIL)) {
      filer["PN"] <- TNPdscr$NEVNERFIL
    } else {
      filer["PN"] <- TNPdscr$TELLERFIL
    }
    if (!(is.na(TNPdscr$STANDARDTNFIL) | TNPdscr$STANDARDTNFIL == "")) {
      STNPdscr <- sqlQuery(globs$dbh, paste("SELECT * FROM TNP_PROD WHERE TNP_NAVN='", TNPdscr$STANDARDTNFIL, "' AND VERSJONFRA<=", datef, " AND VERSJONTIL>", datef, sep = ""), as.is = TRUE)
      if ((is.na(STNPdscr$TELLERFIL) | STNPdscr$TELLERFIL == "")) {
        ok <- 0
        err <- "Feltet TELLERFIL ikke satt!"
      } else {
        filer["ST"] <- STNPdscr$TELLERFIL
      }
      if (!(is.na(STNPdscr$NEVNERFIL) | STNPdscr$NEVNERFIL == "")) {
        filer["SN"] <- STNPdscr$NEVNERFIL
      } else {
        filer["SN"] <- STNPdscr$TELLERFIL
      }
    } else {
      STNPdscr <- TNPdscr
      filer["ST"] <- filer["T"]
      if (!is.na(filer["N"])) {
        filer["SN"] <- filer["N"]
      }
    }
  } else {
    STNPdscr <- list()
  }
  # Denne er ikke så veldig robust for feilspesifkasjon, men den brukes ikke annet til de enkleste tilfellene
  PredFilter <- SettPredFilter(KUBEdscr$REFVERDI, globs = globs)
  
  # Sett Tab-filter
  # For å redusere ressursbruk er det viktig at lange lister med unødvendige ETAB blir barert bort tidlig
  TabConds <- character(0)
  TabFSubTT <- ""
  for (tab in names(KUBEdscr)[grepl("^TAB\\d+$", names(KUBEdscr))]) {
    if (!(is.na(KUBEdscr[[tab]]) || KUBEdscr[[tab]] == "")) {
      tablist <- KUBEdscr[[tab]]
      tab0 <- paste(tab, "_0", sep = "")
      if (!(is.null(KUBEdscr[[tab0]]) || is.na(KUBEdscr[[tab0]]) || KUBEdscr[[tab0]] == "")) {
        tablist <- KUBEdscr[[tab0]]
      }
      minus <- grepl("^-\\[", tablist)
      tablist <- gsub("^-\\[(.*)\\]$", "\\1", tablist)
      tablist <- paste("\"", gsub(",", "\",\"", tablist), "\"", sep = "")
      tabcond <- paste("(", tab, " %in% c(", tablist, "))", sep = "")
      if (minus) {
        tabcond <- paste("!", tabcond, sep = "")
      }
      TabConds <- c(TabConds, tabcond)
    }
    TabFSubTT <- paste(TabConds, collapse = " & ")
  }
  
  FGPs <- list()
  FilDesL <- list()
  tmpBUFFER <- character(0)
  for (fil in unique(filer)) {
    TabFSub <- ifelse(fil == filer["T"], TabFSubTT, "")
    FILinfo <- KlargjorFil(fil, TabFSub = TabFSub, KUBEid = KUBEid, versjonert = versjonert, FILbatch = NA, batchdate = batchdate, globs = globs)
    FGPs[[fil]] <- FILinfo$FGP
    FilDesL[[fil]] <- FILinfo$FILd
    
    if (FILinfo$TilBuffer == 1) {
      tmpBUFFER <- c(tmpBUFFER, fil)
    }
    # FilDesL[[fil]]<-FinnDesign(FinnFilT(fil,batch=batchdate,globs=globs),FGP=FGPs[[fil]],globs=globs)
  }
  return(list(KUBEdscr = KUBEdscr, TNPdscr = TNPdscr, filer = filer, PredFilter = PredFilter, STNPdscr = STNPdscr, FGPs = FGPs, FilDesL = FilDesL, tmpBUFFER = tmpBUFFER))
}

#' SettPredFilter (kb)
#'
#' @param refvstr 
#' @param FGP 
#' @param globs 
#'
#' @return
#' @export
#'
#' @examples
SettPredFilter <- function(refvstr, FGP = list(amin = 0, amax = 120), globs = FinnGlobs()) {
  
  # Bør nok konsolidere SettPredFilter og SettNaboAnoSpec, bør være greit å gjøre dette
  
  is_kh_debug()
  PredFilter <- list()
  Pcols <- character(0)

  # D-develop
  D_develop_predtype <- "IND"
  if (grepl("AAR", refvstr)) {
    D_develop_predtype <- "DIR"
  }
  
  # Må utvikles til å lese KUBEdscr$REFVERDI
  if (is.null(refvstr) || is.na(refvstr)) {
    PredFilter <- list(Gn = data.frame(GEOniv = "L"))
  } else {
    refvstr <- gsub("(.*)ALDER=='*ALLE'*(.*)", paste("\\1", "ALDER==", FGP$amin, "_", FGP$amax, "\\2", sep = ""), refvstr)
    for (del in names(globs$DefDesign$DelKolN)) {
      delN <- globs$DefDesign$DelKolN[del]
      if (globs$DefDesign$DelType[del] == "COL") {
        if (grepl(paste("(^|\\&) *", delN, " *== *'*(.*?)'* *(\\&|$)", sep = ""), refvstr)) {
          Pcols <- c(Pcols, globs$DefDesign$DelKolsF[[del]])
          val <- gsub(paste(".*(^|\\&) *", delN, " *== *'*(.*?)'* *(\\&|$).*", sep = ""), "\\2", refvstr)
          if (globs$DefDesign$DelFormat[del] == "integer") {
            PredFilter[[del]] <- eval(parse(text = paste("data.frame(", delN, "=", as.integer(val), ",stringsAsFactors=FALSE)", sep = "")))
          } else {
            PredFilter[[del]] <- eval(parse(text = paste("data.frame(", delN, "=\"", val, "\",stringsAsFactors=FALSE)", sep = "")))
          }
        }
      } else if (globs$DefDesign$DelType[del] == "INT") {
        if (grepl(paste("(^|\\&) *", delN, "l *== *'*(.*?)'* *($|\\&)", sep = ""), refvstr) &&
            grepl(paste("(^|\\&) *", delN, "h *== *'*(.*?)'* *($|\\&)", sep = ""), refvstr)) {
          Pcols <- c(Pcols, globs$DefDesign$DelKolsF[[del]])
          vall <- gsub(paste(".*(^|\\&) *", delN, "l *== *'*(.*?)'* *($|\\&).*", sep = ""), "\\2", refvstr)
          valh <- gsub(paste(".*(^|\\&) *", delN, "h *== *'*(.*?)'* *($|\\&).*", sep = ""), "\\2", refvstr)
          PredFilter[[del]] <- eval(parse(text = paste("data.frame(", delN, "l=", as.integer(vall), ",", delN, "h=", as.integer(valh), ",stringsAsFactors=FALSE)", sep = "")))
        } else if (grepl(paste("(^|\\&) *", delN, "l{0,1} *== *'*(.*?)'* *($|\\&)", sep = ""), refvstr)) {
          intval1 <- as.integer(gsub(paste("(^|.*\\&) *", delN, "l{0,1} *== *'*(.*?)'* *($|\\&.*)", sep = ""), "\\2", refvstr))
          intval <- c(intval1, intval1)
          # Gammelt: kunne ha f.eks. AAR='2012_2014'. Dette blir for dillete mot annen bruk, må da ha "AARl='2012' & AARh='2014'"
          # intval<-as.integer(unlist(str_split(gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),"\\2",refvstr),"_")))
          # if (length(intval)==1){intval<-c(intval,intval)}
          
          # Gammelt, feil?
          # val<-gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),"\\2",refvstr)
          # refvstr<-gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),
          #          paste("\\1", paste(delN,"l",sep=""),"==\\2 &"," \\1 ",paste(delN,"l",sep=""),"==\\2"," \\3",sep=""),refvstr)
          # refvstr<-gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),
          #              paste("\\1",paste(delN,"l",sep=""),"==",intval[1]," & ",paste(delN,"h",sep=""),"==",intval[2],"\\3",sep=""),refvstr)
          
          # Litt shaky her. For AAR kan man ikke sette 'AARl=y & AARh=y' fordi det vil krasje med AARs intervall ved snitt
          # Derfor bare 'AARl=y'
          refvstr <- gsub(paste(delN, "=", sep = ""), paste(delN, "l=", sep = ""), refvstr)
          Pcols <- c(Pcols, globs$DefDesign$DelKolsF[[del]])
          PredFilter[[del]] <- eval(parse(text = paste("data.frame(", delN, "l=", intval[1], ",", delN, "h=", intval[2], ",stringsAsFactors=FALSE)", sep = "")))
        }
      }
    }
  }
  return(list(Design = PredFilter, PfiltStr = refvstr, Pkols = Pcols, D_develop_predtype = D_develop_predtype))
}


## Stata prikking do file
#' do_stata_prikk (ybk)
#' 
#' Function to censor the data using the STATA method (JRM)#'
do_stata_prikk <- function(dt, spc, batchdate, globs, test = FALSE){
  is_kh_debug()
  
  stataVar <- c("Stata_PRIKK_T", "Stata_PRIKK_N", "Stata_STATTOL_T")
  s_prikk <- sum(sapply(spc[, ..stataVar], get_col), na.rm = TRUE)
  
  RprikkVar <- c("PRIKK_T", "PRIKK_N", "STATTOL_T")
  r_prikk <- sum(sapply(spc[, ..RprikkVar], get_col), na.rm = TRUE)
  
  # Check that R prikk should be empty if Stata prikk should be used
  warn_prikk(r_prikk, s_prikk)
  RES <- NULL
  
  if (s_prikk > 0){
    ## synt <- 'include "F:\\Forskningsprosjekter\\PDB 2455 - Helseprofiler og til_\\PRODUKSJON\\BIN\\Z_Statasnutter\\Rsynt_Postprosess_naboprikking_del_1_LESEFERD_INNV.do'
    sfile <- paste(globs[["path"]], globs[["KubeStataPrikkFil"]], sep = "/")
    synt <- paste0('include "', sfile, '"')
    
    RES <- KjorStataSkript(dt, script = synt, tableTYP = "DT", batchdate = batchdate, globs = globs)
    dt <- RES$TABLE
  } else {
    RES[["feil"]] <- ""
  }
  
  if (RES$feil != "") {
    stop("Noe gikk galt i kjøring av STATA \n", RES$feil)
  }
  
  return(dt)
}

#' kube_spec (ybk)
#' 
#' Saves ACCESS specs + list of dimensions to be used in STATA censoring
kube_spec <- function(spec, dims){
  is_kh_debug()
  
  rootDir <- file.path(fs::path_home(), "helseprofil")
  if (!fs::dir_exists(rootDir))
    fs::dir_create(rootDir)
  
  specDF <- as.data.table(spec)
  varStata <- grep("^Stata", names(specDF), value = TRUE)
  varSpec <- c("KUBE_NAVN", varStata)
  varDF <- specDF[, .SD, .SDcols = varSpec]
  varDF[, DIMS := list(dims)]
  fileSpec <- file.path(rootDir, "kubespec.csv")
  data.table::fwrite(varDF, fileSpec, sep = ";", sep2 = c("", " ", ""))
  message("Create Stata spec in ", fileSpec)
  return(specDF)
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

#' find_dims (vl)
#' 
#' Helper function for kube_spec, finding dimensions in KUBE
find_dims <- function(dt, spec){
  is_kh_debug()
  # List standarddims
  standarddims <- c("GEO",
                    "AAR",
                    "ALDER",
                    "KJONN",
                    "UTDANN",
                    "INNVKAT",
                    "LANDBAK")
  
  # Extract everything written in TAB1, TAB2, and TAB3 in the files involved
  tabdims <- vector()
  for(i in 1:length(spec)){
    tabdims <- c(tabdims, 
                 unlist(spec[[i]][c("TAB1", "TAB2", "TAB3")], use.names = F))
  }
  
  # Remove NA from tabdims, combine with standarddims
  tabdims <- tabdims[!is.na(tabdims)]
  alldims <- c(standarddims, tabdims)
  # Extract column names from dt included in dimension list
  names(dt)[names(dt) %in% alldims]
}

#' LagQCkube (vl)
#' 
#' Saves QC kube containing standard columns defined in globs, 
#' and extra cols existing in the specific KUBE
LagQCKube <- function(KUBEid,
                      KUBE,
                      kubedims,
                      kubevals,
                      batchdate = batchdate,
                      globs = globs){
  QC <- copy(KUBE)
  qccols <- c(globs$QCTabs,setdiff(kubedims, globs$QCTabs),
              globs$QCVals,setdiff(kubevals, globs$QCVals),
              "SPVFLAGG")
  qcmisscols <- setdiff(qccols, names(QC))
  if (length(qcmisscols > 0)) {
    QC[, (qcmisscols) := NA]
  }
  QC <- QC[, ..qccols]
  
  return(QC)
}
