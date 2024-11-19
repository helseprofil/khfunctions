#' @title get_cubeparameters
#' 
#' BØR VÆRE EGEN FIL
#' 
#' Read ACCESS, extracts information needed in LagKUBE
#' Reads files and store them in BUFFER (Global)
get_cubeparameters <- function(KUBEid, batchdate = SettKHBatchDate(), versjonert = FALSE, globs = SettGlobs()) {
  validdates <- paste0("VERSJONFRA <=", FormatSqlBatchdate(batchdate), " AND VERSJONTIL >", FormatSqlBatchdate(batchdate))
  
  params <- list()
  params[["KUBEdscr"]] <- get_cubedescription(KUBE = KUBEid, validdates = validdates, globs = globs)
  params[["TNPdscr"]] <- get_tnpdescription(TNP = params$KUBEdscr$TNP, validdates = validdates, globs = globs)
  isrefverdiP <- params$KUBEdscr$REFVERDI_VP == "P"
  isSTNP <- !is.na(params$TNPdscr$STANDARDTNFIL) && params$TNPdscr$STANDARDTNFIL != ""
  params[["STNPdscr"]] <- get_stnpdescription(TNP = params$TNPdscr, validdates = validdates, refverdiP = isrefverdiP, STNP = isSTNP, globs = globs)
  params[["filer"]] <- get_filenames(params = params, refverdiP = isrefverdiP, STNP = isSTNP)
  params[["FILFILTRE"]] <- get_filfiltre(files = params$filer, validdates = validdates, globs = globs)
  params[["PredFilter"]] <- set_predictionfilter(REFVERDI = params$KUBEdscr$REFVERDI, globs = globs)
  params[["tabfilter"]] <- set_tabfilter(params$KUBEdscr)
  return(params)
}

get_cubedescription <- function(KUBE, validdates, globs){
  KUBER <- as.list(RODBC::sqlQuery(globs$dbh, 
                                   query = paste0("SELECT * FROM KUBER WHERE KUBE_NAVN='", KUBE, "' AND ", validdates), 
                                   as.is = TRUE))
  
  if ((is.na(KUBER$TNP) || KUBER$TNP == "")) stop("Feltet ACCESS::KUBER::TNP er ikke satt for ", KUBEid)
  return(KUBER)
}

get_tnpdescription <- function(TNP, validdates, globs){
  TNP_PROD <- as.list(RODBC::sqlQuery(globs$dbh, 
                                      query = paste0("SELECT * FROM TNP_PROD WHERE TNP_NAVN='", TNP, "' AND ", validdates), 
                                      as.is = TRUE))
  return(TNP_PROD)
}

get_stnpdescription <- function(TNP, validdates, refverdiP, STNP=F, globs){
  if(!refverdiP) return(list())
  if(!STNP) return(TNP)
  
  STNP <- as.list(RODBC::sqlQuery(globs$dbh, 
                                  query = paste0("SELECT * FROM TNP_PROD WHERE TNP_NAVN='", TNP$STANDARDTNFIL, "' AND ", validdates), 
                                  as.is = TRUE))
  return(STNP)
}

get_filenames <- function(params, refverdiP, STNP = F){
  files <- list()
  isteller <- !is.na(params$TNPdscr$TELLERFIL) && params$TNPdscr$TELLERFIL != ""
  if(!isteller) stop("Feltet ACCESS::TNP_PROD::TELLERFIL er ikke satt!")
  files[["TELLER"]] <- params$TNPdscr$TELLERFIL
  isnevnerfil <- !is.na(params$TNPdscr$NEVNERFIL) && params$TNPdscr$NEVNERFIL != ""
  if(isnevnerfil) files[["NEVNER"]] <- params$TNPdscr$NEVNERFIL
  
  if(refverdiP){
    isprednevnerfil <- !is.na(params$TNPdscr$PREDNEVNERFIL) && params$TNPdscr$PREDNEVNERFIL != ""
    if(isprednevnerfil){
      files[["PREDNEVNER"]] <- gsub("^(.*):.*", "\\1", params$TNPdscr$PREDNEVNERFIL)
    } else if(isnevnerfil) {
      files[["PREDNEVNER"]] <- files$NEVNER
    } else {
      files[["PREDNEVNER"]] <- files$TELLER
    }
    
    if(!STNP){
      files[["STANDARDTELLER"]] <- files$TELLER
      if(isnevnerfil) files[["STANDARDNEVNER"]] <- files$NEVNER
    } else {
      isSTNPtellerfilfil <- !is.na(params$STNPdscr$TELLERFIL) && params$STNPdscr$TELLERFIL != ""
      if(!isSTNPtellerfil) stop("Feltet ACCESS::TNP_PROD::TELLERFIL er ikke satt for STANDARDTNFIL: ", params$TNPdscr$STANDARDTNFIL)
      files[["STANDARDTELLER"]] <- params$STNPdscr$TELLERFIL
      
      isSTNPnevnerfil <- !is.na(params$STNPdscr$NEVNERFIL) && params$STNPdscr$NEVNERFIL != ""
      if(isSTNPnevnerfil) files[["STANDARDNEVNER"]] <- params$STNPdscr$NEVNERFIL
      if(!isSTNPnevnerfil) files[["STANDARDNEVNER"]] <- params$STNPdscr$TELLERFIL
    }
  }
  return(files)
}

get_filfiltre <- function(files = NULL, validdates, globs){
  filfiltre <- data.table::setDT(RODBC::sqlQuery(globs$dbh, paste0("SELECT * FROM FILFILTRE WHERE ", validdates), as.is = TRUE))
  filfiltre <- filfiltre[FILVERSJON %in% unique(files)]
  return(filfiltre)
}

#' set_predictionfilter (kb)
#'
#' @param REFVERDI Corresponds to ACCESS::KUBER::REFVERDI
#' @param globs global parameters
set_predictionfilter <- function(REFVERDI, globs = SettGlobs()) {
  
  D_develop_predtype <- ifelse(grepl("AAR", REFVERDI), "DIR", "IND")
  PredFilter <- list()
  Pcols <- character(0)
  delkolN <- globs$DefDesign$DelKolN
  deltype <- globs$DefDesign$DelType
  delformat <- globs$DefDesign$DelFormat
  delkolsF <- globs$DefDesign$DelKolsF
  
  if (is.null(REFVERDI) || is.na(REFVERDI)) {
    PredFilter <- list(Gn = data.frame(GEOniv = "L"))
  } else {
    refverdicolumns <- delkolN[sapply(paste0(delkolN, "(l|h)? *="), grepl, REFVERDI)]
    for (del in names(refverdicolumns)) {
      delN <- refverdicolumns[del]
      if (deltype[del] == "COL") {
        val <- gsub(paste0(".*", delN, " *== *\'(.*?)\'.*"), "\\1", REFVERDI)
        if (delformat[del] == "integer") val <- as.integer(val)
        PredFilter[[del]] <- data.table::setDT(setNames(as.list(val), delN))
      } else if (deltype[del] == "INT") {
        if (grepl(paste0(delN, "l *=="), REFVERDI) && grepl(paste0(delN, "h *=="), REFVERDI)) {
          vall <- gsub(paste0(".*", delN, "l *== *\'(.*?)\'.*"), "\\1", REFVERDI)
          valh <- gsub(paste0(".*", delN, "h *== *\'(.*?)\'.*"), "\\1", REFVERDI)
          intval <- list(vall, valh)
          PredFilter[[del]] <- eval(parse(text = paste0("data.frame(", delN, "l=", as.integer(vall), ",", delN, "h=", as.integer(valh), ",stringsAsFactors=FALSE)")))
          PredFilter[[del]] <- data.table::setDT(setNames(intval, paste0(delN, c("l", "h"))))
        } else if (grepl(paste0(delN, "l *=="), REFVERDI)) {
          intval1 <- as.integer(gsub(paste0(".*", delN, "l *== *\'(.*?)\'.*"), "\\1", REFVERDI))
          intval <- list(intval1, intval1)
          REFVERDI <- gsub(paste0(delN, " *="), paste0(delN, "l="), REFVERDI)
          PredFilter[[del]] <- data.table::setDT(setNames(intval, paste0(delN, c("l", "h"))))
        }
      }
      Pcols <- c(Pcols, delkolsF[[del]])
    }
  }
  return(list(Design = PredFilter, PfiltStr = REFVERDI, Pkols = Pcols, D_develop_predtype = D_develop_predtype))
}

#' @title set_tabfilter
#'
#' @param KUBEdscr 
#'
#' @return
#' @export
#'
#' @examples
set_tabfilter <- function(KUBEdscr){
  TabConds <- character()
  for (tab in names(KUBEdscr)[grepl("^TAB\\d+$", names(KUBEdscr))]) {
    istab <- !is.na(KUBEdscr[[tab]]) && KUBEdscr[[tab]] != ""
    if (istab) {
      tab0 <- paste0(tab, "_0")
      istab0 <- !is.null(KUBEdscr[[tab0]]) && !is.na(KUBEdscr[[tab0]]) && KUBEdscr[[tab0]] != ""
      tablist <- KUBEdscr[[tab]]
      if (istab0) tablist <- KUBEdscr[[tab0]]
      isminus <- grepl("^-\\[", tablist)
      tablist <- gsub("^-\\[(.*)\\]$", "\\1", tablist)
      tablist <- paste0("\"", gsub(",", "\",\"", tablist), "\"")
      tabcond <- paste0("(", tab, " %in% c(", tablist, "))")
      if (isminus) tabcond <- paste0("!", tabcond)
      TabConds <- c(TabConds, tabcond)
    }
  } 
  tabfilter <- paste0(TabConds, collapse = " & ")
  return(tabfilter)
}

