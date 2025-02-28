#' @title get_cubeparameters
#' @description
#' Helper function for [LagKUBE()]. 
#' The function reads tables from ACCESS to get information on which files are used and how they need to be handled. 
#' @param KUBEid name of cube
#' @param batchdate date of current cube batch. Needed to filter out active parameters from ACCESS
#' @param globs global parameters set by [SettGlobs()]
#' @return A list of relevant parameters
get_cubeparameters <- function(KUBEid, batchdate = SettKHBatchDate(), globs = SettGlobs()) {
  
  parameters <- list()
  parameters[["validdates"]] <- paste0("VERSJONFRA <=", FormatSqlBatchdate(batchdate), " AND VERSJONTIL >", FormatSqlBatchdate(batchdate))
  parameters[["CUBEinformation"]] <- get_cube_information(KUBEid = KUBEid, validdates = parameters$validdates, globs = globs)
  parameters[["TNPinformation"]] <- get_tnp_information(TNPname = parameters$CUBEinformation$TNP, validdates = parameters$validdates, globs = globs)
  isrefverdiP <- parameters$CUBEinformation$REFVERDI_VP == "P"
  isSTNP <- !is.na(parameters$TNPinformation$STANDARDTNFIL) && parameters$TNPinformation$STANDARDTNFIL != ""
  parameters[["STNPinformation"]] <- get_stnp_information(TNP = parameters$TNPinformation, validdates = parameters$validdates, isrefverdiP = isrefverdiP, isSTNP = isSTNP, globs = globs)
  parameters[["files"]] <- get_filenames(parameters = parameters, isrefverdiP = isrefverdiP, isSTNP = isSTNP)
  parameters[["FILFILTRE"]] <- get_filfiltre(files = parameters$files, validdates = parameters$validdates, globs = globs)
  parameters[["PredFilter"]] <- set_predictionfilter(REFVERDI = parameters$CUBEinformation$REFVERDI, globs = globs)
  parameters[["fileinformation"]] <- get_filegroup_information(files = parameters$files, filefilters = parameters$FILFILTRE, validdates = parameters$validdates, globs = globs)
  return(parameters)
}

#' @title get_cube_information
#' @description
#' Helper function for [get_cubeparameters()].
#' Reads from table KUBER in the ACCESS database
#' @noRD
#' @param KUBEid cube name
#' @param validdates valid dates to specify active rows in the ACCESS table
#' @param globs 
get_cube_information <- function(KUBEid, validdates, globs){
  KUBER <- as.list(RODBC::sqlQuery(globs$dbh, 
                                   query = paste0("SELECT * FROM KUBER WHERE KUBE_NAVN='", KUBEid, "' AND ", validdates), 
                                   as.is = TRUE))
  
  if ((is.na(KUBER$TNP) || KUBER$TNP == "")) stop("Feltet ACCESS::KUBER::TNP er ikke satt for ", KUBEid)
  return(KUBER)
}

#' @title get_tnp_information
#' @description
#' Helper function for [get_cubeparameters()]
#' Reads from table TNP_PROD in the ACCESS database
#' @noRD
#' @param TNPname Information from TNP column in KUBER table
#' @param validdates valid dates to specify active rows in the ACCESS table
#' @param globs global parameters
get_tnp_information <- function(TNPname, validdates, globs){
  TNP_PROD <- as.list(RODBC::sqlQuery(globs$dbh, 
                                      query = paste0("SELECT * FROM TNP_PROD WHERE TNP_NAVN='", TNPname, "' AND ", validdates), 
                                      as.is = TRUE))
  return(TNP_PROD)
}

#' @title get_stnp_information
#' @description
#' Helper function for [get_cubeparameters()].
#' Reads from table TNP_PROD in the ACCESS database, to get information needed for standardization
#' @noRD
#' @param TNP TNP table generated by [get_tnp_information()]
#' @param validdates valid dates to specify active rows in the ACCESS table
#' @param isrefverdiP TRUE/FALSE, is REFVERDI_VP column in KUBER table == "P", indicating if STNP is to be generated at all
#' @param isSTNP TRUE/FALSE, indicating if STNP should be different from TNP. This is indicated in the STANDARDTNFIL in TNP_PROD table.
#' @param globs global parameters
get_stnp_information <- function(TNP, validdates, isrefverdiP = F, isSTNP = F, globs){
  if(!isrefverdiP) return(list())
  if(!isSTNP) return(TNP)
  
  STNP <- as.list(RODBC::sqlQuery(globs$dbh, 
                                  query = paste0("SELECT * FROM TNP_PROD WHERE TNP_NAVN='", TNP$STANDARDTNFIL, "' AND ", validdates), 
                                  as.is = TRUE))
  return(STNP)
}

#' @title get_filenames
#' @description
#' Helper function for [get_cubeparameters()]
#' Gets a list of files specifies in the TNP_PROD table (TELLERFIL, NEVNERFIL etc.)
#' @param parameters parameters list, to access TNP and STNP with information on which files are needed
#' @param isrefverdiP TRUE/FALSE, is REFVERDI_VP column in KUBER table == "P", indicating if standard files should be included
#' @param isSTNP TRUE/FALSE, indicating if standard files should be different from the main files. This is indicated in the STANDARDTNFIL in TNP_PROD table.
get_filenames <- function(parameters, isrefverdiP, isSTNP = F){
  files <- list()
  isteller <- !is.na(parameters$TNPinformation$TELLERFIL) && parameters$TNPinformation$TELLERFIL != ""
  if(!isteller) stop("Feltet ACCESS::TNP_PROD::TELLERFIL er ikke satt!")
  files[["TELLER"]] <- parameters$TNPinformation$TELLERFIL
  isnevnerfil <- !is.na(parameters$TNPinformation$NEVNERFIL) && parameters$TNPinformation$NEVNERFIL != ""
  if(isnevnerfil) files[["NEVNER"]] <- parameters$TNPinformation$NEVNERFIL
  
  if(isrefverdiP){
    isprednevnerfil <- !is.na(parameters$TNPinformation$PREDNEVNERFIL) && parameters$TNPinformation$PREDNEVNERFIL != ""
    if(isprednevnerfil){
      files[["PREDNEVNER"]] <- gsub("^(.*):.*", "\\1", parameters$TNPinformation$PREDNEVNERFIL)
    } else if(isnevnerfil) {
      files[["PREDNEVNER"]] <- files$NEVNER
    } else {
      files[["PREDNEVNER"]] <- files$TELLER
    }
    
    if(!isSTNP){
      files[["STANDARDTELLER"]] <- files$TELLER
      if(isnevnerfil) files[["STANDARDNEVNER"]] <- files$NEVNER
    } else {
      isSTNPtellerfilfil <- !is.na(parameters$STNPinformation$TELLERFIL) && parameters$STNPinformation$TELLERFIL != ""
      if(!isSTNPtellerfil) stop("Feltet ACCESS::TNP_PROD::TELLERFIL er ikke satt for STANDARDTNFIL: ", parameters$TNPinformation$STANDARDTNFIL)
      files[["STANDARDTELLER"]] <- parameters$STNPinformation$TELLERFIL
      
      isSTNPnevnerfil <- !is.na(parameters$STNPinformation$NEVNERFIL) && parameters$STNPinformation$NEVNERFIL != ""
      if(isSTNPnevnerfil) files[["STANDARDNEVNER"]] <- parameters$STNPinformation$NEVNERFIL
      if(!isSTNPnevnerfil) files[["STANDARDNEVNER"]] <- parameters$STNPinformation$TELLERFIL
    }
  }
  return(files)
}

#' @title get_filfiltre
#' @description
#' Helper function for [get_cubeparameters()]
#' Reads from table FILFILTRE in the ACCESS database, to get information on special treatment of files
#' @param files list of files generated by [get_filenames()]
#' @param validdates valid dates to specify active rows in the ACCESS table
#' @param globs global parameters
get_filfiltre <- function(files = NULL, validdates, globs){
  filfiltre <- data.table::setDT(RODBC::sqlQuery(globs$dbh, paste0("SELECT * FROM FILFILTRE WHERE ", validdates), as.is = TRUE))
  filfiltre <- filfiltre[FILVERSJON %in% unique(files)]
  return(filfiltre)
}

#' @title set_predictionfilter (kb)
#' @description 
#' Helper function for [get_cubeparameters()]
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

#' @title get_filegroup_information
#' @description 
#' Helper function for [get_cubeparameters()]
#' reads from FILGRUPPER in access
#' @noRD
#' @param files list of files to be used in the cube 
#' @param validdates valid dates to specify active rows in the ACCESS table
#' @param globs global parameters
get_filegroup_information <- function(files, filefilters, validdates, globs = SettGlobs()){
  fileinfo <- list()
  
  for(file in unique(files)){
    filefilter <- filefilters[FILVERSJON == file]
    if(nrow(filefilter) > 1) stop("> 1 rad i FILFILTRE funnet for FILVERSJON = ", file)
    filename <- ifelse(nrow(filefilter) == 1, filefilter$ORGFIL, file)
    FILGRUPPER <- as.list(RODBC::sqlQuery(globs$dbh, paste0("SELECT * FROM FILGRUPPER WHERE FILGRUPPE='", filename, "' AND ", validdates), as.is = TRUE))
    if(length(FILGRUPPER$FILGRUPPE) != 1) stop(paste0("FILGRUPPE ", filename, " finnes ikke, er duplisert, eller er satt til inaktiv"))
    
    isalderalle <- !is.na(FILGRUPPER$ALDER_ALLE)
    if(isalderalle && !grepl("^\\d+_\\d+$", FILGRUPPER$ALDER_ALLE)) stop("Feil format p� ALDER_ALLE for FILGRUPPE ", filename)
    if(isalderalle){
      alle_aldre <- as.integer(tstrsplit(FILGRUPPER$ALDER_ALLE, "_"))
      amin <- alle_aldre[1]
      amax <- alle_aldre[2]
    } else {
      amin <- getOption("khfunctions.amin")
      amax <- getOption("khfunctions.amax")
    }
    
    vals <- list()
    for(val in grep("^VAL", getOption("khfunctions.kolorgs"), value = T)) {
      valname <- paste0(val, "navn")
      isvalname <- !is.na(FILGRUPPER[[valname]]) && FILGRUPPER[[valname]] != ""
      valname <- ifelse(isvalname, FILGRUPPER[[valname]], val)
      valmiss <- paste0(val, "miss")
      isvalmiss <-  !is.na(FILGRUPPER[[valmiss]]) && FILGRUPPER[[valmiss]] != ""
      valmiss <- ifelse(isvalmiss, FILGRUPPER[[valmiss]], "0")
      valsumbar <- paste0(val, "sumbar")
      isvalsumbar <- !is.na(FILGRUPPER[[valsumbar]]) && FILGRUPPER[[valsumbar]] != ""
      valsumbar <- ifelse(isvalsumbar, FILGRUPPER[[valsumbar]], "0")
      vals[[valname]] <- list(miss = valmiss, sumbar = valsumbar)
    }
    fileinfo[[file]] <- c(FILGRUPPER, list(vals = vals, amin = amin, amax = amax))
  }
  return(fileinfo)
}

#' @title get_filedesign
#' @description
#' @noRD
#' @param files 
#' @param parameters 
#' @param globs 
get_filedesign <- function(parameters, globs){
  if(!exists("BUFFER", envir = .GlobalEnv)) stop("BUFFER does not exist, files not loaded")
  filedesign <- list()
  files <- unique(parameters$files)
  for(file in files){
    if(is.null(.GlobalEnv$BUFFER[[file]])) stop("File ", file, " is not loaded into BUFFER")
    fileinfo <- parameters$fileinformation[[file]]
    filedesign[[file]] <- FinnDesign(FIL = .GlobalEnv$BUFFER[[file]], FGP = fileinfo, globs = globs)
  }
  return(filedesign)
}

