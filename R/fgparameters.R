#' @title get_filegroup_parameters
#' @description
#' Helper function for `LagFilgruppe()`. 
#' The function reads tables from ACCESS to get information on which files are used and how they need to be handled. 
#' @param user_args user defined arguments to LagFilgruppe
#' @return A list of relevant parameters
get_filegroup_parameters <- function(user_args){
  cat("\n* Henter parametre")
  parameters <- get_global_parameters()
  parameters <- c(parameters, user_args)
  parameters[["filegroup_information"]] <- read_filegroups_and_add_values(parameters = parameters)
  parameters[["read_parameters"]] <- get_read_parameters(parameters = parameters)
  parameters[["n_files"]] <- nrow(parameters$read_parameters)
  parameters[["codebook"]] <- get_codebook(parameters = parameters)
  parameters[["GeoNavn"]] <- data.table::setDT(RODBC::sqlQuery(parameters$dbh, "SELECT GEO AS NYGEO, NAVN FROM GeoNavn", as.is = TRUE))
  parameters[["TKNR"]] <- data.table::setDT(RODBC::sqlQuery(parameters$dbh, "SELECT * from TKNR", as.is = TRUE), key = c("ORGKODE"))
  parameters[["GkBHarm"]] <- data.table::setDT(RODBC::sqlQuery(parameters$dbh, "SELECT * FROM GKBydel2004T", as.is = TRUE), key = c("GK", "Bydel2004"))
  return(c(parameters))
}

#' @noRd
read_filegroups_and_add_values <- function(filegroup = NULL, parameters){
  if(is.null(filegroup)) filegroup <- parameters$name
  if(filegroup == "BEF_Gkny") filegroup <- "BEF_GKny"
  FILGRUPPER <- as.list(RODBC::sqlQuery(parameters$dbh, paste0("SELECT * FROM FILGRUPPER WHERE FILGRUPPE='", filegroup, "' AND ", parameters$validdates), as.is = TRUE))
  if(length(FILGRUPPER$FILGRUPPE) != 1) stop(paste0("FILGRUPPE ", filegroup, " finnes ikke, er duplisert, eller er satt til inaktiv"))
  
  isalderalle <- is_not_empty(FILGRUPPER$ALDER_ALLE)
  if(isalderalle && !grepl("^\\d+_\\d+$", FILGRUPPER$ALDER_ALLE)) stop("Feil format på ALDER_ALLE for FILGRUPPE ", filegroup)
  if(isalderalle){
    alle_aldre <- as.integer(data.table::tstrsplit(FILGRUPPER$ALDER_ALLE, "_"))
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
  return(c(FILGRUPPER, list(vals = vals, amin = amin, amax = amax)))
}

#' @title get_read_parameters
#' @description
#' Reads and combine orginnleskobl, originalfiler, and filgrupper from ACCESS.
#' @noRd
get_read_parameters <- function(parameters){
  orginnleskobl <- data.table::setDT(RODBC::sqlQuery(parameters$dbh, query = paste0("SELECT * FROM ORGINNLESkobl WHERE FILGRUPPE='", parameters$name, "'"), as.is = TRUE))
  orginnleskobl[, FILGRUPPE := fix_befgk_spelling(FILGRUPPE)]
  originalfiler <- data.table::setDT(RODBC::sqlQuery(parameters$dbh, query = paste0("SELECT * FROM ORIGINALFILER WHERE ", gsub("VERSJON", "IBRUK", parameters$validdates)), as.is = TRUE))
  innlesing <- data.table::setDT(RODBC::sqlQuery(parameters$dbh, query = paste0("SELECT * FROM INNLESING WHERE FILGRUPPE='", parameters$name, "' AND ", parameters$validdates), as.is = TRUE))
  innlesing[, FILGRUPPE := fix_befgk_spelling(FILGRUPPE)]
  
  outcols <- c("KOBLID", "FILID", "FILNAVN", "FORMAT", "DEFAAR", setdiff(names(innlesing), "KOMMENTAR"))
  out <- collapse::join(orginnleskobl, originalfiler, how = "i", on = "FILID", overid = 2, verbose = 0)
  out <- collapse::join(out, innlesing, how = "i", on = c("FILGRUPPE", "DELID"), overid = 2, verbose = 0)
  out <- out[, .SD, .SDcols = outcols]
  out[, let(FILNAVN = fix_befgk_spelling(gsub("\\\\", "/", FILNAVN)))]
  out[, let(filepath = file.path(getOption("khfunctions.root"), FILNAVN), FORMAT = toupper(FORMAT))]
  out[AAR == "<$y>", let(AAR = paste0("<", DEFAAR, ">"))]
  return(out)
}

get_codebook <- function(parameters){
  codebook <- data.table::setDT(RODBC::sqlQuery(parameters$dbh, 
                                                paste0("SELECT FELTTYPE, DELID, TYPE, ORGKODE, NYKODE FROM KODEBOK WHERE FILGRUPPE='", 
                                                       parameters$name, "' AND ", parameters$validdates), as.is = TRUE))
  codebook[is.na(ORGKODE), ORGKODE := ""]
  return(codebook)
}
