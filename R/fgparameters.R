#' @title get_filegroup_parameters
#' @description
#' Helper function for `LagFilgruppe()`. 
#' The function reads tables from ACCESS to get information on which files are used and how they need to be handled. 
#' @param filegroup_name name of filgruppe
#' @param batchdate date of current batch. Needed to filter out active parameters from ACCESS
#' @param globs global parameters, defaults to SettGlobs
#' @return A list of relevant parameters
get_filegroup_parameters <- function(name, versjonert, dumps){
  cat("* Henter parametre")
  globs <- SettGlobs()
  parameters <- list()
  parameters[["filegroup_name"]] <- name
  parameters[["batchdate"]] <- SettKHBatchDate()
  parameters[["versjonert"]] <- versjonert
  parameters[["dumps"]] <- dumps
  parameters[["validdates"]] <- paste0("VERSJONFRA <=", FormatSqlBatchdate(parameters$batchdate), " AND VERSJONTIL >", FormatSqlBatchdate(parameters$batchdate))
  parameters[["filegroup_information"]] <- read_filgrupper_and_add_vals(filegroup_name = name, validdates = parameters$validdates, globs = globs)
  parameters[["read_parameters"]] <- get_read_parameters(filegroup_name = name, validdates = parameters$validdates, globs = globs)
  parameters[["n_files"]] <- nrow(parameters$read_parameters)
  parameters[["codebook"]] <- get_codebook(filegroup_name = name, validdates = parameters$validdates, globs = globs)
  parameters[["GeoNavn"]] <- data.table::setDT(RODBC::sqlQuery(globs$dbh, "SELECT GEO AS NYGEO, NAVN FROM GeoNavn", as.is = TRUE))
  parameters[["TKNR"]] <- data.table::setDT(RODBC::sqlQuery(globs$dbh, "SELECT * from TKNR", as.is = TRUE), key = c("ORGKODE"))
  return(c(parameters, globs))
}

#' @noRd
read_filgrupper_and_add_vals <- function(filegroup_name, validdates, globs){
  FILGRUPPER <- as.list(RODBC::sqlQuery(globs$dbh, paste0("SELECT * FROM FILGRUPPER WHERE FILGRUPPE='", filegroup_name, "' AND ", validdates), as.is = TRUE))
  if(length(FILGRUPPER$FILGRUPPE) != 1) stop(paste0("FILGRUPPE ", filegroup_name, " finnes ikke, er duplisert, eller er satt til inaktiv"))
  
  isalderalle <- !is.na(FILGRUPPER$ALDER_ALLE)
  if(isalderalle && !grepl("^\\d+_\\d+$", FILGRUPPER$ALDER_ALLE)) stop("Feil format på ALDER_ALLE for FILGRUPPE ", filegroup_name)
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
get_read_parameters <- function(filegroup_name, validdates, globs){
  orginnleskobl <- data.table::setDT(RODBC::sqlQuery(globs$dbh, query = paste0("SELECT * FROM ORGINNLESkobl WHERE FILGRUPPE='", filegroup_name, "'"), as.is = TRUE))
  originalfiler <- data.table::setDT(RODBC::sqlQuery(globs$dbh, query = paste0("SELECT * FROM ORIGINALFILER WHERE ", gsub("VERSJON", "IBRUK", validdates)), as.is = TRUE))
  innlesing <- data.table::setDT(RODBC::sqlQuery(globs$dbh, query = paste0("SELECT * FROM INNLESING WHERE FILGRUPPE='", filegroup_name, "' AND ", validdates), as.is = TRUE))
  
  outcols <- c("KOBLID", "FILID", "FILNAVN", "FORMAT", "DEFAAR", setdiff(names(innlesing), "KOMMENTAR"))
  out <- collapse::join(orginnleskobl, originalfiler, how = "i", on = "FILID", overid = 2, verbose = 0)
  out <- collapse::join(out, innlesing, how = "i", on = c("FILGRUPPE", "DELID"), overid = 2, verbose = 0)
  out <- out[, .SD, .SDcols = outcols]
  out[, let(FILNAVN = gsub("\\\\", "/", FILNAVN))]
  out[, let(filepath = file.path(getOption("khfunctions.root"), FILNAVN), FORMAT = toupper(FORMAT))]
  out[AAR == "<$y>", let(AAR = paste0("<", DEFAAR, ">"))]
  return(out)
}

get_codebook <- function(filegroup_name, validdates, globs){
  codebook <- data.table::setDT(RODBC::sqlQuery(globs$dbh, 
                                                paste0("SELECT FELTTYPE, DELID, TYPE, ORGKODE, NYKODE FROM KODEBOK WHERE FILGRUPPE='", 
                                                       filegroup_name, "' AND ", validdates), as.is = TRUE))
  codebook[is.na(ORGKODE), ORGKODE := ""]
  return(codebook)
}
