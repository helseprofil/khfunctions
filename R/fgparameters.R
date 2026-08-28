#' @title get_filegroup_parameters
#' @description
#' Helper function for `LagFilgruppe()`. 
#' The function reads tables from ACCESS to get information on which files are used and how they need to be handled. 
#' @param user_args user defined arguments to LagFilgruppe
#' @return A list of relevant parameters
get_filegroup_parameters <- function(user_args){
  print_console_message("\n* Henter parametre")
  parameters <- get_global_parameters()
  parameters <- c(parameters, user_args)
  parameters[["filegroup_information"]] <- read_filegroups_and_add_values(parameters = parameters)
  parameters[["read_parameters"]] <- get_read_parameters(parameters = parameters)
  parameters[["n_files"]] <- nrow(parameters$read_parameters)
  parameters[["codebook"]] <- get_codebook(parameters = parameters)
  parameters[["GeoNavn"]] <- data.table::setDT(RODBC::sqlQuery(parameters$dbh, "SELECT GEO AS NYGEO, NAVN FROM GeoNavn", as.is = TRUE))
  parameters[["TKNR"]] <- data.table::setDT(RODBC::sqlQuery(parameters$dbh, "SELECT * from TKNR", as.is = TRUE), key = c("ORGKODE"))
  parameters[["GkBHarm"]] <- data.table::setDT(RODBC::sqlQuery(parameters$dbh, "SELECT * FROM GKBydel2004T", as.is = TRUE), key = c("GK", "Bydel2004"))
  parameters[["old_locale"]] <- ensure_utf8_encoding()
  parameters[["threads"]] <- set_threads()
  parameters[["duck"]] <- init_duckdb(dbname = "filgruppeduck") 
  parameters[["KnrHarm"]] <- get_geo_recoding(parameters = parameters)
  return(parameters)
}

#' @noRd
read_filegroups_and_add_values <- function(filegroup = NULL, parameters, translate_bef = FALSE){
  if(is.null(filegroup)) filegroup <- parameters$name
  if(grepl("BEF_Gkny", filegroup, ignore.case = TRUE)) filegroup <- "BEF_GKny"
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
  
  valnamecols <- grep("^VAL\\d+navn$", names(FILGRUPPER), value = TRUE)
  valnamecols <- valnamecols[vapply(FILGRUPPER[valnamecols],is_not_empty,logical(1))]
  
  if(translate_bef && grepl("BEF_GKny", filegroup, ignore.case = TRUE)){
    if(is.null(parameters[["TNPinformation"]])) stop("Forsøker å lese parameters$TNPinformation, men den finnes ikke")
    befcol <- valnamecols[unlist(FILGRUPPER[valnamecols], use.names = FALSE) == "BEF"]
    correctbef <- unique(grep("^BEF|^mBEF", unlist(parameters$TNPinformation[c("TELLERKOL", "NEVNERKOL")],
                                                   use.names = FALSE),value = TRUE))
    if(length(correctbef) == 1){
      FILGRUPPER[befcol] <- correctbef
    }
  }
  
  vals <- list()
  for(val in valnamecols) {
    valname <- FILGRUPPER[[val]]
    miss <- sub("navn", "miss", val)
    valmiss <- if(is_not_empty(FILGRUPPER[[miss]])) { FILGRUPPER[[miss]] } else {"0"}
    sumbar <- sub("navn", "sumbar", val)
    valsumbar <- if(is_not_empty(FILGRUPPER[[sumbar]])) { FILGRUPPER[[sumbar]] } else {"0"}
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
