#' @title get_global_parameters
#' @description
#' Sets global parameters used for filgruppe and kube. The two main functions will add specific parameters needed. 
#' @returns list
get_global_parameters <- function(){
  globs <- list()
  globs[["dbh"]] <- connect_khelsa()
  globs[["log"]] <- connect_khlogg()
  globs[["batchdate"]] <- SettKHBatchDate()
  globs[["validdates"]] <- paste0("VERSJONFRA <=", FormatSqlBatchdate(globs$batchdate), " AND VERSJONTIL >", FormatSqlBatchdate(globs$batchdate))
  globs[["GeoKoder"]] <- data.table::setDT(RODBC::sqlQuery(globs$dbh, "SELECT * from GEOKoder", as.is = TRUE), key = "GEO")
  globs[["DefDesign"]] <- get_default_design(globs = globs)
  globs[["LegKoder"]] <- SettLegitimeKoder(globs = globs)
  globs[["TotalKoder"]] <- getOption("khfunctions.totals")
  Stata <- FinnStataExe()
  globs[["StataExe"]] <- Stata$Exe
  globs[["StataVers"]] <- Stata$Vers
  return(globs)
}

#' @title get_default_design
#' @description Setter standard designegenskaper, slik som delenes kolonnenavn og status i omkoding
#' Se tabell KH_DELER
#' @keywords internal
#' @noRd
get_default_design <- function(globs) {
  deler <- data.table::setDT(RODBC::sqlQuery(globs$dbh, "SELECT * FROM KH_DELER WHERE DEL <> 'S'", as.is = TRUE), key = "ID")
  out <- list()
  out[["DelKolN"]] <- setNames(deler$DelKol, deler$DEL)
  out[["DelType"]] <- setNames(deler$TYPE, deler$DEL)
  out[["DelFormat"]] <- setNames(deler$FORMAT, deler$DEL)
  out <- add_delkols(out = out, deler = deler)
  out[["AggPri"]] <- deler[order(AGGREGERPRI), DEL]
  out[["AggVedStand"]] <- deler[AGGREGERvedPRED == 1, DEL]
  out[["IntervallHull"]] <- deler[!is.na(INTERVALLHULL), setNames(INTERVALLHULL, DEL)]
  out[["UBeting"]] <- deler[OMKODbet == "U", DEL]
  out[["BetingOmk"]] <- deler[OMKODbet == "B", DEL]
  out[["BetingF"]] <- deler[OMKODbet == "F", DEL]
  
  out[["DesignKols"]] <- unlist(out$DelKols[c(out$UBeting, out$BetingOmk)])
  out[["DesignKolsF"]] <- c(out$DesignKols, unlist(out$DelKols[out$BetingF]))
  extra <- unlist(out$DelKolsF[c(out$UBeting, out$BetingOmk)])
  extra <- extra[!extra %in% unlist(out$DelKols[c(out$UBeting, out$BetingOmk)])]
  out[["DesignKolsFA"]] <- c(out$DesignKolsF, extra)
  return(out)
}

#' @title add_delkols
#' @description adds DelKols and DelKolsF to default design
#' @keywords internal
#' @noRd
add_delkols <- function(out, deler){
  out[["DelKols"]] <- as.list(out$DelKolN)
  DelKolE <- setNames(deler$DelKolE, deler$DEL)
  intcols <- names(which(out$DelType == "INT"))
  extracols <- deler[!is.na(DelKolE), DEL]
  for(int in intcols) out$DelKols[[int]] <- paste0(out$DelKols[[int]], c("l", "h"))
  out[["DelKolsF"]] <- out$DelKols
  for(col in extracols){
    ecols <- unlist(strsplit(deler[DEL == col, DelKolE], ","))
    out$DelKolsF[[col]] <- c(out$DelKolsF[[col]], ecols)
  }
  return(out)
}

#' @title SettLegitimeKoder
#' @author Kåre Bævre
#' @param globs global parameters, defaults to SettGlobs
#' @keywords internal
#' @noRd
SettLegitimeKoder <- function(globs) {
  Koder <- data.table::setDT(RODBC::sqlQuery(globs$dbh, "SELECT DEL, KODE FROM KH_KODER WHERE DEL <> 'S'", as.is = TRUE))
  return(split(Koder, by = "DEL"))
}

#' @title FinnStataExe 
#' @author Yusman Kamaleri
#' @description Find the most recent version of locally installed Stata
#' @keywords internal
#' @noRd
FinnStataExe <- function() {
  stata_bin <- "StataSE-64.exe"
  program_path <- c("C:/Program Files/", "C:/Program Files (x86)/")
  stata_prog <- grep("Stata", fs::dir_ls(program_path), value = TRUE)
  stata_ver <- stringi::stri_extract(stata_prog, regex = "\\d{2}$")
  Vers <- max(as.numeric(stata_ver))
  stata_path <- grep(Vers, stata_prog, value = TRUE)
  Exe <- file.path(stata_path, stata_bin)
  
  return(list(Exe = Exe, Vers = Vers))
}

#' @title connect_khelsa
#' @description connects to khelsa.mdb
#' @keywords internal
#' @noRd
connect_khelsa <- function(){
  RODBC::odbcConnectAccess2007(file.path(getOption("khfunctions.root"), 
                                         getOption("khfunctions.db")))
}

#' @title connect_khlogg
#' @description connects to khlogg.mdb
#' @keywords internal
#' @noRd
connect_khlogg <- function(){
  RODBC::odbcConnectAccess2007(file.path(getOption("khfunctions.root"), 
                                         getOption("khfunctions.logg")))
}

#' @keywords internal
#' @noRd
SettKHBatchDate <- function() {
  format(Sys.time(), "%Y-%m-%d-%H-%M")
}

#' @keywords internal
#' @noRd
FormatSqlBatchdate <- function(batchdate){
  format(strptime(batchdate, "%Y-%m-%d-%H-%M"), "#%Y-%m-%d#")
}