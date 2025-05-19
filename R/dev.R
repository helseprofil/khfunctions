#' @title uselocal
#' for development, sources all files in the R folder
uselocal <- function(test = FALSE, debug = FALSE){
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  # show_functions <<- debug
  # show_arguments <<- debug
  source("./R/KHmisc.R", encoding = "latin1")
  KH_options()
  if(test) .useTest()
  rfiles <- paste0("./R/", list.files("R", pattern = "\\.R"))
  rfiles <- grep("KHmisc.R|KHsetup.R", rfiles, value = T, invert = T)
  for(i in rfiles) source(i, encoding = "latin1")
  cat("\nLoaded local functions")
}

# Use khelsa and khlogg in the STYRING/test/-folder, for testing access functionality
#' .useTest (VL)
#'
#' @param db path to test db file
#' @param logg path to test log file
.useTest <- function(db = NULL, logg = NULL){
  RODBC::odbcCloseAll()
  if(is.null(db)) db <- getOption("khfunctions.test.db")
  if(is.null(logg)) logg <- getOption("khfunctions.test.logg")
  TESTMODUS <<- TRUE
  options(khfunctions.db = db)
  options(khfunctions.logg = logg)
}

#' .SetKubeParameters (VL)
#'
#' for testing LagKUBE, store all the parameters to global env
#' @param KUBEid name of kube to test on
.SetKubeParameters <- function(KUBEid){
  KUBEid <<- KUBEid
  year <<- NULL
  dumps <<- list()
  assign("write", FALSE, envir = .GlobalEnv)
  geonaboprikk <<- TRUE
}

.SetFilgruppeParameters <- function(filgruppenavn){
  gruppe <<- filgruppenavn
  versjonert <<- TRUE
  assign("write", FALSE, envir = .GlobalEnv)
  dumps <<-  list()
}

fg_get_all_read_args <- function(globs = get_global_parameters()){
  batchdate <- SettKHBatchDate()
  validdates <- paste0("VERSJONFRA <=", FormatSqlBatchdate(batchdate), " AND VERSJONTIL >", FormatSqlBatchdate(batchdate))
  orginnleskobl <- data.table::setDT(RODBC::sqlQuery(globs$dbh, query = paste0("SELECT * FROM ORGINNLESkobl"), as.is = TRUE))
  originalfiler <- data.table::setDT(RODBC::sqlQuery(globs$dbh, query = paste0("SELECT * FROM ORIGINALFILER WHERE ", gsub("VERSJON", "IBRUK", validdates)), as.is = TRUE))
  innlesing <- data.table::setDT(RODBC::sqlQuery(globs$dbh, query = paste0("SELECT * FROM INNLESING WHERE ", validdates), as.is = TRUE))
  
  outcols <- c("KOBLID", "FILID", "FILNAVN", "FORMAT", "DEFAAR", setdiff(names(innlesing), "KOMMENTAR"))
  out <- collapse::join(orginnleskobl, originalfiler, how = "i", on = "FILID", overid = 2, verbose = 0)
  out <- collapse::join(out, innlesing, how = "i", on = c("FILGRUPPE", "DELID"), overid = 2, verbose = 0)
  out <- out[, .SD, .SDcols = outcols]
  out[, let(FILNAVN = gsub("\\\\", "/", FILNAVN))]
  out[, let(filepath = file.path(getOption("khfunctions.root"), FILNAVN), FORMAT = toupper(FORMAT))]
  out[AAR == "<$y>", let(AAR = paste0("<", DEFAAR, ">"))]
  return(out)
}
