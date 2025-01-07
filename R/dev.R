#' @title uselocal
#' for development, sources all files in the R folder
uselocal <- function(test = FALSE, 
                     debug = FALSE){
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  show_functions <<- debug
  show_arguments <<- debug
  source("./R/KHmisc.R", encoding = "latin1")
  KH_options()
  if(test) .useTest()
  source("./R/KHglobs.R", encoding = "latin1")
  rfiles <- paste0("./R/", list.files("R", pattern = "\\.R"))
  rfiles <- grep("KHmisc.R|KHglobs.R|KHsetup.R", rfiles, value = T, invert = T)
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
  versjonert <<- FALSE
  csvcopy <<- FALSE
  dumps <<- list()
  assign("write", FALSE, envir = .GlobalEnv)
  geonaboprikk <<- TRUE
}
