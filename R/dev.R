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
  versjonert <<- FALSE
  csvcopy <<- FALSE
  dumps <<- list()
  assign("write", FALSE, envir = .GlobalEnv)
  geonaboprikk <<- TRUE
}

# 
# ## TEST
# fasit <- FinnDesign(FIL = BUFFER$HKR, FGP = parameters$fileinformation$HKR, globs = globs)
# ny <- find_filedesign(BUFFER$HKR, fileparameters = parameters$fileinformation$HKR, globs = globs)
# for(del in Deler){
#   print(sum(all.equal(fasit$Part[[del]], ny$Part[[del]], check.attributes = F),
#             all.equal(fasit$SKombs[[del]], ny$SKombs[[del]], check.attributes = F)))
# }
# library(microbenchmark)
# microbenchmark(old = FinnDesign(BUFFER$BEF_GKa, parameters$fileinformation$BEF_GKa, globs = globs),
#                new = find_filedesign(BUFFER$BEF_GKa,  parameters$fileinformation$BEF_GKa, globs),
#                times = 5)
