# Set paths
# Need defpath to be distinct from rawPath to not break test and local modus
defpath <- rawPath <- "O:/Prosjekt/FHP/PRODUKSJON"
dbNameFile <- "STYRING/KHELSA.mdb"
dbLogFile <- "STYRING/KHlogg.mdb"

# Uset khelsa and khlogg in the STYRING/test/-folder, for testing access functionality
.useTest <- function(){
  TESTMODUS <<- TRUE
  dbNameFile <<- "STYRING/test/KHELSAtest.mdb"
  dbLogFile <<- "STYRING/test/KHloggtest.mdb"
  source("./R/KHglobs.R")
}

## TEST MODUS
## Change path and dbFile if specified globally else use default
## use in testmodus or local run
if (!exists("runtest")) runtest <- FALSE
testfiles <- NULL
useTest <- FALSE
if (exists("testpath") && exists("testdb")) useTest <- TRUE
if (useTest) {
  defpath <- testpath
  dbNameFile <- testdb
  dbLogFile <- "KHlogg.mdb"
  noLog <- fs::file_exists(file.path(defpaths, dbLogFile))
  if (runtest && isFALSE(noLog)) stop("Finnes ikke KHlogg.mdb fil i ", defpaths)
}

## RUN LOCAL
## This is needed in run_local() function (sets setLocal == TRUE)
if (!exists("setLocal")) setLocal <- FALSE
if ((setLocal)) {
  defpath <- setLocalPath
  dbNameFile <- setDBFile
  dbLogFile <- setLogFile
}