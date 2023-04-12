# Set paths

defpaths <- c(
  "F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON",
  ## "F:/Prosjekter/Kommunehelsa/PRODUKSJON",
  "F:/Prosjekter/Kommunehelsa/PRODUKSJON/DEVELOP",
  "F:/Prosjekter/Kommunehelsa/Data og databehandling/kbDEV",
  "J:/FHI/PRODUKSJON",
  "J:/kbDEV"
)

rawPath <- "F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON"
dbNameFile <- "STYRING/KHELSA.mdb"
dbLogFile <- "STYRING/KHlogg.mdb"

## TEST MODUS
## Change path and dbFile if specified globally else use default
## use in testmodus or local run
if (!exists("runtest")) runtest <- FALSE
testfiles <- NULL
useTest <- FALSE
if (exists("testpath") && exists("testdb")) useTest <- TRUE
if (useTest) {
  defpaths <- testpath
  dbNameFile <- testdb
  dbLogFile <- "KHlogg.mdb"
  noLog <- fs::file_exists(file.path(defpaths, dbLogFile))
  if (runtest && isFALSE(noLog)) stop("Finnes ikke KHlogg.mdb fil i ", defpaths)
}

## RUN LOCAL
## This is needed in run_local function
if (!exists("setLocal")) setLocal <- FALSE
if ((setLocal)) {
  defpaths <- setLocalPath
  dbNameFile <- setDBFile
  dbLogFile <- setLogFile
}
