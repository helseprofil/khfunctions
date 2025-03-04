# Set paths
# Need defpath to be distinct from rawPath to not break test and local modus
# defpath <- rawPath <- getOption("khfunctions.root")
# dbNameFile <- file.path(getOption("khfunctions.db"), getOption("khfunctions.db"))
# dbLogFile <- file.path(getOption("khfunctions.db"), getOption("khfunctions.logg"))

## TEST MODUS
## Change path and dbFile if specified globally else use default
## use in testmodus or local run
# if (!exists("runtest")) runtest <- FALSE
# testfiles <- NULL
# useTest <- FALSE
# if (exists("testpath") && exists("testdb")) useTest <- TRUE
# if (useTest) {
#   defpath <- testpath
#   dbNameFile <- testdb
#   dbLogFile <- "KHlogg.mdb"
#   noLog <- fs::file_exists(file.path(defpaths, dbLogFile))
#   if (runtest && isFALSE(noLog)) stop("Finnes ikke KHlogg.mdb fil i ", defpaths)
# }

## RUN LOCAL
## This is needed in run_local() function (sets setLocal == TRUE)
# if (!exists("setLocal")) setLocal <- FALSE
# if ((setLocal)) {
#   defpath <- setLocalPath
#   dbNameFile <- setDBFile
#   dbLogFile <- setLogFile
# }