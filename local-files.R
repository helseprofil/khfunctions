## localPath : Path to copy files KHELSA and KHLogg. Default is c:/Users/username/DB_helseprofil
## DBFile : Name of Access file if other than KHELSA.mdb
## copy = FALSE will use the existing files in the localPath

run_local <- function(localPath = NULL,
                      DBFile = NULL,
                      copy = TRUE){


  ## Switch modus on
  setLocal <<- TRUE
  
  ## Original files
  filePath <-  "f:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/STYRING"
  oriDBFile <- "KHELSA.mdb"
  oriLogFile <- "KHlogg_template.mdb"

  sysDate <- format(Sys.Date(), "%Y%m%d")
  nyFile <- paste0("KHELSA_", sysDate, ".mdb")
   
  ## Destination to copy to
  bruker <- Sys.info()[["user"]]
  dbDir <- file.path("c:/Users", bruker, "DB_helseprofil")

  if (isFALSE(fs::dir_exists(dbDir)))
    fs::dir_create(dbDir)
  
  
  if (is.null(localPath)){
    setLocalPath <<- dbDir
  } else {
    setLocalPath <<- localPath
  } 

  if (is.null(DBFile)){
    setDBFile <<- nyFile
  } else {
    setDBFile <<- DBFile
  }

  orgFile <- file.path(filePath, oriDBFile)
  orgLog <- file.path(filePath, oriLogFile)

  ## LogFile names can't be changed coz of connection to KHELSA.mdb
  setLogFile <<- "KHlogg.mdb"
  cpFile <- file.path(setLocalPath, setDBFile)
  cpLog <- file.path(setLocalPath, setLogFile)

  if (copy){
    cat("Kopierer DB filen: \nFra: ", orgFile, "\nTil: ", cpFile, "\n... vent ...")
    fs::file_copy(orgFile, cpFile, overwrite = TRUE)
    cat("....\n")
    fs::file_copy(orgLog, cpLog, overwrite = TRUE)
    cat("Ferdig!\n")
  }
  
}

