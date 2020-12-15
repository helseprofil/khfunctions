get_local_file <- function(){

  ## Name of new file
  sysDate <- format(Sys.time(), "%Y%m%d_%H%M")
  dbFile <- paste0("KHELSA_", sysDate, ".mdb")
  
  ## Destination to copy to
  bruker <- Sys.info()[["user"]]
  dbDir <- file.path("c:/Users", bruker, "DB_helseprofil")
  
  if (isFALSE(fs::dir_exists(dbDir)))
    fs::dir_create(dbDir)

  newFile <- file.path(dbDir, dbFile)
  
  ## Get the most recent file in archive folder
  dbAll <- list.files(dbDir)
  dbLast <- sort(dbAll[grepl("^KHELSA_\\d+_\\d+.mdb$",dbAll)],decreasing=TRUE)[1]

  en <- parent.frame()
  
  if (is.null(en$DBFile) && is.null(en$copy)){
    message("\nLokal mappen: ", dbDir)
    message("Kopieres som: ", dbFile)
    message("Nyeste lokalfil er: ", dbLast, "\n")
    cp <- as.integer(readline(prompt = "Vil du kopiere filen? Svar 0=Nei 1=Ja "))
  } else {
    cp = en$copy
  }

  if (is.na(cp))
    stop("\n OPS! Ditt svar er ikke registeret. Run funksjonen linjer per linjer")
  
  return(list(dbDir = dbDir,
              dbFile = dbFile,
              dbLast = dbLast,
              cp = cp))
  
}


## localPath : Path to copy files KHELSA and KHLogg. Default is c:/Users/username/DB_helseprofil
## DBFile : Name of Access file if other than KHELSA.mdb
## copy = FALSE will use the existing files in the localPath, else TRUE. NULL will prompt question

run_local <- function(localPath = NULL,
                      DBFile = NULL,
                      copy = NULL){

  ## Switch modus on
  setLocal <<- TRUE
  
  ## Original files
  filePath <-  "f:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/STYRING"
  oriDBFile <- "KHELSA.mdb"
  oriLogFile <- "KHlogg_template.mdb"

  DB <- get_local_file()

  if (isFALSE(copy))
    message("\nLokalfil: ", DB$dbLast)

  if (is.null(localPath)){
    setLocalPath <<- DB$dbDir
  } else {
    setLocalPath <<- localPath
  } 

  if (DB$cp){
    setDBFile <<- DB$dbFile
  } else {
    setDBFile <<- DB$dbLast
  }
  
  if (!is.null(DBFile))
    setDBFile <<- DBFile
  
  orgFile <- file.path(filePath, oriDBFile)
  orgLog <- file.path(filePath, oriLogFile)

  ## LogFile names can't be changed coz of connection to KHELSA.mdb
  setLogFile <<- "KHlogg.mdb" #standard is STYRING/KHlogg.md
  cpFile <- file.path(setLocalPath, setDBFile)
  cpLog <- file.path(setLocalPath, setLogFile)

  if (is.null(DBFile) && DB$cp){
    cat("Kopierer DB filen: \nFra: ", orgFile, "\nTil: ", cpFile, "\n... vent ...")
    fs::file_copy(orgFile, cpFile, overwrite = TRUE)
    cat("....\n")
    fs::file_copy(orgLog, cpLog, overwrite = TRUE)
    cat("Ferdig!\n")
  }
  
}

