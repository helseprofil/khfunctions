## CHECK files that has TESTING = 1
## --------------------------------

pkg <- c("data.table", "DBI", "odbc")
sapply(pkg, require, character.only = TRUE)

## DB path
dbPath <- normalizePath("C:\\enc\\DBtest\\STYRING", "/")
dbName <- "KHELSA_dev.accdb"
dbCon <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq="
dbFile <- paste(dbPath, dbName, sep = "/")
cs <- paste0(dbCon, dbFile)
con <- dbConnect(odbc::odbc(), .connection_string = cs)


## RDS file Path
rdsPath <- normalizePath("C:\\enc\\DBtest\\PRODUKTER\\MELLOMPROD\\R\\STABLAORG\\NYESTE", "/")
## Source Path
srcPath <- normalizePath("F:\\Prosjekter\\Kommunehelsa\\PRODUKSJON\\ORGDATA\\UDIR\\ELEVUNDER\\ORG\\2017", "/")

show_rds <- function(rdsPath, srcPath, rdsFile, srcFile){

  fileName <- file.path(rdsPath, rdsFile)
  dt <- readRDS(fileName)
  dtNames <- names(dt)
  data.table::setDT(dt)
  rowSelect <- sample(1:nrow(dt), 5)
  subdt <- dt[rowSelect, ]
  geo <- unique(dt$GEOniv)

  ## source file
  fileExt <- tools::file_ext(srcFile)
  srcName <- file.path(srcPath, srcFile)
  srcDT <- switch(fileExt,
                  "csv" = {data.table::fread(srcName)},
                  "xlsx" = {readxl::read_excel(srcName)},
                  {readxl::read_excel(srcName)} #default
                  )

  list(colnames = dtNames, geo = geo, dt = dt, src = srcDT)
}

filNames <- c("ELEVUNDER.rds", "INNTULIKHET.rds")
srcName <- "Elev_2015.csv"

## ELEVUNDER
DT <- show_rds(rdsPath,
               srcPath,
               rdsFile = "ELEVUNDER.rds",
               srcFile = "Elev_2015.csv")

dt <- DT$dt
setDT(dt)
names(dt)
dt

## summary(dt)
dname <- names(dt)
for (j in dname) {
  print(j)
  print(dt[, .N, keyby = j])
 }

dt[GEO == "0101", ]
dt[GEO == "0", ]
dt[GEO == "1014", ]
dt[GEO == "1804", ] #Bodo

## Source File
srcPath <- "F:\\Prosjekter\\Kommunehelsa\\PRODUKSJON\\ORGDATA\\UDIR\\ELEVUNDER\\ORG\\2017"
srcName <- "Elev_2015.csv"
srcFile <- paste(srcPath, srcName, sep = "\\")

DT <- fread(srcFile)
names(DT)
DT[GeografiId == "1014"]
DT[GeografiId == "0101", ]
DT[GeografiId == "1804", ] #Bodø

## Spørsmål - Hvordan får man tallet til TOTANT


## - INNTULIKHET
DT <- show_rds(rdsPath, "INNTULIKHET.rds")
