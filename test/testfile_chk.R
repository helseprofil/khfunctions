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

show_rds <- function(rdsPath, srcPath, rdsFile, srcFile, rds.only = FALSE){

  fileName <- file.path(rdsPath, rdsFile)
  dt <- readRDS(fileName)
  dtNames <- names(dt)
  data.table::setDT(dt)
  rowSelect <- sample(1:nrow(dt), 5)
  subdt <- dt[rowSelect, ]
  geo <- unique(dt$GEOniv)

  srcDT = NULL

  if(rds.only == 0) {
    ## source file
    fileExt <- tools::file_ext(srcFile)
    srcName <- file.path(srcPath, srcFile)
    srcDT <- switch(fileExt,
                    "csv" = {data.table::fread(srcName)},
                    "xlsx" = {readxl::read_excel(srcName)},
                    {readxl::read_excel(srcName)} #default
                    )
  }

  list(colnames = dtNames, geo = geo, dt = dt, src = srcDT)
}

filNames <- c("ELEVUNDER.rds", "INNTULIKHET.rds")


## ELEVUNDER
## Source Path
srcPath <- normalizePath("F:\\Prosjekter\\Kommunehelsa\\PRODUKSJON\\ORGDATA\\UDIR\\ELEVUNDER\\ORG\\2017", "/")
srcName <- "Elev_2015.csv"

show_rds(rdsPath, rdsFile = "ELEVUNDER.rds", rds.only = TRUE)
elvDT <- show_rds(rdsPath,
               srcPath,
               rdsFile = "ELEVUNDER.rds",
               srcFile = "Elev_2015.csv")

dt <- elvDT$dt
setDT(dt)
names(dt)
dt
dd <- elvDT$src

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
DT <- elvDT$src
names(DT)
DT[GeografiId == "1014"]
DT[GeografiId == "0101", ]
DT[GeografiId == "1804", ] #Bodø

## Spørsmål - Hvordan får man tallet til TOTANT


## - INNTULIKHET
## --------------
srcPath = "F:\\Prosjekter\\Kommunehelsa\\PRODUKSJON\\ORGDATA\\SSB\\INNTULIKHET\\ORG\\P90P10\\2020"

DT <- show_rds(rdsPath,
               srcPath,
               "INNTULIKHET.rds",
               "18p9010byd_2009.csv")
DT
show_rds(rdsPath, rdsFile = "INNTULIKHET.rds", rds.only = TRUE)



## source
src <- DT$src
head(src)
src[, P9010 := sub('^(\\")(\\d\\,\\d)(\\")', "\\2", P9010)]
src[, P9010 := sub("\\,", "\\.", P9010)]
src


sub("\\d+$", "\\1", "1234ab56")
sub("\\d+", "\\1", "1234ab56")
sub("\\D+", "\\1", "1234ab56")
sub("\\D+\\d+", "\\1", "1234ab56")
sub("\\D+", "_", "1234ab56")
