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

rdsFile <- paste(rdsPath, "ELEVUNDER.rds", sep = "/")

dt <- readRDS(rdsFile)
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


## Source File
srcPath <- "F:\\Prosjekter\\Kommunehelsa\\PRODUKSJON\\ORGDATA\\UDIR\\ELEVUNDER\\ORG\\2017"
srcName <- "Elev_2015.csv"
srcFile <- paste(srcPath, srcName, sep = "\\")

DT <- fread(srcFile)
names(DT)
DT[GeografiId == "0101", ]
