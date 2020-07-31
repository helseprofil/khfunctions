## Get tabels from SSB for all the GEO changes
## ----------------------------------------------
## This consist only for GEO that has changes else it isn't in the list

pkg <- c("data.table", "openxlsx", "readxl")
sapply(pkg, require, character.only = TRUE)

##-----------------------
## Function for merging
##-----------------------
geo_new = "ssb_fylke.csv"
geo_chg = "fylke_change_ssb.xlsx"
file_path = "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo"

merge_geo <- function(geo_new, geo_chg, file_path){

  ## Geo changes
  filePath <- normalizePath(file_path, winslash = "/")
  fileChg <- file.path(filePath, geo_chg)

  xlTbl <- readxl::read_excel(fileChg)
  names(xlTbl) <- c("v2020", "v2019")
  setDT(xlTbl)

  xlTbl[, geo2020 := as.numeric(gsub("[^0-9]+", "", v2020))]
  xlTbl[, newName2020 := gsub("\\d+\\D[^\\s]", "", v2020)]
  xlTbl[, geo2019 := as.numeric(gsub("[^0-9]+", "", v2019))]
  xlTbl[, name2019 := gsub("\\d+\\D[^\\s]", "", v2019)]

  setnafill(xlTbl, type = "locf", cols = "geo2020") #only for numeric

  # replace missing string with last observed carried forward (locf)
  while(length(ind <- which(is.na(xlTbl$newName2020))) > 0){
    xlTbl$newName2020[ind] <- xlTbl$newName2020[ind - 1]
  }

  xlTbl[, c("v2020", "v2019") := NULL]

  ## New geo
  fileNew <- file.path(filePath, geo_new)
  dt <- data.table::fread(fileNew, fill = TRUE)
  dt[, setdiff(names(dt), c("code", "name")) := NULL] #keep only Code and Name

  ## Merge
  DT <- xlTbl[dt, on = c(geo2020 = "code")]

 return(DT)

}


fylkeChg <- merge_geo(
  geo_new = "ssb_fylke.csv",
  geo_chg = "fylke_change_ssb.xlsx",
  file_path = "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo"
)

kommuneChg <- merge_geo(
  geo_new = "ssb_kommune.csv",
  geo_chg = "kommune_change_ssb.xlsx",
  file_path = "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo"
)

grunnkretsChg <- merge_geo(
  geo_new = "ssb_grunnkrets.csv",
  geo_chg = "grunnkrets_change_ssb.xlsx",
  file_path = "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo"
)



## -----------------
## Connect to DB
## -----------------

dbPath <- normalizePath("C:\\Users\\ybka\\Folkehelseinstituttet\\Folkehelseprofiler - Data mining\\geo_level", winslash = "/")
dbName <- "geo_ssb.accdb"

## With odbc and DBI
pkg <- c("odbc", "DBI")
sapply(pkg, require, character.only = TRUE)

dbCon <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq="
dbFile <- paste(dbPath, dbName, sep = "/")

cs <- paste0(dbCon, dbFile)
con <- dbConnect(odbc::odbc(), .connection_string = cs)


## Write table to Access
dbWriteTable(con, "tblFylkeChg2020", fylkeChg, batch_rows = 1, overwrite = TRUE)
dbWriteTable(con, "tblKommuneChg2020", kommuneChg, batch_rows = 1, overwrite = TRUE)
dbWriteTable(con, "tblGrunnkretsChg2020", grunnkretsChg, batch_rows = 1, overwrite = TRUE)


## Or append to exisiting table
options(odbc.batch_rows = 1)
dbAppendTable(con, "geo", geo)

dbDisconnect(con)















##----------------------------
##  DRAFT
##----------------------------

xlPath <- "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo"

## ## Last Observation Carried Forward (locf)
## while(length(ind <- which(is.na(xlTbl$geo2020))) > 0){
##   xlTbl$geo2020[ind] <- xlTbl$geo2020[ind - 1]
## }

## setnafill(xlTbl, type = "locf", cols = "geo2020") #only for nummeric

## regex
sub("(\\d+)\\D*", "\\1", "200 - Test")
sub("[^0-9]+", "", "200 - Test")


## Fylke
xlFile <- paste(xlPath, "fylke_change_ssb.xlsx", sep = "\\")
xlTbl <- readxl::read_excel(xlFile)
names(xlTbl) <- c("v2020", "v2019")
setDT(xlTbl)

xlTbl[, geo2020 := as.numeric(gsub("[^0-9]+", "", v2020))]
xlTbl[, newName2020 := gsub("\\d+\\D[^\\s]", "", v2020)]
xlTbl[, geo2019 := as.numeric(gsub("[^0-9]+", "", v2019))]
xlTbl[, name2019 := gsub("\\d+\\D[^\\s]", "", v2019)]

setnafill(xlTbl, type = "locf", cols = "geo2020") #only for numeric

while(length(ind <- which(is.na(xlTbl$newName2020))) > 0){
  xlTbl$newName2020[ind] <- xlTbl$newName2020[ind - 1]
}

xlTbl[, c("v2020", "v2019") := NULL]
xlTbl

f2020file <- paste(xlPath, "ssb_fylke.csv", sep = "\\")
f2020 <- fread(f2020file, fill = TRUE)
f2020[, setdiff(names(f2020), c("code", "name")) := NULL]

tblAlle <- xlTbl[f2020, on = c(geo2020 = "code")]
