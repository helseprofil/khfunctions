
## Convert Geo file from SSB

library(data.table)
library(stringi)
library(openxlsx)
library(fs)
library(here)

getOption("encoding")
## obs! write.csv2 uses NA for missing while fwrite uses NULL
## gives consequence when inporting to ACCESS or Excel
## or uses openxlsx package to create xlsx file

## calling here inside "../dir1/geo" will have root at "../dir1"
getwd() #c:/Users/ybka/Documents/GitFH/khfunction/geo
testdir <- here("geo", "test")

## ---------
## 2020 GEO
## ----------
fylke <- fread(here("geo", "fylke", "ssb_fylke_jan2020.csv"), fill = TRUE)
dim(fylke)
oldName <- c("code", "name")
newName <- c("fylkeCode", "fylkeName")
setnames(fylke, oldName, newName)
fylke[, border := 2020]
openxlsx::write.xlsx(fylke, file.path(testdir, "fylke_jan2020.xlsx"), colNames = TRUE)


## Kommune
kommune <- fread(here("geo", "kommune", "ssb_kommune_jan2020.csv"), fill = TRUE)
## give fylkeCode to kommune
kommune[, fylkeCode := as.numeric(gsub("\\d{2}$", "", code))]
## or stri_extract_first_regex(5644, '\\d{2}')
setcolorder(kommune, c(9, 1:8))
newName <- c("kommuneCode", "kommuneName")
setnames(kommune, oldName, newName)
kommune[, border := 2020]
dim(kommune)
## fwrite(kommune, "ssb_kommune02.csv", sep = ";")
## write.csv2(kommune, "ssb_kommune03.csv", row.names = FALSE, fileEncoding = "native.enc")
write.xlsx(kommune, file.path(testdir, "ssb_kommune_jan2020.xlsx"), colNames = TRUE)

## Bydels
bydel <- fread(here("geo", "bydel", "ssb_bydel_jan2020.csv"), fill = TRUE)
bydel2018 <- fread(here("geo", "bydel", "ssb_bydel_jan2018.csv"), fill = TRUE)
bydel2004 <- fread(here("geo", "bydel", "ssb_bydel_jan2004.csv"), fill = TRUE)

## merge bydel
by2020 <- copy(bydel)
by2020[, year := 2020]
by2018 <- copy(bydel2018)
by2018[, year := 2018]
by2004 <- copy(bydel2004)
setdiff(by2020$code, by2018$code)
setdiff(by2018$code, by2004$code)

by2018[by2020, on = "name"]
merge(by2020, by2018, by = "name", all = TRUE, fill = TRUE)
by2004[by2018, on = "name"]


codeChg <- setdiff(by2020$code, by2018$code)
by2018V2 <- copy(by2018)
by2020V2 <- copy(by2020)
## merge by reference ie. 2018 to 2020 data
by2020V2[by2018V2, on = "name", `:=`(precode = i.code, preyr = i.year)]
## Filter those names that have changed codes
chg2020 <- by2020V2[code %in% codeChg, ]
chg2020[, `:=`(new = paste(code, name, sep = " - "),
               prev = paste(precode, name, sep = " - "))]

colDel <- setdiff(names(chg2020), c("new", "prev"))
chg2020[, (colDel) := NULL]



## Steps for merge_name()
## Create year for the respective dataset eg. bydel2020[,year := 2020]
## starts with olderst 3vs2, 2vs1 to track the changes
## To create the "change" file like those from SSB
## change file has two columns for new and old
## eg. merge code + name and precode name



bydel[, kommuneCode := as.numeric(gsub("\\d{2}$", "", code))]
dim(bydel)
bydel[, .N, by = .(kommuneCode)]
setcolorder(bydel, c(9, 1:8))
newName <- c("bydelCode", "bydelName")
setnames(bydel, oldName, newName)
bydel[, border := 2020]
## fwrite(bydel, "ssb_bydels02.csv", sep = ";")
## write.csv2(bydel, "ssb_bydels03.csv", row.names = FALSE, fileEncoding = "Latin1")
write.xlsx(bydel, file.path(testdir, "ssb_bydel_jan2020.xlsx"), colNames = TRUE)


## Grunnkrets
grunnkrets <- fread(here("geo", "grunnkrets", "ssb_grunnkrets_jan2020.csv"), fill = TRUE)
grunnkrets[, kommuneCode := as.numeric(gsub("\\d{4}$", "", code))]
## grunnkrets[, fylkeCode := as.numeric(gsub("\\d{2}$", "", kommuneCode))]
dim(grunnkrets)
newName <- c("grunnkretsCode", "grunnkretsName")
setnames(grunnkrets, oldName, newName)
## not included in the SSB data
grunnkrets[, kommuneCode := 9999][, grunnkretsName := "Uoppgitt kommune"]
## setcolorder(grunnkrets, c(10, 9, 1:8))
setcolorder(grunnkrets, c(9, 1:8))
grunnkrets[, border := 2020]
## fwrite(grunnkrets, "ssb_grunnkrets02.csv", sep = ";")
## write.csv2(grunnkrets, "ssb_grunnkrets03.csv", row.names = FALSE, fileEncoding = "native.enc")
write.xlsx(grunnkrets, file.path(testdir, "ssb_grunnkrets_jan2020.xlsx"), colNames = TRUE)



## Kommune fra grunnkrets som ikke finnes i Kommune tabell
## -------------------------------------------------------
dk <- unique(grunnkrets$kommuneCode)
kk <- unique(kommune$kommuneCode)
setdiff(dk, kk)
## [1] 1850 2100 5012
nokomm <- c(1850, 2100, 5012)
grunnkrets[kommuneCode %in% nokomm, ]

grunnkrets[name %like% "Tysfjord", ]


## Trouble with encoding in Windows and workaround is to open csv in Excel
## then save.as Excel file before importing the Excel file in Access.


## -----------
## 2019 GEO
## -----------

## digit - how many digits from last to be excluded
## level - granularity level ie. fylke, kommune etc
## year - year the merging is valid for
## oldName - varname to be changed
## newName - variablename to change to
## xlfile - to name the exported Excel file

read_ssb <- function(file, digit, level, year, oldName, newName, xlfile){

  if (missing(oldName)) oldName <- c("code", "name")

  if (missing(digit)) digit <- 2
  outDigit <- paste0("\\d{", digit, "}$")

  if (missing(xlfile)) {
    fileName <- sub("\\..*", "", file)
    xlfile <- paste0(fileName, ".xlsx")
  }

  if (missing(newName)){
    alln <- c('Code','Name')
    newName <- sapply(alln, function(x) paste0(level, x))
  }

  dt <- data.table::fread(file, fill = TRUE)

  upCode <- switch(level,
                   "fylke" = "fylkeCode",
                   "kommune" = "fylkeCode",
                   "bydel" = "kommuneCode",
                   "grunnkrets" = "kommuneCode"
                   )

  dt[, (upCode) := as.numeric(sub(outDigit, "", code))]

  setcolorder(dt, c(9, 1:8))
  setnames(dt, oldName, newName)
  dt[, border := year]

  dupName <- duplicated(names(dt))
  if (sum(dupName) != 0) {
    indCol <- which(dupName == TRUE)
    dt[, (indCol) := NULL]
  }

  openxlsx::write.xlsx(dt, xlfile, colNames = TRUE)

}



## Fylke
read_ssb(file = "ssb_fylke2019.csv",
         level = "fylke",
         digit = 0,
         year = 2019
         )

read_ssb(file = "ssb_kommune2019.csv",
         level = "kommune",
         year = 2019)

read_ssb(file = "ssb_grunnkrets2019.csv",
         level = "grunnkrets",
         year = 2019)

read_ssb(file = "ssb_bydel2019.csv",
         level = "bydel",
         year = 2019)







## Read Access DB
## --------------
dbPath <- normalizePath("C:\\Users\\ybka\\Folkehelseinstituttet\\Folkehelseprofiler - Data mining\\geo_level", winslash = "/")
dbName <- "geo_ssb.accdb"

## With odbc and DBI
pkg <- c("odbc", "DBI")
sapply(pkg, require, character.only = TRUE)

dbCon <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq="
dbFile <- paste(dbPath, dbName, sep = "/")

cs <- paste0(dbCon, dbFile)
con <- dbConnect(odbc::odbc(), .connection_string = cs)

dbGetInfo(con)
dbListTables(con)
dbListTables(con, table_name = "tbl%") #all that start with tbl
dbListTables(con, table_name = "%2020") #all end with 2020

geoBasic <- dbReadTable(con, "tblFylke2020")

## Encoding(geoBasic$fylkeName)
Encoding(geoBasic$fylkeName) <- "Latin1"

tblNames <- dbListTables(con, table_name = "tblfylke%")
## tblNames <- c("tblFylke2019", "tblFylke2020")

## list column names
dbListFields(con, tblNames[1])
res <- dbSendQuery(con, "SELECT code, name FROM tblFylke2020")
df <- dbFetch(res)
df




tblList <- lapply(tblNames, function(x) dbReadTable(con, x))
names(tblList) = tblNames
lapply(tblList, data.table::setDT)

## tblFylke2020 <- tblList[[2]]
## tblFylke2019 <- tblList[[1]]

tblList[[1]]$border <- 2019
tblList[[2]]$border <- 2020

tbl2020 <- tblList[[2]]
tbl2019 <- tblList[[1]]

codeList <- tbl2020[["code"]]
tbl2019b <- tbl2019[!(code %in% codeList), ]

tblAll <- data.table::rbindlist(list(tbl2020, tbl2019b))

keepNames <- c("code", "name", "border")
delNames <- base::setdiff(names(tblList[[1]]), keepNames)
## tblFylke[, ..keepNames]

## set(tblFylke,, keepNames, NULL)
tblAll[, (delNames) := NULL] 
tblAll[, code := as.numeric(code)]
tblAll[, granularity := "fylke"]

Encoding(tblAll$name) <- "latin1"




## granularity = "fylke"
## conn = con
## year = c(2019, 2020)

## Function to join tables
## ---------------------------
## connection and database names should be specified first before running the function

dbPath <- normalizePath("C:\\Users\\ybka\\Folkehelseinstituttet\\Folkehelseprofiler - Data mining\\geo_level", winslash = "/")
dbName <- "geo_ssb.accdb"

## With odbc and DBI
pkg <- c("odbc", "DBI")
sapply(pkg, require, character.only = TRUE)

dbCon <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq="
dbFile <- paste(dbPath, dbName, sep = "/")

cs <- paste0(dbCon, dbFile)
con <- dbConnect(odbc::odbc(), .connection_string = cs)


read_tbl <- function(granularity, conn = con, year = c(2019, 2020)){

  tbl <- paste0("tbl", granularity, "%")
  tblNames <- DBI::dbListTables(conn, table_name = tbl)
  
  tblList <- lapply(tblNames, function(x) DBI::dbReadTable(conn, x))
  names(tblList) = tblNames
  lapply(tblList, data.table::setDT)

  tblList[[1]]$border <- year[1]
  tblList[[2]]$border <- year[2]

  tbl2020 <- tblList[[2]]
  tbl2019 <- tblList[[1]]

  codeList <- tbl2020[["code"]]
  tbl2019b <- tbl2019[!(code %in% codeList), ]

  tblAll <- data.table::rbindlist(list(tbl2020, tbl2019b))

  keepNames <- c("code", "name", "border")
  delNames <- base::setdiff(names(tblList[[1]]), keepNames)
  ## tblFylke[, ..keepNames]

  ## set(tblFylke,, keepNames, NULL)
  tblAll[, (delNames) := NULL] 
  tblAll[, code := as.numeric(code)]
  tblAll[, granularity := granularity]
  
  Encoding(tblAll$name) <- "latin1"
  
  return(tblAll)

}


## OBS! Grunnkrets csv file has problem with encoding when exported to Access!
## Alternative is to save the csv file to .xlsx before importing to Access

norge <- data.table::data.table(code = 0, name = "norge", border = 2020, granularity = "norge")
fylke <- read_tbl("fylke")
kommune <- read_tbl("kommune")
grunnkrets <- read_tbl("grunnkrets")
bydel <- read_tbl("bydel")

geo <- data.table::rbindlist(list(norge, fylke, kommune, grunnkrets, bydel))

geo[, .N, by = granularity]

## Create all granularity level
## ----------------------------

geo[granularity == "fylke", fylke := code]

geo[granularity == "kommune", kommune := code]
geo[granularity == "kommune", fylke := gsub("\\d{2}$", "", kommune)]

geo[granularity == "bydel", bydel := code]
geo[granularity == "bydel", fylke := gsub("\\d{4}$", "", code)]
geo[granularity == "bydel", kommune := gsub("\\d{2}$", "", code)]

geo[granularity == "grunnkrets", grunnkrets := code]
geo[granularity == "grunnkrets", fylke := gsub("\\d{6}$", "", code)]
geo[granularity == "grunnkrets", kommune := gsub("\\d{4}$", "", code)]


## Write table to Access
dbWriteTable(con, "tblGeo", geo, batch_rows = 1, overwrite = TRUE)

## Or append to exisiting table
options(odbc.batch_rows = 1)
dbAppendTable(con, "geo", geo)

dbDisconnect(con)



### -------------
## DRAFT
##---------------
## make sample of 5 of each granularity
dd <- geo[sample(.N), c(.SD[1:5],.N), by=granularity]

dd[granularity == "fylke", fylke := code]

dd[granularity == "kommune", kommune := code]
dd[granularity == "kommune", fylke := gsub("\\d{2}$", "", kommune)]

dd[granularity == "bydel", bydel := code]
dd[granularity == "bydel", fylke := gsub("\\d{4}$", "", code)]
dd[granularity == "bydel", kommune := gsub("\\d{2}$", "", code)]

## ## - Doesn't work with magrittr or chaining
## dd[granularity == "grunnkrets", grunnkrets := code] %>%
##   .[, fylke := gsub("\\d{6}$", "", code)] %>%
##   .[, kommune := gsub("\\d{4}$", "", code)]

## dd[granularity == "grunnkrets", grunnkrets := code][
##   , fylke := gsub("\\d{6}$", "", code)][
##   , kommune := gsub("\\d{4}$", "", code)]

dd[granularity == "grunnkrets", grunnkrets := code]
dd[granularity == "grunnkrets", fylke := gsub("\\d{6}$", "", code)]
dd[granularity == "grunnkrets", kommune := gsub("\\d{4}$", "", code)]


library(stringi)
dd[granularity == "kommune", kommune := code][, fylke := stri_extract_last_regex(code, "\\d{2}")]
