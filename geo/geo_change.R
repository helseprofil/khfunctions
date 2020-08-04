## Get tabels from SSB for all the GEO changes
## ----------------------------------------------
## This consist only for GEO that has changes else it isn't in the list

pkg <- c("data.table", "openxlsx", "readxl", "fs")
sapply(pkg, require, character.only = TRUE)

##-----------------------
## Function for merging
##-----------------------
## geo_new - File names for new geo from SSB in csv
## geo_chg - Copy paste from ssb "endringer" to Excel
## year - Valid year for geo code
## type - fylke, kommune, grunnkrets...

merge_geo <- function(geo_new, geo_chg, year, type, file_path = NULL){
  ## Files
  if (!is.null(file_path)){
    filePath <- normalizePath(file_path, winslash = "/")

    fileNew <- file.path(filePath, geo_new) #geo New
    fileChg <- file.path(filePath, geo_chg) #geo Change
} else {
    fileNew <- geo_new
    fileChg <- geo_chg
  }


  ## Geo change
  xlTbl <- readxl::read_excel(fileChg)
  names(xlTbl) <- c("new", "old")
  setDT(xlTbl)

  expNum <- switch(type,
                    "kommune" = "[^0-9]+",
                    "grunnkrets" = "\\s.*",
                    "[^0-9]+")

  expName <- switch(type,
                    "kommune" = "\\d+\\D[^\\s]",
                    "grunnkrets" = "[^A-Za-z]")

  ## Extract code and name separately
  xlTbl[, curr := as.numeric(gsub(expNum, "", new))]
  xlTbl[, currName := gsub(expName, "", new)]
  xlTbl[, prev := as.numeric(gsub(expNum, "", old))]
  xlTbl[, prevName := gsub(expName, "", old)]
  xlTbl[, year := year, ]

  ## replace missing string with last observed carried forward (locf)
  setnafill(xlTbl, type = "locf", cols = "curr") #only for numeric
  ## For string
  while(length(ind <- which(is.na(xlTbl$currName))) > 0){
    xlTbl$currName[ind] <- xlTbl$currName[ind - 1]
  }

  xlTbl[, c("new", "old") := NULL]
  mainCols <- c("code", "name")

  ## New geo
  dt <- data.table::fread(fileNew, fill = TRUE)
  dt[, setdiff(names(dt), mainCols) := NULL] #keep only Code and Name
  
  ## Merge
  DT <- xlTbl[dt, on = c(curr = "code")]
  DT[, currName := NULL]
  setnames(DT, "curr", "code")
  otherCols <- setdiff(names(DT), mainCols)
  setcolorder(DT, c(mainCols, otherCols))

  list(DT = DT, xl = xlTbl, fileChg = fileChg, fileNew = fileNew)

}


## --------------------------------------
## Get all old geo codes for Kommune
## --------------------------------------
file_path = "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo\\kommune"

## Select files
select_ssb <- function(grep.file, grep.change, file.path){
  files <- fs::dir_ls(file.path)
  filInd <- grep(grep.file, files, ignore.case = TRUE)
  chgInd <- grep(grep.change, files[filInd])
  chgFil <- files[filInd][chgInd]
  codeList <- files[filInd][-chgInd]
  list(chgfile = chgFil, allfile = codeList)
}

## Find geo codes that have changed more than once
## -----------------------------------------------
## Check if current codes in previous year is.element in previous codes of current year
## showing that the codes have changed again since previous change

check_element <- function(filenew, filepre){

  DT <- filenew[["DT"]]
  dt <- filepre[["DT"]]
  vecNew <- DT$prev
  vecOld <- dt[!is.na(year), code]
  chg <- is.element(vecOld, vecNew)
  sumChg <- sum(chg)
  vecChg <- vecOld[chg]

  list(total = sumChg, chg = vecChg)
}


## Merge changes of geo codes from previous change ie. code that have changed in 2018
## and have new changes in 2020. Then get the previous codes in 2018 from previous code columns
##
## 30240317 (in 2020) from 2190317 (2019) but was 2190314 (2018)

## raw - if using an exsiting join_change table
join_change <- function(newfile, prevfile, raw = TRUE){
  
  if (raw){
    elMix <- check_element(newfile, prevfile)
  } else {
    vecNew <- newfile[["DT"]]$prev
    indelm <- is.element(vecNew, prevfile[["code"]])
    elMix <- data.table(chg = vecNew[indelm])
  }

  
  dtNew <- newfile[["DT"]]
  altNew <- dtNew[prev  %in% elMix$chg, ]

  if (raw){
    dtPre <- prevfile[["DT"]]
    altPre <- dtPre[code  %in% elMix$chg, ]
  } else {
   altPre <- prevfile
  }
  
  allFile <- merge(altNew, altPre,
                   by.x = "prev", by.y = "code",
                   all = TRUE)

  keepCols <- c("code", "name.x", "prev.y", "prevName.y", "year.y")
  newName <- c("name", "prev", "prevName", "year")

  allFile[, setdiff(names(allFile), keepCols) := NULL]
  setnames(allFile, keepCols[-1], newName)

  return(allFile[])
}


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



## -----------------
## Kommune endringer
## -----------------

kom2017 <- select_ssb(grep.file = "jan2017",
                      grep.change = "change",
                      file.path = file_path
                      )

komChg2017 <- merge_geo(
  geo_new = kom2017$allfile,
  geo_chg = kom2017$chgfile,
  year = 2017,
  type = "kommune"
)

kom2018 <- select_ssb(grep.file = "jan2018",
                      grep.change = "change",
                      file.path = file_path
                      )

komChg2018 <- merge_geo(
  geo_new = kom2018$allfile,
  geo_chg = kom2018$chgfile,
  year = 2018,
  type = "kommune"
)

kom2019 <- select_ssb(grep.file = "jan2019",
                      grep.change = "change",
                      file.path = file_path
                      )

komChg2019 <- merge_geo(
  geo_new = kom2019$allfile,
  geo_chg = kom2019$chgfile,
  year = 2019,
  type = "kommune"
)


kom2020 <- select_ssb(grep.file = "jan2020",
                      grep.change = "change",
                      file.path = file_path
                      )

komChg2020 <- merge_geo(
  geo_new = kom2020$allfile,
  geo_chg = kom2020$chgfile,
  year = 2020,
  type = "kommune"
)

## Find geo codes that have changed more than once
## -----------------------------------------------
## Check if current codes in previous year is.element in previous codes of current year

chgInd2018 <- is.element(komChg2018$prev, komChg2017$curr[!is.na(komChg2016$year)])
sum(chgInd2018)
komChg2018$prev[chgInd2018]

chgInd2019 <- is.element(komChg2019$prev, grunnkretsChg2018$curr[!is.na(grunnkretsChg2018$year)])
sum(chgInd2019)
grunnkretsChg2019$prev[chgInd2019]

chgInd2020 <- is.element(grunnkretsChg2020$prev, grunnkretsChg2019$curr[!is.na(grunnkretsChg2019$year)])
sum(chgInd2020)
grunnkretsChg2020$prev[chgInd2020]


## Merge all the changes files when previous code changes have been handled








## --------------------------------------
## Get all old geo codes for Grunnkrets
## --------------------------------------
file_path = "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo\\grunnkrets"

## Grunnkrets endringer
grunnkrets2016 <- select_ssb(grep.file = "jan2016",
                      grep.change = "change",
                      file.path = file_path
                      )

grunnkretsChg2016 <- merge_geo(
  geo_new = grunnkrets2016$allfile,
  geo_chg = grunnkrets2016$chgfile,
  year = 2016,
  type = "grunnkrets"
)



grunnkrets2017 <- select_ssb(grep.file = "jan2017",
                      grep.change = "change",
                      file.path = file_path
                      )

grunnkretsChg2017 <- merge_geo(
  geo_new = grunnkrets2017$allfile,
  geo_chg = grunnkrets2017$chgfile,
  year = 2017,
  type = "grunnkrets"
)

grunnkrets2018 <- select_ssb(grep.file = "jan2018",
                      grep.change = "change",
                      file.path = file_path
                      )

grunnkretsChg2018 <- merge_geo(
  geo_new = grunnkrets2018$allfile,
  geo_chg = grunnkrets2018$chgfile,
  year = 2018,
  type = "grunnkrets"
)

grunnkrets2019 <- select_ssb(grep.file = "jan2019",
                      grep.change = "change",
                      file.path = file_path
                      )

grunnkretsChg2019 <- merge_geo(
  geo_new = grunnkrets2019$allfile,
  geo_chg = grunnkrets2019$chgfile,
  year = 2019,
  type = "grunnkrets"
)


grunnkrets2020 <- select_ssb(grep.file = "jan2020",
                      grep.change = "change",
                      file.path = file_path
                      )

grunnkretsChg2020 <- merge_geo(
  geo_new = grunnkrets2020$allfile,
  geo_chg = grunnkrets2020$chgfile,
  year = 2020,
  type = "grunnkrets"
)



## Find geo codes that have changed more than once
## -----------------------------------------------
## Check if current codes in previous year is.element in previous codes of current year
## showing that the codes have changed again since previous change

elem2017 <- check_element(grunnkretsChg2017, grunnkretsChg2016)
elem2018 <- check_element(grunnkretsChg2018, grunnkretsChg2017)
elem2019 <- check_element(grunnkretsChg2019, grunnkretsChg2018)
elem2020 <- check_element(grunnkretsChg2020, grunnkretsChg2019)

## check all previous changes
chg2018_2017 <- join_change(grunnkretsChg2018, grunnkretsChg2017)
chg2019_2017 <- join_change(grunnkretsChg2019, grunnkretsChg2017)
chg2019_2018 <- join_change(grunnkretsChg2019, grunnkretsChg2018)

## add all the changes to the current geo ie. 2020
chg2020_1817 <- join_change(grunnkretsChg2020, chg2018_2017, raw = FALSE)
chg2020_2016 <- join_change(grunnkretsChg2020, grunnkretsChg2016)
chg2020_2017 <- join_change(grunnkretsChg2020, grunnkretsChg2017)
chg2020_2018 <- join_change(grunnkretsChg2020, grunnkretsChg2018)
chg2020_2019 <- join_change(grunnkretsChg2020, grunnkretsChg2019)

## Merge all changes datasets ie. Excel files and changes from previous current i.e alt2019_2020
## This file only consist grunnkrets that have changed codes
grunDT <- rbindlist(list(
  chg2020_1817,
  chg2020_2016,
  chg2020_2017,
  chg2020_2018,
  chg2020_2019
))

## Merge to current Geo ie. 2020
grunGEO <- rbindlist(list(grunnkretsChg2020$DT, grunDT), fill = TRUE)
setkeyv(grunGEO, "code")

grunGEO[duplicated(code) | duplicated(code, fromLast = TRUE), ]
dbWriteTable(con, "tblGrunnkretsChange", grunGEO, batch_rows = 1, overwrite = TRUE)














### ------

fylkeChg <- merge_geo(
  geo_new = "ssb_fylke.csv",
  geo_chg = "fylke_change_ssb.xlsx",
  file_path = "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo"
)

grunnkretsmuneChg <- merge_geo(
  geo_new = "ssb_kommune.csv",
  geo_chg = "kommune_change_ssb.xlsx",
  file_path = "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo"
)

grunnkretsChg <- merge_geo(
  geo_new = "ssb_grunnkrets.csv",
  geo_chg = "grunnkrets_change_ssb.xlsx",
  file_path = "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo"
)

dt <- read_excel(paste(file_path, geo_chg, sep = "/"))

## Merge all to a table as lookup-tbl
## -----------------------------------

tblComplete <- rbindlist(list(fylkeChg, kommuneChg, grunnkretsChg))
keepCols <- c("curr", "prev", "name")
tblComplete[, setdiff(names(tblComplete), keepCols) := NULL]

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
dbWriteTable(con, "tblGeoChange", tblComplete, batch_rows = 1, overwrite = TRUE)

## Or append to exisiting table
options(odbc.batch_rows = 1)
dbAppendTable(con, "geo", geo)

dbDisconnect(con)




#----------------------------
##  DRAFT
##----------------------------

xlPath <- "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo"

## ## Last Observation Carried Forward (locf)
## while(length(ind <- which(is.na(xlTbl$curr))) > 0){
##   xlTbl$curr[ind] <- xlTbl$curr[ind - 1]
## }

## setnafill(xlTbl, type = "locf", cols = "curr") #only for nummeric

## regex
sub("(\\d+)\\D*", "\\1", "200 - Test")
sub("[^0-9]+", "", "200 - Test")


## Fylke
xlFile <- paste(xlPath, "fylke_change_ssb.xlsx", sep = "\\")
xlTbl <- readxl::read_excel(xlFile)
names(xlTbl) <- c("new", "old")
setDT(xlTbl)

xlTbl[, curr := as.numeric(gsub("[^0-9]+", "", new))]
xlTbl[, currName := gsub("\\d+\\D[^\\s]", "", new)]
xlTbl[, prev := as.numeric(gsub("[^0-9]+", "", old))]
xlTbl[, prevName := gsub("\\d+\\D[^\\s]", "", old)]

setnafill(xlTbl, type = "locf", cols = "curr") #only for numeric

while(length(ind <- which(is.na(xlTbl$currName))) > 0){
  xlTbl$currName[ind] <- xlTbl$currName[ind - 1]
}

xlTbl[, c("new", "old") := NULL]
xlTbl

f2020file <- paste(xlPath, "ssb_fylke.csv", sep = "\\")
f2020 <- fread(f2020file, fill = TRUE)
f2020[, setdiff(names(f2020), c("code", "name")) := NULL]

tblAlle <- xlTbl[f2020, on = c(curr = "code")]



## Join changes
elem2020
(newChg <- grunnkretsChg2020$DT[prev  %in% elem2020$chg, ])
newChg
## Inspect raw data 2018
raw2020 <- read_excel(grunnkretsChg2020$fileChg)

## Extract codes that have several alterations and carry forward
## the previous codes from 2019 ie. previous.y column to 2020
alt2019 <- grunnkretsChg2019$DT[curr  %in% elem2020$chg, ]
alt2020 <- grunnkretsChg2020$DT[prev  %in% elem2020$chg, ]

alt2019_2020 <- merge(alt2020, alt2019, by.x = "prev", by.y = "curr", all = TRUE)
## keep only the current in 2020 and previous column in 2019
alt2019_2020[, setdiff(names(alt2019_2020), c("curr", "currName.x", "prev.y", "year.y", "prevName.y")) := NULL]
setnames(alt2019_2020, c("currName.x", "prev.y","prevName.y", "year.y"), c("currName", "prev", "prevName", "year"))

