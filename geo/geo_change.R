## Get tabels from SSB for all the GEO changes
## ----------------------------------------------
## This consist only for GEO that has changes else it isn't in the list

pkg <- c("data.table", "openxlsx", "readxl", "fs")
sapply(pkg, require, character.only = TRUE)

##-----------------------
## Function for merging
##-----------------------
## geo_new - File names for new geo from SSB in csv or specified file if tbl=TRUE
## geo_chg - Copy paste from ssb "endringer" to Excel
## year - Valid year for geo code
## type - fylke, kommune, grunnkrets...
## file_path - Folder to these files ie. geo_new and geo_chg
## raw - if TRUE then use Excel and CSV file for change in codes ie. copy/paste from SSB "Endringer"
## tab

add_change <- function(geo_new,
                      geo_chg,
                      year,
                      type = "land",
                      file_path = NULL,
                      raw = TRUE){

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
  ## Use Excel for changes file
  if (raw){
    xlTbl <- readxl::read_excel(fileChg)
  } else {
    xlTbl <- geo_chg
  }
  
  names(xlTbl) <- c("new", "old")
  setDT(xlTbl)

  expNum <- switch(type,
                   "kommune" = "[^0-9]+",
                   "grunnkrets" = "\\s.*",
                   "fylke" = "[^0-9]+", 
                   "[^0-9]\\s.*")

  expName <- switch(type,
                    "kommune" = "\\d+\\D[^\\s]",
                    "grunnkrets" = "[^A-Za-z]",
                    "[^A-Za-z]")
  
  ## Extract code and name separately
  xlTbl[!is.na(new), curr := as.numeric(gsub(expNum, "", new))]
  xlTbl[!is.na(new), currName := gsub(expName, "", new)]
  xlTbl[!is.na(old), prev := as.numeric(gsub(expNum, "", old))]
  xlTbl[!is.na(old), prevName := gsub(expName, "", old)]
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
  if (raw){
    dt <- data.table::fread(fileNew, fill = TRUE)
  } else {
    dt <- geo_new
  }
  
  nCols <- names(dt)
  same <- identical(mainCols, nCols)
  ##keep only Code and Name
  if (same == 0){
    dt[, setdiff(names(dt), mainCols) := NULL]
  }
  
  ## Merge
  DT <- xlTbl[dt, on = c(curr = "code")]
  DT[, currName := NULL]
  setnames(DT, "curr", "code")
  otherCols <- setdiff(names(DT), mainCols)
  setcolorder(DT, c(mainCols, otherCols))

  list(DT = DT[], xl = xlTbl[], fileChg = fileChg, fileNew = fileNew)

}

## Select files with regex
## --------------------------------------
## grep.file - Get downloaded CSV file from SSB. Use regular expression to get all relevant files,
## eg. "jan2019" will grep all file with "jan2019" for both new and change
## grep.change - Get the changes file in xlsx copy/paste from SSB that is related to regexp in
## 'grep.file' and file eg. "change". Regexp will find word "change" for all "jan2019"

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


## -- File changes --
## To detect codes that have several changes backward
## Join changes of geo codes from previous change ie. code that have changed in 2018
## and again have new changes in 2020. Then get the previous codes in 2018 from previous code columns
##
## 30240317 (in 2020) from 2190317 (2019) but was 2190314 (2018)
## raw - if using an exsiting merge_multi() instead of add_change() table then raw = FALSE
merge_multi <- function(newfile, prevfile, raw = TRUE){
  
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

  fileOut <- rbindlist(list(allFile, altNew))
  setkey(fileOut, code, year)
  
  return(fileOut[])
}

## Find code changes from previous year eg. code 2020 vs 2019 or code 2019 vs 2018
## newfile - ealier year for file produced by add_change()
## prefile - previous year for file output from add_change()
## raw - if FALSE then use exisiting table for prefile
find_change <- function(newfile, prefile, raw = TRUE){
  
  dt1 <- newfile[["DT"]]
  
  if (raw){
    dt2 <- prefile[["DT"]]
  } else {
    dt2 <- prefile
  }
  
  DT <- dt2[dt1, on = "code"]

  colN <- names(dt1)[-1]
  for (j in colN){
    coli <- paste0("i.", j)
    DT[is.na(get(coli)), (coli) := get(j)]
  }

  DT[, (colN) := NULL]
  setnames(DT, names(DT)[-1], colN)
  
  ## DT[, prevName := name]
  ## DT[, name := i.name]
  ## cols <- grep("^i.", names(DT))
  ## DT[, (cols) := NULL]
  ## DTout <- DT[!is.na(prev), ][]

  return(DT[])
}


## merge the code changes from previous file (prefile) to the recent file (newfile)
## all - if FALSE then keep only codes that have changes else keep everything
show_change <- function(newfile, prefile, all = FALSE, ...){

  dt <- find_change(newfile, prefile, ...)
  codePre <- unique(dt$code)

  dtNew <- newfile[["DT"]]
  dtx <- dtNew[!(code %in% codePre), ]
  
  DT <- rbindlist(list(dt, dtx))
  setkey(DT)

  if (all){
    DTout <- DT[]
  } else{
    DTout <- DT[!is.na(prev)][]
  }
  
  return(DTout)
}





## Convert raw CSV files to R
## --------------------------
## file - The output after running select_ssb()
## type - Data type ie. kommune, fylke, grunnkrets etc
convert_file <- function(file, type = NULL){
  
  allFiles <- file[["allfile"]]
  for (i in 1:length(allFiles)){
    file <- allFiles[i]
    fnum <- paste0(type, "_0", i)
    dt <- data.table::fread(file, fill = TRUE)
    cols <- c("parentCode", "shortName", "validFrom", "validTo")
    for (j in cols) set(dt, j = j, value = as.numeric(dt[[j]]))
    DT <- list(file = file, dt = dt)
    assign(fnum, DT, env = .GlobalEnv)  
  }
}


## Create table with changes 
##--------------------------
## This produces:
## find_dup = Duplicated codes that shouldn't be there
## allDT for all files with changes
## changeDT for codes that changes either multiple or once up to recent year

## files - List object of files from oldest to recent year. The files are the products of
## add_change() function.
merge_geo <- function(files){
  
  if (inherits(files, "list") == 0) stop("'files' should be a list", call. = FALSE)
  
  fileMx  <- length(files)
  ind <- CJ(1:fileMx, 1:fileMx)
  indSel <- ind[V1 != V2, ][V1 < fileMx, ][V1 < V2, ]
  
  ## create empty list for multiple changes
  join_dt <- vector(mode = "list", length = nrow(indSel))

  for (i in seq_len(nrow(indSel))){

    newFile <- indSel[[2]][i]
    preFile <- indSel[[1]][i]

    d <- merge_multi(newfile = files[[newFile]],
                     prevfile = files[[preFile]])
    
    join_dt[[i]] <- d
    
  }

  joinDT <- rbindlist(join_dt)

  ## Change once
  indChg <- ind[V1 - V2 == 1, ]

  chg_dt <- vector(mode = "list", length = nrow(indChg))

  for (i in seq_len(nrow(indChg))){

    newFile <- indChg[[1]][i]
    preFile <- indChg[[2]][i]

    d <- find_change(newfile = files[[newFile]],
                     prefile = files[[preFile]])

    chg_dt[[i]] <- d
  }

  chgDT <- rbindlist(chg_dt)
  
  ## Keep only those with valid codes for recent year
  recentCodes <- unique(files[[fileMx]]$DT$code)
  currDT <- chgDT[code  %in% recentCodes, ]
 
  ## Merge all changes ie. multiple and change once
  changeDT <- rbindlist(list(joinDT, currDT))

  ## Clean up duplicated lines and delete codes that have not changed
  ## if duplicated lines exists
  indX <- changeDT[, .I[duplicated(changeDT)]]

  if (sum(indX > 0)){
    dtx <- changeDT[-(indX)]
    dtz <- dtx[!is.na(prev), ]
    
    ## check duplicate for find_change() function.
    ## and keep only those that are in newest geo due to multiple changes
    dupInx <- dtz[, .I[(duplicated(prev) | duplicated(prev, fromLast = TRUE))]]
    ## split to 2 DT with and without duplicated 'prev'
    uniDT <- dtz[-dupInx]
    dupDT <- dtz[dupInx]
    
    ## Clean duplicated code if codes aren't in newest geo
    dupCodes <- unique(dupDT$code)
    keepInd <- is.element(dupCodes, recentCodes)
    keepCodes <- dupCodes[keepInd]
    dupUni <- dupDT[code %in% keepCodes, ]

    ## Merge back to the other DT without duplicated previous codes
    CDT <- rbindlist(list(uniDT, dupUni))
    setkey(CDT, code)
  } else {
    CDT <- changeDT
    dupDT <- 0
  }

  ## Merge everything to recent geo list
  ## ---------------------------------------
  dupCodesChg <- unique(CDT$code)## codes that are allready in the changes table
  ## keep only codes in recent geo list that aren't in the changes table
  otherDT <- files[[fileMx]]$DT[!(code  %in% dupCodesChg), ] 
  geoDT <- rbindlist(list(otherDT, CDT), fill = TRUE)
  setkey(geoDT, code, year)

  ## Recode mulitple codes that haven't been converted to recent geo code
  eks <- setdiff(geoDT$code, recentCodes) #get codes that aren't in recent geo

  for (i in eks){
    currGeo <- geoDT[prev == i, ][["code"]]
    geoDT[code == i, code := currGeo]
  }

  setkey(geoDT, code, year)
  list(dupDT = dupDT, chgDT = CDT, allDT = geoDT)
}



## -----------------
## Connect to DB
## -----------------
## on - Open or Close connection
## write - Create table in DB
## filename - Table name
## obj - Object in .env to write 
connect_db <- function(on = TRUE, write = FALSE, tblname = NULL, obj = NULL){

  if (on){
    dbPath <- normalizePath("C:\\Users\\ybka\\Folkehelseinstituttet\\Folkehelseprofiler - Data mining\\geo_level", winslash = "/")
    dbName <- "geo_ssb.accdb"

    ## With odbc and DBI
    pkg <- c("odbc", "DBI")
    sapply(pkg, require, character.only = TRUE)

    dbCon <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq="
    dbFile <- paste(dbPath, dbName, sep = "/")

    cs <- paste0(dbCon, dbFile)
    con <- dbConnect(odbc::odbc(), .connection_string = cs)
  } else { 
    dbDisconnect(con)
  }

  if (write){
    dbWriteTable(con, tblname, obj, batch_rows = 1, overwrite = TRUE)
  }
  
}


## -----------------
## Fylke
## -----------------
file_path = "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo\\fylke"

fylke2018 <- select_ssb(grep.file = "jan2018",
                        grep.change = "change",
                        file.path = file_path)

fylkeChg2018 <- add_change(geo_new = fylke2018$allfile,
                          geo_chg = fylke2018$chgfile,
                          year = 2018,
                          type = "fylke")

fylke2020 <- select_ssb(grep.file = "jan2020",
                        grep.change = "change",
                        file.path = file_path)

fylkeChg2020 <- add_change(geo_new = fylke2020$allfile,
                          geo_chg = fylke2020$chgfile,
                          year = 2020,
                          type = "fylke")

## Find all the changes in codes
## ---------------------------------
## Include previous changes from "Endringer" file if exist
merge_multi(fylkeChg2020, fylkeChg2018)
## Include previous changes from raw CSV data
find_change(fylkeChg2020, fylkeChg2018)
show_change(fylkeChg2020, fylkeChg2018, all = TRUE)

fylkeChange2020_2018 <- show_change(fylkeChg2020, fylkeChg2018)


fylkeDT <- merge_geo(list(fylkeChg2018, fylkeChg2020))
connect_db(, write = TRUE, tblname = "tblFylkeChange", obj = fylkeDT$chgDT)



## Create table to DB
connect_db(write = TRUE,
           tblname = "tblFylkeChange",
           obj = fylkeChange2020_2018)

connect_db(on = FALSE)




## -----------------
## Kommune endringer
## -----------------

file_path = "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo\\kommune"

kom2017 <- select_ssb(grep.file = "jan2017",
                      grep.change = "change",
                      file.path = file_path
                      )

komChg2017 <- add_change(
  geo_new = kom2017$allfile,
  geo_chg = kom2017$chgfile,
  year = 2017,
  type = "kommune"
)

kom2018 <- select_ssb(grep.file = "jan2018",
                      grep.change = "change",
                      file.path = file_path
                      )

komChg2018 <- add_change(
  geo_new = kom2018$allfile,
  geo_chg = kom2018$chgfile,
  year = 2018,
  type = "kommune"
)

kom2019 <- select_ssb(grep.file = "jan2019",
                      grep.change = "change",
                      file.path = file_path
                      )

komChg2019 <- add_change(
  geo_new = kom2019$allfile,
  geo_chg = kom2019$chgfile,
  year = 2019,
  type = "kommune"
)


kom2020 <- select_ssb(grep.file = "jan2020",
                      grep.change = "change",
                      file.path = file_path
                      )

komChg2020 <- add_change(
  geo_new = kom2020$allfile,
  geo_chg = kom2020$chgfile,
  year = 2020,
  type = "kommune"
)


kommuneDT <- merge_geo(list(komChg2017, komChg2018, komChg2019, komChg2020))
connect_db(, write = TRUE, tblname = "tblKommuneChange", obj = kommuneDT$chgDT)



## Find geo codes that have multiple changes
## -----------------------------------------------
## Check for multiple changes
merge_multi(newfile = komChg2018, prevfile = komChg2017) #no changes
merge_multi(newfile = komChg2019, prevfile = komChg2017) #no changes
merge_multi(newfile = komChg2020, prevfile = komChg2017) # 4
merge_multi(newfile = komChg2019, prevfile = komChg2018) #no changes
merge_multi(newfile = komChg2020, prevfile = komChg2018) # 49
merge_multi(newfile = komChg2020, prevfile = komChg2019) #no changes

show_change(newfile = komChg2020, prevfile = komChg2019) #no changes

komJoin2020_2017 <- merge_multi(newfile = komChg2020, prevfile = komChg2017) # 4
komJoin2020_2018 <- merge_multi(newfile = komChg2020, prevfile = komChg2018) # 49

## merge all with multiple changes
komMulti <- rbindlist(list(komJoin2020_2017,
                           komJoin2020_2018))

## Fine kommune codes that have changed from previous year
(tbl2018_2017 <- find_change(komChg2018, komChg2017))
(tbl2019_2018 <- find_change(komChg2019, komChg2018))
(tbl2020_2019 <- find_change(komChg2020, komChg2019))

## Merge all that has changed once
tblChanges <- rbindlist(list(tbl2018_2017,
                             tbl2019_2018,
                             tbl2020_2019))

## should have no duplicate in previous code
tblChanges[duplicated(prev), ]


## keep only those code that still valid in the current year ie. 2020
recentCodes <- unique(komChg2020$DT[["code"]])
validCurr <- tblChanges[code  %in% recentCodes, ]

## Merge all kommune that have changes since 2017
komDT <- rbindlist(list(validCurr,
                        komMulti))
## should have no duplicate for prev.. but what happen to these 4!!
komDT[duplicated(prev), ]


## Merge everything to current Kommune list
## ---------------------------------------
dupCodes <- unique(komDT$code) ## code that allready in the changes table
currDT <- komChg2020$DT[!(code  %in% dupCodes), ] #keep only code that aren't in the changes table
komGEO <- rbindlist(list(currDT, komDT), fill = TRUE) #merge everything

setkeyv(komGEO, "code")

## Connect to DBMS
connect_db(, write = TRUE, tblname = "tblKommuneChange", obj = komGEO)
komGEO[duplicated(code) | duplicated(code, fromLast = TRUE), ]
## dbWriteTable(con, "tblKommuneChange", komGEO, batch_rows = 1, overwrite = TRUE)

dbDisconnect(con)

## Get all kommune CSV files
komFiles <- select_ssb("kommune", "change", file_path)

convert_file(komFiles, "kommune")

connect_db()
## kommune 2017 - two changes in Jan and Apr. Changes in April is used here
kommune02
dbWriteTable(con, "tblKommune2017", kommune02$dt, batch_rows = 1, overwrite = TRUE)

## kommune 2018
kommune06
dbWriteTable(con, "tblKommune2018", kommune06$dt, batch_rows = 1, overwrite = TRUE)




## --------------------------------------
## Get all old geo codes for Grunnkrets
## --------------------------------------
file_path = "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo\\grunnkrets"

## Grunnkrets endringer
grunnkrets2016 <- select_ssb(grep.file = "jan2016",
                      grep.change = "change",
                      file.path = file_path
                      )

grunnkretsChg2016 <- add_change(
  geo_new = grunnkrets2016$allfile,
  geo_chg = grunnkrets2016$chgfile,
  year = 2016,
  type = "grunnkrets"
)



grunnkrets2017 <- select_ssb(grep.file = "jan2017",
                      grep.change = "change",
                      file.path = file_path
                      )

grunnkretsChg2017 <- add_change(
  geo_new = grunnkrets2017$allfile,
  geo_chg = grunnkrets2017$chgfile,
  year = 2017,
  type = "grunnkrets"
)

grunnkrets2018 <- select_ssb(grep.file = "jan2018",
                      grep.change = "change",
                      file.path = file_path
                      )

grunnkretsChg2018 <- add_change(
  geo_new = grunnkrets2018$allfile,
  geo_chg = grunnkrets2018$chgfile,
  year = 2018,
  type = "grunnkrets"
)

grunnkrets2019 <- select_ssb(grep.file = "jan2019",
                      grep.change = "change",
                      file.path = file_path
                      )

grunnkretsChg2019 <- add_change(
  geo_new = grunnkrets2019$allfile,
  geo_chg = grunnkrets2019$chgfile,
  year = 2019,
  type = "grunnkrets"
)


grunnkrets2020 <- select_ssb(grep.file = "jan2020",
                      grep.change = "change",
                      file.path = file_path
                      )

grunnkretsChg2020 <- add_change(
  geo_new = grunnkrets2020$allfile,
  geo_chg = grunnkrets2020$chgfile,
  year = 2020,
  type = "grunnkrets"
)


## CREATE table for changes
gDT <- merge_geo(list(grunnkretsChg2016,
                         grunnkretsChg2017,
                         grunnkretsChg2018,
                         grunnkretsChg2019,
                         grunnkretsChg2020))


connect_db(, write = TRUE, tblname = "tblGrunnkretsChange", obj = gDT$chgDT)


## Find geo codes that have changed more than once
## -----------------------------------------------
## Check if current codes in previous year is.element in previous codes of current year
## showing that the codes have changed again since previous change

elem2017 <- check_element(grunnkretsChg2017, grunnkretsChg2016)
elem2018 <- check_element(grunnkretsChg2018, grunnkretsChg2017)
elem2019 <- check_element(grunnkretsChg2019, grunnkretsChg2018)
elem2020 <- check_element(grunnkretsChg2020, grunnkretsChg2019)

## check all previous changes
(chg2017_2016 <- merge_multi(grunnkretsChg2017, grunnkretsChg2016)) #no changes
(chg2018_2017 <- merge_multi(grunnkretsChg2018, grunnkretsChg2017)) # 7 
(chg2019_2018 <- merge_multi(grunnkretsChg2019, grunnkretsChg2018)) #no changes
(chg2020_2019 <- merge_multi(grunnkretsChg2020, grunnkretsChg2019)) # 24

## ## add all the changes to the current geo ie. 2020
## chg2020_1817 <- merge_multi(grunnkretsChg2020, chg2018_2017, raw = FALSE)
## chg2020_2016 <- merge_multi(grunnkretsChg2020, grunnkretsChg2016)
## chg2020_2017 <- merge_multi(grunnkretsChg2020, grunnkretsChg2017)
## chg2020_2018 <- merge_multi(grunnkretsChg2020, grunnkretsChg2018)
## chg2020_2019 <- merge_multi(grunnkretsChg2020, grunnkretsChg2019)

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

dbDisconnect(con)


## Convert all CSV file to R
grunnFiles <- select_ssb("grunnkrets", "change", file_path)

convert_file(grunnFiles, type = "grunn")

connect_db()
## Send grunnkrets files to Access
dbWriteTable(con, "tblGrunnkrets2016", grunn01$dt, batch_rows = 1, overwrite = TRUE)
dbWriteTable(con, "tblGrunnkrets2017", grunn02$dt, batch_rows = 1, overwrite = TRUE)
dbWriteTable(con, "tblGrunnkrets2018", grunn03$dt, batch_rows = 1, overwrite = TRUE)









### ------

fylkeChg <- add_change(
  geo_new = "ssb_fylke.csv",
  geo_chg = "fylke_change_ssb.xlsx",
  file_path = "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo"
)

grunnkretsmuneChg <- add_change(
  geo_new = "ssb_kommune.csv",
  geo_chg = "kommune_change_ssb.xlsx",
  file_path = "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo"
)

grunnkretsChg <- add_change(
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


## ---------
## TESTING
## -----------
setwd("C:/geo_files/test_data")
library(data.table)
library(writexl)

eraDT <- data.table(code = c(6, 12, 15:16, 21), name = sapply(c(6, 12, 15:16, 21),
                                                              function(x) paste0("era_new", letters[x])))
eraChg <- data.table(newname = sapply(c(6, 15, 15), function(x) paste0(x, " - era_new", letters[x])),
                     prename = sapply(c(20, 25, 30), function(x) paste0(x, " - old_V", letters[x - 5])))
eraDT[code == 6, name := "must_exist"]
eraChg[prename %in% grep("^20", eraChg$prename, value = TRUE), newname := "6 - must_exist"]

fwrite(eraDT, "file_jan2017.csv", sep = ";")
write_xlsx(eraChg, "file_change_jan2017.xlsx")



preDT <- data.table(code = c(1:2, 6, 9:12), name = sapply(c(1:2, 6, 9:12), function(x) paste0("pre", letters[x])))
preDT[code == 6, name := "must_exist"]
preDT[code == 12, name := "era_newl"]
preChg <- data.table(newname = c("1 - prea",
                                 "10 - prej", 
                                 "2 - preb",
                                 "9 - prei"),
                     prename = c("21 - era_newu",
                                 "21 - era_newu",
                                 "15 - era_newo",
                                 "16 - era_newp"))

fwrite(preDT, "file_jan2018.csv", sep = ";")
write_xlsx(preChg, "file_change_jan2018.xlsx")



postDT <- data.table(code = c(1:4, 6, 50), name = sapply(c(1:4, 6, 50), function(x) paste0("post_new", letters[x])))
postDT[code == 6, name := "must_exist"]
postDT[code == 50, name := "no_change"]
postChg <- data.table(newname = c("3 - post_newc",
                                  "3 - post_newc",
                                  "4 - post_newd",
                                  "4 - post_newd"),
                      prename = c("9 - prei",
                                  "10 - prej",
                                  "11 - prek",
                                  "12 - era_newl"))

fwrite(postDT, "file_jan2019.csv", sep = ";")
write_xlsx(postChg, "file_change_jan2019.xlsx")



newDT <- data.table(code = c(3, 5:8, 17, 18, 50),
                    name = sapply(c(3, 5:8, 17, 18, 50),
                                  function(x) paste0("new_",letters[x])))
newDT[code == 3, name := "post_newc"]
newDT[code == 6, name := "must_exist"]
newDT[code == 50, name := "no_change"]

newChg <- data.table(newname = c("5 - new_e",
                                 "7 - new_g",
                                 "17 - new_q",
                                 "18 - new_r"),
                     prename = c("4 - post_newd",
                                 "1 - post_newa",
                                 "2 - post_newb",
                                 "2 - post_newb"))

fwrite(newDT, "file_jan2020.csv", sep = ";")
write_xlsx(newChg, "file_change_jan2020.xlsx")




xfiles <- list(eraChg, preChg, postChg, newChg)
xyear <- c(2017, 2018, 2019, 2020)
setwd("~/Git-work/draft/geo/data")

for (i in seq_len(length(xfiles))) {
    fname <- paste0("file_change_jan", xyear[i], ".xlsx")
    writexl::write_xlsx(xfiles[[i]], fname)
}

cvfiles <- list(eraDT, preDT, postDT, newDT)
for (i in seq_len(length(cvfiles))) {
    fname <- paste0("file_jan", xyear[i], ".csv")
    data.table::fwrite(cvfiles[[i]], fname, sep = ";")
}





library(norgeo)

dt <- geo_set(eraDT, eraChg, year = 2017, raw = FALSE)


## Detecting changes process
## ----------------------------
## Merge current files and code changes
ChgEra <- add_change(eraDT, eraChg, year = 2017, raw = FALSE)
ChgPre <- add_change(preDT, preChg, year = 2018, raw = FALSE)
ChgPost <- add_change(postDT, postChg, year = 2019, raw = FALSE)
ChgNew <- add_change(newDT, newChg, year = 2020, raw = FALSE)

allDT <- merge_geo(list(ChgEra, ChgPre, ChgPost, ChgNew))

## Codes with multiple changes
merge_multi(ChgPre, ChgEra)
merge_multi(ChgPost, ChgEra)
merge_multi(ChgNew, ChgEra)

merge_multi(ChgPost, ChgPre)
merge_multi(ChgNew, ChgPre)

merge_multi(ChgNew, ChgPost)


multiChange <- merge_multi(ChgNew, ChgPost)






## View code that change once
find_change(ChgNew, ChgPost)
find_change(ChgPost, ChgPre)

findChange <- find_change(ChgNew, ChgPost)
## setnames(ddFind, c("code", "name"), c("curr", "currName"))


## Include the change from find_change() to the current newfile
withChange <- show_change(ChgNew, ChgPost)

allWithChange <- rbindlist(list(ChgNew$DT,
                                withChange,
                                multiChange))

allOnlyChange <- rbindlist(list(findChange,
                                multiChange))

allDT <- rbindlist(list(ChgNew$DT,
                        allOnlyChange))

setkey(allChange, code, prev)
allChange  



#### ---------
## Create data for change table
## ---------

newD <- data.table(code = c(3, 5:9), name = c("new3", "old4", "new", "new2",
                                              "new", "old10"))
oldD <- data.table(code = c(4, 1, 2, 10), name = c("old4", "old1", "old2", "old10"))

fwrite(newD, "~/Git-work/draft/geo/data/new_bydel.csv")
fwrite(oldD, "~/Git-work/draft/geo/data/old_bydel.csv")
