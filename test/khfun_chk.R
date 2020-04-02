## Check output from TEST function
## ------------------------------

## Choose the most relevant for path
## Path for Windows
rdsPath <- "c:/enc/DBtest/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE"

gsub("\\\\", "/", readClipboard())
rdsPath <- "F:/Prosjekter/Kommunehelsa/PRODUKSJON/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE"


## Path for linux
rdsPath <- "/f/Prosjekter/Kommunehelsa/TESTOMRAADE/TEST_KHFUN/DBtest/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE"

## ## From Excel file width format
## testFil <- "UFORE.rds"

## testFil <- "ELEVUNDER.rds"


## CSV files
filnavn <- c("ELEVUNDER", "ARBLEDIGE", "LaVINNT_1G")
testFil <- paste(filnavn, "rds", sep = ".")


df <- readRDS(file.path(rdsPath, testFil[2]))

library(data.table)
setDT(df)
df

names(df)
df[, .N, by = GEOniv]
df[, .N, by = GEO]


## EDIT Raw Files for testing
## --------------------------

normalizePath(readClipboard())

rawPath <- "C:\\enc\\DBtest\\ORGDATA\\DataTest\\2019"

rawFile <- c("FHI EUhasten2017.csv")

library(data.table)
DF <- fread(paste(rawPath, rawFile[1], sep = "\\"))

DF[, .N, by = Enhetsnavn]

## get only first 5 row in each groups ie. Enhetsnavn
dt <- DF[DF[,.I[1:5],by=Enhetsnavn]$V1]
dt2 <- DF[DF[,.I[1:5],by=Enhetsnavn][, V1]] #same as above

## Give colname 'ind' for row index instead of V1
DF[, .(ind = .I[1:5]), by=Kjonn]


## Select only 5 groups
print(DF[Prikket == 1, .N, by = Enhetsnavn], topn = 50)
valgEnh <- unique(DF$Enhetsnavn)[1:3]
dtSub <- c(valgEnh, "Aremark","Hasvik", "Grong") #med Prikket
dt <- DF[DF[,.I[1:5],by=.(Enhetsnavn)]$V1][Enhetsnavn  %in% dtSub, ]

fwrite(dt, paste(rawPath, "testPrikk.csv", sep = "\\"), sep = ";")



## Extract Original files with file types
## --------------------------------------

dbfile <- "c:/enc/DBtest/STYRING/KHELSA_dev.mdb"


library(RODBC)
library(glue)
library(DBI)
library(data.table)

innTb <- "INNLESING"
orgTb <- "ORIGINALFILER"
kobTb <- "ORGINNLESkobl"


khcon <- RODBC::odbcConnectAccess2007(dbfile)

## Get all data at once
innDT <- setDT(RODBC::sqlFetch(khcon, innTb))
orgDT <- setDT(RODBC::sqlFetch(khcon, orgTb))
kobDT <- setDT(RODBC::sqlFetch(khcon, kobTb))

lapply(list(inn = innDT, org = orgDT, kob = kobDT), names)
lapply(list(inn = innDT, org = orgDT, kob = kobDT), dim)


kobDT[duplicated(FILID), .N]
kobDT[duplicated(KOBLID), .N]

## check if there is duplicated
kobDT[, id := paste(FILID, FILGRUPPE, DELID, sep = "_")]
kobDT[duplicated(id), .N]
kobDT[duplicated(id), ] ## only missing has duplicated


innCols <- c("FILGRUPPE", "DELID", "RSYNT1")## Select specific








## Give ORIGINALFILER







sqlGet <- glue_sql("
  SELECT {cols*}
  FROM {innles}",
  .con = DBI::ANSI()
  )

khdf <- RODBC::sqlQuery(khcon, sqlGet)

odbcClose(khcon)
