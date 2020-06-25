## Kontrollere Original filer
## --------------------------

bib <- c("RODBC", "DBI", "data.table", "here")
sapply(bib, require, character.only = TRUE)


## Main path
mainPath <- "F:/Prosjekter/Kommunehelsa/PRODUKSJON"

dbfile <- "c:/enc/DBtest/STYRING/KHELSA_dev.mdb"

khcon <- RODBC::odbcConnectAccess2007(dbfile)

## Get all data from Access Table at once
orgTb <- "ORIGINALFILER"
orgDT <- setDT(RODBC::sqlFetch(khcon, orgTb))

RODBC::odbcClose(khcon)

fname <- orgDT[TESTING == 1, .(FILNAVN)][[1]]
fileName <- gsub("\\\\", "/", fname)

csvFile <- file.path(mainPath, fileName)

## Read CSV file
cat(csvFile)


## Fylke
fylke <- data.table::fread("c:/enc/fylke_tbl1158.csv", fill = TRUE)
dim(fylke)
fylke

## Kommune
komm <- data.table::fread("c:/enc/kommune_tbl1160.csv", fill = TRUE)
dim(komm)
komm
str(komm)
summary(komm)

## Bydeler
bydel <- data.table::fread("c:/enc/bydel_tbl1168.csv", fill = TRUE)
dim(bydel)
bydel
summary(bydel)

## Grunnkrets fra SSB
## Level 1 = delområder (noen steder er det lik bydeler f.eks Oslo, Rogaland, Vestland og Trondheim)
## Level 2 = grunnkretser
dt <- data.table::fread("c:/enc/enumerator2020.csv", fill = TRUE)
dim(dt)
dt
summary(dt)
dt[level == 1, ]
