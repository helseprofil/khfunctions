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
