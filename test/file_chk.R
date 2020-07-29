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


## EXCEL File for LesFil
## ---------------------
filgruppe <- "RFU_SIRUS_NH_ROYK_UTDANN"
DT <- readRDS("C:\\enc\\DBtest\\PRODUKTER\\MELLOMPROD\\R\\STABLAORG\\NYESTE\\RFU_SIRUS_NH_ROYK_UTDANN.rds")
setDT(DT)
DT[, .N, by = FYLKE]
DT[, .N, by = GEO]
names(DT)
DT

filbesk <- FinnFilBeskGruppe(filgruppe = filgruppe)


## Explore RDS files
## -----------------

rdsPath <-normalizePath(
  "F:\\Prosjekter\\Kommunehelsa\\PRODUKSJON\\PRODUKTER\\MELLOMPROD\\R\\STABLAORG\\DATERT","/")

rdsFile <- c("ABORT_NH_2020-03-09-09-12.rds", #1
             "ARBLEDIGE_2020-03-25-18-08.rds", #2
             "BARNEHAGE_KVALITET_2020-06-18-15-47.rds", #3
             "ELEVUNDER_NH_2020-03-16-16-36.rds", #4
             "ELEVUNDER_2020-07-23-15-44.rds") #5


## Check file
valgF <- paste(rdsPath, rdsFile[5], sep = "/")

dt <- readRDS(valgF)
data.table::setDT(dt)
names(dt)
dt

summary(dt)
dname <- names(dt)
dt[, .N, get(dname[1])]

for (j in dname) {
  print(j)
  print(dt[, .N, keyby = j])
 }
