## TEST MODUS
## -----------
rm(list = ls())
runtest = TRUE

## Hvis annen filsti og filnavn enn det som er standard
## må det speifiseres her først før source khfunctions
## testpath <- "c:/enc/DBtest/STYRING"
## testdb <- "KHELSA_20201203.mdb"

## Hvor test fil skal havne
## hvis ikke er spesifisert så lages det i PRODUKSJON/TEST mappe i PDB
## testmappe <- "c:/DB_test"

source("KHfunctions.R")


## Valg KOBLID
## -----------
LagFilgruppe("DODE_GK", id = 3534) #en fil
LagFilgruppe("ABORT_NH", id = c(3984, 3256)) #flere filer
