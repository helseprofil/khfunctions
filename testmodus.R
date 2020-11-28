## TEST MODUS
## -----------
rm(list = ls())
runtest = TRUE

## Hvis annen filsti og filnavn enn i PDB
## speifiserer den her først før source khfunctions
testpath <- "F:/Prosjekter/Kommunehelsa/PRODUKSJON"
testdb <- "KHELSA_dev.mdb"


source("KHfunctions.R")

#Denne m? ogs? ofte kj?res ved oppstart. Setter opp ODBC.
KHglobs<-SettGlobs()

## Valg KOBLID
## -----------
testfiles <- c(3534, 2589)
LagFilgruppe("DODE_GK", id = 3534)



testfiles <- 3256
LagFilgruppe("ABORT_NH", id = c(3256, 20))
