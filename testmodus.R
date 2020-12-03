## TEST MODUS
## -----------
rm(list = ls())
runtest = TRUE

## Hvis annen filsti, filnavn enn det som er standard
## må det speifiseres her først før source khfunctions
testpath <- "c:/DBtest"
testdb <- "KHELSA_dev.mdb"
## Hvor test fil skal havne
testmappe <- "c:/DBtest/test"



source("KHfunctions.R")

#Denne m? ogs? ofte kj?res ved oppstart. Setter opp ODBC.
KHglobs<-SettGlobs()

## Valg KOBLID
## -----------
LagFilgruppe("DODE_GK", id = 3534)
LagFilgruppe("ABORT_NH", id = c(3256, 20))
LagFilgruppe("MOBBING_UNGDATA", id = 2631)
LagFilgruppe("ELEVUNDER_NH", id = 3487)
