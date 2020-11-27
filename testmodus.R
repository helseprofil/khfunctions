## TEST MODUS
## -----------
rm(list = ls())
runtest = TRUE
source("KHfunctions.R")

#Denne m? ogs? ofte kj?res ved oppstart. Setter opp ODBC.
KHglobs<-SettGlobs()

## Valg KOBLID
## -----------
testfiles <- c(3534, 2589)

LagFilgruppe("DODE_GK")
