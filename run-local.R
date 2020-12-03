## Bruk lokal kopi av Access for
## KHELSA og KHLogg

rm(list = ls())

## Aktivere kopiering av filer
## mappen c:/Users/din_bruker/DB_helseprofil er laget
## KHELSA.mdb og KHlogg.mdb med datotag lages der
source("local-files.R")


## Hvis skal ikke kopierer filer på nytt bruk copy=FALSE
## run_local(copy = FALSE)
run_local()


source("KHfunctions.R")

#Denne m? ogs? ofte kj?res ved oppstart. Setter opp ODBC.
KHglobs<-SettGlobs()

## Kjør
## -----------
LagFilgruppe("ARBLEDIGE")




