## Bruk lokal kopi av Access for
## KHELSA og KHLogg
rm(list = ls())

## Aktivere kopiering av filer
## mappen c:/Users/din_bruker/DB_helseprofil er laget
## KHELSA.mdb og KHlogg.mdb med datotag lages der
source("local-files.R")

## Du vil bli spurt om du vil kopiere filen og svar med 1=Ja eller 0=Nei
## Evt. kan du bruke run_local(copy=FALSE) hvis du allerede har kopiert
## den nyested Access filen i lokal mappen.
run_local()


source("KHfunctions.R")

#Denne m? også ofte kjøres ved oppstart. Setter opp ODBC.
KHglobs<-SettGlobs()

## Bruk local ACCESS
LagFilgruppe("ARBLEDIGE", versjonert = TRUE)





## KUBE
## -----
rm(list = ls())
source("local-files.R")

## Behøver ikke å kopiere ACCESS på nytt hvis det er samme kjøring
## som brukes til LagFilgruppe
run_local(copy = FALSE)
source("KHfunctions.R")
KHglobs<-SettGlobs()


LagKubeDatertCsv("ARBLEDIGE")
LagKubeDatertCsv("MOBBING_UNGDATA")
