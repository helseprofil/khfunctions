## Bruk lokal kopi av Access KHELSA og KHLogg
## ------------------------------------------
rm(list = ls())
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")

## Tilgjengeliggjgøre funksjon for kopiering av filer i
## mappen c:/Users/din_bruker/DB_helseprofil dvs. run_local().
## KHELSA.mdb og KHlogg.mdb med datotag lages der

kh_source(repo = "khfunctions", branch = "master", file = "local-files.R")

## Du vil bli spurt om du vil kopiere filen og svar med 1=Ja eller 0=Nei
## Evt. kan du bruke run_local(copy=FALSE) hvis du allerede har kopiert
## den nyested Access filen i lokal mappen.

run_local()

## Source KHfunctions
kh_source(repo = "khfunctions", branch = "master", file = "KHfunctions.R")

## Nå skal local ACCESS filer er klare til bruk
## og de vanlige funksjonen kan kjøres
## --------------------------------------------
## FILGRUPPE
LagFilgruppe("ARBLEDIGE", versjonert = TRUE)
LagFilgruppe("ABORT_NH")


## KUBE
## -----
rm(list = ls())
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_source(repo = "khfunctions", branch = "master", file = "local-files.R")

## Behøver ikke å kopiere ACCESS på nytt hvis det er samme kjøring
## som brukes til LagFilgruppe
run_local(copy = FALSE)
kh_source(repo = "khfunctions", branch = "master", file = "KHfunctions.R")
KHglobs<-SettGlobs()


LagKubeDatertCsv("ARBLEDIGE")
LagKubeDatertCsv("MOBBING_UNGDATA")
