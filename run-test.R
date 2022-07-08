## TEST MODUS
## -----------
rm(list = ls())
runtest = TRUE

## Hvis annen filsti og filnavn enn det som er standard
## må det speifiseres her først før source khfunctions
## OBS!! Uncomment kodene nedenfor for å bruke nye filsti og filnavn til databasen
## --------------------------------------------------------------------------
# testpath <- "c:/Path/to/Test/Access"
# testdb <- "KHELSA_20201203.mdb"

## Hvor test fil dvs. produktet eller output, skal havne
## hvis ikke er spesifisert så lages det i PRODUKSJON/TEST mappe i PDB
## OBS!! Uncomment koden nedenfor for å bruke annen testmappe enn default
## --------------------------------------------------------------------------
# testmappe <- "c:/Path/to/DB_output"

source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
## Bytt den gyldige branch navn som skal testet. Test funksjon skal ikke lages i master branch før godkjenning!
kh_source(repo = "khfunctions", branch = "master", file = "KHfunctions.R")


## Valg KOBLID
## -----------
LagFilgruppe("DODE_GK", id = 3534) #en fil
LagFilgruppe("ABORT_NH", id = c(3984, 3256)) #flere filer

## KUBE
## ------------
LagKubeDatertCsv("ARBLEDIGE")
LagKubeDatertCsv("MOBBING_UNGDATA")
