## Test mode
## ---------
rm(list = ls())

ts <- tm$new()
ts <- tm$new(dbPath = "F:/Prosjekter/Kommunehelsa/PRODUKSJON")
ts <- tm$new(dev = TRUE)

#Denne m? ogs? ofte kj?res ved oppstart. Setter opp ODBC.
KHglobs<-SettGlobs()


## List of Filgruppenavn to check
## ------------------------------
gpnavn <- c("ELEVUNDER", "ARBLEDIGE", "REGNEFERD_NH", "DODE_GK")



## tesfil = TRUE is to choose file where TESTING is 1 in ORGINALFILERse
LagFilgruppe(gpnavn[4])

LagFilgruppe("NEET_IMDI") #CSV fil
LagFilgruppe("ELEVUNDER", test = TRUE) #CSV fil testfile
LagFilgruppe("INNTULIKHET", test = TRUE) #CSV fil testfile




## Mer nytte funksjoner for Test Mode
## ----------------------------------

ts$dbFile
ts$dbPath

defpaths
filDB

ts$id
test_files
ts$add_id(3478) 
ts$empty() #tømt alle KOBLID

## Bytt nytt path eller fil
ts$newPath <- "F:/Prosjekter/Kommunehelsa/PRODUKSJON"
ts$newFile <- "KHELSA.mdb"
