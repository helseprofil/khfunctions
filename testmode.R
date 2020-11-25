## Test mode
## ---------

rm(list = ls())
runtest = TRUE
source("KHfunctions.R")

BUFFER<-list(BEF_GKa=KlargjorFil("BEF_GKa",versjonert=TRUE)$FIL)
#BUFFER<-list(BEF_GKa=FinnFilT("BEF_GK_Ta"))

#Denne m? ogs? ofte kj?res ved oppstart. Setter opp ODBC.
KHglobs<-SettGlobs()


## List of Filgruppenavn to check
## ------------------------------
gpnavn <- c("ELEVUNDER", "ARBLEDIGE", "REGNEFERD_NH", "DODE_GK")

## tesfil = TRUE is to choose file where TESTING is 1 in ORGINALFILERse
LagFilgruppe("ELEVUNDER") #CSV fil
LagFilgruppe("ELEVUNDER", test = TRUE) #CSV fil testfile
LagFilgruppe("INNTULIKHET", test = TRUE) #CSV fil testfile
