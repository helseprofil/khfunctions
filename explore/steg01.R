## Explore codes for setting templates

library(here)

source(here("test", "khfun_dev.R"))

##SettGlobs
globs=FinnGlobs()

## Needs only connection to db from globs to read KH_DELER table
Deler<-sqlQuery(globs$dbh,"SELECT * FROM KH_DELER",as.is=TRUE,stringsAsFactors=FALSE)


##------------------------------------
globs <- SettGlobs()
## SettTotalKoder
SettTotalKoder<-function(globs=FinnGlobs()){

  Koder<-sqlQuery(globs$dbh,"SELECT KH_KODER.DEL,KODE, FORMAT FROM KH_KODER INNER JOIN KH_DELER ON KH_KODER.DEL=KH_DELER.DEL WHERE TOTAL=1",as.is=TRUE,stringsAsFactors=FALSE)
  TotKoder<-list()
  for (del in Koder$DEL){
    if (Koder$FORMAT[Koder$DEL==del]=="integer"){
      TotKoder[[del]]<-as.integer(Koder$KODE[Koder$DEL==del])
    } else {
      TotKoder[[del]]<-Koder$KODE[Koder$DEL==del]
    }
  }
  return(TotKoder)
}


dt <- SettTotalKoder()
dt

Koder<-sqlQuery(globs$dbh,"SELECT KH_KODER.DEL,KODE, FORMAT FROM KH_KODER INNER JOIN KH_DELER ON KH_KODER.DEL=KH_DELER.DEL WHERE TOTAL=1",as.is=TRUE,stringsAsFactors=FALSE)

Koder

## -------------------------
## FinnFilgruppeParametre
gruppe = "ELEVUNDER"
batchdate=SettKHBatchDate()
globs=FinnGlobs()

dt <- FinnFilgruppeParametre(gruppe = gruppe)

valf <- names(FGP)[grepl("^VAL\\d+navn$",names(FGP))][1]
val<-gsub("(VAL\\d+)navn","\\1",valf) #alternative is gsub("navn", "", valf)
gsub("navn", "\\1", valf)

fgp <- as.data.table(FGP)
fgp[, get(valf)]
