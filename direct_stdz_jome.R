# J?rgen: Dette skriptet virker, og det er testet at konfidensintervallene 
# blir omtrent de samme som i Stata.
# Noe m? fortsatt fikses:
#   1. Fikse overf?ringen av prosjektmappenavnet i shell-kommandoen i Stata





#setwd("F:/_EP/EPSA/Helseovervaaking/Norgeshelsa/Data og databehandling/Databehandling/Direktestd_i_R")

#--------------------------------------------------------------
## MARS-2020: Sikre at de to pakkene er installert.
## Required packages
pkgs <- c("data.table", "epitools")

## Sjekke hvis finnes og installere hvis ikke finnes
nypkg <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nypkg)) install.packages(nypkg)

sapply(pkgs, require, character.only = TRUE)
#--------------------------------------------------------------

library("epitools")
library("data.table")

start1 <- Sys.time()

args <- commandArgs(TRUE)

# running this like it shoudl be run from stata (but not from command line)
#args <- c("F:/_EP/EPSA/Helseovervaaking/Norgeshelsa/Data og databehandling/UTVIKLING/Databehandling/2015/Zz_Bearbeiding",
#          "INNDATA/data.csv",
#          "INNDATA/stdpop.csv",
#          "dodsarsak",
#          "KJONN")

# hardcoded variables to collapse by (and run standardisation by)
byVar=c("GEO", "AAR")

if(length(args)==0){
  wd <- "F:/_EP/EPSA/Helseovervaaking/Norgeshelsa/Data og databehandling/UTVIKLING/Databehandling/2015/Zz_Bearbeiding"
  countFile <- "INNDATA/data.csv"
  popFile <- "INNDATA/stdpop.csv"
} else if(length(args)>2) {
  wd <- args[1]
  countFile <- args[2]
  popFile <- args[3]
  byVar <- c(byVar,args[4:length(args)])
}

setwd("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/BIN/Z_Direktestd")
setwd(wd)
# we should remove this
#setwd("../../../../Databehandling/Direktestd_i_R")


# jorgen:
#setwd("F:/_EP/EPSA/Helseovervaaking/Norgeshelsa/Data og databehandling/UTVIKLING/direct standardization")



# population data
pop <- fread(popFile)
pop <- pop[order(pop$ALDER),]
setnames(pop,"sumNEVNER","popNEVNER")

# count data
count <- fread(countFile)

start2 <- Sys.time()

# merging
count <- count[ALDER %in% pop$ALDER,]
dim(count)
count <- merge(count, pop, by="ALDER")
dim(count)


# standardizing
setkeyv(count, byVar)
results <- count[,as.list(ageadjust.direct(count=sumTELLER,pop=sumNEVNER, stdpop=popNEVNER)),
                 by=key(count)]

# checking run times
print(Sys.time()-start1)
print(Sys.time()-start2)

# saving results
#write.csv(results,file="UTDATA/results.csv",row.names=FALSE)
write.csv(results,file="UTDATA/resultsPROSJEKT.csv",row.names=FALSE)

