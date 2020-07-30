## Get tabels from SSB for all the GEO changes
## ----------------------------------------------
## This consist only for GEO that has changes else it isn't in the list


pkg <- c("data.table", "openxlsx", "readxl")
sapply(pkg, require, character.only = TRUE)

xlPath <- "C:\\Users\\ybka\\Documents\\GitFH\\khfunction\\geo"


## ## Last Observation Carried Forward (locf)
## while(length(ind <- which(is.na(xlTbl$geo2020))) > 0){
##   xlTbl$geo2020[ind] <- xlTbl$geo2020[ind - 1]
## }

## setnafill(xlTbl, type = "locf", cols = "geo2020") #only for nummeric

## regex
sub("(\\d+)\\D*", "\\1", "200 - Test")
sub("[^0-9]+", "", "200 - Test")


## Fylke
xlFile <- paste(xlPath, "fylke_change_ssb.xlsx", sep = "\\")
xlTbl <- readxl::read_excel(xlFile)
names(xlTbl) <- c("geo2020", "geo2019")
setDT(xlTbl)

xlTbl[, geo20 := as.numeric(gsub("[^0-9]+", "", geo2020))]
xlTbl[, name20 := gsub("\\d+\\D[^\\s]", "", geo2020)]
xlTbl[, geo19 := as.numeric(gsub("[^0-9]+", "", geo2019))]
xlTbl[, name19 := gsub("\\d+\\D[^\\s]", "", geo2019)]

setnafill(xlTbl, type = "locf", cols = "geo20") #only for numeric

while(length(ind <- which(is.na(xlTbl$name20))) > 0){
  xlTbl$name20[ind] <- xlTbl$name20[ind - 1]
}
