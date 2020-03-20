## Check output from TEST function
## ------------------------------

## Choose the most relevant for path
## Path for Windows
rdsPath <- "c:/enc/DBtest/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE"

gsub("\\\\", "/", readClipboard())
rdsPath <- "F:/Prosjekter/Kommunehelsa/PRODUKSJON/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE"


## Path for linux
rdsPath <- "/f/Prosjekter/Kommunehelsa/TESTOMRAADE/TEST_KHFUN/DBtest/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE"

## From Excel file width format
testFil <- "UFORE.rds"

testFil <- "ELEVUNDER.rds"

df <- readRDS(file.path(rdsPath, testFil))

library(data.table)
setDT(df)
df
