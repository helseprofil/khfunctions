## Convert Geo file from SSB

library(data.table)
library(stringi)
fylke <- fread("ssb_fylke.csv", fill = TRUE)

## Kommune
kommune <- fread("ssb_kommune.csv", fill = TRUE)
kommune[, fylkeCode := gsub("\\d{2}$", "", code)]
## or stri_extract_first_regex(5644, '\\d{2}')
setcolorder(kommune, c(9, 1:8))
fwrite(kommune, "ssb_kommune02.csv", sep = ";")

## Bydels
bydel <- fread("ssb_bydels.csv", fill = TRUE)
bydel[, kommuneCode := gsub("\\d{2}$", "", code)]
dim(bydel)
setcolorder(bydel, c(9, 1:8))
fwrite(bydel, "ssb_bydels02.csv", sep = ";")

## Grunnkrets
grunnkrets <- fread("ssb_grunnkrets.csv", fill = TRUE)
grunnkrets[, kommuneCode := gsub("\\d{4}$", "", code)]
dim(grunnkrets)
setcolorder(grunnkrets, c(9, 1:8))
fwrite(grunnkrets, "ssb_grunnkrets02.csv", sep = ";")
