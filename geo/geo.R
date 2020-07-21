## Convert Geo file from SSB

library(data.table)
library(stringi)
fylke <- fread("ssb_fylke.csv", fill = TRUE)

## Kommune
kommune <- fread("ssb_kommune.csv", fill = TRUE)
kommune[, fylkeCode := as.numeric(gsub("\\d{2}$", "", code))]
## or stri_extract_first_regex(5644, '\\d{2}')
setcolorder(kommune, c(9, 1:8))
setnames(kommune, "code", "kommuneCode")
fwrite(kommune, "ssb_kommune02.csv", sep = ";")

## Bydels
bydel <- fread("ssb_bydels.csv", fill = TRUE)
bydel[, kommuneCode := as.numeric(gsub("\\d{2}$", "", code))]
dim(bydel)
setcolorder(bydel, c(9, 1:8))
setnames(bydel, "code", "bydelCode")
fwrite(bydel, "ssb_bydels02.csv", sep = ";")

## Grunnkrets
grunnkrets <- fread("ssb_grunnkrets.csv", fill = TRUE)
grunnkrets[, kommuneCode := as.numeric(gsub("\\d{4}$", "", code))]
dim(grunnkrets)
setcolorder(grunnkrets, c(9, 1:8))
setnames(grunnkrets, "code", "grunnkretsCode")
fwrite(grunnkrets, "ssb_grunnkrets02.csv", sep = ";")


## Kommune fra grunnkrets som ikke finnes i Kommune tabell
## -------------------------------------------------------
dk <- unique(grunnkrets$kommuneCode)
kk <- unique(kommune$code)
setdiff(dk, kk)
## [1] 1850 2100 5012
nokomm <- c(1850, 2100, 5012)
grunnkrets[kommuneCode %in% nokomm, ]

grunnkrets[name %like% "Tysfjord", ]
