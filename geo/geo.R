## Convert Geo file from SSB

library(data.table)
library(stringi)
library(openxlsx)

getOption("encoding")
## obs! write.csv2 uses NA for missing while fwrite uses NULL
## gives consequence when inporting to ACCESS or Excel
## or uses openxlsx package to create xlsx file

fylke <- fread("ssb_fylke.csv", fill = TRUE)
dim(fylke)
oldName <- c("code", "name")
newName <- c("fylkeCode", "fylkeName")
setnames(fylke, oldName, newName)
fylke[, border := 2020]
openxlsx::write.xlsx(fylke, "fylke.xlsx", colNames = TRUE)


## Kommune
kommune <- fread("ssb_kommune.csv", fill = TRUE)
kommune[, fylkeCode := as.numeric(gsub("\\d{2}$", "", code))]
## or stri_extract_first_regex(5644, '\\d{2}')
setcolorder(kommune, c(9, 1:8))
newName <- c("kommuneCode", "kommuneName")
setnames(kommune, oldName, newName)
kommune[, border := 2020]
dim(kommune)
## fwrite(kommune, "ssb_kommune02.csv", sep = ";")
## write.csv2(kommune, "ssb_kommune03.csv", row.names = FALSE, fileEncoding = "native.enc")
write.xlsx(kommune, "ssb_kommune.xlsx", colNames = TRUE)

## Bydels
bydel <- fread("ssb_bydels.csv", fill = TRUE)
bydel[, kommuneCode := as.numeric(gsub("\\d{2}$", "", code))]
dim(bydel)
setcolorder(bydel, c(9, 1:8))
newName <- c("bydelCode", "bydelName")
setnames(bydel, oldName, newName)
bydel[, border := 2020]
## fwrite(bydel, "ssb_bydels02.csv", sep = ";")
## write.csv2(bydel, "ssb_bydels03.csv", row.names = FALSE, fileEncoding = "Latin1")
write.xlsx(bydel, "ssb_bydel.xlsx", colNames = TRUE)


## Grunnkrets
grunnkrets <- fread("ssb_grunnkrets.csv", fill = TRUE)
grunnkrets[, kommuneCode := as.numeric(gsub("\\d{4}$", "", code))]
## grunnkrets[, fylkeCode := as.numeric(gsub("\\d{2}$", "", kommuneCode))]
dim(grunnkrets)
newName <- c("grunnkretsCode", "grunnkretsName")
setnames(grunnkrets, oldName, newName)
## not included in the SSB data
grunnkrets[, kommuneCode := 9999][, grunnkretsName := "Uoppgitt kommune"]
## setcolorder(grunnkrets, c(10, 9, 1:8))
setcolorder(grunnkrets, c(9, 1:8))
grunnkrets[, border := 2020]
## fwrite(grunnkrets, "ssb_grunnkrets02.csv", sep = ";")
## write.csv2(grunnkrets, "ssb_grunnkrets03.csv", row.names = FALSE, fileEncoding = "native.enc")
write.xlsx(grunnkrets, "ssb_grunnkrets.xlsx", colNames = TRUE)



## Kommune fra grunnkrets som ikke finnes i Kommune tabell
## -------------------------------------------------------
dk <- unique(grunnkrets$kommuneCode)
kk <- unique(kommune$kommuneCode)
setdiff(dk, kk)
## [1] 1850 2100 5012
nokomm <- c(1850, 2100, 5012)
grunnkrets[kommuneCode %in% nokomm, ]

grunnkrets[name %like% "Tysfjord", ]


## Trouble with encoding in Windows and workaround is to open csv in Excel
## then save.as Excel file before importing the Excel file in Access.
