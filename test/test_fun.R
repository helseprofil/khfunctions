## Test Excel function

filnavn <-
  "F:\\Prosjekter\\Kommunehelsa\\PRODUKSJON\\ORGDATA\\STATBANK\\BEFPROGN\\ORG\\2019\\Befolkningsfremskriving_BydelerOslo_5-årigealdersgr_2018,2020,2025,2030,2035,2040_11668.xlsx"

source("khfun_dev.R")

dt <- Xls2R.KH(filnavn)
