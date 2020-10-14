test <- function(){

  if (!require(RODBC)) install.packages("RODBC")
  if (!require(data.table)) install.packages("data.table")
  dbfile <- "F:/Prosjekter/Kommunehelsa/PRODUKSJON/STYRING/KHELSA.mdb"

  orgTb <- "ORIGINALFILER"
  khcon <- RODBC::odbcConnectAccess2007(dbfile)

  orgDT <- setDT(RODBC::sqlFetch(khcon, orgTb))

  head(orgDT)
}

test()
