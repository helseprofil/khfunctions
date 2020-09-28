dbtest <- function(test = FALSE){

  if (!require(RODBC)) install.packages("RODBC")
  if (!require(data.table)) install.packages("data.table")

 if( test){
   dbfile <- "C:\\enc\\DBtest\\STYRING\\KHELSA_dev2.accdb"
 } else {
   dbfile <- "F:/Prosjekter/Kommunehelsa/PRODUKSJON/STYRING/KHELSA.mdb"
 }

  orgTb <- "ORIGINALFILER"
  khcon <- RODBC::odbcConnectAccess2007(dbfile)

  orgDT <- setDT(RODBC::sqlFetch(khcon, orgTb))

  head(orgDT)
}

dbtest()
dbtest(TRUE)



## Check ODBC
odbc::odbcListDrivers()



## TEST
library(RODBC)
## Set up driver info and database path
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
MDBPATH <- "C:\\enc\\DBtest\\STYRING\\KHELSA_dev2.accdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

## Establish connection
channel <- odbcDriverConnect(PATH)

## Load data into R dataframe
df <- sqlQuery(channel,
"SELECT [student_id], [first_name], [last_name],
FROM [tbl-students]
ORDER BY [first_name];",
stringsAsFactors = FALSE)

## Close and remove channel
close(channel)
rm(channel)


## With odbc and DBI
pkg <- c("odbc", "DBI")
sapply(pkg, require, character.only = TRUE)

db_con <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq="
MDBPATH <- "C:\\enc\\DBtest\\STYRING\\KHELSA_dev2.accdb"
MDBPATH <- "C:\\enc\\DBtest\\STYRING\\KHELSA_dev.mdb"

cs <- paste0(db_con, MDBPATH)
con <- dbConnect(odbc::odbc(), .connection_string = cs)
