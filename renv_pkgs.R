renv::snapshot()

pkgs <- c(
  "RODBC", "DBI", "foreign", "sas7bdat", "XML", "zoo", "plyr", "sqldf",
  "stringr", "intervals", "data.table", "readxl", "fs"
)

renv::install(pkgs)
renv::install("helseprofil/bat2bat")
