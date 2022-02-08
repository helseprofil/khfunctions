install.packages("renv")


pkgs <- c(
  "RODBC", "DBI", "sas7bdat", "XML", "zoo", "plyr", "sqldf",
  "stringr", "intervals", "data.table", "readxl", "fs", "epitools"
)

renv::install(pkgs)
renv::install("helseprofil/bat2bat")

renv::snapshot()
