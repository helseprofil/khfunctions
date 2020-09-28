## Check the RDS output from function LagFilgruppe()
## ---------------------------------------------
library(data.table)

rdsPath <- normalizePath("F:\\Prosjekter\\Kommunehelsa\\PRODUKSJON\\PRODUKTER\\MELLOMPROD\\R\\STABLAORG\\DATERT", "/")
rdsNew <- "F:/Prosjekter/Kommunehelsa/PRODUKSJON/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE"

show_rds <- function(rdsPath, rdsFile){

  fileName <- file.path(rdsPath, rdsFile)
  dt <- readRDS(fileName)
  dtNames <- names(dt)
  data.table::setDT(dt)
  rowSelect <- sample(1:nrow(dt), 5)
  subdt <- dt[rowSelect, ]
  geo <- unique(dt$GEOniv)

  list(colnames = dtNames, geo = geo, dt = dt)
}

show_rds(rdsPath, "ELEVUNDER_2020-07-01-16-14.rds")
show_rds(rdsPath, "NEET_IMDI_2020-07-02-10-43.rds")
show_rds(rdsPath, "BEF_INNV_NORSKFODT_2020-06-30-11-59.rds")

dd <- show_rds(rdsPath, "ELEVUNDER_2020-07-01-16-14.rds")
str(dd$subset)

dt <- show_rds(rdsNew, "ELEVUNDER.rds")
## gamle Moss kommunekode 0104 og nykode 3002 - ikke finnes
dt$dt[GEO %like% "3002", ]
dt$dt[GEO %like% "^300", ]
dt$dt[nchar(GEO) == 4 & GEO %in%  grep("^30", GEO, value = TRUE), ]


show_rds(rdsNew, "INNTULIKHET.rds")
