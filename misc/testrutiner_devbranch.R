# Før push til github, sjekk at kjøring av følgende kuber gir likt resultat:
# Legg inn path til en fasitfil fra DATERT/R, kjør kube og sammenlign RESULTAT$KUBE med fasit
# Sett samme key og samme kolonnerekkefølge før sammenligning.
  
test_formatting <- function(fasit, ny){
  key <- get_dimension_columns(names(ny))
  data.table::setkeyv(fasit, key)
  data.table::setkeyv(ny, key)
  data.table::setcolorder(fasit, names(ny))
}
  
# FORSVARET_SVOMMMING
LagKUBE("FORSVARET_SVOMMING_3")
forsvaret_svomming_3_fasit <- readRDS("O:/Prosjekt/FHP/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/R/FORSVARET_SVOMMING_3_2025-02-24-10-51.rds")
forsvaret_svomming_3_ny <- RESULTAT$KUBE
test_formatting(forsvaret_svomming_3_fasit, forsvaret_svomming_3_ny)
all.equal(forsvaret_svomming_3_fasit, forsvaret_svomming_3_ny)
# OK 18.03.2025 - før standardisering til periode

# FORSVARET_SVOMMMING
LagKUBE("FORSVARET_SVOMMING_1")
forsvaret_svomming_fasit <- readRDS("O:/Prosjekt/FHP/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/R/FORSVARET_SVOMMING_1_2025-02-24-10-52.rds")
forsvaret_svomming_ny <- RESULTAT$KUBE
test_formatting(forsvaret_svomming_fasit, forsvaret_svomming_ny)
all.equal(forsvaret_svomming_fasit, forsvaret_svomming_ny)
# OK 20.03.2025

# BARNEVERN_TILTAK
barnevern_tiltak_fasit <- readRDS("O:/Prosjekt/FHP/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/R/BARNEVERN_TILTAK_2025-03-17-14-52.rds")
LagKUBE("BARNEVERN_TILTAK")
barnevern_tiltak_ny <- RESULTAT$KUBE
test_formatting(barnevern_tiltak_fasit, barnevern_tiltak_ny)
all.equal(barnevern_tiltak_fasit, barnevern_tiltak_ny)
# OK 18.03.2025

# DEPRESJON_UNGDATA
depresjon_ungdata_fasit <- readRDS("O:/Prosjekt/FHP/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/R/DEPRESJON_UNGDATA_2025-02-24-09-54.rds")
LagKUBE("DEPRESJON_UNGDATA")
depresjon_ungdata_ny <- RESULTAT$KUBE
test_formatting(depresjon_ungdata_fasit, depresjon_ungdata_ny)
all.equal(depresjon_ungdata_fasit, depresjon_ungdata_ny)

# LIVSKVALITET_UNGDATA
livskvalitet_ungdata_fasit <- readRDS("O:/Prosjekt/FHP/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/R/LIVSKVALITET_UNGDATA_2025-02-24-12-44.rds")
LagKUBE("LIVSKVALITET_UNGDATA")
livskvalitet_ungdata_ny <- RESULTAT$KUBE
test_formatting(livskvalitet_ungdata_fasit, livskvalitet_ungdata_ny)
all.equal(livskvalitet_ungdata_fasit, livskvalitet_ungdata_ny)

# NEET_INNVKAT
neet_innvkat_fasit <- readRDS("O:/Prosjekt/FHP/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/R/NEET_INNVKAT_2025-02-11-09-28.rds")
LagKUBE("NEET_INNVKAT")
neet_innvkat_ny <- RESULTAT$KUBE
test_formatting(neet_innvkat_fasit, neet_innvkat_ny)
all.equal(neet_innvkat_fasit, neet_innvkat_ny)

# SYSVAK
LagKUBE("SYSVAK")
sysvak_fasit <- readRDS("O:/Prosjekt/FHP/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/R/SYSVAK_2025-03-06-11-37.rds")
sysvak_ny <- RESULTAT$KUBE
test_formatting(sysvak_fasit, sysvak_ny)
all.equal(sysvak_fasit, sysvak_ny)
# OK 18.03.2025

# SYSVAK_1
sysvak_1_fasit <- readRDS("O:/Prosjekt/FHP/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/R/SYSVAK_1_2025-03-06-07-25.rds")
LagKUBE("SYSVAK_1")
sysvak_1_ny <- RESULTAT$KUBE
test_formatting(sysvak_1_fasit, sysvak_1_ny)
all.equal(sysvak_1_fasit, sysvak_1_ny)
# OK 18.03.2025

## TESTE KJØRETID, gjøres for hhv master branch og develop.

KUBER <- c("ENPERSON", "DEPRESJON_UNGDATA", "LIVSKVLAITET_UNGDATA", "NEED_INNVKAT", "SYSVAK", "SYSVAK_1", "FORSVARET_SVOMMING")
tider <- list()
for(i in KUBER){
  tider[[i]] <- system.time(LagKUBE(i))[["elapsed"]]
}
  