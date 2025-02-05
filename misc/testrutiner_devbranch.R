# Før push til github, sjekk at kjøring av følgende kuber gir likt resultat:
# Legg inn path til en fasitfil fra DATERT/R, kjør kube og sammenlign RESULTAT$KUBE med fasit
# Sett samme key og samme kolonnerekkefølge før sammenligning.
  
test_formatting <- function(fasit, ny){
  key <- get_dimension_columns(names(ny))
  data.table::setkeyv(fasit, key)
  data.table::setkeyv(ny, key)
  data.table::setcolorder(fasit, names(ny))
}
  
# HKR
hkr_fasit <- ...
LagKUBE("HKR")
hkr_ny <- RESULTAT$KUBE
test_formatting(hkr_fasit, hkr_ny)
all.equal(hkr_fasit, hkr_ny)

# ENPERSON
enperson_fasit <- ...
LagKUBE("ENPERSON")
enperson_ny <- RESULTAT$KUBE
test_formatting(enperson_fasit, enperson_ny)
all.equal(enperson_fasit, enperson_ny)

# DEPRESJON_UNGDATA
depresjon_ungdata_fasit <- ...
LagKUBE("DEPRESJON_UNGDATA")
depresjon_ungdata_ny <- RESULTAT$KUBE
test_formatting(depresjon_ungdata_fasit, depresjon_ungdata_ny)
all.equal(depresjon_ungdata_fasit, depresjon_ungdata_ny)

# LIVSKVALITET_UNGDATA
livskvalitet_ungdata_fasit <- ...
LagKUBE("LIVSKVALITET_UNGDATA")
livskvalitet_ungdata_ny <- RESULTAT$KUBE
test_formatting(livskvalitet_ungdata_fasit, livskvalitet_ungdata_ny)
all.equal(livskvalitet_ungdata_fasit, livskvalitet_ungdata_ny)

# NEET_INNVKAT
neet_innvkat_fasit <- ...
LagKUBE("NEET_INNVKAT")
neet_innvkat_ny <- RESULTAT$KUBE
test_formatting(neet_innvkat_fasit, neet_innvkat_ny)
all.equal(neet_innvkat_fasit, neet_innvkat_ny)

# SYSVAK
sysvak_fasit <- ...
LagKUBE("SYSVAK")
sysvak_ny <- RESULTAT$KUBE
test_formatting(sysvak_fasit, sysvak_ny)
all.equal(sysvak_fasit, sysvak_ny)

# SYSVAK_1
sysvak_1_fasit <- ...
LagKUBE("SYSVAK_1")
sysvak_1_ny <- RESULTAT$KUBE
test_formatting(sysvak_1_fasit, sysvak_1_ny)
all.equal(sysvak_1_fasit, sysvak_1_ny)



  