#---------------------------------------------------------------------------------------------
# NB: Alle kommandoer under krever at linja under kj?res en gang ved oppstart
rm(list = ls())
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_source(repo = "khfunctions", branch = "iss49", file = "KHfunctions.R", encoding = "latin1")


# Denne må også ofte kjøres ved oppstart. Setter opp ODBC.
KHglobs <- SettGlobs()

BUFFER <- list(BEF_GKa = KlargjorFil("BEF_GKa", versjonert = TRUE)$FIL)
# BUFFER<-list(BEF_GKa=FinnFilT("BEF_GK_Ta"))


#---------------------------------------------------------------------------------------------
#*********************************************************************************************
# Kj?r innlesing:


LagFilgruppe("VOLD_ANMELDTE", versjonert = TRUE)

#*********************************************************************************************
# LAGE EN NY KUBE (datert), med kopi i csv
k <- LagKubeDatertCsv("INNTULIKHET")

# LAGE EN NY KUBE, uten csv-kopi (f.eks. for bare ? f? en fildump - se neste avsnitt)
k <- LagKUBE("RFU_NH_ROYK_5_fildumpdummy")

#*********************************************************************************************
# DIVERSE VERKT?Y:

#---------------------------------------------------------------------------------------------
# Lage runtimedump
# Liste over dumppunkter: F:\Forskningsprosjekter\PDB 2455 - Helseprofiler og til_\PRODUKSJON\DOK\DUMPPUNKTER.docx
# Dump lagres alltid i F:\Forskningsprosjekter\PDB 2455 - Helseprofiler og til_\PRODUKSJON\RUNTIMEDUMP\

dumps <- list() #-> Ingen dump

# Bestille dumppunkter - eksempler
# Prinsipp: -- dumppunkt="type datafil" --
dumps <- list(maKUBE0 = "CSV", RSYNT2pre = c("CSV", "STATA"))
dumps <- list(KUBE_SLUTTREDIGERpost = "STATA")

# Dump i innlesingen
FG <- LagFilgruppe("KEISERSNITT_NH", versjonert = TRUE, dumps = dumps)

# Dump i kubeproduksjonen
k <- LagKUBE("DAAR_RUSTAB_ANTALL", dumps = dumps)

# Du kan ogs? lage dump i alle RSYNT-punkter, ved ? legge inn en "lagre fil"-kommando som RSYNT.

#---------------------------------------------------------------------------------------------

# TIPS OM SPESIALVERSJONER AV LØYPA:

# - Kjøre lokalt, det vil si kopiere styringsdatabasen til egen PC for å slippe
# nettverksproblemer med ODBC.
# Åpne R-script "run-local.R" i samme katalog.

# - Kjøre innlesing av bare én ORGfil, mens man utarbeider innlesingsspec:
# Åpne R-script "run-test.R" i samme katalog
# (eller se https://github.com/helseprofil/khfunctions/blob/master/run-test.R)

#---------------------------------------------------------------------------------------------

# TA BACKUP AV STYRINGSDATABASEN:
backup("KHELSA.mdb")

#---------------------------------------------------------------------------------------------

SammelignAarganger()

# Lese inn R-datafil, f.eks. en filgruppe. Klikk "FG" i Environmentvinduet for ? se den.
FG <- FinnFilT("INNTULIKHET")

# Lagre et datasett fra R-memory til statafil (manuell fildump)
write.dta(FG, paste("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/RUNTIMEDUMP", "/", "GRSKPOENG_NEVNER_filgruppe.dta", sep = ""))
# Daar-fil tok tre-fire minutter ? lagre (17 mill rader).

# TMP n?dl?sning:
TMP <- readRDS("F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/PRODUKTER/MELLOMPROD/R/STABLAORG/DATERT/DODE_GK_2016-01-15-12-44.rds")
rename(TMP, c("DODE_f", "DODE_a", "DODE_n"), c("DODE.f", "DODE.a", "DODE.n"))
saveRDS(TMP, "F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/PRODUKTER/MELLOMPROD/R/STABLAORG/DATERT/DODE_GK_2016-01-15-12-44.rds")


#---------------------------------------------------------------------------------------------
# TMP: valider ?rgang mot ?rgang KH
# Lager en csv-fil med de to datasettene side om side. Du må selv gjøre sammenlikningen.
# SE OGSÅ Sammenlikne kuber, og Batch mot batch, nedenfor.
KUBENAVN <- "Drikkevann" # Bare for output
tabs <- c("INDIKATOR", "NIVA")
# Filnavn for de to ?rgangene. M? v?re csv!  Ta med filtype i navnet.
# Husk \\ eller / som katalogskilletegn.
kube1filnavn <- "F:\\Forskningsprosjekter\\PDB 2455 - Helseprofiler og til_\\PRODUKSJON\\PRODUKTER\\KUBER\\KOMMUNEHELSA\\KH2020NESSTAR\\DRIKKEVANN_2020-01-08-10-38.csv"
kube2filnavn <- "F:\\Forskningsprosjekter\\PDB 2455 - Helseprofiler og til_\\PRODUKSJON\\PRODUKTER\\KUBER\\KOMMUNEHELSA\\KH2021NESSTAR\\DRIKKEVANN_2021-01-11-15-25.csv"

TmpRutineSammenlignKHkuber(kubefilnavn1, kubefilnavn2, KUBENAVN, tabs)

#---------------------------------------------------------------------------------------------
## BUFFERbatch  !!!!!!!

BUFFER[["BEF_GKu"]] <- FinnFilT("BEF_GK_Tu")

#---------------------------------------------------------------------------------------------
# Se n?rmere p? en stablet fil

FG <- FinnFilT("SVANGERROYK")

subset(FG, AARl == 2020 & GEO == "0214" & KJONN == 1)

subset(FG, AARl == 2012 & GEO %in% c("0214", "0215") & KJONN == 1)


#---------------------------------------------------------------------------------------------
# FinneDubletter for filgruppe
DUB <- SjekkDuplikaterFG("BEFOLK", FullResult = TRUE)$DUB

#---------------------------------------------------------------------------------------------
# For ? konvertere alle de StableFilgruppene til hhv csv og stata
# Merk at dette tar ganske lang tid
KonverterStablaFilgrupper(Format = "csv")
KonverterStablaFilgrupper(Format = "stata")
# Samme for kubene
KonverterKUBER(Format = "csv")
KonverterKUBER(Format = "stata")

#---------------------------------------------------------------------------------------------
# FOR ? SAMMENLIGNE EN KUBE MED KH2014
# (Senere erstattet av Boxplot-metoden)
MM <- KH2014v2015("ENEFHIB")
xyplot(RATE14 ~ RATE15, MM)
hist(MM$RATE15 - MM$RATE14)

# Kj?r for alle KUBER, der det er feil i oppsett vil det komme feilmeldinger p? skjerm, men skriptet g?r greit likevel
FullKH2014v2015()


#---------------------------------------------------------------------------------------------
# KJ?r test for alle, se F:\Prosjekter\Kommunehelsa\PRODUKSJON\PRODUKTER\KUBER\KOMMUNEHELSA\LOGG
# Kuber som ikke er satt opp/i bruk vil gi feilmeldinger p? skjerm, ignorer disse
CompNyOgKlarAlle()

TT <- CompNyOgKlar("RRKOLS", streng = FALSE)
TT$checkm

PrintCompCheck(TT)
# require(lattice)
xyplot(RATE_1 ~ RATE_2, TT$V12)
subset(TT$V12, (!is.na(RATE_1) & is.na(RATE_2)) | (is.na(RATE_1) & !is.na(RATE_2)))
xyplot(SMR_1 ~ SMR_2, TT$V12)


subset(TT$V12, (!is.na(SMR_1) & is.na(SMR_2)) | (is.na(SMR_1) & !is.na(SMR_2)))


#---------------------------------------------------------------------------------------------

KHglobs <- SettGlobs()
KHglobs$dbh

#---------------------------------------------------------------------------------------------
# Sammenlikne kuber
KHglobs$KubeDirDat <- KHglobs$KubeDirDat_KH
KUBEid <- "TRIVSEL"
BATCHDATE1 <- "gammelt batchnummer"
BATCHDATE2 <- "nytt batchnummer"

V1 <- FinnDatertKube(KUBEid, batch = BATCHDATE1)
V2 <- FinnDatertKube(KUBEid, batch = BATCHDATE2)
comp <- SammenlignKuber(V1, V2)

Comparefile <- "F:/Prosjekter/Kommunehelsa/????????????"
write.table(comp$V12, file = Comparefile, sep = ";", na = "", row.names = FALSE)


## Batch mot Batch
## ----------------
## Evt. åpne denne URL https://fhprofil.shinyapps.io/bat2bat/

## bat2bat::run_app()



## Lage Godkjent-mappe for flatfil- og barometerproduksjonen
## ------------
## profil : FHP eller OVP
## modus : Default er "K" og kan velge "F" (fylke) eller "B" (Bydel)
## aar : default er nåværende årgang, ellers spesifiseres ved bruk aar = 2020

godkjent(profil = "FHP", modus = "F")
