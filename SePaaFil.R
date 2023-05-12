# Oppdatert 12.05.2023

# Funksjonene er lastet inn, men husk å restarte prosjektet for å være sikker på at du har siste versjoner.

# GAMMEL VERSJON:
# Må du bruke versjonen av khfunctions før omlegging til allviskube, kan du kommentere ut og kjøre følgende:
# rm(list = ls())
# source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
# kh_source(repo = "khfunctions", branch = "masterpreallvis", file = "KHfunctions.R", encoding = "latin1")

# GJELDENDE VERSJON:
# For å laste inn den gjeldende versjonen av funksjonene skal det holde å restarte prosjektet. 
# Hvis dette ikke skulle fungere, kan du kommentere ut og kjøre følgende:
# rm(list = ls())
# source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
# kh_source(repo = "khfunctions", branch = "master", file = "R/KHsetup.R", encoding = "latin1")

# Laste inn BEF_GKa i buffer, kan være nyttig om man skal lage flere kuber, ellers ikke nødvendig
BUFFER <- list(BEF_GKa = KlargjorFil("BEF_GKa", versjonert = TRUE)$FIL)

# LagFilgruppe
LagFilgruppe("FILGRUPPENAVN", versjonert = TRUE)

# LagKube

## Bare lokalt, lager et objekt som heter RESULTAT, som inneholder full kube, QC-kube og ALLVIS-kube
LagKUBE("KUBENAVN")

## Lagrer daterte CSV-kopier til QC og ALLVIS, ACCESS-specs, og Friskvikfiler
## Lagrer også objektet RESULTAT
LagKubeDatertCsv("KUBENAVN")

# DIVERSE VERKTØY:

# Lage fildumper
# Dump lagres alltid i F:\Forskningsprosjekter\PDB 2455 - Helseprofiler og til_\PRODUKSJON\RUNTIMEDUMP\
# For å lagre dump, lag først objektet "dumps" med formatet dumpnavn = format, som eksempel under. 

dumps <- list(STATAPRIKKpre = "CSV", STATAPRIKKpost = "STATA", RSYNT_POSTPROSESSpost = c("CSV", "STATA"))
dumps <- list() # Dette gir ingen dumper

# Deretter må du i kube- eller filgruppekjøringen bruke følgende
LagKubeDatertCsv("KUBENAVN", dumps = dumps)

# Liste over tilgjengelige dumppunkter

# LagFilgruppe
## RSYNT_PRE_FGLAGRINGpre
## RSYNT_PRE_FGLAGRINGpost
## RESHAPEpre
## RESHAPEpost
## RSYNT2pre
## RSYNT2post
## KODEBOKpre
## KODEBOKpost
## RSYNT1pre
## RSYNT1post

# LagKUBE
## raaKUBE0
## maKUBE0
## anoKUBE1
## anoKUBE2
## anoKUBE3
## anoKUBE4
## KUBE_SLUTTREDIGERpre
## KUBE_SLUTTREDIGERpost
## STATAPRIKKpre
## STATAPRIKKpost # Rett før postprosess, kan brukes som postprosess-pre.
## RSYNT_POSTPROSESSpost


#######################################################
#### HERFRA ER FILEN IKKE OPPDATERT FRA TIDLIGERE #####
#######################################################

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
# Lager en csv-fil med de to datasettene side om side. Du mÃ¥ selv gjÃ¸re sammenlikningen.
# SE OGSÃ… Sammenlikne kuber, og Batch mot batch, nedenfor.
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
## Evt. Ã¥pne denne URL https://fhprofil.shinyapps.io/bat2bat/

## bat2bat::run_app()



## Lage Godkjent-mappe for flatfil- og barometerproduksjonen
## ------------
## profil : FHP eller OVP
## modus : Default er "K" og kan velge "F" (fylke) eller "B" (Bydel)
## aar : default er nÃ¥vÃ¦rende Ã¥rgang, ellers spesifiseres ved bruk aar = 2020

godkjent(profil = "FHP", modus = "F")
