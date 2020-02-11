#---------------------------------------------------------------------------------------------
#NB: Alle kommandoer under krever at linja under kj?res en gang ved oppstart
rm(list = ls())
#source('F:/Prosjekter/Kommunehelsa/PRODUKSJON/BIN/KHfunctions_20190927.r')
#source('F:/Prosjekter/Kommunehelsa/PRODUKSJON/BIN/KHfunctions_20190927_HREG81-84.r')
#source('F:/Prosjekter/Kommunehelsa/PRODUKSJON/BIN/KHfunctions_20191219.r')  #krever gamle pakker
source('F:/Prosjekter/Kommunehelsa/PRODUKSJON/BIN/KHfunctions_20200103.r')  #krever nye pakker

BUFFER<-list(BEF_GKa=KlargjorFil("BEF_GKa",versjonert=TRUE)$FIL)
#BUFFER<-list(BEF_GKa=FinnFilT("BEF_GK_Ta"))

#Denne m? ogs? ofte kj?res ved oppstart. Setter opp ODBC.
KHglobs<-SettGlobs()

#---------------------------------------------------------------------------------------------
# UTDATERT H-2019
    # FOR ? F? kuber standardisert med FAST standardbefolkning (NH), source i stedet dette:
      # OBS: Filgrupper kan ikke kj?res med dette, de m? lages p? forh?nd med det vanlige scriptet.
    #rm(list = ls())
    #source('F:/Prosjekter/Kommunehelsa/PRODUKSJON/BIN/KHfunctionsD.r')

#---------------------------------------------------------------------------------------------
#*********************************************************************************************
#Kj?r innlesing:


LagFilgruppe("FYSAK_UNGDATA",versjonert=TRUE)

#*********************************************************************************************
#LAGE EN NY KUBE (datert), med kopi i csv
k<-LagKubeDatertCsv("INNTULIKHET")

#LAGE EN NY KUBE, uten csv-kopi (f.eks. for bare ? f? en fildump - se neste avsnitt)
k<-LagKUBE("RFU_NH_ROYK_5_fildumpdummy")

#*********************************************************************************************
#DIVERSE VERKT?Y:

#---------------------------------------------------------------------------------------------
#Lage runtimedump
#Liste over dumppunkter: F:\Prosjekter\Kommunehelsa\PRODUKSJON\DOK\DUMPPUNKTER.docx
#Dump lagres alltid i F:\Prosjekter\Kommunehelsa\PRODUKSJON\RUNTIMEDUMP\

dumps=list() #-> Ingen dump

  #Bestille dumppunkter - eksempler
  # Prinsipp: -- dumppunkt="type datafil" --
  dumps=list(maKUBE0="CSV",RSYNT2pre=c("CSV","STATA"))
  dumps=list(KUBE_SLUTTREDIGERpost="STATA")
  
#Dump i innlesingen
FG<-LagFilgruppe("KEISERSNITT_NH",versjonert=TRUE,dumps=dumps)

#Dump i kubeproduksjonen
k <- LagKUBE("DAAR_RUSTAB_ANTALL",dumps=dumps)

#Du kan ogs? lage dump i alle RSYNT-punkter, ved ? legge inn en "lagre fil"-kommando som RSYNT.

#---------------------------------------------------------------------------------------------
#TA BACKUP av skript og KHELSA.mdb (sjekker om behov)
source('F:/Prosjekter/Kommunehelsa/PRODUKSJON/BIN/auto_backup_KH.r')

#---------------------------------------------------------------------------------------------

SammelignAarganger()

#Lese inn R-datafil, f.eks. en filgruppe. Klikk "FG" i Environmentvinduet for ? se den.
FG <- FinnFilT("INNTULIKHET")

#Lagre et datasett fra R-memory til statafil (manuell fildump)
write.dta(FG,paste("F:/Prosjekter/Kommunehelsa/PRODUKSJON/RUNTIMEDUMP","/","DAAR_GK_filgruppe.dta",sep=""))
#Daar-fil tok tre-fire minutter ? lagre (17 mill rader).

#TMP n?dl?sning:
TMP<-readRDS("F:/Prosjekter/Kommunehelsa/PRODUKSJON/PRODUKTER/MELLOMPROD/R/STABLAORG/DATERT/DODE_GK_2016-01-15-12-44.rds")
rename(TMP,c("DODE_f","DODE_a","DODE_n"),c("DODE.f","DODE.a","DODE.n"))
saveRDS(TMP,"F:/Prosjekter/Kommunehelsa/PRODUKSJON/PRODUKTER/MELLOMPROD/R/STABLAORG/DATERT/DODE_GK_2016-01-15-12-44.rds")


#---------------------------------------------------------------------------------------------
#TMP: valider ?rgang mot ?rgang KH
KUBENAVN<-"SPEDBARNDOD_NH"  #Bare for output
tabs<-c("DODSTIDSPKT")
#Filnavn for de to ?rgangene. M? v?re csv!
kube1filnavn<-"F:/Prosjekter/Kommunehelsa/PRODUKSJON/PRODUKTER/KUBER/NORGESHELSA/NH2017NESSTAR/SPEDBARNDOD_NH_2017-09-18-11-09.csv"
kube2filnavn<-"F:/Prosjekter/Kommunehelsa/PRODUKSJON/PRODUKTER/KUBER/NORGESHELSA/NH2018NESSTAR/SPEDBARNDOD_NH_2018-04-09-15-45.csv"

TmpRutineSammenlignKHkuber(kubefilnavn1,kubefilnavn2,KUBENAVN,tabs)

#---------------------------------------------------------------------------------------------
BUFFERbatch  !!!!!!!

BUFFER[["BEF_GKu"]]<-FinnFilT("BEF_GK_Tu")

#---------------------------------------------------------------------------------------------
#Se n?rmere p? en stablet fil

FG<-FinnFilT("SVANGERROYK")

subset(FG,AARl==2020 & GEO=="0214" & KJONN==1)

subset(FG,AARl==2012 & GEO %in% c("0214","0215") & KJONN==1)


#---------------------------------------------------------------------------------------------
#FinneDubletter for filgruppe
DUB<-SjekkDuplikaterFG("BEFOLK",FullResult=TRUE)$DUB

#---------------------------------------------------------------------------------------------
#For ? konvertere alle de StableFilgruppene til hhv csv og stata
#Merk at dette tar ganske lang tid
KonverterStablaFilgrupper(Format="csv")
KonverterStablaFilgrupper(Format="stata")
#Samme for kubene
KonverterKUBER(Format="csv")
KonverterKUBER(Format="stata")

#---------------------------------------------------------------------------------------------
#FOR ? SAMMENLIGNE EN KUBE MED KH2014
# (Senere erstattet av Boxplot-metoden)
MM<-KH2014v2015("ENEFHIB")
xyplot(RATE14~RATE15,MM)
hist(MM$RATE15-MM$RATE14)

#Kj?r for alle KUBER, der det er feil i oppsett vil det komme feilmeldinger p? skjerm, men skriptet g?r greit likevel
FullKH2014v2015()


#---------------------------------------------------------------------------------------------
#KJ?r test for alle, se F:\Prosjekter\Kommunehelsa\PRODUKSJON\PRODUKTER\KUBER\KOMMUNEHELSA\LOGG
#Kuber som ikke er satt opp/i bruk vil gi feilmeldinger p? skjerm, ignorer disse
CompNyOgKlarAlle()

TT<-CompNyOgKlar("RRKOLS",streng=FALSE)
TT$checkm

PrintCompCheck(TT)
#require(lattice)
xyplot(RATE_1~RATE_2,TT$V12)
subset(TT$V12,(!is.na(RATE_1) & is.na(RATE_2)) | (is.na(RATE_1) & !is.na(RATE_2)))
xyplot(SMR_1~SMR_2,TT$V12)


subset(TT$V12,(!is.na(SMR_1) & is.na(SMR_2)) | (is.na(SMR_1) & !is.na(SMR_2)))


#---------------------------------------------------------------------------------------------

KHglobs<-SettGlobs()
KHglobs$dbh
  
#---------------------------------------------------------------------------------------------
#Sammenlikne kuber
KHglobs$KubeDirDat<-KHglobs$KubeDirDat_KH
KUBEid<-"TRIVSEL"
BATCHDATE1<-"gammelt batchnummer"
BATCHDATE2<-"nytt batchnummer"

V1<-FinnDatertKube(KUBEid,batch=BATCHDATE1)
V2<-FinnDatertKube(KUBEid,batch=BATCHDATE2)
comp<-SammenlignKuber(V1,V2)

Comparefile<-"F:/Prosjekter/Kommunehelsa/????????????"
write.table(comp$V12,file=Comparefile,sep=';',na="",row.names = FALSE)




