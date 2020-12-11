#################################################################
# ENDRINGSLOGG
##########################################################
#20190927:
# Endret 1601->5001 og 1902->5401 i linje 1591 
# FIL[GEOniv_omk=="B" & GEOniv=="S" & !grepl("^(0301|1103|4601|5401)",GEO),c("GEO","FYLKE"):=list("999999","99")]
# Dette er forstatt en meget dårlig løsning. Bør ikke være hardkodet!
# Har lagt inn det jeg tror er løsningen, men ikke slått på denne nå.
# 
# Lagt til/endret filter for å luke vekk geokoder som ikke skal rapporteres (ca 3324)
# Bruker attributtet TYP som filter
# Lagt til UtGeoKoder=GeoKoder[TYP=="0"]$GEO i globs
# KUBE<-KUBE[GEO %in% globs$UtGeoKoder[TYP=="0"]$GEO]
# NESSTAR<-NESSTAR[GEO %in% globs$UtGeoKoder[TYP=="0"]$GEO]

#20190827:
#delkols<-globs$DefDesign$DelKols[[del]] flyttet opp til linje 5095 i FinnRedesign
#Fjernet unntak for D_develop_predtype=="DIR" samme sted
#Her var/er det en del rot. Unntaket var egentlig en oppretting av at ting gikk feil over
#Så vidt jeg skjønner skal det være delkols<-globs$DefDesign$DelKols[[del]] gjennom hele FinnRedsign. 
#Det er forstatt for mye dilldall rundt det



#20190821:
# defpaths flyttet opp for synlighet
# Xls2R.KH endret: readxl og read_excel erstatter gammel Xls2TmpCsv.  
# Dirty løsning for integrereing av Direkte standardisering tatt inn
#   D_develop_predtype="IND" er gammel hovedløsning med indirekte standardisering
#   D_develop_predtype="DIR" er quick fix for direkte standardisering
#   Se kommentarer på enkeltpunktene for mer antydninger om hva som må gjøres for å fikse dette mer solid


#20190627:
# GEOstdAAR=globs$KHaargang i RektangulariserKUBE og KNRHarm
# TO-DO: Legg til feilmelding om GEO som kastes ved KNRharm!!!


#version 0.2.4
#Flere viktige oppstramminger/feilretting i detaljreging. Særlig i FinnSumOverAar 
#og behandling av VAL.f=9 og VAL.f=-1 
#Naboprikking (nå VAL.f=4) leder ikke til skjuling
#NYEKOL_RAD_postMA for variabler av type dekningsgrad som trengs å tas etter årssnitt (blir snitt/snitt)

#0.2.3.1: Generalisert NaboAno til betinget



require(RODBC)  #Brukes for kommunikasjon med Access-tabeller og lesing av xls/xlsx
require(DBI)
require(foreign) #Brukes ved lesing av SPSS, dBF
#require(gdata)  #Brukes ved lesing av xls/xlsx filer
require(sas7bdat) #brukes ved lesing av SAS filer
require(XML)
## require(reshape2)  #melt brukes til wide->long
require(zoo)  #na.locf for ? sette inn for NA i innrykket originaltabulering
require(plyr)  #mapvalues for omkoding
require(sqldf)
require(stringr)
require(intervals)
require(data.table) #Bruker data.table for rask merge
require(readxl)
require(fs)
require(bat2bat) #https://github.com/helseprofil/bat2bat

if (!exists("runtest")) runtest = FALSE
testfiles = NULL

#Brukte pather under utvikling (NB: prioritert rekkefølge under)
defpaths<-c(
  "F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON", 
  ## "F:/Prosjekter/Kommunehelsa/PRODUKSJON",
  "F:/Prosjekter/Kommunehelsa/PRODUKSJON/DEVELOP",
  "F:/Prosjekter/Kommunehelsa/Data og databehandling/kbDEV",
  "J:/FHI/PRODUKSJON",
  "J:/kbDEV")

## This is were default path and files are from the previous system.
## rawPath is where all raw data are located
rawPath <- "F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON"
dbNameFile = "STYRING/KHELSA.mdb"
dbLogFile = "STYRING/KHlogg.mdb"

## TEST MODUS
## Change path and dbFile if specified globally
## use in testmodus or local run
if (exists("testpath")) defpaths = testpath
if (runtest && !exists("testdb")) stop("Test Access file not found. Specify with: testdb = 'FileName.mdb'")

if (runtest) {
  dbNameFile = testdb
  
  dbLogFile = "KHlogg.mdb"
  noLog <- fs::file_exists(file.path(defpaths, dbLogFile))
}

if (runtest && isFALSE(noLog)) stop("Finnes ikke KHlogg.mdb fil i ", defpaths)


## RUN LOCAL
## This is needed in run_local function
if (!exists("setLocal")) setLocal = FALSE
if ((setLocal)) {
  defpaths = setLocalPath
  dbNameFile = setDBFile
  dbLogFile = setLogFile
}


#GLOBAL FIXED PARAMETERS, leses bare av SettGlobs, bakes så inn i globs
#Merk at alle elementer angitt i denne lista vil være tilgjengelig i alle hovedrutiner, og evt (mindre robust) i KHglobs
#globsglobs<-list(....)
############################################## ----
globglobs<-list(
  HOVEDmodus="NH",
  KHaargang=2021,
  KHgeoniv="K",
  KHdbname = dbNameFile,
  ## KHdbname="STYRING/KHELSA.mdb",
  KHlogg = dbLogFile, 
  ## KHlogg="STYRING/KHlogg.mdb",
  StablaDir="PRODUKTER/MELLOMPROD/R/STABLAORG/",
  StablaDirNy="PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE",
  StablaDirDat="PRODUKTER/MELLOMPROD/R/STABLAORG/DATERT",
  KubeDir_NH="PRODUKTER/KUBER/NORGESHELSA/",
  KubeDirNy_NH="PRODUKTER/KUBER/NORGESHELSA/NYESTE/R/",
  KubeDirDat_NH="PRODUKTER/KUBER/NORGESHELSA/DATERT/R/",
  KubeDir_KH="PRODUKTER/KUBER/KOMMUNEHELSA/",
  KubeDirNy_KH="PRODUKTER/KUBER/KOMMUNEHELSA/NYESTE/R/",
  KubeDirDat_KH="PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/R/",
  FriskVDir_F="PRODUKTER/KUBER/FRISKVIK_FYLKE/",
  FriskVDir_K="PRODUKTER/KUBER/FRISKVIK_KOMM/",
  FriskVDir_B="PRODUKTER/KUBER/FRISKVIK_BYDEL/",
  ovpDir_F="PRODUKTER/KUBER/OVP_FYLKE/",
  ovpDir_K="PRODUKTER/KUBER/OVP_KOMM/",
  ovpDir_B="PRODUKTER/KUBER/OVP_BYDEL/",
  TNPDirNy="PRODUKTER/MELLOMPROD/R/TNP/NYESTE",
  TNPDirDat="PRODUKTER/MELLOMPROD/R/TNP/DATERT",
  BUFFERdir="BIN/BUFFER",
  DUMPdir="RUNTIMEDUMP",
  kolorgs=c("GEO","AAR","KJONN","ALDER","UTDANN","SIVST","LANDBAK","TAB1","TAB2","TAB3","VAL1","VAL2","VAL3"),
  taborgs=c("GEO","AAR","KJONN","ALDER","TAB1","TAB2","TAB3"),
  NesstarOutputDef=c(MT="MALTALL",T="TELLER",N="NEVNER",RATE="RATE",SMR="SMR",MEIS="MEIS",ST="sumTELLER",SN="sumNEVNER",SPT="sumPREDTELLER",RN="RATE.n"),
  FriskvikTabs=c("GEO","AAR","KJONN","ALDER","ETAB"),
  FriskvikVals=c("sumTELLER","sumNEVNER","RATE","MALTALL","sumPREDTELLER","PREDTELLER","SMR","NORM","MEIS","RATE.n"),
  KubeKols=c("sumTELLER","sumNEVNER","RATE","MALTALL","sumPREDTELLER","PREDTELLER","SMR","NORM","MEIS","RATE.n","ALDER","AAR","SMRtmp"),
  #DesignKols=c("GEOniv","AARl","AARh","KJONN","ALDERl","ALDERh","UTDANN","SIVST","LANDBAK","TAB1","TAB2","TAB3"),
  #OmkKols=c("GEOniv","AARl","AARh","KJONN","ALDERl","ALDERh","UTDANN","SIVST","LANDBAK"),
  #TabKols=c("AARl","AARh","GEOniv","ALDERl","ALDERh","KJONN","UTDANN","SIVST","LANDBAK","GEO","FYLKE","TAB1","TAB2","TAB3"),
  binDir="bin",
  tmpfilerpath="bin\tmpfiler",
  geo_illeg="GGG",
  alder_illeg="888_888",
  alder_ukjent="999_999",
  kjonn_illeg="8",
  kjonn_ukjent="9",
  aar_illeg="8888_8888",
  utdann_illeg="8",
  utdann_ukjent="9",
  landbak_illeg="8",
  landbak_ukjent="9",
  sivst_illeg="8",
  sivst_ukjent="9",
  SisteBatch="9999-01-01-01-01",
  DefDumpFormat="CSV",
  stjstr="************************************************************\n",
  XLScols=as.vector(sapply(c("",as.vector(paste(sapply(c("",LETTERS[]),paste,LETTERS[],sep="")))),paste,LETTERS[],sep=""))
)




#Setter standard designegenskaper, slik som delenes kolonnenavn og status i omkoding
#Se tabell KH_DELER
SettDefDesignKH<-function(globs=FinnGlobs()){
  Deler<-sqlQuery(globs$dbh,"SELECT * FROM KH_DELER",as.is=TRUE,stringsAsFactors=FALSE)
  #DelKols<-lapply(as.list(setNames(Deler$DelKols, Deler$DEL)),function(x){unlist(str_split(x,pattern=","))})
  #Tilrettelegging for enkle oppslag:
  DelKolN<-setNames(Deler$DelKol,Deler$DEL)
  DelKolE<-setNames(Deler$DelKolE,Deler$DEL)
  DelType<-setNames(Deler$TYPE,Deler$DEL)
  DelFormat<-setNames(Deler$FORMAT,Deler$DEL)
  AggPri<-Deler$DEL[order(Deler$AGGREGERPRI)]
  AggVedStand<-Deler$DEL[Deler$AGGREGERvedPRED==1]
  IntervallHull<-setNames(Deler$INTERVALLHULL,Deler$DEL)
  IntervallHull<-IntervallHull[!(is.na(IntervallHull) | IntervallHull=="")]
  
  DelKols<-as.list(DelKolN)
  DelKolsF<-DelKols
  KolsDel<-list()
  for (del in names(DelKols)){
    if (DelType[del]=="INT"){
      DelKols[[del]]<-paste(DelKols[[del]],c("l","h"),sep="")
      DelKolsF[[del]]<-DelKols[[del]]
    }
    if (!(is.na(DelKolE[[del]]) | DelKolE[[del]]=="")){
      DelKolsF[[del]]<-c(DelKolsF[[del]],unlist(str_split(DelKolE[[del]],",")))
    }
    for (kol in DelKols[[del]]){
      KolsDel[[kol]]<-del
    }
  }
  
  
  UBeting<-Deler$DEL[Deler$OMKODbet=="U"]
  BetingOmk<-Deler$DEL[Deler$OMKODbet=="B"]
  BetingF<-Deler$DEL[Deler$OMKODbet=="F"]
  OmkDel<-c(UBeting,BetingOmk)
  #IntervallHull<-list(A="DekkInt/TotInt>0.999 | (NTOT>=10 & NHAR/NTOT>0.8) | (TotInt<=20 & DekkInt>=10) | TotInt<=10")
  
  DesignKols<-c(unlist(DelKols[c(UBeting,BetingOmk)]))
  DesignKolsF<-c(DesignKols,unlist(DelKols[BetingF]))
  DesignKolsFA<-c(DesignKolsF,setdiff(unlist(DelKolsF[c(UBeting,BetingOmk)]),unlist(DelKols[c(UBeting,BetingOmk)])))
  
  
  
  return(
    list(DelKols=DelKols,
         DelKolsF=DelKolsF,
         KolsDel=KolsDel,
         DelKolN=DelKolN,
         DelType=DelType,
         DelFormat=DelFormat,
         UBeting=UBeting,
         BetingOmk=BetingOmk,
         BetingF=BetingF,
         OmkDel=OmkDel,
         DesignKols=DesignKols,
         DesignKolsF=DesignKolsF,
         DesignKolsFA=DesignKolsFA,
         AggPri=AggPri,
         AggVedStand=AggVedStand,
         IntervallHull=IntervallHull,
         AMissAllow=TRUE
         )
  )
}

SettKodeBokGlob<-function(globs=FinnGlobs()){
  
  OmkodD<-sqlQuery(globs$dbh,"SELECT * FROM KH_OMKOD 
                            UNION SELECT ID, DEL, KODE as NYKODE, KODE as ORGKODE, 0 as PRI_OMKOD, 1 AS OBLIG FROM KH_KODER",as.is=TRUE,stringsAsFactors=FALSE)
  KB<-list()  
  
  for (del in names(globs$DefDesign$DelKolN)){
    KBD<-subset(OmkodD,DEL==del)
    if (globs$DefDesign$DelType[del]=="INT"){
      if (nrow(KBD)>0){
        KBD[,c("ORGKODEl","ORGKODEh","NYKODEl","NYKODEh")]<-as.integer(NA)
      } else {
        KBD<-cbind(KBD,data.frame(ORGKODEl=integer(0),ORGKODEh=integer(0),NYKODEl=integer(0),NYKODEh=integer(0)))
      }
    } else if (globs$DefDesign$DelFormat[del]=="integer"){
      KBD$ORGKODE<-as.integer(KBD$ORGKODE)
      KBD$NYKODE<-as.integer(KBD$NYKODE)
    } 
    kbdnames<-names(KBD)
    kbdnames<-gsub("ORGKODE",globs$DefDesign$DelKolN[del],kbdnames)
    kbdnames<-gsub("NYKODE(h|l|)",paste(globs$DefDesign$DelKolN[del],"\\1_omk",sep=""),kbdnames)
    kbdnames<-gsub("NYKODE(h|l|)",paste(globs$DefDesign$DelKolN[del],"\\1_omk",sep=""),kbdnames)
    kbdnames<-gsub("PRI_OMKOD",paste(del,"_pri",sep=""),kbdnames)
    kbdnames<-gsub("OBLIG",paste(del,"_obl",sep=""),kbdnames)
    setnames(KBD,names(KBD),kbdnames)
    KB[[del]]<-KBD[,names(KBD)[!names(KBD) %in% c("ID","DEL")]]
  }
  return(KB)
}

SettLegitimeKoder<-function(globs=FinnGlobs()){
  Koder<-sqlQuery(globs$dbh,"SELECT * FROM KH_KODER",as.is=TRUE,stringsAsFactors=FALSE)
  KodeL<-list()
  for (del in unique(Koder$DEL)){
    KodeD<-subset(Koder,DEL==del)
    if (globs$DefDesign$DelType[del]=="INT"){
      KodeD<-cbind(KodeD,setNames(matrix(as.integer(str_split_fixed(KodeD$KODE,"_",2)),ncol=2),globs$DefDesign$DelKols[[del]]))
    }
    else if (globs$DefDesign$DelFormat[del]=="integer"){
      KodeD<-setNames(cbind(KodeD,as.integer(KodeD$KODE)),c(names(KodeD),globs$DefDesign$DelKols[[del]]))
    }
    else if (globs$DefDesign$DelFormat[del]=="character"){
      KodeD<-setNames(cbind(KodeD,KodeD$KODE),c(names(KodeD),globs$DefDesign$DelKols[[del]]))
    }
    KodeL[[del]]<-KodeD
  }
  return(KodeL)
}

SettTotalKoder<-function(globs=FinnGlobs()){
  Koder<-sqlQuery(globs$dbh,"SELECT KH_KODER.DEL,KODE, FORMAT FROM KH_KODER INNER JOIN KH_DELER ON KH_KODER.DEL=KH_DELER.DEL WHERE TOTAL=1",as.is=TRUE,stringsAsFactors=FALSE)
  TotKoder<-list()
  for (del in Koder$DEL){
    if (Koder$FORMAT[Koder$DEL==del]=="integer"){
      TotKoder[[del]]<-as.integer(Koder$KODE[Koder$DEL==del])
    } else {
      TotKoder[[del]]<-Koder$KODE[Koder$DEL==del]
    }
  }
  return(TotKoder)
}


## FinnStataExe<-function (prior=15:11,PFpath="C:\\Program Files (x86)"){
##   Exe<-""
##   Vers<-0
##   i<-1
##   while (Exe=="" & i<=length(prior)){
##     tmpExe<-paste(PFpath,"\\Stata",prior[i],"\\StataSE-64.exe",sep="")
##     if (file.exists(tmpExe)){
##       Exe<-tmpExe
##       Vers<-prior[i]
##     }
##     i<-i+1
##   }
##   #Exe<-gsub("\\","\\\\",Exe)
##   return(list(Exe=Exe,Vers=Vers))
## }



FinnStataExe <- function(){

  stata_bin <- "StataSE-64.exe"

  program_path <- c("C:/Program Files/", "C:/Program Files (x86)/")

  stata_prog <- grep("Stata", fs::dir_ls(program_path), value = TRUE)

  stata_ver <- stringi::stri_extract(stata_prog, regex="\\d{2}$")
  Vers <- max(as.numeric(stata_ver))

  stata_path <- grep(Vers, stata_prog, value=TRUE)
  Exe <- file.path(stata_path, stata_bin)

  return(list(Exe = Exe, Vers = Vers))
}



#GAMMEL, UTGÅTT
SettKodeBokGn<-function(){
  GnOmk<-list(
    G=c("G"),
    S=c("S","G"),
    K=c("K","S","G"),
    F=c("F","K","S","G"),
    L=c("L","F","K","S","G"),
    B=c("B","S","G")  
  )
  KBGn<-data.frame("GEOniv"=character(0),"GEOniv_omk"=character(0),"Gn_ok"=integer(0),"Gn_pri"=integer(0))
  for (omk in names(GnOmk)){
    KBGn<-rbind(KBGn,data.frame("GEOniv"=GnOmk[[omk]],"GEOniv_omk"=omk,"Gn_ok"=1,"Gn_pri"=1:length(GnOmk[[omk]])))
  }
  return(KBGn)
}

############################################################1
# TO-DO: ----
#
#  SPESIALBEHANDLING AV FILGRUPPE i LagFilgruppe (til slutt) F.EKS. IMPUTER NPR
#        evt også mulighet for å kjøre en STATA .do der (bare navn på do er parameter). Skriv til tmp STATA fil, kjør do, les inn i R. Endelig lagring skjer etterpå, som vanlig
#
#   
#
#
#
#
#
#

##########################################################1
#TRINN 0: INITIERING
#         
##########################################################1

#
SettGlobs<-function(path="",modus=NA,gibeskjed=FALSE) {
  #Setter globale parametre
  #Disse er faste over hver kjøring og endres normalt bare ved systemoppdatering/-migrering
  #Merk at globs$dbh ikke lukkes av seg selv, dermed kan det bli rot med gamle slike om FinnGlobs brukes for mye
  #Bruk evy odbcCloseAll() for å rydde absolutt alle 
  
  #Les globglobs (se topp av fil)
  globs<-globglobs
  if (is.na(modus)){
    modus=globs$HOVEDmodus
  }
  
  if (modus=="KH"){
    globs$KubeDir<-globs$KubeDir_KH
    globs$KubeDirNy<-globs$KubeDirNy_KH
    globs$KubeDirDat<-globs$KubeDirDat_KH
    globs$FriskVDir<-globs$FriskVDir_KH
  } else {
    globs$KubeDir<-globs$KubeDir_NH
    globs$KubeDirNy<-globs$KubeDirNy_NH
    globs$KubeDirDat<-globs$KubeDirDat_NH
    globs$FriskVDir<-globs$FriskVDir_NH
  }
  KHdbname<-globs$KHdbname
  #Sett path om denne ikker er oppgitt:
  
  
  if (path==""){
    if(file.exists(paste(getwd(),KHdbname,sep="/"))){
      path<-getwd()      
      if (gibeskjed==TRUE){cat("Setter path=",path,"fra getwd()\n")}
    } else {
      i<-1
      while (path=="" & i<=length(defpaths)){
        if(file.exists(paste(defpaths[i],KHdbname,sep="/"))){
          path<-defpaths[i]
          cat("Setter path=",path,"fra defpaths\n")
        }
        i<-i+1
      }
    }
    if (path==""){
      cat(globs$stjstr,"******KRITISK FEIL: path ikke funnet\n",globs$stjstr,sep="")
    }
  } else if (!file.exists(paste(path,KHdbname,sep="/"))){
    cat(globs$stjstr,"******KRITISK FEIL: path har ikke hovedfila",KHdbname,globs$stjstr,sep="")
    path<-""
  }
  
  
  
  if (path!=""){

    ## Use other location of KHELSA.mdb and KHlogg.mdb
    ## This is needed due to constant crash with unstable network
    dbFile = globs$KHdbname
    logFile = globs$KHlogg
    
    if (exists("setLocalPath", envir = .GlobalEnv)){
      path = setLocalPath
    } 


    #Sys.getenv("R_ARCH")   gir "/x64"eller "/i386" 
    KHOc<-odbcConnectAccess2007(paste(path,dbFile,sep="/"))
    #KHOc<-odbcConnectAccess(paste(path,KHdbname,sep="/"))
    KHLc<-odbcConnectAccess2007(paste(path,logFile,sep="/"))

    ## OBS!! Have to reset path to original for all the other globs use
    path = rawPath
    
  }
  
  globs<-c(globs,list(dbh=KHOc,log=KHLc,path=path))
  
  GeoNavn<-data.table(sqlQuery(KHOc,"SELECT * from GeoNavn",as.is=TRUE))
  GeoKoder<-data.table(sqlQuery(KHOc,"SELECT * from GEOKoder",as.is=TRUE),key=c("GEO"))
  UtGeoKoder<-GeoKoder[TYP=="O"]$GEO
  KnrHarm<-data.table(sqlQuery(KHOc,"SELECT * from KnrHarm",as.is=TRUE),key=c("GEO"))
  TKNR<-data.table(sqlQuery(KHOc,"SELECT * from TKNR",as.is=TRUE),key=c("ORGKODE"))
  HELSEREG<-data.table(sqlQuery(KHOc,"SELECT * from HELSEREG",as.is=TRUE),key=c("FYLKE"))
  #Gjelder også for soner
  KnrHarmS<-lapply(KnrHarm[,c("GEO","GEO_omk"),with=FALSE],function(x){paste(x,"00",sep="")})
  KnrHarmS<-cbind(as.data.frame(KnrHarmS,stringsAsFactors=FALSE),HARMstd=KnrHarm$HARMstd)
  KnrHarm<-rbind(KnrHarm,KnrHarmS)
  #Må legge til de som ikke omkodes for å lette bruk i merge
  #KnrHarm<-rbind(KnrHarm,data.frame(KNRorg=GeoKoder$GEO[TIL<2008],KNRharm=GeoKoder$GEO[TIL<2008],HARMstd=2008))
  
  
  #GK til bydel. Bør konsolideres med KnrHarm
  GkBHarm<-data.table(sqlQuery(KHOc,"SELECT * FROM GKBydel2004T",as.is=TRUE),key=c("GK,Bydel2004"))
  
  globs$DefDesign<-SettDefDesignKH(globs=globs)
  globs$KB<-SettKodeBokGlob(globs=globs)
  globs$LegKoder<-SettLegitimeKoder(globs=globs)
  globs$TotalKoder<-SettTotalKoder(globs=globs)
  Stata<-FinnStataExe()
  globs$StataExe<-Stata$Exe
  globs$StataVers<-Stata$Vers
  return(c(globs,list(GeoNavn=GeoNavn,GeoKoder=GeoKoder,UtGeoKoder=UtGeoKoder,KnrHarm=KnrHarm,GkBHarm=GkBHarm,TKNR=TKNR,HELSEREG=HELSEREG)))
}

#
FinnGlobs<-function(){
  #Hjelperutine, kjekk å brukes som default. Bruker KHglobs elller setter denne
  globs<-NA
  if (exists("KHglobs")){
    globs<-KHglobs
  } else {
    globs<-SettGlobs()
  }
  return(globs)
}

#KHglobs<-SettGlobs()

ListAlleOriginalFiler<-function(globs=FinnGlobs()){
  print(sqlQuery(globs$log,"DROP TABLE ALLEFILER"))
  setwd(paste(globs$path,"ORGDATA",sep="/"))
  Filer<-setNames(as.data.frame(list.files(recursive=TRUE),stringsAsFactors=FALSE),c("FILNAVN"))
  Filer$TYP<-NA
  Filer$TYP[grepl("/ORG/",Filer$FILNAVN)]<-"ORG"
  Filer$TYP[grepl("/GML/",Filer$FILNAVN)]<-"GML"
  Filer$TYP[grepl("/MOTTAK/",Filer$FILNAVN)]<-"MOTTAK"
  Filer$TYP[grepl("/ARKIV/",Filer$FILNAVN)]<-"ARKIV"
  Filer$FILNAVN<-paste("ORGDATA",Filer$FILNAVN,sep="/")
  Filer$FILNAVN<-gsub("/","\\\\",Filer$FILNAVN)
  
  print(head(Filer))
  sqlSave(globs$log, Filer, "ALLEFILER", rownames = FALSE)
}

##########################################################
#TRINN 1: STABLING AV ORIGINALFILER I FILGRUPPE-FILER
#         Gir ferdige stablede filer i \\StablaFilGrupper
##########################################################

#
LagFilgruppe<-function(gruppe,
                       batchdate=SettKHBatchDate(),
                       globs=FinnGlobs(),
                       diagnose=0,
                       printR=TRUE,
                       printCSV=FALSE,
                       printSTATA=FALSE,
                       versjonert=FALSE,
                       dumps=list(),
                       test = runtest,
                       idtest = testfiles,
                       localDir = setLocal){

  ## test is TRUE when column 'TESTING' in ORIGINALFILER is used
  ## for selecting the file to be processed
  lineMsg <- "\n---------------------------\n"
  if(test)
    message(lineMsg,
            "\n** ---- Test Modus ---- **")
  
  ## To see which DB is currently used
  ## that value in globs$KHdbname and globs$path
  whichPath <- ifelse(localDir, setLocalPath, defpaths[1])
  whichDB <- ifelse(localDir, setDBFile, dbNameFile)
  message(lineMsg,
          "  DB name: ", whichDB, "\n", 
          "  DB path: ", whichPath,  
          lineMsg)

  if (test && is.null(idtest)){
    stop("Ingen KOBLID nr. er spesifisert")
  } 

  if(test){
    cat("  Test filer KOBLID: ", idtest, lineMsg)
  }

  ## testfiles global value is needed by FinnFilBeskGruppe() for filtering
  assign("testfiles", idtest, envir = .GlobalEnv)
  
  
  #Essensielt bare loop over alle delfiler/orignalfiler
  #For hver orignalfil kjøres LagTabellFraFil
  #Stables til tabellen FG
  #Finn filgruppeparametre
  FGP<-FinnFilgruppeParametre(gruppe,batchdate=batchdate,globs=globs)
  #Initier tom tabell
  Filgruppe<-data.frame()
  if (FGP$ok==1){
    #Rydd gammel logg
    sqlQuery(globs$log,paste("DELETE * FROM KODEBOK_LOGG WHERE FILGRUPPE='",gruppe,"' AND SV='S'",sep=""))
    sqlQuery(globs$log,paste("DELETE * FROM INNLES_LOGG WHERE FILGRUPPE='",gruppe,"' AND SV='S'",sep=""))
    #Finn parameterbeskrivelse av delfilene
    delfiler<-FinnFilBeskGruppe(gruppe,batchdate=batchdate,globs=globs)
    if(nrow(delfiler)>0){
      for (i in 1:nrow(delfiler)){

        ## Need root path for raw files
        getSti <- rawPath
        
        filbesk<-delfiler[i,]
        tm<-proc.time()
        filbesk$filn <- file.path(getSti, filbesk$FILNAVN)
        ## filbesk$filn<-paste(globs$path,filbesk$FILNAVN,sep="/")
        filbesk$filn<-gsub("\\\\","/",filbesk$filn)
        #Sett evt default for år basert på aktuelt årstall
        filbesk$AAR<-gsub("<\\$y>",paste("<",filbesk$DEFAAR,">",sep=""),filbesk$AAR)
        #LagTabell
        DF<-LagTabellFraFil(filbesk,FGP,batchdate=batchdate,diagnose=diagnose,globs=globs,versjonert=versjonert,dumps=dumps)
        #Stable delfiler
        Filgruppe<-rbind.fill(Filgruppe,DF)
        
        
        #Stopp klokke, skriv tid og feillogg
        tid<-proc.time()-tm
        stid<-format(Sys.time(), "%Y-%m-%d %X")
      }
    } else {
      #Må gi fornuftig tilbakemelding
    }
    
    #DEV: SPESIALBEHANDLING AV FILGRUPPE HER!! F.EKS. IMPUTER NPR
    
    
    #Diagnostisering og rapportering på hele filgruppa under ett
    
    if(nrow(Filgruppe)>0 & diagnose==1){
      #Finn og rapporter duplikater
      HarDuplikater<-SjekkDuplikater(Filgruppe,batchdate=batchdate,filgruppe=gruppe,versjonert=versjonert,globs=KHglobs)
      sqlQuery(globs$dbh,paste("UPDATE FILGRUPPER SET DUPLIKATER='",HarDuplikater,"' WHERE FILGRUPPE='",gruppe,"'",sep=""))  
      
      #Sjekk design
      FGd<-FinnDesign(Filgruppe,FGP=FGP)
      
      #Er ubalansert?
      subset(FGd$Design,HAR!=1)
      
      FGdT<-FGd$Design
      sqlQuery(globs$log,paste("DELETE * FROM DESIGN WHERE FILGRUPPE='",gruppe,"' AND SV='S'",sep=""))
      #Legg til resterende kolonner 
      tmp<-sqlQuery(globs$log,"SELECT * FROM DESIGN WHERE FILGRUPPE=''")
      tmp[1:nrow(FGdT),]<-NA
      tmp[,names(FGdT)]<-FGdT
      tmp$FILGRUPPE<-gruppe
      tmp$BATCH<-batchdate
      tmp$SV<-"S"
      sqlSave(KHglobs$log,tmp,"DESIGN",rownames=FALSE,append=TRUE)
      if (versjonert==TRUE){
        tmp$SV<-"V"
        sqlSave(KHglobs$log,tmp,"DESIGN",rownames=FALSE,append=TRUE)
      }
      
    }
    #Sett (eksterne) kolonnenavn
    for (val in names(Filgruppe)[grepl("^VAL\\d+$",names(Filgruppe))]){
      valn<-paste(val,"navn",sep="")
      if (grepl("\\S",FGP[[valn]])){
        names(Filgruppe)<-gsub(paste("^",val,"(\\.[fa]|)$",sep=""),paste(FGP[[valn]],"\\1",sep=""),names(Filgruppe))    
      }
    }
    FGP1<-copy(Filgruppe)
    
    if ("RSYNT_PRE_FGLAGRINGpre" %in% names(dumps)){
      for (format in dumps[["RSYNT_PRE_FGLAGRINGpre"]]) {
        DumpTabell(Filgruppe,paste(filbesk$FILGRUPPE,"RSYNT_PRE_FGLAGRINGpre",sep="_"),globs=globs,format=format)
      }
    }
    
    
    ######################################################
    #EVT SPESIALBEHANDLING
    if (!is.na(FGP$RSYNT_PRE_FGLAGRING)){
      synt<-gsub("\\\r","\\\n",FGP$RSYNT_PRE_FGLAGRING)
      error<-""
      ok<-1
      if (grepl("<STATA>",synt)){
        synt<-gsub("<STATA>[ \n]*(.*)","\\1",synt)
        RES<-KjorStataSkript(Filgruppe,synt,batchdate=batchdate,globs=globs)
        if (RES$feil!=""){
          error<-paste("Noe gikk galt i kjøring av STATA",RES$feil,sep="\n")
          ok<-0
        } else {
          Filgruppe<-RES$TABLE
        }
      } else {
        rsynterr<-try(eval(parse(text=synt)),silent=TRUE)
        if(class(rsynterr)=="try-error"){
          ok<-0
          error<-rsynterr
        }
      }
      if (ok==0){
        print(error)
      }
    }
    
    if ("RSYNT_PRE_FGLAGRINGpost" %in% names(dumps)){
      for (format in dumps[["RSYNT_PRE_FGLAGRINGpost"]]) {
        DumpTabell(Filgruppe,paste(filbesk$FILGRUPPE,"RSYNT_PRE_FGLAGRINGpost",sep="_"),globs=globs,format=format)
      }
    }
    
    
    
    #Datostempel
    sqlQuery(globs$dbh,paste("UPDATE FILGRUPPER SET PRODDATO='",format(Sys.time(), "%Y-%m-%d %X"),"' WHERE FILGRUPPE='",gruppe,"'",sep=""))  
    
    #SKRIV RESULTAT    
    path<-globs$path
    
    if (!exists("testmappe")) testmappe  <- file.path(globs$path, "TEST") 

    if (test) {
      printR = FALSE
      testFile <- paste0(gruppe, batchdate, ".rds")
      testPath <- file.path(testmappe, testFile)
      message("Testfil: ", testPath)
      saveRDS(Filgruppe, file = testPath)
    }
    
    if (printR){
      utfiln<-paste(path,"/",globs$StablaDirNy,"/",gruppe,".rds",sep="")
      #save(Filgruppe,file=utfiln)
      print(utfiln)
      saveRDS(Filgruppe,file=utfiln)
      if (versjonert==TRUE){
        utfild<-paste(path,"/",globs$StablaDirDat,"/",gruppe,"_",batchdate,".rds",sep="")
        file.copy(utfiln,utfild)
      }
    }
  }

  ## return(Filgruppe)
  ht2(Filgruppe)
}

## show head and tail 
ht2 <- function(x, n = 3){
  rbind(head(x, n), tail(x, n))
}


#
LagFlereFilgrupper<-function(filgrupper=character(0),batchdate=SettKHBatchDate(),globs=FinnGlobs(),printR=TRUE,printCSV=FALSE,printSTATA=FALSE,versjonert=FALSE){
  #SKall rundt LagFilGruppe, lager og lagrer evt til fil
  #Default er å ta alle grupper, ellers angis ønsket batch i filgrupper-argumentet 
  if (length(filgrupper)==0){
    #filgrupper<-as.matrix(sqlQuery(globs$dbh,"SELECT DISTINCT Filgruppe from INNLESING WHERE Bruk=1",as.is=TRUE))
    filgrupper<-as.matrix(sqlQuery(globs$dbh,"SELECT DISTINCT Filgruppe from FILGRUPPER",as.is=TRUE))
  } 
  cat("BATCH:",batchdate,"\n")
  #HOVEDLOOP
  for (gruppe in filgrupper){
    FG<-LagFilgruppe(gruppe,batchdate=batchdate,globs=globs,versjonert=versjonert) 
  }
}

#
LagTabellFraFil<-function (filbesk,FGP,batchdate=SettKHBatchDate(),diagnose=0,globs=FinnGlobs(),versjonert=FALSE,echo=FALSE,dumps=list()) {
  
  klokke<-proc.time()
  ######################################################
  #INNLESING
  filn<-filbesk$filn
  cat("\n#################\nLAGER TABELL FRA FIL:\n",filn,"\n")
  LestFil<-LesFil(filbesk,batchdate=batchdate,globs=globs,dumps=dumps)
  ok<-LestFil$ok
  DF<-LestFil$DF
  
  if (echo==TRUE){
    cat("\nETTER INNLES\n#############################\n")
    print(head(DF))
  }
  
  #   ######################################################
  #   #EVT SPESIALBEHANDLING
  #   if (!is.na(filbesk$RSYNT1)){
  #     filbesk$RSYNT1<-gsub("\\\r","\\\n",filbesk$RSYNT1)
  #     eval(parse(text=filbesk$RSYNT1))
  #   }
    #cat("\nETTER INNLES\n#############################\n")
  
  
  
  if (ok==1){
    
    ######################################################
    #Omdøp kolonnenavn. 
    #NB: for oversiktelighet i parameterfila gjøres dette både før og etter reshape
    #Dvs: kolonnenavn generert i reshape tillates å avvike fra standardnavn, disse endres etterpå
    #Valdiering skjer ved siste endring
    kolorgs<-globs$kolorgs
    #Finn kolonner spesifisert i filbesk
    HarCols<-filbesk[kolorgs[grepl("^[^-<]",filbesk[kolorgs])]]
    HarCols<-HarCols[HarCols %in% names(DF)]
    #Sett standard kolonnenavn
    names(DF)<-mapvalues(names(DF),HarCols,names(HarCols))
    
   
    
    ######################################################
    
    #EVT INNFYLLING AV TABULATOR N?R DENNE ER INNRYKKET
    if (!is.na(filbesk$FYLLTAB)){
      TAB<-as.character(read.csv(text=filbesk$FYLLTAB,header=FALSE,stringsAsFactors=FALSE))
      if (all(TAB %in% names(DF))){
        DF[,TAB][DF[,TAB]==""]<-NA
        DF[,TAB]<-na.locf(DF[,TAB],na.rm=FALSE)
      } else {
        TilFilLogg(filbesk$KOBLID,"FYLLTABERR",paste("Kolonner",paste(TAB[!TAB %in% names(DF)],collapse=","), " finnes ikke",sep=""),batchdate=batchdate,globs=globs)
        ok<-0
      }
    }
    
    #EVT KASTING AV KOLONNER FØR RESHAPE (GJØR melt LETTERE Å BRUKE)
    if (!is.na(filbesk$KASTKOLS)){
      eval(parse(text=paste("DF<-DF[,-",filbesk$KASTKOLS,"]",sep="")))
    }
    
    if ("RESHAPEpre" %in% names(dumps)){
      for (format in dumps[["RESHAPEpre"]]) {
        DumpTabell(DF,paste(filbesk$FILGRUPPE,filbesk$KOBLID,"RESHAPEpre",sep="_"),globs=globs,format=format)
      }
    }
    
    #if (!(is.na(filbesk$RESHAPEid) || filbesk$RESHAPEid=='')){
    if (!(is.na(filbesk$RESHAPEvar) || filbesk$RESHAPEvar=='')){
      rshpDF<-ReshapeTab(DF,filbesk,batchdate=batchdate,globs=globs)
      DF<-rshpDF$DF
      ok<-min(ok,rshpDF$ok)
      #cat("\nETTER RESHAPE\n#############################\n")
      #print(head(DF))
    }
    
    if ("RESHAPEpost" %in% names(dumps)){
      for (format in dumps[["RESHAPEpost"]]) {
        DumpTabell(DF,paste(filbesk$FILGRUPPE,filbesk$KOBLID,"RESHAPEpost",sep="_"),globs=globs,format=format)
      }
    }
    
    
    TilFilLogg(filbesk$KOBLID,"RESHAPEh",DFHeadToString(DF),batchdate=batchdate,globs=globs)    
  }
  
  if (ok==1){
        
    #Må splitte evt kolonne fra MULTIHEAD
    if (!is.na(filbesk$MULTIHEAD)){
      mhl<-LesMultiHead(filbesk$MULTIHEAD)
      DF[,mhl$colnames]<-str_split_fixed(DF[,mhl$varname],mhl$sep,2)
    }
    
    if ("RSYNT2pre" %in% names(dumps)){
      for (format in dumps[["RSYNT2pre"]]) {
        DumpTabell(DF,paste(filbesk$FILGRUPPE,filbesk$KOBLID,"RSYNT2pre",sep="_"),globs=globs,format=format)
      }
    }
    
    ######################################################
    #EVT SPESIALBEHANDLING
    if (!is.na(filbesk$RSYNT2)){
      synt<-gsub("\\\r","\\\n",filbesk$RSYNT2)
      error<-""
      ok<-1
      if (grepl("<STATA>",synt)){
        synt<-gsub("<STATA>[ \n]*(.*)","\\1",synt)
        RES<-KjorStataSkript(DF,synt,batchdate=batchdate,globs=globs)
        if (RES$feil!=""){
          error<-paste("Noe gikk galt i kjøring av STATA",RES$feil,sep="\n")
          ok<-0
        } else {
          DF<-RES$TABLE
        }
      } else {
        rsynterr<-try(eval(parse(text=synt)),silent=TRUE)
        if(class(rsynterr)=="try-error"){
          ok<-0
          error<-rsynterr
        }
      }
      if (ok==0){
        print(error)
        TilFilLogg(filbesk$KOBLID,"RSYNT2ERR",error,batchdate=batchdate,globs=globs)
      }
    }
    
    if ("RSYNT2post" %in% names(dumps)){
      for (format in dumps[["RSYNT2post"]]) {
        DumpTabell(DF,paste(filbesk$FILGRUPPE,filbesk$KOBLID,"RSYNT2post",sep="_"),globs=globs,format=format)
      }
    }
    
  }
  
  if (ok==1){
    
    ######################################################
    #Omdøp kolonnenavn, runde 2. 
      
    #Finn kolonner spesifisert i filbesk
    HarCols<-filbesk[kolorgs[grepl("^[^-<]",filbesk[kolorgs])]]
    HarCols<-HarCols[HarCols %in% names(DF)]
    #Sett standard kolonnenavn
    names(DF)<-mapvalues(names(DF),HarCols,names(HarCols))
    
    #Finn kolonner med standardverdi ('<.*>' i filbesk)
    DefVCols<-kolorgs[grepl("^<.*>",filbesk[kolorgs])]
    DefV<-matrix(sub("^<(.*)>$","\\1",filbesk[DefVCols]),nrow=1)
    #Sett standardverdier (får ikke til dette med enklere syntaks når det kan være tuppel, virker klønete)
    DF<-setNames(data.frame(DF,DefV,stringsAsFactors = FALSE),c(names(DF),DefVCols))
    
    #Sjekk for ikke-eksisterende/feilskrevet
    colerr<-"" 
    if (!all(names(HarCols) %in% names(DF))){
      colerr<-paste(colerr,"Kolonnene <",HarCols[!(names(HarCols) %in% names(DF))],"> finnes ikke\n")
      ok<-0
    }
    
    #Sjekk at påkrevde kolonner finnes
    oblkols<-c("GEO","AAR","VAL1")
    if (!all(oblkols %in% names(DF))){
      colerr<-paste(colerr,"KRITISK: Kolonnene <",oblkols[!(oblkols %in% names(DF))],"> finnes ikke\n") 
      ok<-0
    }
    if (ok==0){TilFilLogg(filbesk$KOBLID,"KOLNAVNERR",colerr,batchdate=batchdate,globs=globs)}
    
    
  } 
  
  if (echo==TRUE){
    cat("\nETTER TRINN2\n#############################\n")
    print(head(DF))
  }  
  
  
  if (ok==1){    
    #Merge GEO delt i to
    if (filbesk$GEOd2!="-" & !is.na(filbesk$GEOd2)){
      DF[,filbesk$GEOd2]<-gsub("^(\\d|\\d{3})$","0\\1",DF[,filbesk$GEOd2])
      DF$GEO<-paste(DF$GEO,DF[,filbesk$GEOd2],sep="")
    }
    
    #KAST USPESIFISERTE KOLONNER
    DF<-DF[,names(DF)[names(DF) %in% kolorgs]]
    
  }
  
  if (echo==TRUE){
    cat("\nETTER TRINN3\n#############################\n")
    print(head(DF))
  }
  TilFilLogg(filbesk$KOBLID,"INNLES_OK",ok,batchdate=batchdate,globs=globs)
  
  
  if (!is.na(filbesk$GRUNNKRETS) && filbesk$GRUNNKRETS==1){
    setDT(DF)
    setkeyv(DF,"GEO")
    setkeyv(globs$GkBHarm,"GK")
    DF<-globs$GkBHarm[DF]
    DF[is.na(Bydel2004),Bydel2004:=paste(substr(GK,1,4),"00",sep="")]
    DF[,GK:=NULL]
    setnames(DF,"Bydel2004","GEO")
    tabkols<-names(DF)[!grepl("^VAL\\d$",names(DF))]
    valkols<-names(DF)[grepl("^VAL\\d$",names(DF))]
    setkeyv(DF,tabkols)
    lp<-paste("list(",
              paste(valkols,"=as.character(sum(as.numeric(",valkols,")))",
              sep="",collapse=","),
              ")",sep="")
    DF<-as.data.frame(DF[,eval(parse(text=lp)), by=tabkols])
  }
  
 
  
  ######################################################
  #SKILL EVT UT SOM EGEN FUNKSJON
  #Nullstill logg
  if ("KODEBOKpre" %in% names(dumps)){
    for (format in dumps[["KODEBOKpre"]]) {
      DumpTabell(DF,paste(filbesk$FILGRUPPE,filbesk$KOBLID,"KODEBOKpre",sep="_"),globs=globs,format=format)
    }
  }
  sqlQuery(globs$log,paste("DELETE * FROM KODEBOK_LOGG WHERE KOBLID=",filbesk$KOBLID,sep=""))
  if (ok==1){
    colClass<-sapply(DF,class)
    if (any(colClass!="character")){
      cat("Advarsel! Kolonnene ",names(DF)[colClass!="character"]," er ikke character (",colClass[colClass!="character"],")\n",sep="")
      ## DF[,colClass!="character"]<-as.character(DF[,colClass!="character"])
      
      ## The above code creates lots of duplicated columns so it's changed as below
      noneSTR <- names(colClass)[colClass != "character"]
      DF[noneSTR] <- lapply(DF[noneSTR], as.character)
    }
    DF[is.na(DF)]<-""
    
    ## TABS
    ## ------------------------------------------------------------
    ## KOPI_KOL will use the TAB1:TAB3 and defined in
    ## INNLESING tabel KOPI_KOL column with Existing_col=New_col
    ## New_col should be defined in one of the TABs
    allTabs <- c("TAB1","TAB2","TAB3")
    
    if (!is.na(filbesk$KOPI_KOL)) {
      message("Kopi kolonne er: ", filbesk$KOPI_KOL)
      spVal <- unlist(strsplit(filbesk$KOPI_KOL, "="))
      spVal <- trimws(spVal)

      if (isFALSE(spVal %in% names(filbesk)))
        stop("Har ikke funnet kolonnenavn som: ")

      spTab <- grep("<kopi_kol>", filbesk[allTabs], ignore.case=TRUE)

      if (length(spTab) == 0)
        stop("Hvor skal kolonne ", spVal[1], " kopieres til?")
      
      dfTab <- names(filbesk[allTabs][spTab])

      DF[dfTab] <- DF[spVal[1]]
    }

    
    #VASK AV TABx
    for (tab in allTabs){
      if (tab %in% names(DF)){
        tabKB<-setNames(as.data.frame(table(DF[,tab],useNA="ifany"),stringsAsFactors=FALSE),c("ORG","FREQ"))
        tabKB$KBOMK<-KBomkod(tabKB$ORG,type=tab,filbesk=filbesk,batchdate=batchdate,globs=globs)
        tabKB$OMK<-gsub("^-$","XXXKASTXXX",tabKB$KBOMK) #Dirty tricks. Beskytter '-' mot uttrykket nedenfor, uten å gjøre regexp unødvendig komplisert
        tabKB$OMK<-gsub("[- ,\\/]","_",tabKB$KBOMK)
        tabKB$OMK<-gsub("XXXKASTXXX","-",tabKB$KBOMK)
        tabKB$OK<-1
        SkrivKBLogg(KB=tabKB,type=tab,filbesk=filbesk,FGP$FILGRUPPE,batchdate=batchdate,globs=globs)
        DF[,tab]<-mapvalues(DF[,tab],tabKB$ORG,tabKB$OMK,warn_missing = FALSE)
      }
    }
    
    ## GEO
    ## -----------------------------------------------------------------------
    #RENSK GEO (Alle er legit inntil videre??? Eller kod til 9999???)
    if ("GEO" %in% names(DF)){
      org<-setNames(as.data.frame(table(DF$GEO,useNA="ifany"),stringsAsFactors=FALSE),c("ORG","FREQ"))
      geo<-GEOvask(org,filbesk=filbesk,batchdate=batchdate,globs=globs)
      
      SkrivKBLogg(KB=geo,type="GEO",filbesk=filbesk,FGP$FILGRUPPE,batchdate=batchdate,globs=globs)
      TilFilLogg(filbesk$KOBLID,"GEO_ok",ifelse(0 %in% geo$OK,0,1),batchdate=batchdate,globs=globs)
      
      DF$GEOniv<-mapvalues(DF$GEO,geo$ORG,geo$GEOniv,warn_missing = FALSE)
      DF$FYLKE<-mapvalues(DF$GEO,geo$ORG,geo$FYLKE,warn_missing = FALSE)
      DF$GEO<-mapvalues(DF$GEO,geo$ORG,geo$OMK,warn_missing = FALSE)  #NB: rekkefølge har betydning   
    }
    #RENSK ALDER
    #Sett intervall for alder ALLE
    if ("ALDER" %in% names(DF)){
      DF$ALDER<-gsub(" \\Wr\\b"," år",DF$ALDER,perl=TRUE)   #Problem med codebook i dbf
      
      org<-setNames(as.data.frame(table(DF$ALDER,useNA="ifany"),stringsAsFactors=FALSE),c("ORG","FREQ"))
      alder<-ALDERvask(org,FGP=FGP,filbesk=filbesk,batchdate=batchdate,globs=globs)
      
      #Kast der ALDEr koder til "-" (må ta det her og ikek generelle under pga intervall)
      DF<-subset(DF,!ALDER %in% subset(alder,OMK=="-")$ORG)
      
      SkrivKBLogg(KB=alder,type="ALDER",filbesk=filbesk,FGP$FILGRUPPE,batchdate=batchdate,globs=globs)
      TilFilLogg(filbesk$KOBLID,"ALDER_ok",ifelse(globs$alder_illeg %in% alder$OMK,0,1),batchdate=batchdate,globs=globs)
      DF$ALDERl<-as.integer(mapvalues(DF$ALDER,alder$ORG,alder$LO,warn_missing = FALSE))
      DF$ALDERh<-as.integer(mapvalues(DF$ALDER,alder$ORG,alder$HI,warn_missing = FALSE))
      #DF$ALDERl<-as.numeric(mapvalues(DF$ALDER,alder$ORG,alder$LO,warn_missing = FALSE))
      #DF$ALDERh<-as.numeric(mapvalues(DF$ALDER,alder$ORG,alder$HI,warn_missing = FALSE))
    }
    
    
    #RENSK KJ?NN
    if ("KJONN" %in% names(DF)){
      
      org<-setNames(as.data.frame(table(DF$KJONN,useNA="ifany"),stringsAsFactors=FALSE),c("ORG","FREQ"))
      kjonn<-KJONNvask(org,filbesk=filbesk,batchdate=batchdate,globs=globs)
      
      SkrivKBLogg(KB=kjonn,type="KJONN",filbesk=filbesk,FGP$FILGRUPPE,batchdate=batchdate,globs=globs)
      TilFilLogg(filbesk$KOBLID,"KJONN_ok",ifelse(globs$kjonn_illeg %in% kjonn$OMK,0,1),batchdate=batchdate,globs=globs)
      
      DF$KJONN<-as.integer(mapvalues(DF$KJONN,kjonn$ORG,kjonn$OMK,warn_missing = FALSE))
    }
    
    #AAR TIL INTERVALL
    if ("AAR" %in% names(DF)){
      org<-setNames(as.data.frame(table(DF$AAR,useNA="ifany"),stringsAsFactors=FALSE),c("ORG","FREQ"))
      
      aar<-AARvask(org,filbesk=filbesk,batchdate=batchdate,globs=globs)
        
      SkrivKBLogg(KB=aar,type="AAR",filbesk=filbesk,FGP$FILGRUPPE,batchdate=batchdate,globs=globs)
      TilFilLogg(filbesk$KOBLID,"AAR_ok",ifelse(globs$aar_illeg %in% aar$OMK,0,1),batchdate=batchdate,globs=globs)
      
      #Kast der AAR koder til "-" (må ta det her og ikek generelle under pga intervall)
      DF<-subset(DF,!AAR %in% subset(aar,OMK=="-")$ORG)
      
      DF$AARl<-as.integer(mapvalues(DF$AAR,aar$ORG,aar$LO,warn_missing = FALSE))
      DF$AARh<-as.integer(mapvalues(DF$AAR,aar$ORG,aar$HI,warn_missing = FALSE))
    }
    
        
    #RENSK UTDANN
    if ("UTDANN" %in% names(DF)){
      
      org<-setNames(as.data.frame(table(DF$UTDANN,useNA="ifany"),stringsAsFactors=FALSE),c("ORG","FREQ"))
      utdann<-UTDANNvask(org,filbesk=filbesk,batchdate=batchdate,globs=globs)
      
      SkrivKBLogg(KB=utdann,type="UTDANN",filbesk=filbesk,FGP$FILGRUPPE,batchdate=batchdate,globs=globs)
      TilFilLogg(filbesk$KOBLID,"UTDANN_ok",ifelse(globs$utdann_illeg %in% utdann$OMK,0,1),batchdate=batchdate,globs=globs)
      
      DF$UTDANN<-as.integer(mapvalues(DF$UTDANN,utdann$ORG,utdann$OMK,warn_missing = FALSE))
    }
    
    
    #RENSK SIVST
    if ("SIVST" %in% names(DF)){
      
      org<-setNames(as.data.frame(table(DF$SIVST,useNA="ifany"),stringsAsFactors=FALSE),c("ORG","FREQ"))
      sivst<-SIVSTvask(org,filbesk=filbesk,batchdate=batchdate,globs=globs)
      
      SkrivKBLogg(KB=sivst,type="SIVST",filbesk=filbesk,FGP$FILGRUPPE,batchdate=batchdate,globs=globs)
      TilFilLogg(filbesk$KOBLID,"SIVST_ok",ifelse(globs$sivst_illeg %in% sivst$OMK,0,1),batchdate=batchdate,globs=globs)
      
      DF$SIVST<-as.integer(mapvalues(DF$SIVST,sivst$ORG,sivst$OMK,warn_missing = FALSE))
    }
    
    
    #RENSK LANDBAK
    if ("LANDBAK" %in% names(DF)){
      
      org<-setNames(as.data.frame(table(DF$LANDBAK,useNA="ifany"),stringsAsFactors=FALSE),c("ORG","FREQ"))
      landbak<-LANDBAKvask(org,filbesk=filbesk,batchdate=batchdate,globs=globs)
      
      SkrivKBLogg(KB=landbak,type="LANDBAK",filbesk=filbesk,FGP$FILGRUPPE,batchdate=batchdate,globs=globs)
      TilFilLogg(filbesk$KOBLID,"LANDBAK_ok",ifelse(globs$landbak_illeg %in% landbak$OMK,0,1),batchdate=batchdate,globs=globs)
      
      DF$LANDBAK<-as.integer(mapvalues(DF$LANDBAK,landbak$ORG,landbak$OMK,warn_missing = FALSE))
    }
    
    if ("KODEBOKpost" %in% names(dumps)){
      for (format in dumps[["KODEBOKpost"]]) {
        DumpTabell(DF,paste(filbesk$FILGRUPPE,filbesk$KOBLID,"KODEBOKpost",sep="_"),globs=globs,format=format)
      }
    }
    
    #DROPP ALLE MED '-' I TABULERING (merk: AAR og ALDER måtte tas over pga intervall)
    DF<-subset(DF,rowSums(DF[,names(DF) %in% globs$taborgs]=="-")==0)
    
    #VASK VERDIER. Litt annen prosess, bruker KB, men tabulerer bare ikke-numeriske. 
    #Setter numerisk, med flagg for type NA
    for (val in c("VAL1","VAL2","VAL3")){
      #Bedre, men funker ikke i forhold til logg
      #for (val in names(DF)[grepl("VAL\\d+$",names(DF))]){  
      if (val %in% names(DF)){
        DF[is.na(DF[,val]),val]<-""
        
        valKB<-KBomkod(DF[,val],type=val,valsubs=TRUE,filbesk=filbesk,batchdate=batchdate,globs=globs)
        valKBut<-valKB$subsant
        
        valok<-1
        valf<-paste(val,".f",sep="")
        vala<-paste(val,".a",sep="")
        valomk<-paste(val,"omk",sep="")
        
        
        #Lag omkodet verdi med numerisk. Ikke numerisk blir foreløpig NA
        suppressWarnings(DF[,valomk]<-as.numeric(valKB$omk))
        DF[,valf]<-0
        DF[,vala]<-1
        DF[valKB$omk==".." & DF[,val]!=valKB$omk,valf]<-1
        DF[valKB$omk=="." & DF[,val]!=valKB$omk,valf]<-2
        DF[valKB$omk==":" & DF[,val]!=valKB$omk,valf]<-3
        
        
        #Behandle (resterende) ikke-numeriske
        nonNum<-which(is.na(DF[,valomk]) & DF[,val]==valKB$omk)
        if (length(nonNum)>0){
          #Kodebok
          valKB<-setNames(as.data.frame(table(DF[nonNum,val],useNA="ifany"),stringsAsFactors=FALSE),c("ORG","FREQ"))
          valKB$KBOMK<-KBomkod(valKB$ORG,type=val,filbesk=filbesk,batchdate=batchdate,globs=globs)
          valKB$OMK<-valKB$KBOMK
          
          
          #Gjør nytt forsøk på numerisk konvertering etter omkoding
          kbNUM<-suppressWarnings(as.numeric(valKB$OMK))
          
          #Legitime
          valKB$OK<-0
          Num2<-which(!is.na(kbNUM))
          valKB$OK[Num2]<-1
          valKB$OK[valKB$OMK %in% c(".","..",":")]<-1
          if (0 %in% valKB$OK){valok<-0}
          valKBut<-rbind(valKBut,valKB)
          
          #if(valok==0){print(valKB)}
           
          #Internt, regnbart format med numerisk flagg i "VAL1f" etc
          #".." = 1, "." = 2, ":" = 3  
          valKB$kbNUM<-kbNUM
          valKB$FLAG<-0
          valKB$FLAG[valKB$OMK==".."]<-1
          valKB$FLAG[valKB$OMK=="."]<-2
          valKB$FLAG[valKB$OMK==":"]<-3
          valKB$FLAG[valKB$OK==0]<-8
          #valKB$kbNUM[valKB$FLAG>0]<-0
          
          #if(valok==0){print(valKB)}
          DF[nonNum,valomk]<-as.numeric(mapvalues(DF[nonNum,val],valKB$ORG,valKB$kbNUM,warn_missing = FALSE))
          #DF[nonNum,valomk]<-suppressWarnings(as.numeric(mapvalues(DF[nonNum,val],valKB$ORG,valKB$kbNUM,warn_missing = FALSE)))
          DF[nonNum,valf]<-as.integer(mapvalues(DF[nonNum,val],valKB[,"ORG"],valKB[,"FLAG"],warn_missing = FALSE))
        }
        
        if (nrow(valKBut)>0){
          SkrivKBLogg(KB=valKBut,type=val,filbesk=filbesk,FGP$FILGRUPPE,batchdate=batchdate,globs=globs)
        }
        
       
        DF[,val]<-NULL
        DF<-setNames(DF,mapvalues(names(DF),valomk,val))
        
        #########################
        # DEVELOP20191219
        #########################
        
        reskaler<-as.numeric(filbesk[[eval(paste("SKALA","_",val,sep=""))]])
        
        if (!(reskaler==1 | is.na(reskaler))){
          DF[,val]<-DF[,val]*filbesk[[eval(paste("SKALA","_",val,sep=""))]]
        }
        
      }
      
      TilFilLogg(filbesk$KOBLID,paste(val,"OK",sep="_"),valok,batchdate=batchdate,globs=globs)
    }
    
    
    
    
    
    
        
    default.stringsAsFactors=TRUE 
    Kols<-c(globs$DefDesign$DesignKolsFA[globs$DefDesign$DesignKolsFA %in% names(DF)],names(DF)[grepl("^VAL\\d+(\\.(f|a)|)$",names(DF))])
    if (echo==TRUE){
      print(Kols)
      cat("Nest siste trinn\n#########################\n")
    }

    
    #print(filbesk)
    #kAN KRÆSJE VED UKJENT KOLNAVN!
    #print(FGP)
    DF<-DF[,Kols]
   
    #Kast rader for inaktive GEO med alle VAL==NA (må gjøres fordi alle kommunekoder gir utrapportert tall fra STATBANK og 0/NA er ikke nøytralt for ikke-sumerbare kolonner, jfr MEDIANINNT)
    #Merk at ekte NA settes inn igjen når det rektangulariseres på aktive kommuner ved kubeproduksjon
    GeoFra<-setNames(globs$GeoKoder$FRA,globs$GeoKoder$GEO)
    GeoTil<-setNames(globs$GeoKoder$TIL,globs$GeoKoder$GEO)
    valkols<-FinnValKols(names(DF))
    #Skjønner ikke hvorfor dette ikke funker
    
    DF2<-DF[!((unlist(GeoTil[DF$GEO])<=DF$AARl | unlist(GeoFra[DF$GEO])>=DF$AARh) & rowSums(is.na(data.frame(DF[,valkols])))==length(valkols)),]
    DF<-DF2
    
    
    #Aggreger ned. Unntaksvis der filene er "ucollapset" 
    #etter f.eks omkoding av alder til aldersgrupper
    #Om ikke dette gjøres blir det masse dubletter
    if(!is.na(filbesk$AGGERGER_DF) & filbesk$AGGERGER_DF==1){
      print("SKAL COLLAPSE")
      print(dim(DF))
      DF<-KHaggreger(DF,globs=globs)
      print(dim(DF))
    }
    
    
    DF$KOBLID<-filbesk$KOBLID
    DF$ROW<-1:nrow(DF)
        
    TilFilLogg(filbesk$KOBLID,"FINALh",DFHeadToString(DF),batchdate=batchdate,globs=globs)    
  }
  TilFilLogg(filbesk$KOBLID,"TidLagTab",(proc.time()-klokke)[3],batchdate=batchdate,globs=globs)
  
  if(versjonert==TRUE){
    SVcloneRecord(globs$log,"INNLES_LOGG",filbesk$KOBLID)
    SVcloneRecord(globs$log,"KODEBOK_LOGG",filbesk$KOBLID)
    #SVcloneRecord(globs$log,"KODEBOK_LOGG",filbesk$KOBLID)
  }
  
  if (ok==0){
    DF<-data.frame()
    #DF<-DF[0,] #Fungerer ikke mht class, som kan være feil
  }
 
  return(DF)
}

#
LesFil<-function (filbesk,batchdate=SettKHBatchDate(),globs=FinnGlobs(),dumps=character()) {
  klokke<-proc.time()
  DF<-data.frame()
  ok<-1
  filn<-filbesk$filn
  format<-filbesk$FORMAT
  opt<-filbesk$INNLESARG
  
  
  #Initier log
  sqlQuery(globs$log,paste("DELETE * FROM INNLES_LOGG WHERE KOBLID=",filbesk$KOBLID,"AND SV='S'",sep=""))
  sqlQuery(globs$log,paste("INSERT INTO INNLES_LOGG ( KOBLID,BATCH, SV, FILGRUPPE) SELECT =",filbesk$KOBLID,",'",batchdate,"', 'S','",FinnFilGruppeFraKoblid(filbesk$KOBLID),"'",sep=""))
  
  #Sjekk om fil eksisterer
  if(file.access(filn,mode=0)==-1){
    TilFilLogg(filbesk$KOBLID,"FILNAVNERR",paste("KRITISK FEIL: ",filn," finnes ikke",sep=""),batchdate=batchdate,globs=globs)
    ok<-0
  } else if (file.access(filn,mode=4)==-1){
    TilFilLogg(filbesk$KOBLID,"FILNAVNERR",paste("KRITISK FEIL: ",filn," finnes, men lar seg ikke lese",sep=""),batchdate=batchdate,globs=globs)
    ok<-0
  } else { 
    
    default.stringsAsFactors<-FALSE
    
    format<-toupper(format)
    formats<-c("CSV","XLS","XLSX","SPSS","DBF","SAS","HTML")
    if (!format %in% formats){
      ok<-0
      TilFilLogg(filbesk$KOBLID,"INNLESARGerr",paste("FORMAT ",format, " ikke kjent, kjenner bare (",paste(formats,collapse=","),")",sep=""),batchdate=batchdate,globs=globs)
    } else {
      #LES INN FIL
      #Skreddersydd feilstyring
      innleserr<-""
      if (format=='XLS' || format =='XLSX'){
        expr<-paste("Xls2R.KH(filn",ifelse(is.na(opt),"",paste(",",opt,sep="")),",globs=globs)",sep="")
        xls<-eval(parse(text=expr))
        DF<-xls$DF
        ok<-xls$ok
        innleserr<-xls$err
      } else {
        #Feilstyring fra eksterne rutiner med try()
        if (format=='CSV'){
          expr<-paste("KHCsvread(filn",ifelse(is.na(opt),"",paste(",",opt,sep="")),")",sep="")
          INNLES<-try(eval(parse(text=expr)),silent=TRUE)
        } else if (format=='SPSS'){
          INNLES<-try(as.data.frame(read.spss(file=filn, use.value.labels = FALSE,max.value.labels = 0),stringsAsFactors=FALSE),silent=TRUE)
          #ALternativ metode: T<-spss.get(file=fil)   
        } else if (format=='DBF'){
          #DEV sl? av Field name: '***NULL***' changed to: 'X...NULL...'
          INNLES<-try(suppressMessages(read.dbf(file=filn,as.is=TRUE)),silent=TRUE)      
        } else if (format=='SAS'){
          INNLES<-try(read.sas7bdat(file=filn),silent=TRUE)
        } else if (format=="HTML"){
          INNLES<-try(eval(parse(text=paste("DF<-readHTMLTable(doc=filn,as.data.frame = TRUE,stringsAsFactors=FALSE",ifelse(is.na(opt),"",paste(",",opt,sep="")),")",sep=""))),silent=TRUE)
        }
        if(class(INNLES)=="try-error"){
          innleserr<-INNLES          
          ok<-0
        } else {
          DF<-INNLES          
        }
      }
      #Må sikre at data.frame, noen filer kan være bare en skalar (jfr ENPERSON)
      DF<-as.data.frame(DF,stringsAsFactors=FALSE)
      if (ok==0){
        TilFilLogg(filbesk$KOBLID,"INNLESARGerr",innleserr,batchdate=batchdate,globs=globs)
      } else {
        #PRINT INNLES
        TilFilLogg(filbesk$KOBLID,"INNLESh",DFHeadToString(DF),batchdate=batchdate,globs=globs)
      }
      
    }
  }
  
  #Fortsett hvis lest inn er ok
  if(ok==1){    
    #Gjør om innlest CSV-aktig til tabell
    
    if (format %in% c("CSV","XLS","XLSX")){
      eval(parse(text=paste("DF<-cSVmod(DF,filbesk,",ifelse(is.na(opt),"",paste(",",opt,sep="")),",globs=globs)",sep="")))
    }
    
    #Sett header manuelt
    #IKKE robust for feil parameter
    if (!is.na(filbesk$MANHEADER)){
      mh<-unlist(str_split(filbesk$MANHEADER,"="))
      mh[1]<-gsub("\\[|\\]","",mh[1])
      eval(parse(text=paste("mhs<-",mh[2],sep="")))
      eval(parse(text=paste("mhi<-c(",mh[1],")",sep="")))
      names(DF)[mhi]<-mhs
      
      #Skjønner ikke helt hvorfor ikke denne enkler funker:
      #eval(parse(text=paste("names(DF)",filbesk$MANHEADER,sep="")))    
    }
    
    #Fix header
    names(DF)<-gsub("^\\s","",names(DF))
    names(DF)<-gsub("\\s$","",names(DF))
    names(DF)[names(DF)==""]<-paste("C",which(names(DF)==""),sep="")
    
    ## These variables are to be use in Stata process (request from Jørgen)
    ## They will be deleted when Stata RSYNT1 is completed below
    DF$filgruppe <- filbesk$FILGRUPPE
    DF$delid <- filbesk$DELID
    DF$tab1_innles <- filbesk$TAB1

    #DEV dette b?r v?re un?dvendig '' skal v?re lest inn som NA
    #DF[DF==""]<-NA
    #des<-lapply(DF,class)
    #if(length(des[des=="factor"])>0){
    #  cat("FACTOR i DF <",paste(names(des[des=="factor"]),collapse="><"),">, det er ugreit\n",sep="")
    #}
    
    
    TilFilLogg(filbesk$KOBLID,"modINNLESh",DFHeadToString(DF),batchdate=batchdate,globs=globs)
    
    
    
    if ("RSYNT1pre" %in% names(dumps)){
      for (format in dumps[["RSYNT1pre"]]) {
        DumpTabell(DF,paste(filbesk$FILGRUPPE,filbesk$KOBLID,"RSYNT1pre",sep="_"),globs=globs,format=format)
      }
    }
    
    ######################################################
    #EVT SPESIALBEHANDLING
    if (!is.na(filbesk$RSYNT1)){
      synt<-gsub("\\\r","\\\n",filbesk$RSYNT1)
      error<-""
      ok<-1
      if (grepl("<STATA>",synt)){
        synt<-gsub("<STATA>[ \n]*(.*)","\\1",synt)
        RES<-KjorStataSkript(DF,synt,batchdate=batchdate,globs=globs)
        if (RES$feil!=""){
          error<-paste("Noe gikk galt i kjøring av STATA",RES$feil,sep="\n")
          ok<-0
        } else {
          DF<-RES$TABLE
        }
      } else {
        rsynterr<-try(eval(parse(text=synt)),silent=TRUE)
        if(class(rsynterr)=="try-error"){
          ok<-0
          error<-rsynterr
        }
      }
      if (ok==0){
        print(error)
        TilFilLogg(filbesk$KOBLID,"RSYNT1ERR",error,batchdate=batchdate,globs=globs)
      }
    }
    
  }
  
  if ("RSYNT1post" %in% names(dumps)){
    for (format in dumps[["RSYNT1post"]]) {
      DumpTabell(DF,paste(filbesk$FILGRUPPE,filbesk$KOBLID,"RSYNT1post",sep="_"),globs=globs,format=format)
    }
  }
  
  #sink(file=paste(globs$path,"/hoder.txt",sep=""),append=TRUE)
  #cat("\n#################\nFIL: ")
  #cat(filn)
  #cat("\n")
  #print(head(T))
  #sink()

  ## These variables are needed only in RSYNT1 for Stata
  DF[c("filgruppe","delid","tab1_innles")] <- NULL

  TilFilLogg(filbesk$KOBLID,"TidLesFil",(proc.time()-klokke)[3],batchdate=batchdate,globs=globs)
  default.stringsAsFactors=TRUE 
  return(list(DF=DF,ok=ok))
}



#Funksjoner brukt i innlesing
##########################################################

#
KHCsvread<-function (filn,header=FALSE,skip=0,colClasses="character",sep=";",quote = "\"",dec = ".",fill=FALSE,encoding = "unknown",blank.lines.skip=FALSE,na.strings=c("NA"),brukfread=TRUE,...) {
  if(!(quote=="\"" && dec=="." && fill==FALSE && encoding == "unknown")){
    brukfread<-FALSE
  } 
  if (brukfread==TRUE){
    csvT<-as.data.frame(fread(filn,header=FALSE,skip=0,colClasses="character",sep=sep,na.strings=na.strings))
  } else {
    csvT<-as.data.frame(read.csv(filn,header=FALSE,skip=0,colClasses="character",sep=sep,quote=quote,dec=dec,fill=TRUE,encoding=encoding,blank.lines.skip=FALSE,na.strings=na.strings))
  }
  return(csvT)
}

#
cSVmod<-function(DF,filbesk,header=TRUE,skip=0,slettRader=integer(0),sisteRad=-1,TomRadSlutt=FALSE,FjernTommeRader=FALSE,FjernTommeKol=TRUE,globs=FinnGlobs(),...){
  #Ved bruk av undertabeller med titler som ikke står i egen kolonne
  #Lager egen kolonne av undertitler som blir ekta TAB
  #Ikke så veldig elegant, men funker for de får tilfellene der dette trengs og som ellers ville trengt håndsøm
  #Syntaks UNDERTABLOK er TAB:kolonne:kommasep liste undertitler:kommasep liste/skalar offset av disse (dvs antall raders forrykking)
  if (!is.na(filbesk$UNDERTABLOK)){
    utl<-unlist(str_split(filbesk$UNDERTABLOK,":"))
    loks<-as.numeric(unlist(str_split(utl[3],",")))
    offsets<-as.numeric(unlist(str_split(utl[4],",")))
    nytab<-character(nrow(DF))
    nytab[loks+offsets]<-DF[loks,as.numeric(utl[2])]
    nytab[nytab==""]<-NA
    nytab<-na.locf(nytab,na.rm=FALSE)
    DF<-cbind(DF,nytab,stringsAsFactors=FALSE)
  }
  
  if (length(slettRader)>0){
    DF<-DF[-slettRader,]
  }
  if (skip>0){
    DF<-DF[-(1:skip),]
  }
  if (sisteRad>0) {
    DF<-DF[1:(sisteRad-skip-length(slettRader)),]
  }
  
  if (TomRadSlutt==TRUE){
    tomr<-which(rowSums(is.na(DF) | DF=="") == ncol(DF))
    if (!is.na(tomr[1])){
      DF<-DF[1:(tomr[1]-1),]
    }      
  }
  if (FjernTommeRader==TRUE){
    DF<-DF[rowSums(is.na(DF) | DF=="") != ncol(DF),]
  }
  if (FjernTommeKol==TRUE){
    DF<-DF[,colSums(is.na(DF) | DF=="") != nrow(DF)]
  }
  #Må sikre at data.frame, noen filer kan være bare en skalar (jfr ENPERSON)
  DF<-as.data.frame(DF,stringsAsFactors=FALSE)
  
  #Sett header. Default er vanlige Excel-kolonnenavn
  names(DF)<-globs$XLScols[1:length(names(DF))]
  
  #Bruk av flernivå header.
  #Ikke særlig elegant syntaks, men prinsippet er rett fram
  #Disse pastes (evt) sammen til en header
  #Etter reshape splittes kolonneraden (som nå har blitt en kolonne) 
  #i sine respektive kolonner
  #Kan også være pastet sammen originalt
  #Syntaks gir radnummer for de ulike leddene i multihead "c(TABNAVN1=rad1,TABNAVN2=rad2,...)
  if (!is.na(filbesk$MULTIHEAD)){
    #Prossesser parameterstreng for multihead, gir liste med relevante deler
    mhl<-LesMultiHead(filbesk$MULTIHEAD)
    #Juster radnummerering for skip
    mhl$rader<-mhl$rader-skip
    headers<-DF[mhl$rader,]
    headers[headers==""]<-NA
    #Fyll inn ved "sparse" utfylling, slik som ved "innrykket" tabulering i kolonner
    headers<-na.locf(t(headers),na.rm=FALSE)
    #Paste sammen
    headstr<-apply(headers,1,paste,collapse=mhl$sep)
    #Sett nye kolonnenavn for de som dekkes av headstr, 
    #resten beholder sine standard ("excel") genererte navn
    nonempty<-as.vector(which(headstr!=""))
    names(DF)[nonempty]<-headstr[nonempty]
    #Dropp linjer brukt til header
    DF<-DF[-(1:length(mhl$rader)),]
  } else if (header==TRUE){
    if (nrow(DF)>1){
      #Bruk defaultnavn i celler der header mangler
      nonempty<-as.vector(which(DF[1,]!=""))
      names(DF)[nonempty]<-DF[1,nonempty]
      DF<-DF[-1,]
    } else {
      print("*******************ADVARSEL: Kan ikke sette header fra fil med bare en rad. Skal vel ha opsjon 'header=FALSE'")
    }
  }
  if (!is.na(filbesk$UNDERTABLOK)){
    names(DF)[ncol(DF)]<-gsub("^(.*?):.*","\\1",filbesk$UNDERTABLOK) 
  }
  names(DF)<-gsub("^ *| *$","",names(DF))
  #names(DF)<-gsub("[ ,./()+-]","_",names(DF))   #Skal navn fikses? Argumenter for og mot. Valgt: Nei!
  return(DF)
}

#
LesMultiHead<-function(mhstr){
  #Leser parameterstreng for multihead og gjør om til relevante variable
  #Velger å kalle på denne funksjonen ved behov for samme inputstreng heller enn å porssessere strengen en gang og sende bitene rundt
  #Finn evt angitt separator (trengs bare settes dersom det er snakk om en originalt pastet rad med annen seaprator enn "|"
  if (grepl("sep=\".\"",mhstr)){
    sep<-sub(".*,sep=\"(.)\"","\\1",mhstr)
    mhstr<-sub("(.*),sep=\".\"","\\1",mhstr)
  } else {
    sep<-"&"
  }
  #Les inn rader som inneholder deler
  eval(parse(text=paste("mh<-c(",mhstr,")")))
  colnames<-names(mh)
  #Sett paste av tabnavn. Denne blir senere splitta til kolonnenavn
  varname<-paste(names(mh),collapse="_")
  #Fjern rader som er duplikater, dvs som allerede er pastet sammen originalt 
  rader<-mh[!duplicated(mh)]
  return(list(rader=rader,sep=sep,colnames=colnames,varname=varname))
}

#
#
Xls2R.KH.Gammel<-function(xlsfil,ark="",globs=FinnGlobs(),brukfread=TRUE,na.strings=c("NA"),ryddOpp=1,...){
  err<-""
  ok<-1
  DF<-data.frame()
  #step 1: Validate sheetname with fuzzy match
  rdbh<-odbcConnectExcel2007(xlsfil)
  #rdbh<-odbcConnectExcel(xlsfil)
  tables<-sqlTables(rdbh)$TABLE_NAME
  close(rdbh)
  
  tables<-gsub("\'","",tables)  
  tables<-gsub("\\$","",tables)  #Something is strange with resepct to $ in R's regexp-syntax, but should work
  if (ark=="" | is.na(ark)){
    ark<-tables[1]
  } else if (!(ark %in% tables)){
    kand<-tables[grepl(ark,tables,ignore.case=TRUE)]
    if (length(kand)==1){
      ark <- kand[1]
    } else if (length(kand)>1){
      err<-paste("Arknavn ",ark," ikke unik, passer med flere av (",paste(tables,collapse=","),")",sep="")
      ok<-0
    } else {
      err<-paste("Arknavn ",ark," finnes ikke (",paste(tables,collapse=","),")",sep="")
      ok<-0
    }    
  }
  if (ok==1){
    #Step 2: convert xls to temporary csv
    tmpcsvfil<-Xls2TmpCsv(xlsfil,sheet=ark,globs=globs)
    #Step 3: read csv. Note: fread (from data.table) can be picky if number of columns in each row is not equal
    #But it is considerably faster than read.csv
    if (brukfread==TRUE){
      INNLES<-try(as.data.frame(fread(tmpcsvfil,sep=",",colClasses="character",header=FALSE,skip=0,na.strings=na.strings)))
    } else {
      INNLES<-try(read.csv(tmpcsvfil,sep=",",colClasses="character",header=FALSE,skip=0,blank.lines.skip=FALSE,na.strings=na.strings))
    }
    if(class(INNLES)=="try-error"){
      err<-INNLES
      ok<-0
    } else {
      DF<-INNLES
    }
    
    #Clean up!!!
    if (ryddOpp==1){
      file.remove(tmpcsvfil)
    }
  }
  return(list(DF=DF,ok=ok,err=err))
}

Xls2R.KH<-function(xlsfil,ark="",globs=FinnGlobs(),brukfread=TRUE,na.strings=c("NA"),ryddOpp=1,...){
  err<-""
  ok<-1
  DF<-data.frame()
  #step 1: Validate sheetname with fuzzy match
  #rdbh<-odbcConnectExcel2007(xlsfil)
  #rdbh<-odbcConnectExcel(xlsfil)
  #tables<-sqlTables(rdbh)$TABLE_NAME
  #close(rdbh)
  
  tables<-excel_sheets(xlsfil)
  
  tables<-gsub("\'","",tables)  
  tables<-gsub("\\$","",tables)  #Something is strange with resepct to $ in R's regexp-syntax, but should work
  if (ark=="" | is.na(ark)){
    ark<-tables[1]
  } else if (!(ark %in% tables)){
    kand<-tables[grepl(ark,tables,ignore.case=TRUE)]
    if (length(kand)==1){
      ark <- kand[1]
    } else if (length(kand)>1){
      err<-paste("Arknavn ",ark," ikke unik, passer med flere av (",paste(tables,collapse=","),")",sep="")
      ok<-0
    } else {
      err<-paste("Arknavn ",ark," finnes ikke (",paste(tables,collapse=","),")",sep="")
      ok<-0
    }    
  }
  if (ok==1){
    
    INNLES<-try(as.data.frame(read_excel(xlsfil,sheet=ark,col_names=FALSE,col_types="text",skip=0,na=na.strings)))
    if(class(INNLES)=="try-error"){
      err<-INNLES
      ok<-0
    } else {
      DF<-setNames(INNLES, globs$XLScols[1:ncol(INNLES)])
      ## ## Finner ut hvis hele kolonne er missing
      ## colMISS = sapply(DF, function(x) all(is.na(x)))
      ## missUT = attributes(colMISS[colMISS==1])$names
      ## DF[missUT] = NULL
    }
    
  }
  return(list(DF=DF,ok=ok,err=err))
}
#
Xls2TmpCsv<-function(xlsfil,sheet,globs=FinnGlobs()){
  #Calls on VB-script that converts sheet in xlsfil to csv 
  #Should use tempdir()?
  orig_wd <- getwd()
  setwd(paste(globs$path,globs$binDir,sep="/"))
  xlsfil<-gsub("/","\\\\",xlsfil)
  #print(paste("XLStoCSVconverter.vbs \"", xlsfil, "\" \"",sheet,"\"",sep=""))
  shell(paste("XLStoCSVconverter.vbs \"", xlsfil, "\" \"",sheet,"\"",sep=""), intern = TRUE)
  #shell(paste("XLStoCSVconverter.vbs \"", xlsfil, "\"",sep=""), intern = TRUE)
  setwd(orig_wd)
  return(paste(globs$path,"/",globs$binDir,"/tmpfiler/XLStoCSVconverterTmpCsv",".csv",sep=""))
}

#
ReshapeTab<-function (DELF,filbesk,batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  #Reshape av DELF basert på parametre i filbesk
  ok<-1
  if (!(is.na(filbesk$RESHAPEid) || filbesk$RESHAPEid=='')){
    idvars<-eval(parse(text=paste("c(",filbesk$RESHAPEid,")")))
  } else {
    idvars<-NULL
  }
  #idvars<-SettCols(filbesk$RESHAPEid,names(T))
  #mevars<-SettCols(filbesk$RESHAPEmeas,names(T))
  mevars<-NULL
  if (!(is.na(filbesk$RESHAPEmeas) || filbesk$RESHAPEmeas=='')){
    mevars<-eval(parse(text=paste("c(",filbesk$RESHAPEmeas,")")))
  }
  varname<-"variable"
  valname<-"value"
  #varname må tas fra MULTIHEAD om denne brukes
  if (!is.na(filbesk$MULTIHEAD)){
    varname<-LesMultiHead(filbesk$MULTIHEAD)$varname
  } else if (!(is.na(filbesk$RESHAPEvar) || filbesk$RESHAPEvar=='')){
    varname<-as.character(filbesk$RESHAPEvar)
  }
  if (!(is.na(filbesk$RESHAPEval) || filbesk$RESHAPEval=='')){
    valname<-as.character(filbesk$RESHAPEval)
  }
  
  if(all(idvars %in% names(DELF)) & (is.null(mevars) | all(mevars %in% names(DELF)))){  
    DELF[,idvars]<-sapply(DELF[,idvars],as.character)  #Må være av samme type for at ikke reshape skal kræsje  
    if (!is.null(mevars)){
      DELF<-data.table::melt(as.data.table(DELF),id.vars=idvars,measure.vars=mevars,variable.name=varname,value.name=valname,na.rm=FALSE)      
    } else {
      DELF<-data.table::melt(as.data.table(DELF),id.vars=idvars,variable.name=varname,value.name=valname,na.rm=FALSE)
    }
    data.table::setDF(DELF)
    DELF[,varname]<-as.character(DELF[,varname])   #Kan ha blitt factor, og det gir krøll senere
  } else {
    rshperr<-""
    if (!all(idvars %in% names(DELF))){
      rshperr<-paste(rshperr,"Ukjente idvars <",paste(idvars[!idvars %in% names(DELF)],">."))              
    }
    if (!is.null(mevars) & !all(mevars %in% names(DELF))){
      rshperr<-paste(rshperr,"Ukjente mevars <",paste(mevars[!mevars %in% names(DELF)],">."))              
    }
    TilFilLogg(filbesk$KOBLID,"RESHAPEERR",rshperr,batchdate=batchdate,globs=globs)
    ok<-0
  }
  
  
  return(list(DF=DELF,ok=ok))
}


#VASK AV TAB-kolonner og VAl-kolonner
##########################################################
subsant<-data.frame(ORG=character(0),KBOMK=character(0),OMK=character(0),FREQ=integer(0),OK=integer(0))


KBomkod<-function(org,type,filbesk,valsubs=FALSE,batchdate=NULL,globs=FinnGlobs()) {
    
    datef<-format(Sys.time(), "#%Y-%m-%d#")
  if (!is.null(batchdate)){
    datef<-format(strptime(batchdate, "%Y-%m-%d-%H-%M"),"#%Y-%m-%d#")
  }
  omk<-org
  kbf<-paste(type,"kb",sep="")
  sql<-paste("SELECT TYPE, ORGKODE, NYKODE FROM KODEBOK WHERE 
             FELTTYPE='",type,
             "' AND FILGRUPPE='",filbesk$FILGRUPPE,
             "' AND (DELID='",filbesk$DELID,"' OR DELID='FELLES')",
             " AND VERSJONFRA<=",datef, 
             " AND VERSJONTIL>",datef,sep="")
  kbok<-sqlQuery(globs$dbh,sql,as.is=TRUE)
  kbok[is.na(kbok)]<-""
  subsant<-data.frame(ORG=character(0),KBOMK=character(0),OMK=character(0),FREQ=integer(0),OK=integer(0))
  if (nrow(kbok)>0){
    KBsubs<-subset(kbok,TYPE=="SUB")   #Regulæruttrykk
    KB<-subset(kbok,TYPE=="KB")       #Oppslagsliste
    i<-1
    while (i<=nrow(KBsubs)){
      KBsub<-KBsubs[i,]
      if (valsubs==TRUE){
        subsant<-rbind(subsant,data.frame(ORG=KBsub$ORGKODE,KBOMK=paste("<",KBsub$NYKODE,">",sep=""),OMK=paste("<",KBsub$NYKODE,">",sep=""),FREQ=length(grepl(KBsub$ORGKODE,omk,perl=TRUE)),OK=1))
      }
      #omk<-sub(eval(parse(text=KBsub$ORGKODE)),eval(parse(text=KBsub$NYKODE)),omk)
      omk<-sub(KBsub$ORGKODE,KBsub$NYKODE,omk,perl=TRUE)
      i<-i+1
    }
    if (valsubs==TRUE){
      #Ta bare numeriske fra
      KB<-KB[!is.na(suppressWarnings(as.numeric(KB$ORGKODE))),]
      if (nrow(KB)>0){
        freq<-table(omk)
        freq<-freq[KB$ORGKODE]
        if (!is.na(freq)){
           subsant<-rbind(subsant,data.frame(ORG=KB$ORGKODE,KBOMK=KB$NYKODE,OMK=KB$NYKODE,FREQ=freq,OK=1))
          #tmp2<-as.data.frame(table(DF$GEO,useNA="ifany"),stringsAsFactors=FALSE)
          omk<-mapvalues(omk,KB$ORGKODE,KB$NYKODE,warn_missing = FALSE)
        }        
      }
    } else {
      omk<-mapvalues(omk,KB$ORGKODE,KB$NYKODE,warn_missing = FALSE)
    }    
  }
  
  if (valsubs==FALSE){
    return(omk)
  } else {
    return(list(omk=omk,subsant=subsant))
  }
}
# KBomkod<-function(org,type,filbesk,valsubs=FALSE,batchdate=NULL,globs=FinnGlobs()) {
#   datef<-format(Sys.time(), "#%Y-%m-%d#")
#   if (!is.null(batchdate)){
#     datef<-format(strptime(batchdate, "%Y-%m-%d-%H-%M"),"#%Y-%m-%d#")
#   }
#   omk<-org
#   kbf<-paste(type,"kb",sep="")
#   sql<-paste("SELECT TYPE, ORGKODE, NYKODE FROM KODEBOK WHERE 
#                 FELTTYPE='",type,
#                 "' AND FILGRUPPE='",filbesk$FILGRUPPE,
#                 "' AND (DELID='",filbesk$DELID,"' OR DELID='FELLES')",
#                 " AND VERSJONFRA<=",datef, 
#                 " AND VERSJONTIL>",datef,sep="")
#   kbok<-sqlQuery(globs$dbh,sql,as.is=TRUE)
#   kbok[is.na(kbok)]<-""
#   subsant<-data.frame(ORG=character(0),KBOMK=character(0),OMK=character(0),FREQ=integer(0),OK=integer(0))
#   if (nrow(kbok)>0){
#     KBsubs<-subset(kbok,TYPE=="SUB")   #Regulæruttrykk
#     KB<-subset(kbok,TYPE=="KB")       #Oppslagsliste
#     
#     #For verdifeltene må omkoding tas før regexp for riktig eksport til LOGG_kODEBOK
#     #For andre må regexp tas før omoding for riktig resultat, derfor litt omstendelig før
#     if (valsubs==TRUE){
#       omk2<-mapvalues(omk,KB$ORGKODE,KB$NYKODE,warn_missing = FALSE)
#       tmp<-data.table(ORG=omk[omk!=omk2],KBOMK=omk2[omk!=omk2],key=c("ORG","KBOMK"))
#       if (nrow(tmp)>0){
#         tmp<-as.data.frame(table(tmp,useNA="ifany"),stringsAsFactors=FALSE)
#         subsant<-rbind(subsant,data.frame(ORG=tmp$ORG,KBOMK=tmp$KBOMK,OMK=tmp$KBOMK,FREQ=tmp$Freq,OK=1,stringsAsFactors=FALSE))
#       }
#       omk<-omk2         
#     }
#     i<-1
#     while (i<=nrow(KBsubs)){
#       KBsub<-KBsubs[i,]
#       omk<-sub(KBsub$ORGKODE,KBsub$NYKODE,omk)
#       if (valsubs==TRUE){
#         subsant<-rbind(subsant,data.frame(ORG=KBsub,KBOMK="<SUB>",OMK="<SUB>",FREQ=length(grepl(KBsub,omk)),OK=1))
#       }
#       i<-i+1
#     }
#     if (valsubs==FALSE){      
#       omk<-mapvalues(omk,KB$ORGKODE,KB$NYKODE,warn_missing = FALSE)
#     }
#   }
#   if (valsubs==FALSE){
#     return(omk)
#   } else {
#     return(list(omk=omk,subsant=subsant))
#   }
# }

GEOvask<-function (geo,filbesk=data.frame(),batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  if (nrow(filbesk)==0){
    geo<-setNames(as.data.frame(geo,stringsAsFactors=FALSE),c("GEO"))
    geo$KBOMK<-geo[,1]
  } else { 
    geo$KBOMK<-KBomkod(geo$ORG,type="GEO",filbesk=filbesk,batchdate=batchdate,globs=globs)
   if (!is.na(filbesk$TKNR)){
      suppressWarnings(geo$KBOMK[geo$ORG==geo$KBOMK]<-mapvalues( geo$ORG[geo$ORG==geo$KBOMK],globs$TKNR$ORGKODE,globs$TKNR$NYKODE,warn_missing = FALSE))
    }
  }
  geo$OMK<-geo$KBOMK
  geo$OK<-1
  #Litt dirty her, sprintf funker d?rlig p? Windows: sprintf("%04s","214") -> " 0214"
  #M? bruke sprintf("%04s",as.numeric("0214")) for ? f? "0214", det blir for dumt
  geo$OMK<-sub("^\\s*","",geo$OMK,ignore.case = TRUE)
  geo$OMK<-sub("\\s*$","",geo$OMK,ignore.case = TRUE)
  geo$OMK<-sub("^0{1,2}(( hele|) landet| *$)","0",geo$OMK,ignore.case = TRUE)
  geo$OMK<-sub("^(Hele +|)landet( i alt|) *$","0",geo$OMK,ignore.case = TRUE)
  geo$OMK<-sub("^Fylke (\\d{1,2})$","\\1",geo$OMK)
  geo$OMK<-sub("^(\\d{6})a{0,1}( +[A-ZÆØÅ].*| *$)","\\1",geo$OMK)
  geo$OMK<-sub("^(\\d{5})a{0,1}( +[A-ZÆØÅ].*| *$)","0\\1",geo$OMK)
  geo$OMK<-sub("^(\\d{4})( +[A-ZÆØÅ].*| *$)","\\1",geo$OMK)
  geo$OMK<-sub("^(\\d{3})( +[A-ZÆØÅ].*| *$)","0\\1",geo$OMK)
  geo$OMK<-sub("^([012][1-9]|10|20|88|99)( +[A-ZÆØÅ].*| *$)","\\1",geo$OMK)
  geo$OMK<-sub("^([1-9])( +[A-ZÆØÅ].*| *$)","0\\1",geo$OMK)
 
  geo$OMK<-sub("^(\\d{4})xx*","\\1",geo$OMK,ignore.case = TRUE)
    
  #Kode fra navn
  #Må bli mer avansert for å bli robust. Koder nå 1 til flere (Nes, etc)
  UGeo<-data.frame(NAVN=geo$OMK[!grepl("^\\d+$",geo$OMK)])
  if(nrow(UGeo)>0){
    GeoNavn<-sqlQuery(globs$dbh,"SELECT * from GeoNavn",as.is=TRUE)
    omk<-sqldf("SELECT GEO, UGeo.NAVN FROM UGeo INNER JOIN GeoNavn ON UGeo.NAVN=GeoNavn.NAVN") 
    geo$OMK<-mapvalues(geo$OMK,omk$NAVN,omk$GEO,warn_missing = FALSE)
  }
  
  
  if (grepl("4",filbesk$SONER)){
    geo$OMK[nchar(geo$OMK)==4]<-paste(geo$OMK[nchar(geo$OMK)==4],"00",sep="")
  }
  
  #Finn ukjente koder. Sett til ukjent (99) under fylke eller by om mulig, ellers 
  #TMP<-globs$GeoKoder
  #ukjent<-sqldf("SELECT OMK FROM geo LEFT JOIN TMP ON geo.OMK=TMP.GEO WHERE TMP.ID Is NULL")
  #print(head(globs$GeoKoder))
  #print(geo[1:50,])
  #print(which(!(geo[,"OMK"] %in% globs$GeoKoder$GEO)))
  ukjent<-geo$OMK[!(geo$OMK %in% c(globs$GeoKoder$GEO,"-"))]
  
  
  ukjent99<-ukjent
  ukjent99<-sub("^\\d{2}$",99,ukjent99) #Ukjent fylke
  ukjent99<-gsub("^(\\d{2})\\d{2}$",paste("\\1","99",sep=""),ukjent99) #Ukjent kommune
  ukjent99<-sub("^(\\d{2})(\\d{2})00$",paste("\\1","9900",sep=""),ukjent99) #Ukjent bydel
  ukjent99<-sub("^(\\d{4})([1-9]\\d|0[1-9])$",paste("\\1","99",sep=""),ukjent99) #Ukjent bydel
  
  #Sjekk om legitime 99-ukjente
  ukjent<-ukjent[ukjent99 %in% globs$GeoKoder$GEO]
  ukjent99<-ukjent99[ukjent99 %in% globs$GeoKoder$GEO]
  geo$OMK<-mapvalues(geo$OMK,ukjent,ukjent99,warn_missing = FALSE)
  
  ukjent<-geo$OMK[!(geo$OMK %in% c(globs$GeoKoder$GEO,"-"))]
  heltukjent<-ukjent
  heltukjent[nchar(ukjent)==6]<-999999
  heltukjent[nchar(ukjent)==4]<-9999
  heltukjent[nchar(ukjent)==2]<-99
  geo$OMK<-mapvalues(geo$OMK,ukjent,heltukjent,warn_missing = FALSE)
  
  #Sett GEOniv
  geo$GEOniv<-as.character(NA)
  geo$GEOniv[nchar(geo$OMK)==8]<-"G"
  if (grepl("6",filbesk$SONER)){
    geo$GEOniv[nchar(geo$OMK)==6]<-"S"
  } else {
    geo$GEOniv[nchar(geo$OMK)==6]<-"B"
    geo$OMK[nchar(geo$OMK)==6]<-gsub("^(\\d{4})00$",paste("\\1","99",sep=""),geo$OMK[nchar(geo$OMK)==6])
  }
  geo$GEOniv[nchar(geo$OMK)==4]<-"K"
  # geo$GEOniv[nchar(geo$OMK)==2 & !geo$OMK %in% c(51:54)]<-"F"
  geo$GEOniv[nchar(geo$OMK)==2 & !geo$OMK %in% c(81:84)]<-"F"
  # geo$GEOniv[geo$OMK %in% c(51:54)]<-"H"
  geo$GEOniv[geo$OMK %in% c(81:84)]<-"H"
  geo$GEOniv[geo$OMK==0]<-"L"
  geo$GEOniv[geo$OMK=="-"]<-"-"
  geo$GEOniv[is.na(geo$GEOniv)]<-"U"      
  
  #Ekte ulegit
  geo$OK[geo$GEOniv=="-"]<-1
  #DEVELOP: bare et GEOniv, sett ukjent på dette nivået
  #Fil har bare kommunedata -> bruker 8888
  if (sum(c("G","B","F","L") %in% geo$GEOniv)==0){
    geo$OMK[geo$OK==0]<-"8888"
    geo$GEOniv[geo$OK==0]<-"K"
  } else {
    geo$OMK[geo$OK==0]<-globs$geo_illeg
  }    
  #Sett fylke
  geo$FYLKE<-NA
  subfylke<-which(geo$GEOniv %in% c("G","S","K","F","B"))
  geo$FYLKE[subfylke]<-substr(geo$OMK[subfylke],1,2)
  geo$FYLKE[geo$GEOniv %in% c("H","L")]<-"00"
  
  return(geo)
}

ALDERvask<-function(alder,filbesk=data.frame(),FGP=list(amin=0,amax=120),batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  amax<-FGP$amax
  amin<-FGP$amin
  if (nrow(filbesk)==0){
    alder<-setNames(as.data.frame(alder,stringsAsFactors=FALSE),c("ALDER"))
    alder$KBOMK<-alder[,1]
  } else { 
    alder$KBOMK<-KBomkod(alder$ORG,type="ALDER",filbesk=filbesk,batchdate=batchdate,globs=globs)
  }
  alder$OMK<-alder$KBOMK
  alder$OK<-1
  #alder$OMK<-sub("^ *(\\d+) *[\\_\\-] *(\\d+) *(.r|) *, *totalt$","\\1_\\2",alder$OMK,ignore.case = TRUE)
  alder$OMK<-sub("_år$"," år",alder$OMK)
  alder$OMK<-sub("(.+?),* *totalt *$","\\1",alder$OMK,ignore.case=TRUE)
  alder$OMK<-sub("^ *(\\d+) *[-_] *(\\d+)( +år| *$)","\\1_\\2",alder$OMK,ignore.case = TRUE)
  alder$OMK<-sub("^ *(\\d+) *- *high( +år| *$)","\\1_",alder$OMK,ignore.case = TRUE)
  alder$OMK<-sub("^ *low *- *(\\d+)( +år| *$)","_\\1",alder$OMK,ignore.case = TRUE)
  alder$OMK<-sub("^ *(\\d+) *\\+( +år| *$)","\\1_",alder$OMK,ignore.case = TRUE)
  alder$OMK<-sub("^ *(\\d+) +år +\\+ *$","\\1_",alder$OMK,ignore.case = TRUE)
  alder$OMK<-sub("^ *(\\d+) *-( +år| *$)","_\\1",alder$OMK,ignore.case = TRUE)
  alder$OMK<-sub("^ *(\\d+) *år *-$","_\\1",alder$OMK,ignore.case = TRUE)
  alder$OMK<-sub("^ *- *(\\d+)( +år| *$)","_\\1",alder$OMK,ignore.case = TRUE)
  alder$OMK<-sub("^ *(\\d+) år (og|eller) eldre","\\1_",alder$OMK,ignore.case = TRUE)
  alder$OMK<-sub("^ *over (\\d+)( å?r| *$)","\\1_",alder$OMK,ignore.case = TRUE)
  #alder$OMK<-sub("^ *under (\\d+)( +år| *$)","_\\1",alder$OMK,ignore.case = TRUE)  # Dette blri galt, må erstatte med "_(\\1-1)", men får ikke det til. Må bruke kdoebok
  alder$OMK<-sub("^ *(\\d+)( ?r|) *(og|eller) (yngre|under)","_\\1",alder$OMK,ignore.case = TRUE)
  alder$OMK<-sub("^ *(\\d+) *( +år| *$)","\\1_\\1",alder$OMK,ignore.case = TRUE)
  alder$OMK<-sub("^ *(Alle( *aldre.*|)|Totalt{0,1}|I alt) *$",paste(amin,"_",amax,sep=""),alder$OMK,ignore.case = TRUE)
  alder$OMK<-sub("^ *(Ukjent|Uoppgitt|Ikke kjent) *$",globs$alder_ukjent,alder$OMK,ignore.case = TRUE)
  
  #alder$OMK[is.na(alder$OMK)]<-"999_999"
  alder$OMK<-sub("^(\\d+)_$",paste("\\1_",amax,sep=""),alder$OMK)
  alder$OMK<-sub("^_(\\d+)$",paste(amin,"_\\1",sep=""),alder$OMK)
  
  #Ukjent????????
  #!Må ha to amax, en for ukjent som er høyere, se også ulest under!!!
  
  okformat<-grepl("^\\d+_\\d+$|^-$",alder$OMK)
  #Ugyldig verdi/ukjent kode
  alder$OMK[!okformat]<-globs$alder_illeg
  alder$OK[!okformat]<-0
  
  #Sett intervall
  alder[,c("LO","HI")]<-suppressWarnings(matrix(as.integer(str_split_fixed(alder$OMK,"_",2)),ncol=2))
  #Ugyldig intervall
  alder$OMK[alder$HI<alder$LO]<-globs$alder_illeg
  alder$OMK[alder$HI>130 & !(alder$OMK %in% c(globs$alder_illeg,globs$alder_ukjent))]<-globs$alder_illeg
  alder[,c("LO","HI")]<-suppressWarnings(matrix(as.integer(str_split_fixed(alder$OMK,"_",2)),ncol=2))
  return(alder)
}

KJONNvask<-function (kjonn,filbesk=data.frame(),batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  if (nrow(filbesk)==0){
    kjonn<-setNames(as.data.frame(kjonn,stringsAsFactors=FALSE),c("KJONN"))
    kjonn$KBOMK<-kjonn[,1]
  } else { 
    kjonn$KBOMK<-KBomkod(kjonn$ORG,type="KJONN",filbesk=filbesk,batchdate=batchdate,globs=globs)
  }  
  kjonn$OK<-1
  kjonn$OMK<-kjonn$KBOMK
  kjonn$OMK<-sub("^ *(M|Menn|Mann|gutt(er|)|g|1) *$","1",kjonn$OMK,ignore.case = TRUE)
  kjonn$OMK<-sub("^ *(K|F|Kvinner|Kvinne|jente(r|)|j|2) *$","2",kjonn$OMK,ignore.case = TRUE)
  kjonn$OMK<-sub("^ *(Tot(alt{0,1}|)|Begge([ \\._]*kjønn|)|Alle|A|0|M\\+K) *$","0",kjonn$OMK,ignore.case = TRUE)
  kjonn$OMK<-sub("^ *(Uspesifisert|Uoppgitt|Ikke spesifisert|Ikke oppgitt|Ukjent|) *$","9",kjonn$OMK,ignore.case = TRUE)
  #kjonn$OMK[is.na(kjonn$ORG)]<-9
  
  #Ugyldig verdi/ukjent kode
  kjonn$OMK[!(kjonn$OMK %in% c(0,1,2,9,"-"))]<-globs$kjonn_illeg
  kjonn$OK[!(kjonn$OMK %in% c(0,1,2,9,"-"))]<-0
  
  return(kjonn)
}

UTDANNvask<-function (utdann,filbesk=data.frame(),batchdate=SettKHBatchDate(),globs=FinnGlobs(),regexp=FALSE){
  if (nrow(filbesk)==0){
    utdann<-setNames(as.data.frame(utdann,stringsAsFactors=FALSE),c("UTDANN"))
    utdann$KBOMK<-utdann[,1]
  } else { 
    utdann$KBOMK<-KBomkod(utdann$ORG,type="UTDANN",filbesk=filbesk,batchdate=batchdate,globs=globs)
  }  
  utdann$OK<-1
  utdann$OMK<-utdann$KBOMK
  utdann$OMK<-sub("^0([0-4])$","\\1",utdann$OMK,ignore.case = TRUE)
  if(regexp==TRUE){
    utdann$OMK<-sub("^ *(grunnskole) *$","1",utdann$OMK,ignore.case = TRUE)
    utdann$OMK<-sub("^ *(videregående( skole|)) *$","2",utdann$OMK,ignore.case = TRUE)
    utdann$OMK<-sub("^ *(Universitet.*) *$","3",utdann$OMK,ignore.case = TRUE)
    utdann$OMK<-sub("^ *(anne[nt]|ingen|uopgitt|ukjent) *$","4",utdann$OMK,ignore.case = TRUE)
  }
  utdann$OMK<-sub("^ *(alle) *$","0",utdann$OMK,ignore.case = TRUE)
  
  #Ugyldig verdi/ukjent kode
  utdann$OMK[!(utdann$OMK %in% c(0,1,2,3,4,"-"))]<-globs$utdann_illeg
  utdann$OK[!(utdann$OMK %in% c(0,1,2,3,4,"-"))]<-0
  return(utdann)
}

SIVSTvask<-function (sivst,filbesk=data.frame(),batchdate=SettKHBatchDate(),globs=FinnGlobs(),regexp=FALSE){
  if (nrow(filbesk)==0){
    sivst<-setNames(as.data.frame(sivst,stringsAsFactors=FALSE),c("SIVST"))
    sivst$KBOMK<-sivst[,1]
  } else { 
    sivst$KBOMK<-KBomkod(sivst$ORG,type="SIVST",filbesk=filbesk,batchdate=batchdate,globs=globs)
  }  
  sivst$OK<-1
  sivst$OMK<-sivst$KBOMK
  if(regexp==TRUE){
    sivst$OMK<-sub("^ *(ugift|ug) *$","1",sivst$OMK,ignore.case = TRUE)
    sivst$OMK<-sub("^ *(gift|g) *$","2",sivst$OMK,ignore.case = TRUE)
    sivst$OMK<-sub("^ *(enke.*|e) *$","3",sivst$OMK,ignore.case = TRUE)
    sivst$OMK<-sub("^ *(skilt|separert|s|skilt\\/separert) *$","4",sivst$OMK,ignore.case = TRUE)
    sivst$OMK<-sub("^ *(annen) *$","5",sivst$OMK,ignore.case = TRUE)
    sivst$OMK<-sub("^ *(ukjent|uoppgitt) *$","9",sivst$OMK,ignore.case = TRUE)
  }
  sivst$OMK<-sub("^ *(alle) *$","0",sivst$OMK,ignore.case = TRUE)
  
  #Ugyldig verdi/ukjent kode
  sivst$OMK[!(sivst$OMK %in% c(0,1,2,3,4,5,9,"-"))]<-globs$sivst_illeg
  sivst$OK[!(sivst$OMK %in% c(0,1,2,3,4,5,9,"-"))]<-0
  
  return(sivst)
}

LANDBAKvask<-function (landbak,filbesk=data.frame(),batchdate=SettKHBatchDate(),globs=FinnGlobs(),regexp=FALSE){
  if (nrow(filbesk)==0){
    landbak<-setNames(as.data.frame(landbak,stringsAsFactors=FALSE),c("LANDBAK"))
    landbak$KBOMK<-landbak[,1]
  } else { 
    landbak$KBOMK<-KBomkod(landbak$ORG,type="LANDBAK",filbesk=filbesk,batchdate=batchdate,globs=globs)
  }  
  landbak$OK<-1
  landbak$OMK<-landbak$KBOMK
  if(regexp==TRUE){
    landbak$OMK<-sub("^ *(Europa.*) *$","1",landbak$OMK,ignore.case = TRUE)
    landbak$OMK<-sub("^ *(Afrika) *$","2",landbak$OMK,ignore.case = TRUE)
    landbak$OMK<-sub("^ *(Asia.*) *$","3",landbak$OMK,ignore.case = TRUE)
    landbak$OMK<-sub("^ *(Nord[ -]{1,3}Amerika) *$","4",landbak$OMK,ignore.case = TRUE)
    landbak$OMK<-sub("^ *(Sør.*Amerika) *$","5",landbak$OMK,ignore.case = TRUE)
    landbak$OMK<-sub("^ *(Oseania) *$","6",landbak$OMK,ignore.case = TRUE)
    landbak$OMK<-sub("^ *(Statsløse) *$","7",landbak$OMK,ignore.case = TRUE)
    landbak$OMK<-sub("^ *(Uoppgitt|Ukjent) *$","8",landbak$OMK,ignore.case = TRUE)
    landbak$OMK<-sub("^ *(Andre) *$","9",landbak$OMK,ignore.case = TRUE)
  }  
  landbak$OMK<-sub("^ *(Alle) *$","0",landbak$OMK,ignore.case = TRUE)
  
  #Ugyldig verdi/ukjent kode
  landbak$OMK[!(landbak$OMK %in% c(-1,0,1,2,3,4,5,6,7,8,9,"-"))]<-globs$landbak_illeg
  landbak$OK[!(landbak$OMK %in% c(-1,0,1,2,3,4,5,6,7,8,9,"-"))]<-0
  
  return(landbak)
}

AARvask<-function (aar,filbesk=data.frame(),batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  if (nrow(filbesk)==0){
    aar<-setNames(as.data.frame(aar,stringsAsFactors=FALSE),c("AAR"))
    aar$KBOMK<-aar[,1]
  } else { 
    aar$KBOMK<-KBomkod(aar$ORG,type="AAR",filbesk=filbesk,batchdate=batchdate,globs=globs)
  }  
  aar$OMK<-aar$KBOMK
  aar$OK<-1
  
  aar$OMK<-sub("^Høsten ","",aar$OMK)
  aar$OMK<-sub("^(\\d+) *[_-] *(\\d+)$","\\1_\\2",aar$OMK)
  aar$OMK<-sub("^ *(\\d+) *$","\\1_\\1",aar$OMK)
  
  #Ugyldig verdi/ukjent kode
  okformat<-grepl("^\\d+_\\d+$|^-$",aar$OMK)
  aar$OMK[!okformat]<-globs$aar_illeg
  aar$OK[!okformat]<-0
  
  #Sett intervall
  aar[,c("LO","HI")]<-suppressMessages(matrix(as.integer(str_split_fixed(aar$OMK,"_",2)),ncol=2))
  #Ugyldig intervall
  aar$OMK[aar$HI<aar$LO]<-globs$aar_illeg
  aar[,c("LO","HI")]<-suppressMessages(matrix(as.integer(str_split_fixed(aar$OMK,"_",2)),ncol=2))
  return(aar)
}
  


#Diverse prosedyrer for INNLESING-trinnet
###########################################

SjekkDuplikater<-function(FG,filgruppe,FullResult=FALSE,echo=0,batchdate=SettKHBatchDate(),versjonert=FALSE,globs=FinnGlobs()){
  HarDuplikater<-0
  if (identical(class(FG),"data.frame")){FG<-data.table(FG)}
  orgkeys<-key(FG)
  tabkols<-globs$DefDesign$DesignKolsFA
  tabkols<-tabkols[tabkols %in% names(FG)]
  valkols<-FinnValKols(names(FG))
  setkeym(FG,tabkols)
  
  dubi<-duplicated(FG)
  
  DUB<-data.table()
  result<-c(ANTdNO=0,fANTV1=0,ANTdNOp=0,fANTV1p=0,ANTdNOg=0,fANTV1g=0)
  #dubi<-duplicated(FG[,tabkols,with=FALSE])
  if (any(dubi)){
    HarDuplikater<-1
    DUB<-FG[dubi,]
    eval(parse(text=paste(
      "DUB[,dNO:=rank(",paste(valkols,collapse=","),"),by=tabkols]",sep=""
    )))
    DUB[,antV:=length(unique(dNO)),by=tabkols]
    DUB[,antK:=length(unique(KOBLID)),by=tabkols]
    
    
    #Positive verdier
    eval(parse(text=paste(
      "DUBp<-subset(DUB,",paste(valkols,"!=0",sep="",collapse=" | "),")",sep=""
    )))
    if (nrow(DUBp)>0){
      eval(parse(text=paste(
        "DUBp[,dNOp:=rank(",paste(valkols,collapse=","),"),by=tabkols]",sep=""
      )))
      DUBp[,antVp:=length(unique(dNOp)),by=tabkols]
      DUBp[,antKp:=length(unique(KOBLID)),by=tabkols]
      setkeyv(DUB,names(DUB))
      setkeyv(DUBp,names(DUB))
      DUB<-DUBp[DUB]
      DUB[is.na(dNOp),dNOp:=0]
      DUB[is.na(antVp),antVp:=0]
      DUB[is.na(antKp),antKp:=0]
    } else {
      DUB[,dNOp:=0]
      DUB[,antVp:=0]
      DUB[,antKp:=0]
    }
    
    
    #Hold ##99-geokoder utenom. Her blir det lagd dubeltter når to illegitime KNR blir samme ##99 etc
    DUBg<-subset(DUB,!grepl("99$",GEO))
    if (nrow(DUBg)>0){
      eval(parse(text=paste(
        "DUBg[,dNOg:=rank(",paste(valkols,collapse=","),"),by=tabkols]",sep=""
      )))
      DUBg[,antVg:=length(unique(dNOg)),by=tabkols]
      DUBg[,antKg:=length(unique(KOBLID)),by=tabkols]
      setkeyv(DUB,names(DUB))
      setkeyv(DUBg,names(DUB))
      DUB<-DUBg[DUB]
      DUB[is.na(dNOg),dNOg:=0]
      DUB[is.na(antVg),antVg:=0]
      DUB[is.na(antKg),antKg:=0]
    } else {
      DUB[,dNOg:=0]
      DUB[,antVg:=0]
      DUB[,antKg:=0]
    }  
    
    ANTdNO<-nrow(DUB)
    fANTV1<-nrow(subset(DUB,antV>1))
    ANTdNOp<-nrow(subset(DUB,dNOp>0))
    fANTV1p<-nrow(subset(DUB,antVp>1))
    ANTdNOg<-nrow(subset(DUB,dNOg>0))
    fANTV1g<-nrow(subset(DUB,antVg>1))
    result<-c(ANTdNO=ANTdNO,fANTV1=fANTV1,ANTdNOp=ANTdNOp,fANTV1p=fANTV1p,ANTdNOg=ANTdNOg,fANTV1g=fANTV1g)
    
    #Skriv dubletter til logg
    sqlQuery(globs$log,paste("DELETE * FROM DUBLETT WHERE FILGRUPPE='",filgruppe,"' AND SV='S'",sep=""))
    #Legg til resterende kolonner 
    #Må ha ok kolonnenavn til database 
    setnames(DUB,names(DUB),gsub("^(VAL\\d+)\\.f$","\\1f",names(DUB)))
    
    
    tmp<-sqlQuery(globs$log,"SELECT * FROM DUBLETT WHERE KOBLID=-1")
    tmp2<-tmp
    tmp[1:nrow(DUB),]<-NA
    tmp[,intersect(names(tmp),names(DUB))]<-DUB[,intersect(names(tmp),names(DUB)),with=FALSE]
    tmp$FILGRUPPE<-filgruppe
    tmp$BATCHID<-batchdate
    tmp$SV<-"S"
    if (echo>=1){
      cat("HAR",nrow(DUB),"duplikater (",nrow(subset(DUB,dNOg>0)),"), det kan gjerne ta",nrow(DUB)/100,"sek  å skrive ut logg\n")
    }
    if (echo>=2){print(DUB)}
    if (nrow(DUB)<1000){
      sqlSave(globs$log,tmp,"DUBLETT",rownames=FALSE,append=TRUE)
      if (echo>=1){cat("Ferdig dublogg\n")}
      if (versjonert==TRUE){
        tmp$SV<-"V"
        sqlSave(globs$log,tmp,"DUBLETT",rownames=FALSE,append=TRUE)
      }
    }
  }
  setkeym(FG,orgkeys)
  if (FullResult==TRUE){
    return(list(DUB=DUB,ANT=result)) 
  } else {
    return(result)
  }
}

SjekkDuplikaterFG<-function(filgruppe,FullResult=TRUE){
  return(SjekkDuplikater(FinnFilT(filgruppe,IDKOLS=TRUE),filgruppe,FullResult=FullResult))
}

FullDuplikatSjekk<-function(globs=FinnGlobs()){
  FILGRUPPER<-unlist(sqlQuery(globs$dbh,"SELECT DISTINCT FILGRUPPE FROM FILGRUPPER"))
  FGRapport<-c(FILGRUPPE=character(0),ANTdNO=integer(0),fANTV1=integer(0),ANTdNOp=integer(0),fANTV1p=integer(0),ANTdNOg=integer(0),fANTV1g=integer(0))
  for (fgruppe in FILGRUPPER){
    #if (grepl("_L$",fgruppe) || !grepl("_",fgruppe)){
    if (!(grepl("_",fgruppe) | fgruppe %in% c("NETTOFLYTT"))){
      print(fgruppe)
      print(SjekkDuplikaterFG(fgruppe,FullResult=FALSE))
      FGRapport<-rbind(FGRapport,c(FILGRUPPE=fgruppe,SjekkDuplikaterFG(fgruppe,FullResult=FALSE)))
    }
  }
  return(FGRapport)
}


#
LesFilNo <-function(id,y="",globs=FinnGlobs()){
  filbesk<-FinnFilBeskFilid(id,globs=globs)
  print(filbesk$FILNAVN)
  filbesk$filn<-paste(globs$path,filbesk$FILNAVN,sep="/")
  filbesk$filn<-gsub("\\\\","/",filbesk$filn)
  #filn<-paste(globs$path,filbesk$FILNAVN,sep="\\")
  #print(filn)
  #filn<-sub("\\[\\$y\\]",y,filn)
  #return(LesFil(filn=filn,format=filbesk$FORMAT,opt=filbesk$INNLESARG))
  return(LesFil(filbesk,globs=globs)$DF)
}

#
LagTabellFraFilNo <-function(id,batchdate=SettKHBatchDate(),y="",globs=FinnGlobs(),echo=FALSE){
  filbesk<-FinnFilBeskFilid(id,batchdate=batchdate,globs=globs)
  FGP<-FinnFilgruppeParametre(filbesk$FILGRUPPE,batchdate=batchdate,globs=globs)
  return(LagTabellFraFil(filbesk,FGP=FGP,globs=globs,echo=echo))
}

#
FinnFilgruppeParametre<-function(gruppe,batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  dbh<-globs$dbh
  datef<-format(strptime(batchdate, "%Y-%m-%d-%H-%M"),"#%Y-%m-%d#")
  FGPaktiv<-as.integer(sqlQuery(globs$dbh,paste("SELECT count(*) FROM FILGRUPPER WHERE FILGRUPPE='",gruppe,"'",
                                        "AND VERSJONFRA<=",datef," AND VERSJONTIL>", datef,"
                                        ",sep=""),as.is=TRUE))
  FGPfinnes<-as.integer(sqlQuery(globs$dbh,paste("SELECT count(*) FROM FILGRUPPER WHERE FILGRUPPE='",gruppe,"'",sep=""),as.is=TRUE))
  ok<-0
  resultat<-list()
  if (FGPfinnes==0){
    KHerr(paste("Filgruppe",gruppe,"finnes ikke. Droppes.")) 
  } else if (FGPaktiv==0){
    KHerr(paste("Filgruppe",gruppe,"finnes, men er satt inaktiv. Droppes.")) 
  } else {
    ok<-1
    FGP<-as.list(sqlQuery(globs$dbh,paste("SELECT * FROM FILGRUPPER WHERE FILGRUPPE='",gruppe,"'",
                                          "AND VERSJONFRA<=",datef," AND VERSJONTIL>", datef,"
                                          ",sep=""),as.is=TRUE))
    #Sette endelig default alder ALLE
    #Default er 0_ALDinf    der ALDinf er global parameter i HOVEDPARAMETRE
    amin<-0
    amax<-as.numeric(sqlQuery(dbh,"SELECT ALDinf FROM HOVEDPARAMETREg")[1])
    #Evt egen def for filgruppe fra ALDER_ALLE i tabell FILGRUPPER
    if(!is.na(FGP$ALDER_ALLE)){
      if (grepl("^\\d+_(\\d+|)$",FGP$ALDER_ALLE)){
        alle_aldre<-unlist(strsplit(FGP$ALDER_ALLE,"_"))                       
        if (length(alle_aldre)==1){
          amin<-as.numeric(alle_aldre[1])
        } else if (length(alle_aldre)==2){
          amin<-as.numeric(alle_aldre[1])
          amax<-as.numeric(alle_aldre[2])
        }
      } else {
        cat("FEIL!!!!!! Feil format FGP$ALDER_ALLE",FGP$ALDER_ALLE,"\n")
      }
    }
    
    vals<-list()
    for(valf in names(FGP)[grepl("^VAL\\d+navn$",names(FGP))]){
      val<-gsub("(VAL\\d+)navn","\\1",valf)
      valn<-ifelse(is.na(FGP[[valf]]) || FGP[[valf]]=="",val,FGP[[valf]])
      valmissf<-paste(val,"miss",sep="")
      valmiss<-ifelse(is.na(FGP[[valmissf]]) || FGP[[valmissf]]=="","0",FGP[[valmissf]])
      valsumf<-paste(val,"sumbar",sep="")
      valsum<-ifelse(is.na(FGP[[valsumf]]) || FGP[[valsumf]]=="","0",FGP[[valsumf]])
      vals[[valn]]<-list(miss=valmiss,sumbar=valsum)
    }
    resultat<-c(FGP,list(vals=vals,amin=amin,amax=amax))
  }
  return(c(resultat,list(ok=ok)))
}

#
FinnFilBeskGruppe<-function(filgruppe,batchdate=NULL,globs=FinnGlobs(), test = runtest, testID = testfiles){
  #Default er å finne filbesk gyldige nå (Sys.time)
  datef<-format(Sys.time(), "#%Y-%m-%d#")
  #ALternativt kan man finne for en historisk batchdate
  if (!is.null(batchdate)){
    datef<-format(strptime(batchdate, "%Y-%m-%d-%H-%M"),"#%Y-%m-%d#")
  }
  sqlt<-paste("SELECT KOBLID, ORIGINALFILER.FILID AS FILID, FILNAVN, FORMAT, DEFAAR, INNLESING.*
              FROM INNLESING INNER JOIN 
              (  ORGINNLESkobl INNER JOIN ORIGINALFILER 
              ON ORGINNLESkobl.FILID = ORIGINALFILER.FILID)
              ON   (INNLESING.DELID = ORGINNLESkobl.DELID) 
              AND (INNLESING.FILGRUPPE = ORGINNLESkobl.FILGRUPPE)
              WHERE INNLESING.FILGRUPPE='",filgruppe,"'  
              AND ORIGINALFILER.IBRUKFRA<=",datef,"
              AND ORIGINALFILER.IBRUKTIL>", datef,"
              AND INNLESING.VERSJONFRA<=",datef," 
              AND INNLESING.VERSJONTIL>",datef,sep="")
  fb<-sqlQuery(globs$dbh,sqlt,stringsAsFactors=FALSE)
  
  ## Picking up files path that is refered to in ORIGINALFILER
  ## --------------------------------------------------------
  if (test)
    fb <- subset(fb, KOBLID  %in% testID)
  
  invisible(fb)
}

#

FinnFilBeskFilid<-function(filid,batchdate=NULL,globs=FinnGlobs()){
  #Default er å finne filbesk gyldige nå (Sys.time)
  datef<-format(Sys.time(), "#%Y-%m-%d#")
  #ALternativt kan man finne for en historisk batchdate
  if (!is.null(batchdate)){
    datef<-format(strptime(batchdate, "%Y-%m-%d-%H-%M"),"#%Y-%m-%d#")
  }
  sqlt<-paste("SELECT KOBLID, ORIGINALFILER.FILID AS FILID, FILNAVN, FORMAT, DEFAAR, INNLESING.*
              FROM INNLESING INNER JOIN 
              (  ORGINNLESkobl INNER JOIN ORIGINALFILER 
              ON ORGINNLESkobl.FILID = ORIGINALFILER.FILID)
              ON   (INNLESING.DELID = ORGINNLESkobl.DELID) 
              AND (INNLESING.FILGRUPPE = ORGINNLESkobl.FILGRUPPE)
              WHERE ORIGINALFILER.FILID=",filid,"	
              AND ORIGINALFILER.IBRUKFRA<=",datef,"
              AND ORIGINALFILER.IBRUKTIL>", datef,"
              AND INNLESING.VERSJONFRA<=",datef," 
              AND INNLESING.VERSJONTIL>",datef,sep="")
  fb<-sqlQuery(globs$dbh,sqlt,as.is=TRUE,stringsAsFactors=FALSE)
  fb$filn<-paste(globs$path,fb$FILNAVN,sep="/")
  fb$filn<-gsub("\\\\","/",fb$filn)
  fb$AAR<-gsub("<\\$y>",paste("<",fb$DEFAAR,">",sep=""),fb$AAR)  
  return(fb)
}

FinnFilGruppeFraKoblid<-function(koblid,globs=FinnGlobs()){
  return(as.character(sqlQuery(globs$dbh,paste("SELECT FILGRUPPE FROM ORGINNLESkobl WHERE KOBLID=",koblid,sep=""),stringsAsFactors=FALSE)))  
}

#
TilFilLogg<-function (koblid,felt,verdi,batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  #Sjekk om finnes rad for filid, eller lag ny
  if (nrow(sqlQuery(globs$log,paste("SELECT * FROM INNLES_LOGG WHERE KOBLID=",koblid," AND SV='S' AND BATCH='",batchdate,"'",sep="")))==0){
    print("**************Hvorfor er jeg egentlig her?*********************'")
    sqlQuery(globs$log,paste("DELETE * FROM INNLES_LOGG WHERE KOBLID=",koblid,"AND SV='S'",sep=""))
    upd<-paste("INSERT INTO INNLES_LOGG ( KOBLID, BATCH, SV, FILGRUPPE ) SELECT=",koblid,",'",batchdate,"', 'S',",FinnFilGruppeFraKoblid(koblid),sep="")
    sqlQuery(globs$log,upd)
  }
  if (is.character(verdi)){
    verdi<-paste("'",verdi,"'",sep="")
    verdi<-gsub("\\n","' & Chr(13) & Chr(10) & '",verdi)   #Veldig sær \n i Access!
  }
  upd<-paste("UPDATE INNLES_LOGG SET ",felt,"=",verdi," WHERE KOBLID=",koblid," AND SV='S' AND BATCH='",batchdate,"'",sep="")
  tmp<-sqlQuery(globs$log,upd)  
  #cat("********\n",tmp,"__________\n")
}

#
SkrivKBLogg<-function(KB,type,filbesk,gruppe,batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  sqlQuery(globs$log,paste("DELETE * FROM KODEBOK_LOGG WHERE KOBLID=",filbesk$KOBLID," AND TYPE='",type,"' AND SV='S'",sep=""))
  sqlSave(globs$log,cbind(KOBLID=filbesk$KOBLID,FILGRUPPE=gruppe,FELTTYPE=type, SV="S", KB[,c("ORG","KBOMK","OMK","FREQ","OK")],BATCHDATE=batchdate),"KODEBOK_LOGG",rownames=FALSE,append=TRUE)
}

SVcloneRecord<-function(dbh,table,koblid){
  design<-names(sqlQuery(dbh,paste("SELECT * FROM ",table," WHERE KOBLID=-1",sep="")))
  felt<-paste(design,collapse=",")
  feltm<-sub("SV","'V' AS SV",felt)
  sql<-paste("INSERT INTO ", table,"(",felt,")",
             "SELECT ", feltm, "FROM ", table,
             "WHERE KOBLID=",koblid, "AND SV='S'"             
  )
  sqlQuery(dbh,sql)
}

##########################################################
#TRINN 2: 
#         
##########################################################

#BUFFER<-SetBuffer(filer=c("NPR","DAAR","BEFOLK","RESEPT"))

SetBuffer<-function(filer=c("BEFOLK"),globs=FinnGlobs()){
  BUFFER<-list()
  for (filtag in filer){
    fil<-FinnFilN(filtag,globs=globs)
    if (fil$ok==1){
      BUFFER[[filtag]]<-readRDS_KH(fil$filn)
      cat("Lest inn ",filtag,"=",fil$filn,"i buffer\n")
    } else {
      print(fil$err)
    }
  }
  return(BUFFER)
}


readRDS_KH<-function(file,IDKOLS=FALSE,...){
  FIL<-readRDS(file,...)
  if(IDKOLS==FALSE){
    if ("KOBLID" %in% names(FIL)){FIL$KOBLID<-NULL}
    if ("ROW" %in% names(FIL)){FIL$ROW<-NULL}
  }
  return(FIL)
}

LagFlereKuber<-function(KUBEidA,versjonert=FALSE,csvcopy=FALSE,globs=FinnGlobs(),dumps=list()){
  batchdate=SettKHBatchDate()
  loggfile<-paste(globs$path,"/",globs$KubeDir,"LOGG/",batchdate,".txt",sep="")
  sink(loggfile,split=TRUE)
  cat("BATCH:",batchdate,"\n")
  for (KUBEid in KUBEidA){
    KK<-LagKUBE(KUBEid,batchdate=batchdate,versjonert=versjonert,csvcopy=csvcopy,globs=globs,dumps=dumps)
  }
  sink()
}

LagKubeDatertCsv<-function(KUBEID,dumps=list()){
  invisible(LagFlereKuber(KUBEID,versjonert=TRUE,csvcopy=TRUE,dumps=dumps))
}

KlargjorFil<-function(FilVers,TabFSub="",rolle="",KUBEid="",versjonert=FALSE,FILbatch=NA,batchdate=SettKHBatchDate(),GeoHarmDefault=1,globs=FinnGlobs()){
  TilBuffer<-0
  if (!exists("BUFFER")){
    .GlobalEnv$BUFFER<-list()
  }
  datef<-format(strptime(batchdate, "%Y-%m-%d-%H-%M"),"#%Y-%m-%d#")
  
  FilterDscr<-as.list(sqlQuery(globs$dbh,paste("SELECT * FROM FILFILTRE WHERE FILVERSJON='",FilVers,"' AND VERSJONFRA<=",datef," AND VERSJONTIL>",datef,sep=""),as.is=TRUE))
  
  #Har oppsatt filter
  if (length(FilterDscr$FILVERSJON)>0){
    FGP<-FinnFilgruppeParametre(FilterDscr$ORGFIL,batchdate=batchdate,globs=globs)
    if (is.null(BUFFER[[FilVers]])){
      if (!is.na(FilterDscr$SUBSET)){
        if (FilterDscr$SUBSET!=""){
          if (TabFSub!=""){
            TabFSub<-paste(TabFSub,FilterDscr$SUBSET,sep=" & ")
          } else {
            TabFSub<-FilterDscr$SUBSET
          }
        }
      }
      
      FILn<-FinnFil(FilterDscr$ORGFIL,batch=FILbatch,versjonert=versjonert)    
      FIL<-FILn$FT
      sqlQuery(globs$log,paste("INSERT INTO KUBEBATCH (KUBEBATCH,FILBATCH) SELECT '",KUBEid,"_",batchdate,"','",FilterDscr$ORGFIL,"_",FILn$batch,"'",sep=""))
      if (TabFSub!=""){
        #print("ASKJDLKJASLDKJL  TabFSub")
        cat("Filtrer med tab-filter, før er dim(FIL)",dim(FIL))
        FIL<-eval(parse(text=paste("subset(FIL,",TabFSub,")",sep="")))
        cat(" og etter", dim(FIL),"\n")
      }
      
      
      orgkols<-copy(names(FIL))
      if (grepl("\\S",FilterDscr$KOLLAPSdeler)){
        cat("Før aggregering er dim(FIL)",dim(FIL))
        tabkols<-FinnTabKols(names(FIL))
        setkeyv(FIL,tabkols)
        kolldel<-unlist(str_split(FilterDscr$KOLLAPSdeler,","))
        kolldelN<-unlist(globs$DefDesign$DelKolsF[kolldel])
        #FIL[,(kolldelN):=NULL]
        FIL[,(kolldelN):=KHglobs$TotalKoder[kolldel]]
        FIL<-FIL[,lapply(.SD,sum),by=tabkols]
        FIL[,(kolldelN):=KHglobs$TotalKoder[kolldel]]
        FIL<-FIL[,orgkols,with=FALSE]
        cat(" og etter", dim(FIL),"\n")
      }
      
      if (!(is.na(FilterDscr$NYEKOL_KOL_preRAD) | FilterDscr$NYEKOL_KOL_preRAD=="")){
        FIL<-LeggTilNyeVerdiKolonner(FIL,FilterDscr$NYEKOL_KOL_preRAD,slettInf=TRUE)
      }
	    Filter<-SettFilterDesign(FilterDscr,bruk0=FALSE,FGP=FGP,globs=globs)
      if (length(Filter)>0){
        FIL<-OmkodFil(FIL,FinnRedesign(FinnDesign(FIL),list(Parts=Filter)),globs=globs,echo=1)
      } 
	    
      if (FilterDscr$GEOHARM==1){
        rektiser<-ifelse (FilterDscr$REKTISER==1,1,0)
        FIL<-GeoHarm(FIL,vals=FGP$vals,rektiser=rektiser,batchdate=batchdate,globs=globs)
      }
      if (!(is.na(FilterDscr$NYETAB) | FilterDscr$NYETAB=="")){
        FIL<-AggregerRader(FIL,FilterDscr$NYETAB,FGP=FGP)
      }
	    
      if (grepl("\\S",FilterDscr$NYEKOL_RAD)){
        FIL<-LeggTilSumFraRader(FIL,FilterDscr$NYEKOL_RAD,FGP=FGP,globs=globs)
      }
      if (!(is.na(FilterDscr$NYEKOL_KOL) | FilterDscr$NYEKOL_KOL=="")){
        FIL<-LeggTilNyeVerdiKolonner(FIL,FilterDscr$NYEKOL_KOL,slettInf=TRUE)
      }
	    
	    if (!(is.na(FilterDscr$NYKOLSmerge) | FilterDscr$NYKOLSmerge=="")){
	      NY<-eval(parse(text=FilterDscr$NYKOLSmerge))
        tabK<-intersect(FinnTabKols(names(NY)),FinnTabKols(names(FIL)))
        setkeyv(NY,tabK)
        setkeyv(FIL,tabK)
        FIL<-NY[FIL]
	    }
      
	    #FF_RSYNT1
	    if (!(is.na(FilterDscr$FF_RSYNT1) | FilterDscr$FF_RSYNT1=="")){
	      FilterDscr$FF_RSYNT1<-gsub("\\\r","\\\n",FilterDscr$FF_RSYNT1)
	      rsynt1err<-try(eval(parse(text=FilterDscr$FF_RSYNT1)),silent=TRUE)
	      print("***AD HOC MANIPULERING\n")
	      if(class(rsynt1err)=="try-error"){
	        print(rsynt1err)
	      } 
	    }
      
      .GlobalEnv$BUFFER[[FilVers]]<-FIL
      TilBuffer<-1  
    }
    #Bruk ferdig lagret versjon
    else {
      FIL<-copy(BUFFER[[FilVers]])
      print(FilVers)
      #print(BUFFERbatch)
      if (versjonert==TRUE){
        #sqlQuery(globs$log,paste("INSERT INTO KUBEBATCH (KUBEBATCH,FILBATCH) SELECT '",KUBEid,"_",batchdate,"','",FilterDscr$ORGFIL,"_",BUFFERbatch[[FilVers]],"'",sep=""))
      }
    }
  }
  #Har ikke oppsatt filter, bruk rå
  else {
    FILn<-FinnFil(FilVers,versjonert=versjonert,batch=FILbatch)
    FIL<-FILn$FT
    #sqlQuery(globs$log,paste("INSERT INTO KUBEBATCH (KUBEBATCH,FILBATCH) SELECT '",KUBEid,"_",batchdate,"','",FilVers,"_",FILn$batch,"'",sep=""))
	
    if (TabFSub!=""){
      cat("Filtrer med tab-filter, før er dim(FIL)",dim(FIL))
      FIL<-eval(parse(text=paste("subset(FIL,",TabFSub,")",sep="")))
      cat(" og etter", dim(FIL),"\n")
    }
    FGP<-FinnFilgruppeParametre(FilVers,batchdate=batchdate,globs=globs)
    if (GeoHarmDefault==1){
      FIL<-GeoHarm(FIL,vals=FGP$vals,rektiser=FALSE,batchdate=batchdate,globs=globs)
    }
    .GlobalEnv$BUFFER[[FilVers]]<-FIL
    #.GlobalEnv$BUFFERbatch[[FilVers]]<-FILn$batch
    TilBuffer<-1
  }
 
  
  FILd<-FinnDesign(FIL,FGP=FGP,globs=globs)
  return(list(FIL=FIL,FGP=FGP,FILd=FILd,TilBuffer=TilBuffer))
}

SettFilterDesign<-function(KUBEdscr,OrgParts=list(),bruk0=TRUE,FGP=list(amin=0,amax=120),globs=FinnGlobs()){
  Deler<-list()
  for (del in names(globs$DefDesign$DelKolN)){
    #for (del in names(unlist(globs$DefDesign$DelKolN[ORGd$OmkDeler]))){
    #if (del %in% names(ORGd$Part) | grepl("^T\\d$",del)){  
    #Les liste
    koldel<-globs$DefDesign$DelKolN[del]
    koldel0<-paste(koldel,"_0",sep="")
    
    if (bruk0==TRUE && !is.null(KUBEdscr[[koldel0]]) && !is.na(KUBEdscr[[koldel0]]) && KUBEdscr[[koldel0]]!=""){
      delListStr<-KUBEdscr[[koldel0]]
    } else {
      delListStr<-KUBEdscr[[koldel]]
    }
    if (!(is.null(delListStr) || is.na(delListStr) || delListStr=="")){
      delListStr<-gsub("^ *| *$","",delListStr)
      minus<-grepl("^-\\[",delListStr)
      delListStr<-gsub("^-\\[(.*)\\]$","\\1",delListStr)
      delListA<-unlist(str_split(delListStr,","))
      if (globs$DefDesign$DelType[del]=="INT"){
        if (del=="A"){
          delListA<-gsub("ALLE",paste(FGP$amin,"_",FGP$amax,sep=""),delListA)
          delListA<-gsub("^_(\\d+)",paste(FGP$amin,"_\\1",sep=""),delListA)
          delListA<-gsub("(\\d+)_$",paste("\\1_",FGP$amax,sep=""),delListA)
        }
        delListA<-gsub("^(\\d+)$","\\1_\\1",delListA)
        delListA<-as.data.table(matrix(as.integer(str_split_fixed(delListA,"_",2)),ncol=2))
      } 
      else if (globs$DefDesign$DelFormat[del]=="integer"){
        delListA<-as.integer(delListA)
      } else if (globs$DefDesign$DelFormat[del]=="numeric"){
        delListA<-as.numeric(delListA)
      } 
      listDT<-setnames(as.data.table(delListA),globs$DefDesign$DelKols[[del]])
      if (minus==TRUE){
        if (!is.null(OrgParts[[del]])){
          setkeyv(listDT,names(listDT))
          setkeyv(OrgParts[[del]],names(listDT))
          Deler[[del]]<-OrgParts[[del]][!listDT,]
        } else {
          print("**********************KAN IKKE BRUKE -[liste] i SettFilterDesign når ikke OrgParts")          
        }
      } else {
        Deler[[del]]<-listDT
      }
    } 
    else if (globs$DefDesign$DelType[del]=="INT"){
      start<-KUBEdscr[[paste(koldel,"_START",sep="")]]
      stopp<-KUBEdscr[[paste(koldel,"_STOP",sep="")]]
      if (!(is.null(start) | is.null(stopp))){
        if (!(is.na(start) | is.na(stopp))){
          if (!(start=="" | stopp=="")){
            if (stopp>=start){
              if (!is.null(OrgParts[[del]])){
                Deler[[del]]<-subset(OrgParts[[del]],eval(parse(text=paste(koldel,"l>=",start," & ",koldel,"h<=",stopp,sep=""))))
              } else  {
                #Deler[[del]]<-setNames(as.data.frame(cbind(start:stopp,start:stopp)),paste(koldel,c("l","h"),sep=""))
              }
            }
            else {
              cat("FEIL!!!!!!!! kan ikke ha start ",start,"> stopp ", stopp,"\n")
            }
          }
        }        
      }    
    } 
  }
  return(Deler)
}



SettFilInfoKUBE<-function(KUBEid,batchdate=SettKHBatchDate(),versjonert=FALSE,globs=FinnGlobs()){
  datef<-format(strptime(batchdate, "%Y-%m-%d-%H-%M"),"#%Y-%m-%d#")
  KUBEdscr<-as.list(sqlQuery(globs$dbh,paste("SELECT * FROM KUBER WHERE KUBE_NAVN='",KUBEid,"' AND VERSJONFRA<=",datef," AND VERSJONTIL>",datef,sep=""),as.is=TRUE))
  if ((is.na(KUBEdscr$TNP) | KUBEdscr$TNP=="")){
    ok<-0
    err<-"Feltet TNP ikke satt!"
  } else {
    TNPdscr<-sqlQuery(globs$dbh,paste("SELECT * FROM TNP_PROD WHERE TNP_NAVN='",KUBEdscr$TNP,"' AND VERSJONFRA<=",datef," AND VERSJONTIL>",datef,sep=""),as.is=TRUE)  
  }
  
  filer<-character(0)
  if ((is.na(TNPdscr$TELLERFIL) | TNPdscr$TELLERFIL=="")){
    ok<-0
    err<-"Feltet TELLERFIL ikke satt!"
  } else {
    filer["T"]<-TNPdscr$TELLERFIL
  }
  if (!(is.na(TNPdscr$NEVNERFIL) | TNPdscr$NEVNERFIL=="")){
    filer["N"]<-TNPdscr$NEVNERFIL
  }
  
  
  #Evt ekstrafiler med info for standardisering
  if (KUBEdscr$REFVERDI_VP=="P"){
    if (!(is.na(TNPdscr$PREDNEVNERFIL) | TNPdscr$PREDNEVNERFIL=="")){
      filer["PN"]<-gsub("^(.*):(.*)","\\1",TNPdscr$PREDNEVNERFIL)
    } else if (!is.na(TNPdscr$NEVNERFIL)){
      filer["PN"]<-TNPdscr$NEVNERFIL
    } else {
      filer["PN"]<-TNPdscr$TELLERFIL
    }
    if (!(is.na(TNPdscr$STANDARDTNFIL) | TNPdscr$STANDARDTNFIL=="")){
      STNPdscr<-sqlQuery(globs$dbh,paste("SELECT * FROM TNP_PROD WHERE TNP_NAVN='",TNPdscr$STANDARDTNFIL,"' AND VERSJONFRA<=",datef," AND VERSJONTIL>",datef,sep=""),as.is=TRUE)  
      if ((is.na(STNPdscr$TELLERFIL) | STNPdscr$TELLERFIL=="")){
        ok<-0
        err<-"Feltet TELLERFIL ikke satt!"        
      } else {
        filer["ST"]<-STNPdscr$TELLERFIL
      }
      if (!(is.na(STNPdscr$NEVNERFIL) | STNPdscr$NEVNERFIL=="")){
        filer["SN"]<-STNPdscr$NEVNERFIL
      } else {
        filer["SN"]<-STNPdscr$TELLERFIL
      }
    } else {
      STNPdscr<-TNPdscr
      filer["ST"]<-filer["T"]
      if (!is.na(filer["N"])){
        filer["SN"]<-filer["N"]        
      }      
    }    
  } else {
    STNPdscr<-list()
  }
  #Denne er ikke så veldig robust for feilspesifkasjon, men den brukes ikke annet til de enkleste tilfellene
  PredFilter<-SettPredFilter(KUBEdscr$REFVERDI,globs=globs)
  
  #Sett Tab-filter
  #For å redusere ressursbruk er det viktig at lange lister med unødvendige ETAB blir barert bort tidlig
  TabConds<-character(0)
  TabFSubTT<-""
  for (tab in names(KUBEdscr)[grepl("^TAB\\d+$",names(KUBEdscr))]){
    if (!(is.na(KUBEdscr[[tab]]) || KUBEdscr[[tab]]=="")){
      tablist<-KUBEdscr[[tab]]
      tab0<-paste(tab,"_0",sep="")
      if (!(is.null(KUBEdscr[[tab0]]) || is.na(KUBEdscr[[tab0]]) || KUBEdscr[[tab0]]=="")){
        tablist<-KUBEdscr[[tab0]]
      }
      minus<-grepl("^-\\[",tablist)
      tablist<-gsub("^-\\[(.*)\\]$","\\1",tablist)
      tablist<-paste("\"",gsub(",","\",\"",tablist),"\"",sep="")
      tabcond<-paste("(",tab," %in% c(",tablist,"))",sep="")
      if (minus){tabcond<-paste("!",tabcond,sep="")}
      TabConds<-c(TabConds,tabcond)
    }
    TabFSubTT<-paste(TabConds,collapse=" & ")
  }
  
  FGPs<-list()
  FilDesL<-list()
  tmpBUFFER<-character(0)
  for (fil in unique(filer)){
    TabFSub<-ifelse(fil==filer["T"],TabFSubTT,"")
    FILinfo<-KlargjorFil(fil,TabFSub=TabFSub,KUBEid=KUBEid,versjonert=versjonert,FILbatch=NA,batchdate=batchdate,globs=globs)
    FGPs[[fil]]<-FILinfo$FGP
    FilDesL[[fil]]<-FILinfo$FILd
    
    if (FILinfo$TilBuffer==1){
      tmpBUFFER<-c(tmpBUFFER,fil)
    }
    #FilDesL[[fil]]<-FinnDesign(FinnFilT(fil,batch=batchdate,globs=globs),FGP=FGPs[[fil]],globs=globs)  
  }
  return(list(KUBEdscr=KUBEdscr,TNPdscr=TNPdscr,filer=filer,PredFilter=PredFilter,STNPdscr=STNPdscr,FGPs=FGPs,FilDesL=FilDesL,tmpBUFFER=tmpBUFFER))
}



LagKUBE<-function(KUBEid,
                  lagRapport=0,
                  batchdate=SettKHBatchDate(),
                  versjonert=FALSE,
                  bare_TN=0,
                  drop_TN=0,
                  tmpbryt=0,
                  FullUt=0,
                  csvcopy=FALSE,
                  globs=FinnGlobs(),
                  echo=0,dumps=list()){
  
  datef<-format(strptime(batchdate, "%Y-%m-%d-%H-%M"),"#%Y-%m-%d#")
  rapport<-list(KUBE=KUBEid,lagRapport=lagRapport)
  
  #Les inn nødvendig informasjon om filene involvert (skilt ut i egen funksjon for lesbarhet)
  ##################################################
  Finfo<-SettFilInfoKUBE(KUBEid,batchdate=batchdate,versjonert=versjonert,globs=globs)
  KUBEdscr<-Finfo$KUBEdscr
  TNPdscr<-Finfo$TNPdscr
  filer<-Finfo$filer
  PredFilter<-Finfo$PredFilter
  D_develop_predtype <- Finfo$PredFilter$D_develop_predtype
  STNPdscr<-Finfo$STNPdscr
  FGPs<-Finfo$FGPs
  FilDesL<-Finfo$FilDesL
  KUBEd<-list()
  
  if (KUBEdscr$MODUS=="KH"){
    globs$KubeDir<-globs$KubeDir_KH
    globs$KubeDirNy<-globs$KubeDirNy_KH
    globs$KubeDirDat<-globs$KubeDirDat_KH
    globs$FriskVDir<-globs$FriskVDir_KH
  } else {
    globs$KubeDir<-globs$KubeDir_NH
    globs$KubeDirNy<-globs$KubeDirNy_NH
    globs$KubeDirDat<-globs$KubeDirDat_NH
    globs$FriskVDir<-globs$FriskVDir_NH
  }
  
  
  # TRINN 1 LAG TNF
  ####################################################
  if (drop_TN==0){
    
    cat("******LAGER TNF\n")
    TNtab<-LagTNtabell(filer,FilDesL,FGPs,TNPdscr,KUBEdscr=KUBEdscr,rapport=rapport,globs=globs)
    TNF<-TNtab$TNF
    
    KUBEd<-TNtab$KUBEd
    if(TNPdscr$NEVNERKOL!="-"){
      TNF<-LeggTilNyeVerdiKolonner(TNF,"RATE={TELLER/NEVNER}")
    }
    if(echo==1){
      cat("TNF:\n")
      print(TNF)
    }
    cat("------FERDIG TNF\n")
  }
  
  
  if (bare_TN==1){
    RESULTAT=list(KUBE=TNF,TNPdscr=TNPdscr)
  }
  # Prediker referanseverdi om dette er etterspurt
  if (KUBEdscr$REFVERDI_VP=="P"  && bare_TN==0){
    print("*****PREDIKER!!!")
    #Må først finne design for (den syntetiske) koblinga ST, SN og PN
    ################################################
    
    #Bruk PREDfilter på ST og evt SN
    #Finn så FellesTab for disse
    #STdFilt<-FinnRedesignForFilter(FilDesL[[filer["ST"]]],PredFilter$Design,globs=globs)$Dekk
    #STdFilt<-STdFilt[,setdiff(names(STdFilt),PredFilter$Pkols),with=FALSE]
    #STFd<-FinnDesign(STdFilt,FGP=FGPs[[filer["ST"]]],globs=globs)
    cat("***Skal finne felles design ST,SN,PN\n")
    STFd<-FinnDesignEtterFiltrering(FilDesL[[filer["ST"]]],PredFilter$Design,PredFilter$Pkols,FGP=FGPs[[filer["ST"]]],globs=globs)
    if (!is.na(filer["SN"])){
      SNFd<-FinnDesignEtterFiltrering(FilDesL[[filer["SN"]]],PredFilter$Design,PredFilter$Pkols,FGP=FGPs[[filer["SN"]]],globs=globs)
      STNFd<-FinnFellesTab(STFd,SNFd,globs=globs)$FDes
    } else{
      STNFd<-STFd
    }
    #Finn FellesTab også med PN, denne gjelder som Til-design for PN
    STNPFd<-FinnFellesTab(STNFd,FilDesL[[filer["PN"]]],globs=globs)$FDes
    #Må filtrere STNPFd med Predfilter igjen for å finne ny STNFd som gir til-design for ST og SN 
    #(merge med PN kan ha endra fra STNFd-versjonen over
    STNFd<-FinnDesignEtterFiltrering(STNPFd,PredFilter$Design,FGP=FGPs[[filer["ST"]]],globs=globs)
    cat("---Satt felles design ST,SN,PN\n")
    
    STN<-copy(LagTNtabell(filer,FilDesL,FGPs,STNPdscr,TT="ST",NN="SN",Design=STNFd,rapport=rapport,globs=globs)$TNF)
    
    #Fjern PredFilter$Pkols
    STN[,(PredFilter$Pkols):=NULL]
    
    #SETT RATE HER er mest effektivt!
    STN[!(NEVNER==0 & NEVNER.f==0),c("PREDRATE","PREDRATE.f","PREDRATE.a"):=list(TELLER/NEVNER,pmax(TELLER.f,NEVNER.f),pmax(TELLER.a,NEVNER.a))]
    STN[NEVNER==0 & NEVNER.f==0,c("PREDRATE","PREDRATE.f","PREDRATE.a"):=list(0,pmax(TELLER.f,2),pmax(TELLER.a,NEVNER.a))]
    #MÅ JUKSE DET TIL LITT MED NEVNER 0. Bruken her er jo slik at dette er tomme celler, og ikke minst vil raten nesten garantert skulle ganges med et PREDTELLER=0
    #Tillater TELLER<=2 for å unngå evt numeriske problemer. Virker helt uskyldig gitt bruken
    STN[TELLER<=2 & TELLER.f==0 & NEVNER==0 & NEVNER.f==0,c("PREDRATE","PREDRATE.f","PREDRATE.a"):=list(0,0,pmax(TELLER.a,NEVNER.a))]
    
    ukurante<-nrow(subset(STN,is.na(TELLER) | is.na(NEVNER)))
    if (ukurante>0){
      cat("!!! NAs i ST og/eller SN (",ukurante,"), dette vil gi problemer i PREDTELLER\n")
    }
    soppelkols<-setdiff(names(STN),c(FinnTabKols(names(STN)),paste("PREDRATE",c("",".f",".a"),sep="")))
    if (length(soppelkols)>0){
      STN[,(soppelkols):=NULL]
    }
    
    cat("------FERDIG med STN\n")
    cat("***Lager PN\n")
    PNrd<-FinnRedesign(FilDesL[[filer["PN"]]],STNPFd,globs=globs)
    PN<-OmkodFil(FinnFilT(filer["PN"]),PNrd,globs=globs)
    #PN<-GeoHarm(PN,vals=FGPs[[filer["PN"]]]$vals,globs=globs)  #Trenger vel ikke det, den er GeoHarm i Klargjør, trenger ikke rektangularisere her
    
    if (!(is.na(TNPdscr$PREDNEVNERFIL) | TNPdscr$PREDNEVNERFIL=="")){
      PredNevnerKol<-gsub("^(.*):(.*)","\\2",TNPdscr$PREDNEVNERFIL)
    } else {
      PredNevnerKol<-TNPdscr$NEVNERKOL
    }
    PNnames<-gsub(paste("^",PredNevnerKol,"(\\.f|\\.a|)$",sep=""),"PREDNEVNER\\1",names(PN))
    setnames(PN,names(PN),PNnames)
    soppelkols<-setdiff(names(PN),c(FinnTabKols(names(PN)),paste("PREDNEVNER",c("",".f",".a"),sep="")))
    if (length(soppelkols)>0){
      PN[,(soppelkols):=NULL]
    }
    cat("---Ferdig PN\n")
    #return(list(STN=STN,PN=PN))
    cat("******Lager STNP, dette kan bli en stor tabell før kollaps til PT\n")
    commonkols<-intersect(FinnTabKols(names(PN)),FinnTabKols(names(STN)))
    setkeyv(STN,commonkols)
    setkeyv(PN,commonkols)
    mismatch<-nrow(STN[!PN,allow.cartesian=TRUE])
    if (mismatch>0){
      cat("!!!!!ADVARSEL: Mismatch i STN[PN,] på ",mismatch,"kolonner\n")
    }
    
    #Finn omkoding til KUBEd, dvs design for TNF
    #NB: Her må det aggregeres opp for standardisering
    PNd<-FinnDesign(PN,FGP=FGPs[[filer["PN"]]],globs=globs)  
    #Burde kanskje bruke STNFd i stedet, men da må den få på PredFilterDimensjonene. Må uansett sende til FinDesigmm
    RD<-FinnRedesign(PNd,list(Part=KUBEd$MAIN),SkalAggregeresOpp=globs$DefDesign$AggVedStand,globs=globs)
    cat("Før merge: dim(PN)",dim(PN)," og dim(STN)",dim(STN))
    STNP<-STN[PN,allow.cartesian=TRUE]
    STNP[,c("PREDTELLER","PREDTELLER.f","PREDTELLER.a"):=list(PREDRATE*PREDNEVNER,pmax(PREDRATE.f,PREDNEVNER.f),pmax(PREDRATE.a,PREDNEVNER.a))]
    #Kast overflødige kolonner
    kastkols<-setdiff(names(STNP),c(FinnTabKols(names(STNP)),"PREDTELLER","PREDTELLER.f","PREDTELLER.a"))
    STNP[,(kastkols):=NULL]
    cat(" og etter mergre dim(STNP)",dim(STNP),"\n")
    PT<-OmkodFil(STNP,RD,globs=globs,echo=1)
    cat("-----PREDTELLER (PT) ferdig med dim(PT)", dim(PT),"\n")
    cat("***Merger med TNF\n")
    orgdim<-dim(TNF)
    
    #Merge PT med TNF til ferdig kube
    tabkols<-FinnTabKols(names(TNF))
    setkeyv(TNF,tabkols)
    setkeyv(PT,tabkols)
    KUBE<-PT[TNF]
    KUBE<-SettMergeNAs(KUBE,FGPs[[filer[TT]]]$vals) 
    cat("Før merge KUBE<-PT[TNF] er dim(TNF)",orgdim," og etter merge dim(KUBE)",dim(KUBE),"\n")
    cat("------FERDIG MED PREDIKSJON\n")
    fullresult<-list(KUBE=KUBE,STN=STN,TNF=TNF,PN=PN,PT=PT,RD=RD,STNPFd=STNPFd)
    
  } 
  else {
    KUBE<-copy(TNF)
    fullresult<-list(TNF=TNF)
  }
  #Fjern temporære BUFFER filer
   
  rydd<-setdiff(Finfo$tmpBUFFER,c("BEF_GKa","BEF_GKu"))
  .GlobalEnv$BUFFER[rydd]<-NULL
  
  if ("raaKUBE0" %in% names(dumps)){
    for (format in dumps[["raaKUBE0"]]) {
      DumpTabell(KUBE,paste(KUBEid,"raaKUBE0",sep="_"),globs=globs,format=format)
    }
  }
  
  if (tmpbryt>0){raaKUBE0<-copy(KUBE)}
  if (tmpbryt==1){return(fullresult)}
  if (bare_TN==0){
    
    if (D_develop_predtype=="DIR"){
      #Sett skala for teller (må gjøres før rate brukes i MEISskala) 
      if (!(is.na(KUBEdscr$RATESKALA) | KUBEdscr$RATESKALA=="")){
        KUBE[,RATE:=RATE*as.numeric(KUBEdscr$RATESKALA)]
      }
      
      #FINN MEISskala Merk at dette gjelder både ved REFVERDI_VP=P og =V  
      ########################################################################
      
      if (KUBEdscr$REFVERDI_VP=="P"){
        VF<-eval(parse(text=paste("subset(KUBE,",PredFilter$PfiltStr,")",sep="")))  
        #Evt hvis en eller flere element i PredFilter ikke er med i Design for TNF og må lages
        if(nrow(VF)==0){
          cat("************************************\nNOE RART MED PredFilter, IKKE I KUBEDESIGN, MÅ UT PÅ NY OMKODING.\nER DETTE RETT?\n")
          VF<-OmkodFilFraPart(TNF,PredFilter$Design,FGP=FGPs[[filer["T"]]],globs=globs)
        }
        
        VF[,MEISskala:=RATE]
        VFtabkols<-setdiff(intersect(names(VF),globs$DefDesign$DesignKolsFA),PredFilter$Pkols)
        VF<-VF[,c(VFtabkols,"MEISskala"),with=FALSE]
        
        
        setkeyv(KUBE,VFtabkols)
        setkeyv(VF,VFtabkols)
        KUBE<-VF[KUBE]
      } else {
        KUBE[,MEISskala:=NA_real_]
      }
      
      print("D-develop")
      cat("Meisskala1:\n")
      print(unique(KUBE$MEISskala))
    }

    
    
    
    
    #Finn "snitt" for ma-år. 
    #DVs, egentlig lages forløpig bare summer, snitt settes etter prikking under
    #Snitt tolerer missing av type .f=1 ("random"), men bare noen få anonyme .f>1, se KHaggreger
    #Rapporterer variabelspesifikk VAL.n som angir antall år brukt i summen når NA holdt utenom
    ###################################################################3
    
    setkeyv(KUBE,c("AARl","AARh"))
    aar<-unique(KUBE[,c("AARl","AARh"),with=FALSE])
    int_lengde<-as.integer(unique(KUBE[,AARh-AARl+1]))
    if (length(int_lengde)>1){
      KHerr(paste("!!!!!!HAR ULIKE LENGDER PÅ INTERVALLER!!"))
    }
   
    #Må "balansere" NA i teller og nevner slik sumrate og sumnevner balanserer  (Bedre/enklere å gjøre det her enn i KHaggreger)
    #Kunne med god grunn satt SPVFLAGG her og så bare operert med denne som en egenskap for hele linja i det som kommer
    #Men for å ha muligheten for å håndtere de forskjellige varibalene ulikt og i full detalj lar jeg det stå mer generelt
    #Slik at dataflyten støtter en slik endring
    
    tuppel<-intersect(c("TELLER","NEVNER","RATE"), names(KUBE))
    tuppel.f<-paste(tuppel,".f",sep="")
    fmax<-paste("pmax(",paste(tuppel.f,collapse=","),")",sep="")
    if (length(tuppel)>0){
      KUBE[eval(parse(text=fmax))>0,(tuppel):=list(NA)]
      KUBE[eval(parse(text=fmax))>0,(tuppel.f):=eval(parse(text=fmax))]
      #Om enkeltobservasjoner ikke skal brukes, men samtidig tas ut av alle summeringer
      #kan man ha satt VAL=0,VAL.f=-1 
      #Dette vil ikke ødelegge summer der tallet inngår. Tallet selv, eller sumemr av kun slike tall, settes nå til NA 
      #Dette brukes f.eks når SVANGERROYK ekskluderer Oslo, Akershus. Dette er skjuling, så VAL.f=3
      KUBE[eval(parse(text=fmax))==-1,(tuppel):=list(NA)]
      KUBE[eval(parse(text=fmax))==-1,(tuppel):=list(3)]
    }
    
    
    ma_satt<-0
    orgintMult<-1
    if (KUBEdscr$MOVAV>1){
      if (any(aar$AARl!=aar$AARh)){
        KHerr(paste("Kan ikke sette snitt (ma=",ma,") når det er intervaller i originaldata",sep=""))
      } else {
        ma<-KUBEdscr$MOVAV
        
        #Finner evt hull i år for hele designet
        AntYMiss=max(aar$AARl)-min(aar$AARl)+1-length(aar$AARl)
        if (AntYMiss>0){cat("Setter SumOverAar med AntYMiss=",AntYMiss,"\n")}
        maKUBE<-FinnSumOverAar(KUBE,per=ma,FyllMiss=TRUE,AntYMiss=AntYMiss,na.rm=TRUE,report_lpsvars=TRUE,globs=globs)
        
        #sett rate på nytt
        if(TNPdscr$NEVNERKOL!="-"){
          maKUBE<-LeggTilNyeVerdiKolonner(maKUBE,"RATE={TELLER/NEVNER}")
          if (D_develop_predtype=="DIR"){
            if (!(is.na(KUBEdscr$RATESKALA) | KUBEdscr$RATESKALA=="")){
              maKUBE[,RATE:=RATE*as.numeric(KUBEdscr$RATESKALA)]
            }
          }
        }
        ma_satt<-1
        #maKUBE<-maKUBE[,names(maKUBE)[!grepl(".n$",names(maKUBE))],with=FALSE]  #Kast .n kolonner, ferdig med disse
        KUBE<-maKUBE
      }
    } else {
      #Må legge til VAL.n for regning under når orignale periodesummer, evt n=1 når originale snitt
      valkols<-FinnValKols(names(KUBE))
      orgint_n<-int_lengde[1]
      n<-orgint_n
      if (!is.na(FGPs[[filer["T"]]]$ValErAarsSnitt)){
        n<-1
        orgintMult<-orgint_n
      }
      lp<-paste("KUBE[,c(\"",paste(valkols,".n",collapse="\",\"",sep=""),"\"):=list(",n,")]",sep="")
      KUBE[,eval(parse(text=lp))]
    }
    
    
    if (FGPs[[filer["T"]]][["B_STARTAAR"]]>0){
      valK=FinnValKols(names(KUBE))
      KUBE[GEOniv=="B" & AARl<FGPs[[filer["T"]]][["B_STARTAAR"]],(valK):=NA]
      KUBE[GEOniv=="B" & AARl<FGPs[[filer["T"]]][["B_STARTAAR"]],(paste(valK,".f",sep="")):=9]
    }
    
    if ("maKUBE0" %in% names(dumps)){
      for (format in dumps[["maKUBE0"]]) {
        DumpTabell(KUBE,paste(KUBEid,"maKUBE0",sep="_"),globs=globs,format=format)
      }
    }
    
    raaKUBE<-copy(KUBE)
    #Anonymiser og skjul
    ##################################################################
    
    #Anonymiser, trinn 1 Filtrer snitt som ikke skal brukes pga for mye anonymt
    #Se KHaggreger!
    raaKUBE<-copy(KUBE)
    
   
    #Anonymiser, trinn 1   Filtrer snitt som ikke skal brukes pga for mye anonymt fra original
    if (ma_satt==1){
      valkols<-FinnValKols(names(KUBE))
      anon_tot_tol<-0.2
      
      lp<-paste("KUBE[,':='(",
                paste(valkols,"=ifelse(",valkols,".n>0 & ",valkols,".fn3/",valkols,".n>=",anon_tot_tol,",NA,",valkols,"),",
                      valkols,".f=ifelse(",valkols,".n>0 & ",valkols,".fn3/",valkols,".n>=",anon_tot_tol,",3,",valkols,".f)",
                      sep="",collapse=","),
                ")]",sep="")
      eval(parse(text=lp))
    }    
    
    if ("anoKUBE1" %in% names(dumps)){
      for (format in dumps[["anoKUBE1"]]) {
        DumpTabell(KUBE,paste(KUBEid,"anoKUBE1",sep="_"),globs=globs,format=format)
      }
    }
    
    #Anonymiser, trinn 2 Ekte anonymisering basert på liten teller, liten nevner og liten N-T
    if (!(is.na(KUBEdscr$PRIKK_T) | KUBEdscr$PRIKK_T=="")){
      #T<=PRIKK_T
      cat("T-PRIKKER",nrow(subset(KUBE,TELLER<=KUBEdscr$PRIKK_T)),"rader\n")
      KUBE[TELLER<=KUBEdscr$PRIKK_T & TELLER.f>=0,c("TELLER","TELLER.f","RATE","RATE.f"):=list(NA,3,NA,3)]
      #N-T<=PRIKK_T
      cat("N-T-PRIKKER",nrow(subset(KUBE,NEVNER-TELLER<=KUBEdscr$PRIKK_T)),"rader\n")
      KUBE[NEVNER-TELLER<=KUBEdscr$PRIKK_T & TELLER.f>=0 & NEVNER.f>=0,c("TELLER","TELLER.f","RATE","RATE.f"):=list(NA,3,NA,3)]    
    } 
    
    if (!(is.na(KUBEdscr$PRIKK_N) | KUBEdscr$PRIKK_N=="")){
      #N<PRIKK_N
      cat("N-PRIKKER",nrow(subset(KUBE,NEVNER<=KUBEdscr$PRIKK_N)),"rader\n")
      KUBE[NEVNER<=KUBEdscr$PRIKK_N & NEVNER.f>=0,c("TELLER","TELLER.f","RATE","RATE.f"):=list(NA,3,NA,3)]
    }
    
    if ("anoKUBE2" %in% names(dumps)){
      for (format in dumps[["anoKUBE2"]]) {
        DumpTabell(KUBE,paste(KUBEid,"anoKUBE2",sep="_"),globs=globs,format=format)
      }
    }
    #Anonymiser trinn 3. Anonymiser naboer
    if (!(is.na(KUBEdscr$OVERKAT_ANO) | KUBEdscr$OVERKAT_ANO=="")){
      #DEVELOP: BRuk .f=4 her slik at ikke slår ut i HULL under
      KUBE<-AnonymiserNaboer(KUBE,KUBEdscr$OVERKAT_ANO,FGP=FGPs[[filer[["T"]]]],D_develop_predtype,globs=globs)
    }
    if ("anoKUBE3" %in% names(dumps)){
      for (format in dumps[["anoKUBE3"]]) {
        DumpTabell(KUBE,paste(KUBEid,"anoKUBE3",sep="_"),globs=globs,format=format)
      }
    }
    
    raaKUBE2<-copy(KUBE)
     #Anonymiser trinn 4. Skjule svake og skjeve tidsserrier
    SvakAndelAvSerieGrense<-0.5
    HullAndelAvSerieGrense<-0.2
    
    if (!(is.na(KUBEdscr$STATTOL_T) | KUBEdscr$STATTOL_T=="")){
      tabkols<-setdiff(intersect(names(KUBE),globs$DefDesign$DesignKolsFA),c(globs$DefDesign$DelKols[["Y"]]))
      KUBE[TELLER.f<9,AntAar:=.N,by=tabkols]
      KUBE[TELLER.f<9,SVAK:=sum(is.na(TELLER) | TELLER<=KUBEdscr$STATTOL_T),by=tabkols]
      KUBE[TELLER.f<9,HULL:=sum(TELLER.f==3),by=tabkols]
      KUBE[TELLER.f<9,SKJUL:=ifelse(SVAK/AntAar>SvakAndelAvSerieGrense | HULL/AntAar>HullAndelAvSerieGrense,1,0)]
      
      
      cat("Skjuler",nrow(subset(KUBE,SKJUL==1)),"rader\n")
      KUBE[SKJUL==1,c("TELLER","TELLER.f"):=list(NA,3)]
      KUBE[SKJUL==1,c("RATE","RATE.f"):=list(NA,3)]
      KUBE[,c("SVAK","HULL","SKJUL","AntAar"):=NULL]
    }
    raaKUBE3<-copy(KUBE)
    if ("anoKUBE4" %in% names(dumps)){
      for (format in dumps[["anoKUBE4"]]) {
        DumpTabell(KUBE,paste(KUBEid,"anoKUBE4",sep="_"),globs=globs,format=format)
      }
    }
    
    
    ####################################################
    # LAYOUT
    ####################################################3
    
    if ("KUBE_SLUTTREDIGERpre" %in% names(dumps)){
      for (format in dumps[["KUBE_SLUTTREDIGERpre"]]) {
        DumpTabell(KUBE,paste(KUBEid,"KUBE_SLUTTREDIGERpre",sep="_"),globs=globs,format=format)
      }
    }
    
    ######################################################
    #EVT SPESIALBEHANDLING
    if (!(is.na(KUBEdscr$SLUTTREDIGER) | KUBEdscr$SLUTTREDIGER=="")){
      synt<-gsub("\\\r","\\\n",KUBEdscr$SLUTTREDIGER)
      error<-""
      ok<-1
      if (grepl("<STATA>",synt)){
        synt<-gsub("<STATA>[ \n]*(.*)","\\1",synt)
        RES<-KjorStataSkript(KUBE,synt,tableTYP="DT",batchdate=batchdate,globs=globs)
        if (RES$feil!=""){
          error<-paste("Noe gikk galt i kjøring av STATA",RES$feil,sep="\n")
          ok<-0
        } else {
          KUBE<-RES$TABLE
        }
      } else {
        rsynterr<-try(eval(parse(text=synt)),silent=TRUE)
        if(class(rsynterr)=="try-error"){
          ok<-0
          error<-rsynterr
        }
      }
      if (ok==0){
        print(error)
      }
    }
    
    
    
    if ("KUBE_SLUTTREDIGERpost" %in% names(dumps)){
      for (format in dumps[["KUBE_SLUTTREDIGERpost"]]) {
        DumpTabell(KUBE,paste(KUBEid,"KUBE_SLUTTREDIGERpost",sep="_"),globs=globs,format=format)
      }
    }
    
    #mapvalues(KUBE$SPVFLAGG,c(0,1,2,3),c(0,2,1,3),warn_missing = FALSE)     #BRUKER 1='.",2='.." i NESSTAR
    
    OrgKubeKolNames<-names(KUBE)
    
    #Alle kolonner settes for alle KUBER uavhengig av om TELLER, NEVNER, RATE, MALTALL, PRED=V/P etc    
    if (!"NEVNER" %in% names(KUBE)){
      KUBE[,NEVNER:=NA]
    }
    if (!"RATE" %in% names(KUBE)){
      KUBE[,RATE:=NA]
    }
    
    if (D_develop_predtype!="DIR"){
      #Sett skala for teller
      if (!(is.na(KUBEdscr$RATESKALA) | KUBEdscr$RATESKALA=="")){
        KUBE[,RATE:=RATE*as.numeric(KUBEdscr$RATESKALA)]
      }
    }

    
    #Legg til manglende kolonner for homogen behandling under
    missKol<-setdiff(unlist(lapply(c("TELLER","NEVNER","RATE","PREDTELLER"),function(x){paste(x,c("",".f",".a",".n"),sep="")})),names(KUBE))
    if (length(missKol)>0){
      KUBE[,(missKol):=NA]
    }
    
    #Behold sum, disse sendes til Friskvik
    KUBE[,sumTELLER:=orgintMult*TELLER]
    KUBE[,sumNEVNER:=orgintMult*NEVNER]
    KUBE[,sumPREDTELLER:=orgintMult*PREDTELLER]
    
    #Ta snitt for alt annet enn RATE (der forholdstallet gjør snitt uønsket)
    #VAL:=VAL/VAL.n
    valkols<-setdiff(FinnValKols(names(KUBE)),c("RATE","SMR"))
    if (length(valkols)>0){
      lp<-paste("KUBE[,c(\"",paste(valkols,collapse="\",\""),"\"):=list(",
                paste(valkols,"=",valkols,"/",valkols,".n",
                      sep="",collapse=","),
                ")]",sep="")
      KUBE[,eval(parse(text=lp))]
    }
    
    if (!(is.na(TNPdscr$NYEKOL_RAD_postMA) | TNPdscr$NYEKOL_RAD_postMA=="")){
      KUBE<-LeggTilNyeVerdiKolonner(KUBE,TNPdscr$NYEKOL_RAD_postMA,slettInf=TRUE,postMA=TRUE)
    }
    
   
    
    if (grepl("\\S",KUBEdscr$MTKOL)){
      maltall=KUBEdscr$MTKOL
      KUBE[,eval(parse(text=paste("MALTALL:=",KUBEdscr$MTKOL,sep="")))]
    } else if(TNPdscr$NEVNERKOL=="-"){
      maltall="TELLER"
      KUBE[,MALTALL:=TELLER]
    } else {
      maltall="RATE"
      KUBE[,MALTALL:=RATE]
    }
    #maltallt<-intersect(paste(maltall,""))

    if (D_develop_predtype=="DIR"){
      print("D-develop")
      cat("Meisskala3:\n")
      print(unique(KUBE$MEISskala))
      print(KUBE)
      
      
      #SETT SMR og MEIS
      
      if (KUBEdscr$REFVERDI_VP=="P"){
        KUBE[,SMR:=sumTELLER/sumPREDTELLER*100]
        KUBE[,MEIS:=(sumTELLER/sumPREDTELLER)*MEISskala]
      } else if (KUBEdscr$REFVERDI_VP=="V"){
        KUBE[,SMR:=NA_real_]
        KUBE[,MEIS:=MALTALL]
      } else {
        KUBE[,SMR:=NA_real_]
        KUBE[,MEIS:=MALTALL]
      }
    } else {
      #SETT SMRtmp. For å lage NORMSMR under må denne settes før NORM. Derfor kan jeg ikke sette SMR=MALTALL/NORM nå.
      #Men NORMSMR er selvsagt alltid 100 for REFVERDI_P="V"
      if (KUBEdscr$REFVERDI_VP=="P"){   
        KUBE[,SMRtmp:=sumTELLER/sumPREDTELLER*100]
      } else if (KUBEdscr$REFVERDI_VP=="V"){
        KUBE[,SMRtmp:=100]
      } else {
        KUBE[,SMRtmp:=NA]
      }
    }
    
    #FINN "LANDSNORMAL". Merk at dette gjelder både ved REFVERDI_VP=P og =V  
    ########################################################################
    
    if (D_develop_predtype=="DIR"){
      #Midlertidig dirty løsning
      #KUBER:REFVERDI bør omdøpes til KUBER:PREDFILTER og det er denne som brukes i SettPredFilter
      #Det bør så lages en ny kolonne KUBER:REFGEOn som har GEOniv for referanseverdi. Denne brukes primært for å sette SMR i modus=V 
      RefGEOn<-"L"
      RefGEOnFilt<-paste("GEOniv=='",RefGEOn,"'",sep="")
      VF<-eval(parse(text=paste("subset(KUBE,",RefGEOnFilt,")",sep="")))  
    } else {
      VF<-eval(parse(text=paste("subset(KUBE,",PredFilter$PfiltStr,")",sep="")))  
    }
    
    #Evt hvis en eller flere element i PredFilter ikke er med i Design for TNF og må lages
    if(nrow(VF)==0){
      cat("************************************\nNOE RART MED LANDSNORM, IKKE I KUBEDESIGN, MÅ UT PÅ NY OMKODING.\nER DETTE RETT?\n")
      VF<-OmkodFilFraPart(TNF,PredFilter$Design,FGP=FGPs[[filer["T"]]],globs=globs)
    }

    if (D_develop_predtype=="IND"){    
      VFtabkols<-setdiff(intersect(names(VF),globs$DefDesign$DesignKolsFA),PredFilter$Pkols)
      if (maltall %in% c("TELLER","RATE")){
        setnames(VF,c(paste(maltall,c("",".f",".a",".n"),sep=""),"SMRtmp"),c(paste("NORM",c("",".f",".a",".n"),sep=""),"NORMSMR"))
        VF<-VF[,c(VFtabkols,paste("NORM",c("",".f",".a",".n"),sep=""),"NORMSMR"),with=FALSE]
      } else {
        setnames(VF,c(maltall,"SMRtmp"),c("NORM","NORMSMR"))
        VF<-VF[,c(VFtabkols,"NORM","NORMSMR"),with=FALSE]
      }
    } else {
      VF[,lopendeMEISref:=MEIS]
      VFtabkols<-setdiff(intersect(names(VF),globs$DefDesign$DesignKolsFA),c("GEOniv","GEO","FYLKE"))
      VF<-VF[,c(VFtabkols,"lopendeMEISref"),with=FALSE]
    }
    setkeyv(KUBE,VFtabkols)
    setkeyv(VF,VFtabkols)
    
    KUBE<-VF[KUBE]
  
    if (D_develop_predtype=="IND"){  
      #Juster SMR proporsjonalt slik at NORM (landet) alltid har SMR=100
      #SMR>100 kan oppstå dersom det f.eks. er noen med ukjent alder/kjønn.
      #Ratene for ukjent alder/kjønn vil ikke matche nevner fra BEF, derfor vil det predikeres for får døde relativt til observert
      if (KUBEdscr$REFVERDI_VP=="P"){
        KUBE[,SMR:=sumTELLER/sumPREDTELLER*100]
      } else if (KUBEdscr$REFVERDI_VP=="V"){
        KUBE[,SMR:=MALTALL/NORM*100]
      } else {
        KUBE[,SMR:=NA]
      }
  
      KUBE[,SMR:=100*(SMR/NORMSMR)]
  
      KUBE[,MEIS:=SMR*NORM/100]
    } else {
      KUBE[,lopendeFORHOLDSVERDI:=MEIS/lopendeMEISref*100]
      
      #D-develop
      #Dirty tricks i denne midlertidige løsninga, bruker gamle utnavn
      KUBE[,SMR:=lopendeFORHOLDSVERDI]
      KUBE[,NORM:=lopendeMEISref]
    }

    #Bytt til eksterne TAB-navn for ekstradimensjoner
    FGP<-FGPs[[filer[["T"]]]]
    etabs<-character(0)
    for (etab in names(KUBE)[grepl("^TAB\\d+$",names(KUBE))]){
      if (grepl("\\S",FGP[[etab]])){
        setnames(KUBE,etab,FGP[[etab]]) 
        etabs<-c(etabs,FGP[[etab]])
      }
    }
    
    
    #SETT UTKOLONNER
    if (!(is.na(KUBEdscr$NESSTARTUPPEL) | KUBEdscr$NESSTARTUPPEL=="")){
      NstarTup<-unlist(str_split(KUBEdscr$NESSTARTUPPEL,","))
    } else if (KUBEdscr$REFVERDI_VP=="P"){
      NstarTup<-c("T","RATE","SMR","MEIS")
    } else {
      NstarTup<-character(0)
    }
    OutVar<-globs$NesstarOutputDef[NstarTup]
    
    
    
    if (!(is.na(KUBEdscr$EKSTRAVARIABLE) | KUBEdscr$EKSTRAVARIABLE=="")){
      hjelpeVar<-unlist(str_split(KUBEdscr$EKSTRAVARIABLE,","))
      OutVar<-c(OutVar,hjelpeVar)
    }  
    
    KHtabs<-c("GEO","AAR","KJONN","ALDER","UTDANN","SIVST","LANDBAK")
    tabs<-c(KHtabs,etabs)
    if (!(is.na(KUBEdscr$DIMDROPP) | KUBEdscr$DIMDROPP=="")){
      dimdropp<-unlist(str_split(KUBEdscr$DIMDROPP,","))
      tabs<-setdiff(tabs,dimdropp)
    } 
    
    KUBE[,AAR:=paste(AARl,"_",AARh,sep="")]
    if (all(c("ALDERl","ALDERh") %in% names(KUBE))){ 
      KUBE[,ALDER:=paste(ALDERl,"_",ALDERh,sep="")]
    } else {
      tabs<-setdiff(tabs,"ALDER")
    } 
    if (!"KJONN" %in% names(KUBE)){ 
      tabs<-setdiff(tabs,"KJONN")
    }
    
    ######################################################
    #EVT SPESIALBEHANDLING
    if (!(is.na(KUBEdscr$RSYNT_POSTPROSESS) | KUBEdscr$RSYNT_POSTPROSESS=="")){
      synt<-gsub("\\\r","\\\n",KUBEdscr$RSYNT_POSTPROSESS)
      error<-""
      ok<-1
      if (grepl("<STATA>",synt)){
        synt<-gsub("<STATA>[ \n]*(.*)","\\1",synt)
        RES<-KjorStataSkript(KUBE,synt,tableTYP="DT",batchdate=batchdate,globs=globs)
        if (RES$feil!=""){
          error<-paste("Noe gikk galt i kjøring av STATA",RES$feil,sep="\n")
          ok<-0
        } else {
          KUBE<-RES$TABLE
        }
      } else {
        rsynterr<-try(eval(parse(text=synt)),silent=TRUE)
        if(class(rsynterr)=="try-error"){
          ok<-0
          error<-rsynterr
        }
      }
      if (ok==0){
        print(error)
      }
    }
    
    
    #LAYOUT
    utkols<-c(tabs,OutVar)
    NESSTAR<-copy(KUBE)
    
    #SKJUL HELE TUPPELET  
    #FLAGG PER VARIABEL KAN/BØR VURDERES!
    #Litt tricky å finne riktig ".f"-kolloner. Må ikke ta med mBEFc f.eks fra BEF fila dersom denne er irrelevant
    fvars<-intersect(names(NESSTAR),paste(union(globs$NesstarOutputDef,OutVar),".f",sep=""))
    #fvars<-intersect(names(NESSTAR),c(OrgKubeKolNames[grepl(".f$",OrgKubeKolNames)],"NORM.f","SMR.f"))
    NESSTAR[,SPVFLAGG:=0]
    if (length(fvars)>0){
      #Dette er unødvendig krongelete. Men dersom f.eks RATE.f=2 pga TELLER.f=1, ønskes SPVFLAGG=1
      NESSTAR[,tSPV1:=eval(parse(text=paste("pmax(",paste(lapply(fvars,function(x){paste(x,"*(",x,"!=2)",sep="")}),collapse=","),",na.rm = TRUE)",sep="")))]
      NESSTAR[,tSPV2:=eval(parse(text=paste("pmax(",paste(fvars,collapse=","),",na.rm = TRUE)",sep="")))]
      NESSTAR[,SPVFLAGG:=ifelse(tSPV1==0,tSPV2,tSPV1)]
      NESSTAR[,c("tSPV1","tSPV2"):=NULL]
      NESSTAR[SPVFLAGG>0,eval(parse(text=paste("c(\"",paste(OutVar,collapse="\",\""),"\"):=list(NA)",sep="")))]
    }
    NESSTAR[is.na(SPVFLAGG),SPVFLAGG:=0]
    NESSTAR[,SPVFLAGG:=mapvalues(SPVFLAGG,c(-1,9,4),c(3,1,3),warn_missing = FALSE)]
    
    
    #Filtrer bort GEO som ikke skal rapporteres 
    KUBE<-KUBE[GEO %in% globs$UtGeoKoder]
    NESSTAR<-NESSTAR[GEO %in% globs$UtGeoKoder]
    #NESSTAR<-NESSTAR[!grepl("99|9900$|1902\\d{2}|5401\\d{2}|03011[67]",GEO),]
    
    #Filtrer bort uønskede tabs
    if ("ALDER" %in% names(KUBE)){
      NESSTAR<-NESSTAR[!ALDER %in% c("999_999","888_888"),]
    } 
    if ("KJONN" %in% names(NESSTAR)){
      NESSTAR<-NESSTAR[!KJONN %in% c(8,9),]
    }
    
    LagAlleFriskvikIndikatorerForKube(KUBEid=KUBEid,KUBE=NESSTAR,aargang=globs$KHaargang,modus=KUBEdscr$MODUS,FGP=FGPs[[filer["T"]]],versjonert=versjonert,batchdate=batchdate,globs=globs)
    
    NESSTAR<-NESSTAR[,c(utkols,"SPVFLAGG"),with=FALSE]
    
    if (tmpbryt==2){
      print("TMPBRYT=2")
      return(list(raaKUBE0=raaKUBE0,raaKUBE=raaKUBE,raaKUBE2=raaKUBE2,raaKUBE3=raaKUBE3,KUBE=KUBE,TNF=TNF,NESSTAR=NESSTAR))
    }
    
    cat("---------------------KUBE FERDIG")
    CompForrigeKube<-0
    ForKub<-FinnDatertKube(KUBEid,silent=TRUE)
    if (!is.logical(ForKub)){
      #CompForrigeKube<-SammenlignKuber(KUBE,FinnDatertKube(KUBEid))
#       if (is.logical(CompForrigeKube$check) && CompForrigeKube$check==TRUE){
#           cat("    (identisk med forrige daterte versjon)\n")
#       } else {
#         cat("\nAVVIK FRA FORRIGE daterte versjon:\n")
#         print(CompForrigeKube$checkm)
#         cat("--------------\n")
#       }
    } else {
      cat("      (Ingen eldre vesjon å sammenligne med)\n")
      CompForrigeKube<-NA
    }
    #Ad hoc redigering
    ##################################################################
    
    #FRISKVIK
    ################################################################
    
    
    #RESULTAT<-(list(TNF=TNF,STN=STNc,STNP=STNP,KUBE=KUBE))
    RESULTAT<-list(KUBE=KUBE,NESSTAR=NESSTAR,CFK=CompForrigeKube)
  }  
  #SKRIV RESULTAT    
  path<-globs$path
  printR<-TRUE
  if (printR){
    utfiln<-paste(path,"/",globs$KubeDirNy,"/",KUBEid,".rds",sep="")
    #save(Filgruppe,file=utfiln)
    print(utfiln)
    saveRDS(KUBE,file=utfiln)
    if (versjonert==TRUE){
      utfild<-paste(path,"/",globs$KubeDirDat,"/",KUBEid,"_",batchdate,".rds",sep="")
      #cat("Kopierer datert",utfild, "jasdkljasl",globs$KubeDirDat,"\n")
      file.copy(utfiln,utfild)
      if (csvcopy==TRUE){
        utfild<-gsub("(.*)/R/(.*)","\\1/csv/\\2",utfild)
        utfild<-gsub("(.*)\\.rds$","\\1.csv",utfild)
        print(utfild)
        write.table(NESSTAR,file=utfild,sep=';',na="",row.names = FALSE)
      } 
    }
  }
  cat("-------------------------KUBE",KUBEid,"FERDIG--------------------------------------\n")
  return(RESULTAT)
  
  
}


FinnRedesignForFilter<-function(ORGd,Filter,globs=FinnGlobs()){
  MODd<-Filter
  for (del in setdiff(names(ORGd$Part),names(Filter))){
    MODd[[del]]<-copy(ORGd$Part[[del]])
  }
  return(FinnRedesign(ORGd,list(Part=MODd),globs=globs))
}

FinnDesignEtterFiltrering<-function(ORGd,Filter,FilterKols=character(0),FGP=list(amin=0,amax=120),globs=FinnGlobs()){
  FiltD<-FinnRedesignForFilter(ORGd,Filter,globs=globs)$Dekk
  FiltD<-FiltD[,setdiff(names(FiltD),FilterKols),with=FALSE]
  return(FinnDesign(FiltD,FGP=FGP,globs=globs))
}



LagAlleFriskvikIndikatorerForKube<-function(KUBEid,globs=FinnGlobs(),modus=globs$HOVEDMODUS,aargang=format(Sys.time(), "%Y"),...){
  
  #indikatorer<-unlist(sqlQuery(globs$dbh,paste("SELECT INDIKATOR FROM ",friskvikTAB,aargang," WHERE KUBE_NAVN='",KUBEid,"'",sep=""),as.is=TRUE))
  indikatorer<-sqlQuery(globs$dbh,paste("SELECT INDIKATOR, ID FROM FRISKVIK WHERE AARGANG=",aargang,"AND KUBE_NAVN='",KUBEid,"'",sep=""),as.is=TRUE)
  
  if (dim(indikatorer)[1]>0){
    for (i in 1:dim(indikatorer)[1]){
      cat("Lager Friskvikfil for ",indikatorer[i,1],"\n")
      LagFriskvikIndikator(id=indikatorer[i,2],aargang=aargang,modus=modus,globs=globs,...)
    }
  }
}

LagFriskvikIndikator<-function(id,KUBE=data.table(),FGP=list(amin=0,amax=120),versjonert=FALSE,aargang=format(Sys.time(), "%Y"),batchdate=SettKHBatchDate(),globs=FinnGlobs(),modus=globs$HOVEDMODUS){
  
  
  #FVdscr<-sqlQuery(globs$dbh,paste("SELECT * FROM ",aargang," WHERE INDIKATOR='",indikator,"' AND VERSJONFRA<=",datef," AND VERSJONTIL>",datef,sep=""),as.is=TRUE)
  FVdscr<-sqlQuery(globs$dbh,paste("SELECT * FROM FRISKVIK WHERE ID=",id,sep=""),as.is=TRUE)
  
  moduser<-unlist(str_split(FVdscr$MODUS,""))


  ## FHP and Oppveksprofile (OVP) folders specification
  ## -------------------------------------------------
  profile <- FVdscr$PROFILTYPE

  switch(profile,
         "FHP" = {
           setDir_K <- globs$FriskVDir_K
           setDir_B <- globs$FriskVDir_B
           setDir_F <- globs$FriskVDir_F
         },
         "OVP" = {
           setDir_K <- globs$ovpDir_K
           setDir_B <- globs$ovpDir_B
           setDir_F <- globs$ovpDir_F
         }
         )


  
  for (modus in moduser){
    if (modus %in% c("K","F","B")){
      FriskVDir<-""
      GEOfilter<-character(0)

      if (modus=="K"){
        FriskVDir<-setDir_K
        GEOfilter<-c("K","F","L")
      } else if (modus=="B"){
        FriskVDir<-setDir_B
        GEOfilter<-c("B","K","F","L")
      } else if (modus=="F"){
        FriskVDir<-setDir_F
        GEOfilter<-c("F","L")
      }


      ## ## orignal code 
      ## if (modus=="K"){
      ##   FriskVDir<-globs$FriskVDir_K
      ##   GEOfilter<-c("K","F","L")
      ## } else if (modus=="B"){
      ##   FriskVDir<-globs$FriskVDir_B
      ##   GEOfilter<-c("B","K","F","L")
      ## } else if (modus=="F"){
      ##   FriskVDir<-globs$FriskVDir_F
      ##   GEOfilter<-c("F","L")
      ## }

      
      #FILTRER RADER
      filterA<-"(GEOniv %in% GEOfilter)"
      if (grepl("\\S",FVdscr$ALDER) & FVdscr$ALDER !="-"){
        FVdscr$ALDER<-gsub("^(\\d+)$","\\1_\\1",FVdscr$ALDER)
        FVdscr$ALDER<-gsub("^(\\d+)_$",paste("\\1_",FGP$amax,sep=""),FVdscr$ALDER)
        FVdscr$ALDER<-gsub("^_(\\d+)$",paste(FGP$amin,"_\\1",sep=""),FVdscr$ALDER)
        FVdscr$ALDER<-gsub("^ALLE$",paste(FGP$amin,"_",FGP$amax,sep=""),FVdscr$ALDER)
        filterA<-c(filterA,paste("ALDER=='",FVdscr$ALDER,"'",sep=""))
      }
      for (tab in c("AARh","KJONN","SIVST","UTDANN","LANDBAK")){
        if (grepl("\\S",FVdscr[[tab]]) & FVdscr[[tab]]!="-"){
          filterA<-c(filterA,paste(tab,"==",FVdscr[[tab]],sep=""))
        }
      }
      if (grepl("\\S",FVdscr$EKSTRA_TAB) & FVdscr$EKSTRA_TAB !="-"){
        filterA<-c(filterA,FVdscr$EKSTRA_TAB)
        KUBE$ETAB<-FVdscr$EKSTRA_TAB
      }
      filter<-paste(filterA,collapse=" & ")
      FRISKVIK<-subset(KUBE,eval(parse(text=filter)))
      
      
      defrows<-nrow(subset(KHglobs$GeoKoder,FRA<=aargang & TIL>aargang & TYP=="O" & GEOniv %in% GEOfilter))
      if (nrow(FRISKVIK)!=defrows){
        KHerr(paste("FEIL I FRISKVIKFILTER", filter, "GIR bare", nrow(FRISKVIK),"/",defrows,"rader!"))           
      }
      
      #SISTE RYDD KOLONNER (bare for TabKols)
      MissKol<-setdiff(c("GEO","AAR","KJONN","ALDER","UTDANN","SIVST","LANDBAK","ETAB"),names(FRISKVIK))
      if (length(MissKol)>0){
        FRISKVIK[,(MissKol):=NA]
      }
      
      MissKol2<-setdiff(c(globs$FriskvikTabs,globs$FriskvikVals),names(FRISKVIK))
      if (length(MissKol2)>0) {
        KHerr(paste("FEIL: Kolonnene", MissKol2, "mangler i Friskvik!"))
        FRISKVIK[,(MissKol2):=NA]
      }
      
      if(grepl("\\S",FVdscr$ALTERNATIV_MALTALL)){
        FRISKVIK$MALTALL<-FRISKVIK[[FVdscr$ALTERNATIV_MALTALL]]
        kastkols<-setdiff(globs$FriskvikVals,"MALTALL")
        FRISKVIK[,(kastkols):=NA]
      }
     
      
      FRISKVIK<-FRISKVIK[,c(globs$FriskvikTabs,globs$FriskvikVals),with=FALSE]
      
      versjonert=TRUE
      #SKRIV UT
      if (versjonert==TRUE){

        setPath <- paste(globs$path,"/", FriskVDir,aargang,"/csv/", sep = "")

        ## Check path if doesn't exist so create
        if (!fs::dir_exists(setPath)) fs::dir_create(setPath)

        utfiln <- paste0(setPath, FVdscr$INDIKATOR,"_",batchdate,".csv")
        cat("-->> FRISKVIK EKSPORT:",utfiln,"\n")
        data.table::fwrite(FRISKVIK, utfiln, sep = ";", row.names = FALSE)
        

        ## BELOW is the original code
        ## --------------------------
        ## #utfiln<-paste(globs$path,"/",globs$FriskVDir,aargang,"/stata/",indikator,"_",batchdate,".dta",sep="")
        ## utfiln<-paste(globs$path,"/",FriskVDir,aargang,"/csv/",FVdscr$INDIKATOR,"_",batchdate,".csv",sep="")
        ## cat("FRISKVIK EKSPORT:",utfiln,"\n")
        ## write.table(FRISKVIK,file=utfiln,sep=';',row.names = FALSE)
        ## #write.dta(FRISKVIK,file=utfiln)
      }
    } else {
      cat("ADVARSEL!!!!!!!! modus ",modus,"i FRISKVIK støttes ikke\n")
    }
  }
}

FinnSumOverAar<-function(KUBE,per=0,FyllMiss=FALSE,AntYMiss=0,na.rm=FALSE,report_lpsvars=TRUE,globs=FinnGlobs()){
  UT<-KUBE[0,]
  tabs<-setdiff(FinnTabKols(names(KUBE)),c("AARl","AARh"))
  valkols<-FinnValKols(names(KUBE))
  #Utrykk for KH-aggregering (med hjelpestørrelses for snitt)
  if (na.rm==FALSE){
    lpv<-paste(valkols,"=sum(",valkols,",na.rm=",na.rm,"),",
             valkols,".f=max(",valkols,".f),",
             valkols,".a=sum(",valkols,".a*(!is.na(",valkols,") & ",valkols,"!=0))",
             sep="",collapse=",")
  }
  if (na.rm==TRUE){
    lpv<-paste(valkols,"=sum(",valkols,",na.rm=",na.rm,"),",
              valkols,".f=0,",
              valkols,".a=sum(",valkols,".a*(!is.na(",valkols,") & ",valkols,"!=0)),",
              valkols,".fn1=sum(",valkols,".f %in% 1:2),",
              valkols,".fn3=sum(",valkols,".f==3),",
              valkols,".fn9=sum(",valkols,".f==9),",
              valkols,".n=sum(",valkols,".f==0)",
              #valkols,".n=sum(as.numeric(!is.na(",valkols,")))",
              sep="",collapse=",")
    lpsvars<-unlist(lapply(valkols,function(x){paste(x,c(".fn1",".fn3",".fn9",".n"),sep="")}))
    UT[,(lpsvars):=NA_integer_]
  }
 
  aara<-unique(KUBE$AARh)
  if (FyllMiss==TRUE){
    aara<-(min(aara)+per-1):max(aara)
  } else {
    aara<-intersect((min(aara)+per-1):max(aara),aara)
  }
  cat("Finner",per,"-års sum for ")
  for (aar in aara){
    cat(aar," ")
    lp<-paste("list(AARl=",aar-per+1,",AARh=",aar,",",lpv,")",sep="")
    UT<-rbind(UT,KUBE[AARh %in% c((aar-per+1):aar),eval(parse(text=lp)), by=tabs][,names(UT),with=FALSE])
  }
  cat("\n")
  for (valkol in valkols){
    eval(parse(text=paste("UT[",valkol,".f>0,",valkol,":=list(NA)]",sep="")))
  }  
  if (na.rm==TRUE){
    if (AntYMiss<=per){
      for (valkol in valkols){
        eval(parse(text=paste("UT[",valkol,".fn9>",AntYMiss,",c(\"",valkol,"\",\"",valkol,".f\"):=list(NA,9)]",sep="")))
      }  
    }
  }
  f9s<-names(UT)[grepl(".f9$",names(UT))]
  if(length(f9s)>0){UT[,(f9s):=NULL]}
  if (na.rm==TRUE & report_lpsvars==FALSE){
    UT[,(lpsvars):=NULL]
  }
  return(UT)
}





FinnSnittOverAar<-function(KUBE,ma=1,AntYMiss=0,globs=FinnGlobs()){
  KUBEd<-FinnDesign(KUBE)
  if (ma>1){
    PERIODER<-KUBEd$Part$Y
    PERIODER$AARl<-PERIODER$AARh-ma+1
    PERd<-KUBEd
    PERd$Part$Y<-PERIODER
    PERd$OmkDesign<-KUBEd$OmkDesign
    PERd$OmkDesign$AARl<-PERd$OmkDesign$AARh-ma+1
    if (AntYMiss>0){
      globs$DefDesign$IntervallHull[["Y"]]=paste("NTOT-NHAR<=",AntYMiss,sep="")
    }
    RD<-FinnRedesign(KUBEd,PERd,globs=globs)
    maKUBE<-OmkodFil(KUBE,FinnRedesign(KUBEd,PERd,globs=globs),snitt=TRUE,globs=globs)
    valnames<-setdiff(names(maKUBE),globs$DefDesign$DesignKolsFA)
    setnames(maKUBE,valnames,paste("ma",ma,valnames,sep=""))
  } else {
    maKUBE<-KUBE
  }
  return(maKUBE)
}

#Bør nok konsolidere SettPredFilter og SettNaboAnoSpec, bør være greit å gjøre dette
SettPredFilter<-function(refvstr,FGP=list(amin=0,amax=120),globs=FinnGlobs()){
  PredFilter<-list()
  Pcols<-character(0)
  
  #D-develop
  D_develop_predtype <- "IND"
  if (grepl("AAR",refvstr)){
    D_develop_predtype<-"DIR"
  }
  
  #Må utvikles til å lese KUBEdscr$REFVERDI 
  if (is.null(refvstr) || is.na(refvstr)){
    PredFilter=list(Gn=data.frame(GEOniv="L"))
  } else {
    refvstr<-gsub("(.*)ALDER=='*ALLE'*(.*)",paste("\\1","ALDER==",FGP$amin,"_",FGP$amax,"\\2",sep=""),refvstr)
    for (del in names(globs$DefDesign$DelKolN)){
      delN<-globs$DefDesign$DelKolN[del]
      if (globs$DefDesign$DelType[del]=="COL"){
        if (grepl(paste("(^|\\&) *",delN," *== *'*(.*?)'* *(\\&|$)",sep=""),refvstr)){
          Pcols<-c(Pcols,globs$DefDesign$DelKolsF[[del]])
          val<-gsub(paste(".*(^|\\&) *",delN," *== *'*(.*?)'* *(\\&|$).*",sep=""),"\\2",refvstr)
          if (globs$DefDesign$DelFormat[del]=="integer"){
            PredFilter[[del]]<-eval(parse(text=paste("data.frame(",delN,"=",as.integer(val),",stringsAsFactors=FALSE)",sep="")))
          } else {
            PredFilter[[del]]<-eval(parse(text=paste("data.frame(",delN,"=\"",val,"\",stringsAsFactors=FALSE)",sep="")))
          }
        }
      }
      else if (globs$DefDesign$DelType[del]=="INT"){
        if (grepl(paste("(^|\\&) *",delN,"l *== *'*(.*?)'* *($|\\&)",sep=""),refvstr)
            && grepl(paste("(^|\\&) *",delN,"h *== *'*(.*?)'* *($|\\&)",sep=""),refvstr)){
          Pcols<-c(Pcols,globs$DefDesign$DelKolsF[[del]])
          vall<-gsub(paste(".*(^|\\&) *",delN,"l *== *'*(.*?)'* *($|\\&).*",sep=""),"\\2",refvstr)
          valh<-gsub(paste(".*(^|\\&) *",delN,"h *== *'*(.*?)'* *($|\\&).*",sep=""),"\\2",refvstr)
          PredFilter[[del]]<-eval(parse(text=paste("data.frame(",delN,"l=",as.integer(vall),",",delN,"h=",as.integer(valh),",stringsAsFactors=FALSE)",sep="")))
        } else if (grepl(paste("(^|\\&) *",delN,"l{0,1} *== *'*(.*?)'* *($|\\&)",sep=""),refvstr)){
          intval1<-as.integer(gsub(paste("(^|.*\\&) *",delN,"l{0,1} *== *'*(.*?)'* *($|\\&.*)",sep=""),"\\2",refvstr))
          intval<-c(intval1,intval1)
          #Gammelt: kunne ha f.eks. AAR='2012_2014'. Dette blir for dillete mot annen bruk, må da ha "AARl='2012' & AARh='2014'" 
          #intval<-as.integer(unlist(str_split(gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),"\\2",refvstr),"_")))
          #if (length(intval)==1){intval<-c(intval,intval)} 
          
          #Gammelt, feil?
          #val<-gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),"\\2",refvstr)
          #refvstr<-gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),
          #          paste("\\1", paste(delN,"l",sep=""),"==\\2 &"," \\1 ",paste(delN,"l",sep=""),"==\\2"," \\3",sep=""),refvstr)
          #refvstr<-gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),
          #              paste("\\1",paste(delN,"l",sep=""),"==",intval[1]," & ",paste(delN,"h",sep=""),"==",intval[2],"\\3",sep=""),refvstr)
          
          #Litt shaky her. For AAR kan man ikke sette 'AARl=y & AARh=y' fordi det vil krasje med AARs intervall ved snitt
          #Derfor bare 'AARl=y'
          refvstr<-gsub(paste(delN,"=",sep=""),paste(delN,"l=",sep=""),refvstr)
          Pcols<-c(Pcols,globs$DefDesign$DelKolsF[[del]])
          PredFilter[[del]]<-eval(parse(text=paste("data.frame(",delN,"l=",intval[1],",",delN,"h=",intval[2],",stringsAsFactors=FALSE)",sep="")))
        }
      }
      
    }
  }
  return(list(Design=PredFilter,PfiltStr=refvstr,Pkols=Pcols,D_develop_predtype=D_develop_predtype))
}

SettPredFilterGml<-function(refvstr,FGP=list(amin=0,amax=120),globs=FinnGlobs()){
  PredFilter<-list()
  Pcols<-character(0)
  #Må utvikles til å lese KUBEdscr$REFVERDI 
  if (is.null(refvstr) || is.na(refvstr)){
    PredFilter=list(Gn=data.frame(GEOniv="L"))
  } else {
    refvstr<-gsub("(.*)ALDER=='*ALLE'*(.*)",paste("\\1","ALDER==",FGP$amin,"_",FGP$amax,"\\2",sep=""),refvstr)
    for (del in names(globs$DefDesign$DelKolN)){
      delN<-globs$DefDesign$DelKolN[del]
      if (globs$DefDesign$DelType[del]=="COL"){
        if (grepl(paste("(^|\\&) *",delN," *== *'*(.*?)'* *(\\&|$)",sep=""),refvstr)){
          Pcols<-c(Pcols,globs$DefDesign$DelKolsF[[del]])
          val<-gsub(paste(".*(^|\\&) *",delN," *== *'*(.*?)'* *(\\&|$).*",sep=""),"\\2",refvstr)
          if (globs$DefDesign$DelFormat[del]=="integer"){
            PredFilter[[del]]<-eval(parse(text=paste("data.frame(",delN,"=",as.integer(val),",stringsAsFactors=FALSE)",sep="")))
          } else {
            PredFilter[[del]]<-eval(parse(text=paste("data.frame(",delN,"=\"",val,"\",stringsAsFactors=FALSE)",sep="")))
          }
        }
      }
      else if (globs$DefDesign$DelType[del]=="INT"){
        if (grepl(paste("(^|\\&) *",delN,"l *== *'*(.*?)'* *($|\\&)",sep=""),refvstr)
            && grepl(paste("(^|\\&) *",delN,"h *== *'*(.*?)'* *($|\\&)",sep=""),refvstr)){
          Pcols<-c(Pcols,globs$DefDesign$DelKolsF[[del]])
          vall<-gsub(paste(".*(^|\\&) *",delN,"l *== *'*(.*?)'* *($|\\&).*",sep=""),"\\2",refvstr)
          valh<-gsub(paste(".*(^|\\&) *",delN,"h *== *'*(.*?)'* *($|\\&).*",sep=""),"\\2",refvstr)
          PredFilter[[del]]<-eval(parse(text=paste("data.frame(",delN,"l=",as.integer(vall),",",delN,"h=",as.integer(valh),",stringsAsFactors=FALSE)",sep="")))
        } else if (grepl(paste("(^|\\&) *",delN," *== *'*(.*?)'* *($|\\&)",sep=""),refvstr)){
          intval<-as.integer(unlist(str_split(gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),"\\2",refvstr),"_")))
          if (length(intval)==1){intval<-c(intval,intval)} 
          #val<-gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),"\\2",refvstr)
          #refvstr<-gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),
          #          paste("\\1", paste(delN,"l",sep=""),"==\\2 &"," \\1 ",paste(delN,"l",sep=""),"==\\2"," \\3",sep=""),refvstr)
          refvstr<-gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),
                        paste("\\1",paste(delN,"l",sep=""),"==",intval[1]," & ",paste(delN,"h",sep=""),"==",intval[2],"\\3",sep=""),refvstr)
          Pcols<-c(Pcols,globs$DefDesign$DelKolsF[[del]])
          PredFilter[[del]]<-eval(parse(text=paste("data.frame(",delN,"l=",intval[1],",",delN,"h=",intval[2],",stringsAsFactors=FALSE)",sep="")))
        }
      }
      
    }
  }
  return(list(Design=PredFilter,PfiltStr=refvstr,Pkols=Pcols))
}

#Bør nok konsolidere SettPredFilter og SettNaboAnoSpec, bør være greit å gjøre dette
SettNaboAnoSpec<-function(ovkatspec,FGP=list(amin=0,amax=120),globs=FinnGlobs()){
  Foverkat<-list()
  if (!(is.null(ovkatspec) || is.na(ovkatspec))){
    specs<-unlist(str_split(ovkatspec,";"))
    i<-1
    for (spec in specs){
      if (grepl("\\[(.*?)\\]=\\[.*\\]",spec)){
        subcond<-gsub("^\\[(.*?)\\]=\\[.*\\]","\\1",spec)
        subcond<-paste("(",subcond,")",sep="")
        ovkatstr<-gsub("^\\[(.*?)\\]=\\[(.*)\\]","\\2",spec) 
      } else {
        subcond<-"TRUE"
        ovkatstr<-spec
      }
      
      overkat<-list()
      ovkatstr<-gsub("([^=]+)=([^=]+)","\\1==\\2",ovkatstr)
      ovkatstr<-gsub("(.*)ALDER=='*ALLE'*(.*)",paste("\\1","ALDER==",FGP$amin,"_",FGP$amax,"\\2",sep=""),ovkatstr)
      ovkatstr<-gsub("(.*)ALDER=='*(\\d+)_('| )(.*)",paste("\\1","ALDER==\\2_",FGP$amax,"\\3\\4",sep=""),ovkatstr)
      for (del in names(globs$DefDesign$DelKolN)){
        delN<-globs$DefDesign$DelKolN[del]
        if (globs$DefDesign$DelType[del]=="COL"){
          if (grepl(paste("(^|\\&) *",delN," *== *'*(.*?)'* *(\\&|$)",sep=""),ovkatstr)){
            over<-gsub(paste(".*(^|\\&) *(",delN," *== *'*.*?'*) *(\\&|$).*",sep=""),"\\2",ovkatstr)
            overkat[[del]]<-list(over=over,kols=delN)
          }
        } else if (globs$DefDesign$DelType[del]=="INT"){
          if (grepl(paste("(^|\\&) *",delN,"l *== *'*(.*?)'* *($|\\&)",sep=""),ovkatstr)
              && grepl(paste("(^|\\&) *",delN,"h *== *'*(.*?)'* *($|\\&)",sep=""),ovkatstr)){
            overl<-gsub(paste(".*(^|\\&) *(",delN,"l *== *'*.*?)'* *($|\\&).*",sep=""),"\\2",ovkatstr)
            overh<-gsub(paste(".*(^|\\&) *(",delN,"h *== *'*.*?)'* *($|\\&).*",sep=""),"\\2",ovkatstr)
            overkat[[del]]<-list(over=paste(overl,overh,sep=" & "),kols=paste(delN,c("l","h"),sep=""))
          } else if (grepl(paste("(^|\\&) *",delN," *== *'*(.*?)'* *($|\\&)",sep=""),ovkatstr)){
            intval<-unlist(str_split(gsub(paste("(^|.*\\&) *",delN," *== *'*(.*?)'* *($|\\&.*)",sep=""),"\\2",ovkatstr),"_"))
            if (length(intval)==1){intval<-c(intval,intval)} 
            over<-paste(paste(delN,"l",sep=""),"==",intval[1]," & ",paste(delN,"h",sep=""),"==",intval[2],sep="")
            overkat[[del]]<-list(over=over,kols=paste(delN,c("l","h"),sep=""))
          }
        }
      }
      Foverkat[[i]]<-list(subcond=subcond,overkat=overkat)
      i<-i+1
    }
  }
  return(Foverkat)  
}

AnonymiserNaboer<-function(FG,ovkatstr,FGP=list(amin=0,amax=120),D_develop_predtype="IND",globs=FinnGlobs()){
  FG<-copy(FG)
  AoverkSpecs<-SettNaboAnoSpec(ovkatstr,FGP=FGP,globs=globs)
  
  vals<-FinnValKols(names(FG))
  #FinnValKolsF funker ikke riktig!!!! Både pga nye flag slik som fn9 og pga verdikolonner uten .f (MEISskala) etc
  #Må utbedres gjennomgripende, men kan ikke gjøre dette nå derfor bare denne ad hoc løsninga
  if (D_develop_predtype=="IND"){
    alletabs<-setdiff(names(FG),FinnValKolsF(names(FG)))
  } else {
    alletabs<-intersect(c("GEO","GEOniv","FYLKE","AARl","AARh","ALDERl","ALDERh","KJONN","TAB1","TAB2","UTDANN","SIVST","LANDBAK"),names(FG))
  }
  for (ovkSpec in AoverkSpecs){
    FGt<-FG[eval(parse(text=ovkSpec$subcond)),]
    FGr<-FG[!eval(parse(text=ovkSpec$subcond)),]
    overkats<-ovkSpec$overkat
    for (val in vals){
      eval(parse(text=paste(
        "FGt[,",val,".na:=0]",sep=""
      )))
    }
    for (i in 1:length(overkats)){
      kombs<-combn(names(overkats),i)
      for (j in 1:ncol(kombs)){
        substrs<-character(0)
        overtabs<-character(0)
        for (del in kombs[,j]){
          substrs<-c(substrs,overkats[[del]]$over)
          overtabs<-c(overtabs,overkats[[del]]$kols)
        }
        substr<-paste("(",substrs,")",sep="", collapse=" | ")
        for (val in vals){
          bycols<-setdiff(alletabs,overtabs)
          eval(parse(text=paste(
            "FGt[!(",substr,"),",val,".na:=ifelse((",val,".na==1 | any(",val,".f %in% 3:4)),1,0),by=bycols]",sep=""
          )))
        }
        #FG[substr,VAL.na:=ifelse(any(VAL.f==3),1,0),by=setdiff(alletabs,overtabs)]
      }
    }
     
    for (val in vals){
      eval(parse(text=paste(
        "FGt[",val,".na==1,",val,".f:=4]",sep=""
      )))
      eval(parse(text=paste(
        "FGt[",val,".na==1,",val,":=NA]",sep=""
      )))
      eval(parse(text=paste(
        "FGt[,",val,".na:=NULL]",sep=""
      )))
    }
    
    FG<-rbind(FGt,FGr)
  }
  return(FG)
}

RektangulariserKUBE<-function(orgnames,KubeD,vals=list(),batchdate=SettKHBatchDate(),globs=FinnGlobs(),GEOstdAAR=globs$KHaargang){
  delDFstr<-character(0)
  delkolsA<-character(0)
  for (del in names(KubeD)){
    delkols<-globs$DefDesign$DelKols[[del]]
    if (all(delkols %in% orgnames)){
      delkolsA<-c(delkolsA,delkols)
      delDFstr<-c(delDFstr,paste("as.data.frame(KubeD[[\"",del,"\"]])",sep=""))  
    } 
  }
  delerliste<-paste(delDFstr,collapse=",")
  DELER<-data.table(eval(parse(text=paste("expand.grid.df(",delerliste,")",sep=""))))
  DELER<-DELER[,delkolsA,with=FALSE]
  REKT<-data.table()
  #Switch for TYP=="O" ?? 
  for (Gn in KubeD[["Gn"]][["GEOniv"]]){
    GEOK<-subset(globs$GeoKoder,FRA<=GEOstdAAR & TIL>GEOstdAAR & GEOniv==Gn)
    
    #FYLKE
    subfylke<-which(GEOK$GEOniv %in% c("G","S","K","F","B"))
    GEOK$FYLKE<-NA
    GEOK$FYLKE[subfylke]<-substr(GEOK$GEO[subfylke],1,2)
    GEOK$FYLKE[GEOK$GEOniv %in% c("H","L")]<-"00"
    DELERg<-subset(DELER,GEOniv==Gn)
    REKT<-rbind(data.table(expand.grid.df(data.frame(DELERg),data.frame(GEO=GEOK$GEO,FYLKE=GEOK$FYLKE))),REKT)
  }
  return(REKT)
}

LagTNtabell<-function(filer,FilDesL,FGPs,TNPdscr,TT="T",NN="N",Design=NULL,KUBEdscr=NULL,rapport=list(),globs=FinnGlobs()){
  KUBEd<-list()
  
  #Finn initiellt design før evt lesing av KUBEdscr, dette for å kunne godta tomme angivelser der (gir default fra InitDes)
  if (is.null(Design)){
    if (is.na(filer[NN])){
      InitDes<-FilDesL[[filer[TT]]]
    } else {
      FTab<-FinnFellesTab(FilDesL[[filer[TT]]],FilDesL[[filer[NN]]],globs=globs)
      InitDes<-FTab$FDes
      #Må legge til deler som evt bare er i den ene slik at disse blir del av KUBEd
      for (del in setdiff(names(FilDesL[[filer[TT]]]$Part),names(FilDesL[[filer[NN]]]$Part))){
        InitDes$Part[[del]]<-FilDesL[[filer[TT]]]$Part[[del]]
      }
      for (del in setdiff(names(FilDesL[[filer[NN]]]$Part),names(FilDesL[[filer[TT]]]$Part))){
        InitDes$Part[[del]]<-FilDesL[[filer[NN]]]$Part[[del]]
      }
    }
  } else {
    InitDes<-Design
  }
  
  KUBEd<-list()
  if (!is.null(KUBEdscr)){
    KUBEd<-FinnKubeDesignB(KUBEdscr,InitDes,FGP=FGPs[[filer[TT]]],globs=globs)
    TNdes<-list(Part=KUBEd$TMP)
  } else {
    TNdes<-InitDes
  }
  rektangularisert<-0
  geoharmonisert<-1   #Bygg ut til at de ikke trenger være geoharm fra KlargjørFil
  
  
  
  RDT<-FinnRedesign(FilDesL[[filer[TT]]],TNdes,globs=globs)
  if (nrow(RDT$Udekk)>0){
    KHerr("UDEKKA i RDT")
  }
  cat("***Lager TF fra",filer[TT],"\n")
  TF<-OmkodFil(FinnFilT(filer[TT]),RDT,globs=globs,echo=1)
  
  #TF<-GeoHarm(TF,vals=FGPs[[filer[TT]]]$vals,globs=globs) #Trengs ikke om KUBEd, da tas rektisering i 
  if (!is.na(filer[NN])){
    RDN<-FinnRedesign(FilDesL[[filer[NN]]],TNdes,globs=globs)
    if (nrow(RDN$Udekk)>0){
      KHerr("UDEKKA i RDN")
    }
    cat("Lager NF fra",filer[NN],"\n")
    NF<-OmkodFil(FinnFilT(filer[NN]),RDN,globs=globs,echo=1)
    
    
    #NF<-GeoHarm(NF,vals=FGPs[[filer[NN]]]$vals,globs=globs)
  }
  
  #Hvis TN er hoved i KUBE, brukes full rektangularisering
  
  if (length(KUBEd)>0){
    KubeDRekt<-RektangulariserKUBE(names(TF),KUBEd$TMP,globs=globs)
    
    kast<-unique(setdiff(TF$GEO,KubeDRekt$GEO))
    kastTell<-kast[!grepl("99$",kast)]
    if (length(kastTell)>0){
      cat("############################### ADVARSEL!!!!!!!!!!!!!!!!! ####################################################\n")
      cat("GEO ",paste(kastTell,collapse=",")," kastes ved rektangulariseing!!\n")
      cat("Dessuten kastes ",length(setdiff(kast,kastTell)), "99-koder!!\n")
      print(TF[GEO %in% kast])
      cat("##############################################################################################################\n")
    } else if (length(setdiff(kast,kastTell))>0) {
      cat("Kaster ",length(setdiff(kast,kastTell)), "99-koder ved rektangulerisering.\n")
    }
    
    
    setkeyv(KubeDRekt,intersect(names(KubeDRekt),names(TF)))
    setkeyv(TF,intersect(names(KubeDRekt),names(TF)))
    TF<-TF[KubeDRekt]
    cat("REktangularisering TF, dim(KUBEd)", dim(KubeDRekt),"dim(TF)", dim(TF),"\n")
    TF<-SettMergeNAs(TF,FGPs[[filer[TT]]]$vals) 
    
    if (!is.na(filer[NN])){
      setkeyv(KubeDRekt,intersect(names(KubeDRekt),names(NF)))
      setkeyv(NF,intersect(names(KubeDRekt),names(NF)))
      NF<-NF[KubeDRekt]
      cat("REktangularisering NF dim(NF)", dim(NF),"\n")
      NF<-SettMergeNAs(NF,FGPs[[filer[NN]]]$vals)
      setkeyv(TF,intersect(names(TF),names(NF)))
      setkeyv(NF,intersect(names(TF),names(NF)))
      TNF<-TF[NF]
    } else {
      TNF<-TF
    }
    
    
    
    rektangularisert<-1
    cat("--TNF ferdig merget med KUBEd, dim(TNF)",dim(TNF),"\n")
  } else if (!is.na(filer[NN])){
    #DEVELOP HER 20160122
    #kolsT<-unlist(globs$DefDesign$DelKolsF[FilDesL[[filer[TT]]]$OmkDeler])
    #kolsN<-unlist(globs$DefDesign$DelKolsF[FilDesL[[filer[NN]]]$OmkDeler])
    kolsT<-FilDesL[[filer[TT]]]$KolNavn
    kolsN<-FilDesL[[filer[NN]]]$KolNavn
    kols<-intersect(kolsT,kolsN)
    #print(names(FGPs[[filer[TT]]]))
    #kols<-intersect(FinnTabKols(names(filer[TT])),FinnTabKols(names(filer[NN])))
    #print(FilDesL[[filer[TT]]])
    setkeyv(TF,kols)
    setkeyv(NF,kols)
    #print(kols)
    
    #Når KubeD=NULL er det ikke nødvendig å fange implisitt 0 i teller selv om nevner finnes, 
    #derfor bare join TF->NF
    cat("TNF merges TNF<-NF[TF]\n")
    
    TNF<-NF[TF]
    TNF<-SettMergeNAs(TNF,c(FGPs[[filer[TT]]]$vals,FGPs[[filer[NN]]]$vals)) 
    cat("--TNF ferdig merget TNF<-NF[TF] gir dim(TF)",dim(TF),", dim(NF)",dim(NF),", og dim(TNF)",dim(TNF),"\n")   
  } else {
    TNF<-TF
    cat("--TNF ferdig, har ikke nevner, så TNF<-TF\n")
  }
  
  
  
  #Evt prossesering av nye kolonner etter merge/design
  if (!(is.na(TNPdscr$NYEKOL_RAD) | TNPdscr$NYEKOL_RAD=="")){
    FGPtnf<-FGPs[[filer[TT]]]
    FGPtnf$vals<-c(FGPs[[filer[TT]]]$vals,FGPs[[filer[NN]]]$vals)
    TNF<-LeggTilSumFraRader(TNF,TNPdscr$NYEKOL_RAD,FGP=FGPs[[filer[TT]]],globs=globs)
  }
  tabkosl<-FinnTabKols(names(TNF))
  
  if (!(is.na(TNPdscr$NYEKOL_KOL) | TNPdscr$NYEKOL_KOL=="")){
    TNF<-LeggTilNyeVerdiKolonner(TNF,TNPdscr$NYEKOL_KOL,slettInf=TRUE)
  }
  
  dimorg<-dim(TNF)
  TNF<-FiltrerTab(TNF,KUBEd$MAIN,globs=globs)
  if(!identical(dimorg,dim(TNF))){
    cat("Siste filtrering av TNF, hadde dim(TNF)",dimorg, "fik dim(TNF)",dim(TNF),"\n")
  }
  
  
  #Siste felles trinn for alle
  #SETT TELLER OG NEVNER navn
  TNnames<-names(TNF)
  TNnames<-gsub(paste("^",TNPdscr$TELLERKOL,"(\\.f|\\.a|)$",sep=""),"TELLER\\1",TNnames)
  TNnames<-gsub(paste("^",TNPdscr$NEVNERKOL,"(\\.f|\\.a|)$",sep=""),"NEVNER\\1",TNnames)
  #NEVNERKOL=='-' gir TELLER->MALTALL ??
  setnames(TNF,names(TNF),TNnames)
  
  cat("---Ferdig i LagTNtabell\n")
  
  return(list(TNF=TNF,KUBEd=KUBEd))
}


FiltrerTab<-function(FT,KubeD,globs=FinnGlobs()){
  orgkols<-names(FT)
  for (del in names(KubeD)){
    tKOLS<-globs$DefDesign$DelKols[[del]]
    if (all(tKOLS %in% names(FT))){
      KubeD[[del]]<-KubeD[[del]][,tKOLS,with=FALSE]  #Burde være unødvendig, men noen ganger har HAR-kolonner blitt med 
      setkeyv(FT,tKOLS)
      setkeyv(KubeD[[del]],tKOLS)
      FT<-FT[KubeD[[del]],nomatch=0]
    }
  }
  return(FT)
}



#Finn Kubedesign fra parametre i KUBEdscr og orignalt design ORGd
#Essensielt bare en parsing av parametre

FinnKubeDesignB<-function(KUBEdscr,ORGd,FGP=list(amin=0,amax=120),globs=FinnGlobs()){
  KubeD<-list(
    TMP=FinnKubeDesign(KUBEdscr,ORGd,bruk0=TRUE,FGP=FGP,globs=globs),
    MAIN=FinnKubeDesign(KUBEdscr,ORGd,bruk0=FALSE,FGP=FGP,globs=globs)
  )
}

FinnKubeDesign<-function(KUBEdscr,ORGd,bruk0=TRUE,FGP=list(amin=0,amax=120),globs=FinnGlobs()){
  Deler<-list()
  for (del in names(globs$DefDesign$DelKolN)){
  #for (del in names(unlist(globs$DefDesign$DelKolN[ORGd$OmkDeler]))){
    #if (del %in% names(ORGd$Part) | grepl("^T\\d$",del)){  
    if (del %in% names(ORGd$Part)){  
      #Les liste
      koldel<-globs$DefDesign$DelKolN[del]
      koldel0<-paste(koldel,"_0",sep="")
      
      if (bruk0==TRUE && !is.null(KUBEdscr[[koldel0]]) && !is.na(KUBEdscr[[koldel0]]) && KUBEdscr[[koldel0]]!=""){
        delListStr<-KUBEdscr[[koldel0]]
      } else {
        delListStr<-KUBEdscr[[koldel]]
      }
      if (!(is.null(delListStr) || is.na(delListStr) || delListStr=="")){
        minus<-grepl("^-\\[",delListStr)
        delListStr<-gsub("^-\\[(.*)\\]$","\\1",delListStr)
        delListA<-unlist(str_split(delListStr,","))
        if (globs$DefDesign$DelType[del]=="INT"){
          if (del=="A"){
            delListA<-gsub("ALLE",paste(FGP$amin,"_",FGP$amax,sep=""),delListA)
            delListA<-gsub("^_(\\d+)",paste(FGP$amin,"_\\1",sep=""),delListA)
            delListA<-gsub("(\\d+)_$",paste("\\1_",FGP$amax,sep=""),delListA)
          }
          delListA<-gsub("^(\\d+)$","\\1_\\1",delListA)
          delListA<-as.data.table(matrix(as.integer(str_split_fixed(delListA,"_",2)),ncol=2))
        } 
        else if (globs$DefDesign$DelFormat[del]=="integer"){
          delListA<-as.integer(delListA)
        } else if (globs$DefDesign$DelFormat[del]=="numeric"){
          delListA<-as.numeric(delListA)
        } 
        listDT<-setnames(as.data.table(delListA),globs$DefDesign$DelKols[[del]])
        if (minus==TRUE){
          setkeyv(listDT,key(ORGd$Part[[del]]))
          Deler[[del]]<-ORGd$Part[[del]][!listDT,]
        } else {
          Deler[[del]]<-listDT
        }
      } 
      else if (globs$DefDesign$DelType[del]=="INT"){
        delN<-globs$DefDesign$DelKolN[del]
        start<-KUBEdscr[[paste(delN,"_START",sep="")]]
        stopp<-KUBEdscr[[paste(delN,"_STOP",sep="")]]
        if (!(is.null(start) | is.null(stopp))){
          if (stopp>=start){
            Deler[[del]]<-subset(ORGd$Part[[del]],eval(parse(text=paste(delN,"l>=",start," & ",delN,"h<=",stopp,sep=""))))
          }
          else {
            cat("FEIL!!!!!!!! kan ikke ha start ",start,"> stopp ", stopp,"\n")
          }
        }
        else {
          Deler[[del]]<-ORGd$Part[[del]]
        }
      } 
      else {
        Deler[[del]]<-ORGd$Part[[del]]
      }
      
    } 
  }
  return(Deler)
}


FinnFilN<-function(filstr,versjonert=FALSE,batch=NA,globs=FinnGlobs()){
  
  if(!is.na(batch)){
    filn<-paste(globs$path,globs$StablaDirDat,filstr,"_",batch,".rds",sep="")
  } else if(versjonert==TRUE){
    orgwd<-getwd()
    path<-paste(globs$path,"/",globs$StablaDirDat,sep="")
    print(path)
    setwd(path)
    Filer<-unlist(list.files(include.dirs = FALSE))
    Filer<-Filer[grepl(paste("^",filstr,"_",sep=""),Filer)]
    filn<-paste(path,"/",Filer[order(Filer)][length(Filer)],sep="")
    batch<-gsub(".*_(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2})$","\\1",Filer[order(Filer)][length(Filer)])
    setwd(orgwd)
  }
  else {
    filn<-paste(globs$path,globs$StablaDirDat,filstr,"_",batch,".rds",sep="")
  }
  ok<-1
  err<-""
  if(file.access(filn,mode=0)==-1){
    err<-paste("KRITISK FEIL: ",filn," finnes ikke",sep="")
    ok<-0
  } else if (file.access(filn,mode=4)==-1){
    err<-paste("KRITISK FEIL: ",filn," finnes, men lar seg ikke lese",sep="")
    ok<-0
  } 
  return(list(filn=filn,ok=ok,err=err,batch=batch))
}

FinnFil<-function(FILID,versjonert=FALSE,batch=NA,ROLLE="",TYP="STABLAORG",IDKOLS=FALSE,globs=FinnGlobs()){
  FT<-data.frame()
  if (is.na(batch) & exists("BUFFER") && FILID %in% names(BUFFER)){
    FT<-copy(BUFFER[[FILID]])
    cat("Hentet ", ROLLE,"FIL ", FILID," fra BUFFER (",dim(FT)[1]," x ",dim(FT)[2],")\n",sep="")
  } else {
    if(!is.na(batch)){
      filn<-paste(globs$path,"/",globs$StablaDirDat,"/",FILID,"_",batch,".rds",sep="")
    } else if(versjonert==TRUE){
      orgwd<-getwd()
      path<-paste(globs$path,"/",globs$StablaDirDat,sep="")
      setwd(path)
      Filer<-unlist(list.files(include.dirs = FALSE))
      Filer<-Filer[grepl(paste("^",FILID,"_(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}).rds$",sep=""),Filer)]
      if (length(Filer)>0){
        filn<-paste(path,"/",Filer[order(Filer)][length(Filer)],sep="")
        batch<-gsub(".*_(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}).rds$","\\1",Filer[order(Filer)][length(Filer)])
      } else {
        filn<-paste(path,"/",FILID,".rds",sep="")
      }
      setwd(orgwd)
    }
    else {
      filn<-paste(globs$path,"/",globs$StablaDirNy,"/",FILID,".rds",sep="")
      print(filn)
    }
    if(file.access(filn,mode=0)==-1){
      cat("KRITISK FEIL: ",filn," finnes ikke\n")
    } else if (file.access(filn,mode=4)==-1){
      cat("KRITISK FEIL: ",filn," finnes, men lar seg ikke lese\n")
    } else {
      FT<-readRDS_KH(filn,IDKOLS=IDKOLS)
      cat("Lest inn ", ROLLE,"FIL ", FILID," (",dim(FT)[1]," x ",dim(FT)[2],"), batch=",batch,"\n",sep="")
    }
  }
  return(list(FT=as.data.table(FT),batch=batch))
}

FinnFilT<-function(...){
  return(FinnFil(...)$FT)
}


FinnKubeT<-function(fila,batch=NA,globs=FinnGlobs()){
  if(is.na(batch)){
    filn<-paste(globs$path,"/",globs$KubeDirNy,fila,".rds",sep="")  
  } else {
    filn<-paste(globs$path,"/",globs$KubeDirDat,fila,"_",batch,".rds",sep="")
  }
  KUBE<-data.table()
  if(file.access(filn,mode=0)==-1){
    cat("KRITISK FEIL: ",filn," finnes ikke\n")
  } else if (file.access(filn,mode=4)==-1){
    cat("KRITISK FEIL: ",filn," finnes, men lar seg ikke lese\n")
  } else {
    KUBE<-readRDS(filn)
  }
  return(KUBE)
}


GeoHarm<-function(FIL,vals=list(),rektiser=TRUE,FDesign=list(),batchdate=SettKHBatchDate(),globs=FinnGlobs(),GEOstdAAR=globs$KHaargang){
  if (identical(class(FIL),"data.frame")){FIL<-data.table(FIL)}
  keyorg<-key(FIL)
  geoomk<-globs$KnrHarm
  FIL$GEO<-mapvalues(FIL$GEO,geoomk$GEO,geoomk$GEO_omk,warn_missing = FALSE)
  FIL[,FYLKE:=NULL]
  FIL<-KHaggreger(FIL,vals=vals,globs=globs)
  #Rektangulariser
  if (rektiser==TRUE){
    REKT<-data.table()
    if(length(FDesign)==0){
      FDesign<-FinnDesign(FIL)
    }
    FDes<-FDesign$Design
    #Switch for TYP=="O" ?? 
    for (Gn in FDesign$Part[["Gn"]][["GEOniv"]]){
      GEOK<-subset(globs$GeoKoder,FRA<=GEOstdAAR & TIL>GEOstdAAR & GEOniv==Gn)$GEO
      FDesG<-FDes[HAR==1 & GEOniv==Gn,intersect(names(FIL),names(FDes)),with=FALSE]
      REKT<-rbind(data.table(expand.grid.df(data.frame(FDesG),data.frame(GEO=GEOK))),REKT)
    } 
    setkeyv(REKT,names(REKT))
    setkeyv(FIL,names(REKT))
    FIL<-FIL[REKT]
    FIL<-SettMergeNAs(FIL,vals=vals)
  }
  
  
  FIL[,FYLKE:=ifelse(GEOniv %in% c("H","L"),"00",substr(GEO,1,2))]
  return(FIL)
}



KHaggreger<-function(FIL,vals=list(),snitt=FALSE,globs=FinnGlobs()){
  orgclass<-class(FIL)
  orgcols<-names(FIL)
  if (identical(orgclass,"data.frame")){FIL<-data.table(FIL)}
  orgkeys<-key(FIL)
  tabnames<-globs$DefDesign$DesignKolsFA[globs$DefDesign$DesignKolsFA %in% names(FIL)]
  #tabnames<-names(FIL)[!grepl("^VAL\\d+(f|)$",names(FIL))]
  valkols<-names(FIL)[!names(FIL) %in% tabnames]
  valkols<-valkols[!grepl("\\.(f|a)",valkols)]
  valkols<-valkols[!valkols %in% c("KOBLID","ROW")]
  setkeym(FIL,tabnames)  #Sjekk om key ok for å effektivisere?
  
  if (snitt==FALSE){
    lp<-paste("list(",
              paste(valkols,"=sum(",valkols,"),",
                    valkols,".f=max(",valkols,".f),",
                    valkols,".a=sum(",valkols,".a*(!is.na(",valkols,") & ",valkols,"!=0))",
                    sep="",collapse=","),
              ")",sep="")
    FILa<-FIL[, eval(parse(text=lp)), by=tabnames]
  } else {
    #Sett også hjelpestørrelser for vurdering av snitt
    lp<-paste("list(",
              paste(valkols,"=sum(",valkols,",na.rm=TRUE),",
                    valkols,".f=max(",valkols,".f),",
                    valkols,".a=sum(",valkols,".a*(!is.na(",valkols,") & ",valkols,"!=0)),",
                    valkols,".fn1=sum(",valkols,".f==1),",
                    valkols,".fn3=sum(",valkols,".f>1),",
                    valkols,".n=.N",
                    sep="",collapse=","),
              ")",sep="")
    FILa<-FIL[, eval(parse(text=lp)), by=tabnames]
    #Anonymiser, trinn 1
    #Filtrer snitt som ikke skal brukes pga for mye anonymt
    anon_tot_tol<-0.2
    lp<-paste("FILa[,':='(",
              paste(valkols,"=ifelse(",valkols,".fn3/",valkols,".n>=",anon_tot_tol,",NA,",valkols,"),",
                    valkols,".f=ifelse(",valkols,".fn3/",valkols,".n>=",anon_tot_tol,",3,",valkols,".f)",
                    sep="",collapse=","),
              ")]",sep="")
    eval(parse(text=lp))            
    
    FILa<-FILa[,c(orgcols,paste(valkols,".n",sep="")),with=FALSE]
  }
  vals<-vals[valkols]
  usumbar<-valkols[unlist(lapply(vals[valkols],function(x){x$sumbar==0}))]
  for (val in valkols){
    if (!is.null(vals[[val]]) && vals[[val]]$sumbar==0){
      eval(parse(text=paste(
        "FILa[",val,".a>1,c(\"",val,"\",\"",val,".f\"):=list(NA,2)]",sep="")))
    }
  }
  setkeym(FIL,orgkeys)
  if (identical(orgclass,"data.frame")){FIL<-data.frame(FIL)}
  return(FILa)
}
  
KHaggregerM<-function(FILn="FG",orgcols,vals=list(),snitt=FALSE,globs=FinnGlobs()){
    
    
    sumexp<-list()
    
    sumexp$tabnames<-globs$DefDesign$DesignKolsFA[globs$DefDesign$DesignKolsFA %in% orgcols]
    #tabnames<-names(FIL)[!grepl("^VAL\\d+(f|)$",names(FIL))]
    valkols<-names(FIL)[!orgcols %in% tabnames]
    valkols<-valkols[!grepl("\\.(f|a)",valkols)]
    valkols<-valkols[!valkols %in% c("KOBLID","ROW")]
    setkeym(FIL,tabnames)  #Sjekk om key ok for å effektivisere?
    if (snitt==FALSE){
      sumexp$tr1<-paste("list(",
                paste(valkols,"=sum(",valkols,"),",
                      valkols,".f=max(",valkols,".f),",
                      valkols,".a=sum(",valkols,".a*(!is.na(",valkols,") & ",valkols,"!=0))",
                      sep="",collapse=","),
                ")",sep="")
      #FILa<-FIL[, eval(parse(text=lp)), by=tabnames]
    } 
    else {
      #Sett også hjelpestørrelser to vurdering av snitt
      sumexp$tr1<-paste("list(",
                paste(valkols,"=sum(",valkols,",na.rm=TRUE),",
                      valkols,".f=max(",valkols,".f),",
                      valkols,".a=sum(",valkols,".a*(!is.na(",valkols,") & ",valkols,"!=0)),",
                      valkols,".fn1=sum(",valkols,".f==1),",
                      valkols,".fn3=sum(",valkols,".f>1),",
                      valkols,".n=.N",
                      sep="",collapse=","),
                ")",sep="")
      #FILa<-FIL[, eval(parse(text=lp)), by=tabnames]
      #Anonymiser, trinn 1
      #Filtrer snitt som ikke skal brukes pga for mye anonymt
      anon_tot_tol<-0.2
      sumexp$anon<-paste(FGn,"[,':='(",
                paste(valkols,"=ifelse(",valkols,".fn3/",valkols,".n>=",anon_tot_tol,",NA,",valkols,"),",
                      valkols,".f=ifelse(",valkols,".fn3/",valkols,".n>=",anon_tot_tol,",3,",valkols,".f)",
                      sep="",collapse=","),
                ")]",sep="")
      #eval(parse(text=lp))            
      #FILa<-FILa[,orgcols,with=FALSE]
    }
  
    return(sumexp)
  
  vals<-vals[valkols]
  usumbar<-valkols[unlist(lapply(vals[valkols],function(x){x$sumbar==0}))]
  for (val in valkols){
    if (!is.null(vals[[val]]) && vals[[val]]$sumbar==0){
      eval(parse(text=paste(
        "FILa[",val,".a>1,c(\"",val,"\",\"",val,".f\"):=list(NA,2)]",sep="")))
    }
  }
  setkeym(FIL,orgkeys)
  if (identical(orgclass,"data.frame")){FIL<-data.frame(FIL)}
  return(FILa)
}

SySammenFiler<-function(FILID1,FILID2,batch1=NA,batch2=NA,ROLLE1="",ROLLE2="",globs=FinnGlobs()){
  return(SySammenTabeller(FinnFilT(FILID1,batch=batch1,ROLLE=ROLLE1,globs=globs),
                       FinnFilT(FILID2,batch=batch2,ROLLE=ROLLE2,globs=globs)))  
}

LeggDelTilDesign<-function(OrgDes,NyDel,globs=FinnGlobs()){
  OrgDes$Part<-c(OrgDes$Part,NyDel)
  #OrgDes$OmkDesign<-
  #Kryss NyDeler med gamle, HAR er OK
  delerlist<-paste("as.data.frame(NyDel[[\"",names(NyDel),"\"]])",sep="",collapse=",")
  OrgDes$OmkDesign<-data.table(eval(parse(text=paste("expand.grid.df(as.data.frame(OrgDes$OmkDesign),",delerlist,")",sep=""))))
  setkeym(OrgDes$OmkDesign,globs$DefDesign$DesignKols[globs$DefDesign$DesignKols %in% names(OrgDes$OmkDesign)])
  return(OrgDes)
}

FinnFellesTab<-function(DF1,DF2,globs=FinnGlobs(),echo=0){
  #Diff<-union(setdiff(names(DF1$Part),names(DF2$Part)),setdiff(names(DF2$Part),names(DF1$Part)))
  cat("Starter i FinnFellesTab.")
  FTabs<-list()
  for (del in intersect(names(DF1$Part),names(DF2$Part))){
    FTabs[[del]]<-unique(rbind(DF1$Part[[del]],DF2$Part[[del]]))
  }
  RD1<-FinnRedesign(DF1,list(Parts=FTabs))
  RD2<-FinnRedesign(DF2,list(Parts=FTabs))
  omktabs<-names(RD1$FULL)[grepl("_omk$",names(RD1$FULL))]
  setkeyv(RD1$FULL,omktabs)
  setkeyv(RD2$FULL,omktabs)
  Dekk1<-unique(RD1$FULL[,omktabs,with=FALSE])
  Dekk2<-unique(RD2$FULL[,omktabs,with=FALSE])
  Dekk12<-Dekk1[Dekk2,nomatch=0]
  setnames(Dekk12,names(Dekk12),gsub("_omk$","",names(Dekk12)))
  FDes<-FinnDesign(Dekk12)
  cat(" Ferdig i FinnFellesTab\n")
  return(list(Dekk=Dekk12,FDes=FDes))
}


SySammenTabeller<-function(F1,F2,SJEF=0,FGP1=list(amin=0,amax=120),FGP2=list(amin=0,amax=120),SkalAggregeresOpp1=character(),SkalAggregeresOpp2=character(),globs=FinnGlobs(),etabmatchOK=TRUE,rapport=list(),FullResult=FALSE,echo=FALSE){
  ok<-1
  if (identical(class(F1),"data.frame")){F1<-data.table(F1)}
  if (identical(class(F2),"data.frame")){F2<-data.table(F2)}
  orgkey1<-key(F1)
  orgkey2<-key(F2)
  
  #MÅ FIKSE EVT KOLLISJON AV FELTNAVN!
  
  
  FU<-data.table()
    
  etabs1<-names(F1)[grepl("^TAB\\d+$",names(F1))]
  etabs2<-names(F2)[grepl("^TAB\\d+$",names(F2))] 
  
  atabs<-globs$DefDesign$DesignKolsFA
  atabs1<-atabs[atabs %in% names(F1)]
  atabs2<-atabs[atabs %in% names(F2)]
  atabs<-intersect(atabs1,atabs2)
  
  if (etabmatchOK==FALSE && length(etabs1)>0 && length(etabs2)>0){
    ok<-0
    print("FEIL!!!!! Noe tull med etabmatchOK")  #Usikker på hvorfor jeg har satt denne muligheten, bruker den ikke
  } else {
    DF1<-FinnDesign(F1,FGP=FGP1)
    DF2<-FinnDesign(F2,FGP=FGP2)
   
    if (SJEF==0){
      DFF<-FinnFellesTab(DF1,DF2,SkalAggregeresOpp1=SkalAggregeresOpp1,SkalAggregeresOpp2=SkalAggregeresOpp2,globs=globs)
      #Er dette riktig mht Dekk??
      rapport["TRINN"]<-rapport["F1"]
      rapport["KALL"]<-"SySammenFil-0"
      F1<-OmkodFil(F1,DFF$KB12,rapport=rapport,globs=globs)
      rapport["TRINN"]<-rapport["F2"]
      F2<-OmkodFil(F2,DFF$KB21,rapport=rapport,globs=globs) 
      setkeym(F1,atabs)
      setkeym(F2,atabs)
      FU<-merge(F1,F2,all=TRUE,allow.cartesian=TRUE)
    } else if (SJEF==1){
      RD21<-FinnRedesign(DF2,DF1,SkalAggregeresOpp=SkalAggregeresOpp2,globs=globs)
      rapport["KALL"]<-"SySammenFil-1"
      F2<-OmkodFil(F2,RD21,rapport=rapport,globs=globs)
      setkeym(F1,atabs)
      setkeym(F2,atabs)
      print("Tung merge")
      FU<-F2[F1,allow.cartesian=TRUE]
      print("OVER")
    }
   #RAPPORTER INNSETTING???
    FU<-SettMergeNAs(FU,c(FGP1$vals,FGP2$vals))  
    
    if (echo==TRUE){
      print(F1)
      print(F2)
      prit(FU)
    }   
  }
  setkeym(F1,orgkey1)
  setkeym(F2,orgkey2)      
  if (FullResult==TRUE){
    return(list(SF=FU,F1=F1,F2=F2))
  } else {
    return(FU)
  }
}

SettMergeNAs<-function(FT,valsdef=list()){
  vals<-gsub("^(.*)\\.f$","\\1",names(FT)[grepl("^(.*)\\.f$",names(FT))])
  
  for (ValK in vals){
    if (ValK %in% names(vals)){
      if (valsdef[[ValK]][["miss"]]==".."){
        valt<-c(0,1,1)
      } else if (valsdef[[ValK]][["miss"]]=="."){
        valt<-c(0,2,1)
      } else if (valsdef[[ValK]][["miss"]]==":"){
        valt<-c(0,3,1)
      } else if (!is.na(as.numeric(valsdef[[ValK]][["miss"]],warn_missing = FALSE))){
        valt<-c(as.numeric(valsdef[[ValK]][["miss"]],warn_missing = FALSE),0,1)
      }
    } else {
      valt<-c(0,0,1)  #Default er (implisitt 0). Merk at valsdef er tom bare for avledete kolonner, her er 0 naturlig default
    }
    #miss<-eval(parse(text=paste(
    #  "nrow(FT[is.na(",ValK,") & (",ValK,".f==0 | is.na(",ValK,".f)),])",sep=""
    #)))
    miss<-eval(parse(text=paste(
      "FT[is.na(",ValK,") & (",ValK,".f==0 | is.na(",ValK,".f)),]",sep=""
    )))
    if (nrow(miss)>0){
      cat("SettMergeNAs, setter inn",nrow(miss),"default for",ValK,"\n")
    }
    
    eval(parse(text=paste(
        "FT[is.na(",ValK,") & (",ValK,".f==0 | is.na(",ValK,".f)),c(\"",ValK,"\",\"",ValK,".f\",\"",ValK,".a\"):=list(",paste(valt,collapse=","),")]",sep=""
    )))
  }
  return(FT)  
}

EkstraherRadSummer<-function(FIL,pstrorg,FGP=list(amin=0,amax=120),globs=FinnGlobs()){
  setDT(FIL)
  amin<-FGP$amin
  amax<-FGP$amax
  
  #Modifiser syntaks pstrorg
  ##########################
  
  #Rydd i "feil syntaks"
  #NB: takler ikke "|" (or) i pstrorg
  
  pstrorg<-gsub("(?<!=)=(?!=)","==",pstrorg,perl=TRUE)
  pstrorg<-gsub(" *== *(?=c\\()"," %in% ",pstrorg,perl=TRUE)
  
  #Standard "alle"-verdier
  pstrorg<-gsub("(^ *|& *)ALDER( *&| *$)","ALDER==\\1\"ALLE\"\\2",pstrorg)
  pstrorg<-gsub("(^ *|& *)(KJONN|UTD|LAND|SIVST)( *&| *$)","\\1\\2==0\\3",pstrorg)
  
  #Intervaller
  #Er det mulig å abstrahere her, dvs å ta alle "INT"-deler med samme syntaks???
  pstrorg<-gsub("ALDER *(={1,2}) *\"*ALLE\"*",paste("ALDERl==",amin," & ALDERh==",amax,sep=""),pstrorg)
  pstrorg<-gsub("ALDER *(={1,2}) *(\\d+)$","ALDERl==\\2 & ALDERh==\\2",pstrorg)
  pstrorg<-gsub("AAR *(={1,2}) *(\\d+)$","AARl==\\2 & AARh==\\2",pstrorg)
  
  #Klipp opp pstrorg
  ##########################
  
  
  #Finn kolonner involvert i pstrorg
  alletabs<-str_replace(unlist(str_split(pstrorg," *& *")),"^(\\w*?) *(%in%.*|==.*| *$)","\\1")
  
  #Fjern de som ikke er del av subset betingelse
  #subsetstr<-gsub("(?<=[&^]) *\\w+ *(?=[&$])","",pstrorg,perl=TRUE)
  #subsetstr<-gsub("&+","&",subsetstr)
  subsetstr<-gsub("^ *\\w+ *(&|$)|(^|&) *\\w+ *$","",pstrorg,perl=TRUE)
  subsetstr<-gsub("& *\\w+ *&","&",subsetstr,perl=TRUE)
  
  
  #Splitt i kolonnenavn og verdi
  subtabs<-str_replace(unlist(str_split(subsetstr," *& *")),"^(\\w+) *(%in%.*|==.*)","\\1")
  subvals<-str_replace(unlist(str_split(subsetstr," *& *")),"^.*(%in% *|== *)(\\w+)","\\2")
  subvals<-setNames(subvals,subtabs)
  #Filtrer til de som er aktuelle for omkoding  
  subvals<-subvals[names(subvals) %in% globs$DefDesign$DesignKols]
  
  #Omkod disse
  if (length(subvals)>0){
    #For omkodbare kolonner må disse omkodes til søkte verdier (for generalitet må det omkodes selv om disse finnes)
    OmkParts<-list()
    for (del in names(globs$DefDesign$DelKols)){
      if (all (globs$DefDesign$DelKols[[del]] %in% names(subvals))){
        dvals<-subvals[globs$DefDesign$DelKols[[del]]]
        if (KHglobs$DefDesign$DelFormat[[del]]=="integer"){
          dvals<-setNames(as.integer(dvals),names(dvals))   
        }
        OmkParts[[del]]<-setNames(data.frame(matrix(dvals,ncol=length(dvals))),names(dvals))
      } else if (any (globs$DefDesign$DelKols[[del]] %in% names(subvals))){
        print("VARSKU HER!!!!!!!!!!!!!!! FEIL i EkstraherRadSummer!") 
      }
    }
    #omk[,]<-as.numeric(omk[,])
    #omkD<-FinnDesign(omk,amin=amin,amax=amax,globs=globs)
    print("Til OmkodFil fra EkstraherRadSummer, dette kan fort gi udekt ved ubalansert design. Dette faller bort igjen ved NF[TNF")
    FIL<-OmkodFil(FIL,FinnRedesign(FinnDesign(FIL),list(Parts=OmkParts)),globs=globs,echo=1)
    
  }
  if (subsetstr!=""){
    FIL<-eval(parse(text=paste("subset(FIL,",subsetstr,")",sep="")))
  }
  #cat("ALLEtabs: ",alletabs," names(FIL): ",names(FIL), "SETT: ",names(FIL)[!names(FIL) %in% alletabs],"\n")
  #print(alletabs)
  #print(head(FIL[,names(FIL)[!names(FIL) %in% alletabs],with=FALSE]))
  FIL<-KHaggreger(FIL[,!names(FIL) %in% alletabs,with=FALSE],globs=globs)
  return(FIL)  
}


LeggTilSumFraRader<-function(TNF,NYdscr,FGP=list(amin=0,amax=120),globs=FinnGlobs()){
  if (!(is.na(NYdscr) | NYdscr=="")){
    for (sumfra in unlist(str_split(NYdscr,";"))){
      #cat("SUMFRA: ",sumfra,"\n")
      if (grepl("^ *(.+?) *= *(.+?)\\{(.*)\\} *$",sumfra)){
        nycol<-gsub("^ *(.+?) *= *(.+?)\\{(.*)\\} *$","\\1",sumfra)
        gmlcol<-gsub("^ *(.+?) *= *(.+?)\\{(.*)\\} *$","\\2",sumfra)
        expr<-gsub("^ *(.+?) *= *(.+?)\\{(.*)\\} *$","\\3",sumfra)
        #cat("nycol:",nycol,"gmlcol:",gmlcol,"expr:",expr,"\n")
        NF<-EkstraherRadSummer(TNF,expr,FGP=FGP,globs=globs)
        gmlcols<-paste(gmlcol,c("",".f",".a"),sep="")
        nycols<-paste(nycol,c("",".f",".a"),sep="")
        setnames(NF,gmlcols,nycols)
        #print(NF)
        #Sy sammen
        commontabs<-globs$DefDesign$DesignKolsFA[globs$DefDesign$DesignKolsFA %in% names(NF)]
        
        
        #Er usikker på om hva som egentlig er best her.
        #Siden OmkodFraPart brukt i EkstraherRadSummer gir full rektulangusering kan man ha satt
        #deler i NF som er udekket i TNF. 1) Disse ønskes vel egentlig ikke med
        #men motsatt, 2) dersom TNF ha manglende GEO-koder som finnes i NF er det kanskje ønskelig å ha disse med
        #Jeg velger å sette venstre join TNF->NF slik at problem 1 faller bort
        #Så lenge herværende prosedyre bare kjøres etter at TNF er rektangularisert mht GEO faller også 2) bort
        #Dette gjelder i standard produskjonsløype (LagTnTabell, LagKUBE etc)
        
        setkeym(TNF,commontabs)
        setkeym(NF,commontabs)
        dimorg<-dim(TNF)
        TNF<-NF[,c(commontabs,nycols),with=FALSE][TNF]
        cat("LeggTilSumFraRader. Før er dim(TNF)",dimorg,"og dim(NF)",dim(NF),"etter er dim(TNF)",dim(TNF),"\n")
        #altså ikke
        #TNF<-merge(TNF,NF[,c(commontabs,nycols),with=FALSE],all=TRUE,by=commontabs)
        
        #TNF<-merge(TNF,NF[,c(commontabs,nycols),with=FALSE],all=TRUE,by=commontabs)
        TNF<-SettMergeNAs(TNF,list(gmlcol=FGP$vals,nycol=FGP$vals[gmlcol]))
        
        #print(TNF)
      } else {
        cat("FEIL!!!!!: NYEKOL_RAD har feil format:",NYdscr,"\n")
      }
    }
  }
  
  
  return(TNF)
}



AggregerRader<-function(FG,nyeexpr,FGP){
  if (!(is.na(nyeexpr) || nyeexpr=="")){
    nytabs<-unlist(str_split(nyeexpr,";"))
    for (nytab in nytabs){
      #PARSING av syntaks
      nylab<-gsub("^\\[(.*?)\\]=.*","\\1",nytab)
      subexp<-gsub(".*\\]=\\{(.*)\\}","\\1",nytab)
      if (grepl("%in%|==",subexp)){
        tab<-gsub("^ *(.*) *(%in%|==).*","\\1",subexp)
      } else {
        tab<-gsub("^ *(.*) *$","\\1",subexp)
        subexp<-TRUE
      }
      if (!tab %in% names(FG)){
        tabE<-names(FGP)[which(FGP==tab)]
        subexp<-gsub("^ *tab(.*)",paste(tabE,"\\1",sep=""),subexp)
        tab<-tabE
      }
      FG2<-eval(parse(text=paste("subset(FG,",subexp,")",sep="")))
      FG2<-KHaggreger(FG2[,setdiff(names(FG),tab),with=FALSE])
      FG2[,eval(parse(text=paste(tab,":='",nylab,"'",sep="")))]
      FG<-rbind(FG,FG2[,names(FG),with=FALSE])
    }
  }
  return(FG)
}

LeggTilNyeVerdiKolonner<-function(TNF,NYEdscr,slettInf=TRUE,postMA=FALSE){
  TNF<-copy(TNF) #Får uønsket warning om self.reference under om ikke gjør slik
  setDT(TNF)
  valKols<-gsub("^(.+)\\.f$","\\1",names(TNF)[grepl(".+\\.f$",names(TNF))])
  #FinnValKols(names(TNF))
  if (!(is.na(NYEdscr) | NYEdscr=="")){
    for (nycolexpr in unlist(str_split(NYEdscr,";"))){
      nycol<-gsub("^(.*?)=(.*)$","\\1",nycolexpr)
      expr<-gsub("^(.*?)=(.*)$","\\2",nycolexpr)
      invKols<-valKols[sapply(valKols,FUN=function(x){grepl(x,expr)})]
      eval(parse(text=paste(
      "TNF[,c(\"",paste(nycol,c("",".f",".a"),collapse="\",\"",sep=""),"\")
      :=list(",expr,",pmax(",paste(invKols,".f",collapse=",",sep=""),"),
                      pmax(",paste(invKols,".a",collapse=",",sep=""),"))]",sep=""        
      )))
      if (postMA==TRUE){
        eval(parse(text=paste(
          "TNF[,c(\"",paste(nycol,c(".n",".fn1",".fn3",".fn9"),collapse="\",\"",sep=""),"\")
        :=list(1,0,0,0)]",sep=""        
        )))
      }
      if (slettInf==TRUE){
        eval(parse(text=paste("suppressWarnings(",
          "TNF[",nycol,"%in% c(Inf,NaN,NA),c(\"",paste(nycol,c("",".f"),collapse="\",\"",sep=""),"\"):=list(NA,2)])",sep=""        
        )))
      }
    }
  }
  return(TNF)
}

#
OmkodFil<-function(FIL,RD,globs=FinnGlobs(),echo=0){
  orgkols<-names(FIL)
  setDT(FIL)
  tabnames<-FinnTabKols(names(FIL))
  valkols<-FinnValKols(names(FIL))  
  lp<-paste(valkols,"=sum(",valkols,"),",
            valkols,".f=max(",valkols,".f),",
            valkols,".a=sum(",valkols,".a*(!is.na(",valkols,") & ",valkols,"!=0))",
            sep="",collapse=",")
  
  if (nrow(RD$FULL)>0){
    for (del in names(RD$Filters)){
      setkeyv(FIL,names(RD$Filters[[del]]))
      setkeyv(RD$Filters[[del]],names(RD$Filters[[del]]))
      if (echo==1){cat("Filtrerer",del,"før dim(FIL)=",dim(FIL))}
      if (any(duplicated(RD$Filters[[del]]))){
        print("CARTESIAN????")
        print(RD$Filters[[del]])
        print(RD$Filters[[del]][duplicated(RD$Filters[[del]]),])
      }
      FIL<-FIL[RD$Filters[[del]],nomatch=0]
      if (echo==1){cat(" og etter",dim(FIL),"\n")}      
    }
    
    #NB! Rekkefølge er essensiell, dvs at ubeting kommer til slutt
    beting<-intersect(globs$DefDesign$AggPri[length(globs$DefDesign$AggPri):1],c(globs$DefDesign$BetingOmk,globs$DefDesign$BetingF))
    ubeting<-intersect(globs$DefDesign$AggPri[length(globs$DefDesign$AggPri):1],c(globs$DefDesign$UBeting))
    
    
    
    for (del in intersect(c(beting,ubeting),names(RD$KBs))){
      orgtabs<-names(RD$KBs[[del]])[!grepl("_omk$",names(RD$KBs[[del]]))]
      omktabs<-names(RD$KBs[[del]])[grepl("_omk$",names(RD$KBs[[del]]))]
      bycols<-c(setdiff(tabnames,gsub("_omk","",omktabs)),omktabs)
      
      #Sjekk type omkoding som trengs. 
      #Dersom hver orgkode skal til mange omkkoder 
      #er det uheldig å merge FIL[KB] om FIL er stor siden det lages mange kopier av orglinjer i FIL
      #i slike tilfeller kobles i stedet inn en loop over omkoding til hver omktab  (jfr laging av tiårssnitt i KREFT)
      
      setkeyv(RD$KBs[[del]],orgtabs)
      replikfaktor<-RD$KBs[[del]][,list(N=.N),by=orgtabs][,mean(N)]
      setkeyv(FIL,orgtabs)
      if (echo==1){cat("Omkoder", del, "dim(FIL) er ",dim(FIL), "originalt")}
      if (nrow(FIL)<1000000 | replikfaktor<4 | del=="Gn"){
        FIL<-FIL[RD$KBs[[del]],nomatch=0,allow.cartesian=TRUE]
        #FIL<-FIL[RD$KBs[[del]],nomatch=0]
        if (echo==1){cat(" og",dim(FIL),"etter merge")}
        if (del=="Gn"){
          #Omkod geo
          FIL[GEOniv_omk=="K",GEO:=substr(GEO,0,4)]
          FIL[GEOniv_omk=="F",GEO:=FYLKE]
          FIL[GEOniv_omk=="L",c("GEO","FYLKE"):=list("0","00")]
          #FIL[GEOniv_omk=="B" & GEOniv=="S" & grepl("^(0301|1103|1201|1601)",GEO),c("GEO","FYLKE"):=list(substr(GEO,0,6),substr(GEO,0,2))]
          #FIL[GEOniv_omk=="B" & GEOniv=="S" & !grepl("^(0301|1103|1201|1601)",GEO),c("GEO","FYLKE"):=list("999999","99")]
          FIL[GEOniv_omk=="B" & GEOniv=="S" & !grepl("^(0301|1103|1201|1601|4601|5001)",GEO),c("GEO","FYLKE"):=list("999999","99")]
          #Dette er dårlig, bør endre til
          #FIL[GEOniv_omk=="B" & GEOniv=="S" & !GEO %in% globs$GeoKoder[GEOniv=="B"]$GEO,c("GEO","FYLKE"):=list("999999","99")]
          FIL[GEOniv_omk=="H" & GEOniv!="H",GEO:=mapvalues(FYLKE,globs$HELSEREG$FYLKE,globs$HELSEREG$HELSEREG,warn_missing = FALSE)] 
          #FIL[GEOniv_omk=="H" & GEOniv!="H",FYLKE:="00"]
          FIL[GEOniv_omk=="H",FYLKE:="00"]
        }
        setkeyv(FIL,bycols)
        lpl<-paste("list(",lp,")",sep="")
        FIL<-FIL[, eval(parse(text=lpl)), by=bycols]
        #Dette skulle vel vært bedre, men blir alt for tregt? når ikke bycols er key
        #FIL<-FIL[RD$KBs[[del]],nomatch=0,allow.cartesian=TRUE][, eval(parse(text=lp)), by=bycols]
      } else {
        KB<-copy(RD$KBs[[del]])
        setkeyv(KB,omktabs)
        OMKs<-unique(KB[,omktabs,with=FALSE])
        FILt<-FIL[0,]
        for (i in 1:nrow(OMKs)){
          OMK<-OMKs[i,]
          print(OMK)
          KBt<-KB[OMK]
          setkeyv(KBt,orgtabs)
          FILd<-FIL[KBt,nomatch=0,allow.cartesian=TRUE]
          setkeyv(FILd,bycols)
          lpt<-paste("list(",paste(gsub("_omk$","",names(OMK)),OMK,sep="=",collapse=","),",",lp,")",sep="")
          FILt<-rbind(FILt,FILd[,eval(parse(text=lpt)), by=bycols][,names(FILt),with=FALSE])
        }
        FIL<-FILt
      }
      if (echo==1){cat(" og til slutt",dim(FIL),"\n")}
      setnames(FIL,names(FIL),gsub("_omk$","",names(FIL)))    
    }
  }
  
  if (nrow(RD$Udekk)>0){
    UDekk<-copy(RD$Udekk)
    restkols<-setdiff(tabnames,names(UDekk))
    setkeyv(FIL,names(UDekk))
    setkeyv(UDekk,names(UDekk))
    FIL<-FIL[!UDekk,]
    valkolsF<-unlist(lapply(valkols,function(x){paste(x,c("",".f",".a"),sep="")}))
    ## feil med recycling av := lest NEWS 1.12.2 data.table
    ## UDekk[,(valkolsF):=list(NA,9,0)]
    UDekk[, (valkols) := NA]
    valg_f <- grep(".f$", valkolsF, value = TRUE)
    UDekk[, (valg_f) := 9]
    valg_a <- grep(".a$", valkolsF, value = TRUE)
    UDekk[, (valg_a) := 0]
    if (length(restkols)>0){
      rest<-as.data.frame(unique(FIL[,restkols,with=FALSE]))
      UDekk<-data.table(expand.grid.df(rest,as.data.frame(UDekk)))
    }
    FIL<-rbind(FIL[,orgkols,with=FALSE],UDekk[,orgkols,with=FALSE])
    cat("UDEKKA:",nrow(RD$Udekk),"\n")
    #print(subset(RD$Udekk,GEOniv!="B"))
    print(RD$Udekk)
  }
  return(FIL)
}



ModifiserDesignFullRekt<-function(Nytt,Org=list(),globs=FinnGlobs()){
  Nkombs<-1
  for (del in names(Org$Part)){
    Nkombs<-Nkombs*nrow(Org$Part[[del]])    
  }
  
  for (del in names(Nytt)){
    delT<-as.data.table(Nytt[[del]])
    delT[,paste(del,"_HAR",sep="")]<-1
    Org$Part[[del]]<-delT
    Nkombs<-Nkombs*nrow(delT)
  }
  
  
  delerlist<-paste("as.data.frame(Org[[\"Part\"]][[\"",names(Org$Part),"\"]])",sep="",collapse=",")
  #delerlist<-paste("as.data.frame(Org[[\"Part\"]][[\"",names(Org$Part)[names(Org$Part) %in% c("Gn","Y","K","A")],"\"]])",sep="",collapse=",")
  FullDesign<-data.table(eval(parse(text=paste("expand.grid.df(",delerlist,")",sep=""))))
  FullDesign[,HAR:=1]
  OmkKols<-globs$DefDesign$DesignKols[globs$DefDesign$DesignKols %in% names(FullDesign)]
  setkeym(FullDesign,OmkKols)
  Org[["OmkDesign"]]<-FullDesign[,list(HAR=max(HAR)),by=OmkKols]
  
  #Merk, det gir bare mening å bruke denne for å lage et TIL-design, da trengs ikke de følgende delene
  #Om det modifiserte designet skal brukes som et FRA-design må også disse endres. Det er en klønete operasjon (og som vel knapt er veldefinert)
  #Kan altså IKKE bruke FinnFellesTab(Org,ModifiserDesign(PredFilter,Org))
  
  Org[["Design"]]<-NULL
  Org[["SKombs"]]<-NULL
  Org[["FKombs"]]<-NULL  
  
  return(Org)
}

ModifiserDesign<-function(Nytt,Org=list(),globs=FinnGlobs()){
  Nkombs<-1
  for (del in names(Org$Part)){
    Nkombs<-Nkombs*nrow(Org$Part[[del]])    
  }
  
  
  for (del in names(Nytt)){
    delT<-as.data.table(Nytt[[del]])
    delT[,paste(del,"_HAR",sep="")]<-1
    Org$Part[[del]]<-delT
    Nkombs<-Nkombs*nrow(delT)
  }
  
  if (any(grepl("_HAR",names(Org$OmkDesign)))){
    cat("************************************************************\n*\n*  OBSN NOE RART MED ModifiserDesign HAR\n*\n*********************************\n")
  }
  
  omkDeler<-intersect(names(Nytt),c(globs$DefDesign$UBeting,globs$DefDesign$BetingOmk))
  if (length(omkDeler)>0){
    NyKols<-unlist(globs$DefDesign$DelKols[omkDeler])
    NyKols<-intersect(names(Org$OmkDesign),NyKols)
    OmkDesignGmlKols<-setdiff(names(Org$OmkDesign),c(NyKols,"HAR"))
    delerlist<-paste("as.data.frame(Nytt[[\"",names(Nytt),"\"]])",sep="",collapse=",")
    #Skal beholdes
    if (length(OmkDesignGmlKols)>0){
      OmkDesGml<-Org$OmkDesign[,c(OmkDesignGmlKols,"HAR"),with=FALSE]
      setkeyv(OmkDesGml,OmkDesignGmlKols)
      OmkDesGml<-OmkDesGml[,list(HAR=max(HAR)),by=OmkDesignGmlKols]
      delerlist<-paste(delerlist,",as.data.frame(OmkDesGml)",sep="")
    }
    OmkDesNy<-data.table(eval(parse(text=paste("expand.grid.df(",delerlist,")",sep=""))))
    if (length(OmkDesignGmlKols)==0){
      OmkDesNy[,HAR:=1]
    }
    OmkKols<-globs$DefDesign$DesignKols[globs$DefDesign$DesignKols %in% names(OmkDesNy)]
    setkeym(OmkDesNy,OmkKols)
    Org[["OmkDesign"]]<-OmkDesNy
  }
  #Merk, det gir bare mening å bruke denne for å lage et TIL-design, da trengs ikke de følgende delene
  #Om det modifiserte designet skal brukes som et FRA-design må også disse endres. Det er en klønete operasjon (og som vel knapt er veldefinert)
  #Kan altså IKKE bruke FinnFellesTab(Org,ModifiserDesign(PredFilter,Org))
  
  Org[["Design"]]<-NULL
  Org[["SKombs"]]<-NULL
  Org[["FKombs"]]<-NULL  
 
  return(Org)
}




#NEINEI dette funker ikke!!!!!
ModifiserDesignFraParts<-function(NyPart,Org=list(),FGP=list(amin=0,amax=120),globs=FinnGlobs()){
  if (length(Org)==0){
    delerlist<-paste("as.data.frame(NyPart[[\"",names(NyPart),"\"]])",sep="",collapse=",")
    FullDesign<-data.table(eval(parse(text=paste("expand.grid.df(",delerlist,")",sep=""))))
  } else {
    FullDesign<-Org$Design
    #Filtrer FullDesign til NyPart
    for (del in names(NyPart)){
      setkeyv(NyPart[[del]],globs$DefDesign$DelKols[[del]])
      setkeyv(FullDesign,globs$DefDesign$DelKols[[del]])
      FullDesign<-FullDesign[NyPart[[del]]]
    }
    FullDesign<-subset(FullDesign,HAR==1)
  }
  return(FinnDesign(FullDesign,FGP=FGP,globs=globs))
}
 

OmkodFilFraPartM<-function(Fil,Part,FGP=list(amin=0,amax=120),rapport=list(),globs=FinnGlobs()){
  rapport["KALL"]<-"OmkodFilFraPart"
  for (del in names(Part)){
    
    Fil<-OmkodFilFraPart(Fil,Part[[del]],FGP,rapport=rapport,globs=globs) 
  }
  return(Fil)
}


OmkodFilFraPart<-function(Fil,Part,FGP=list(amin=0,amax=120),rapport=list(),globs=FinnGlobs(),echo=0){
  rapport["KALL"]<-"OmkodFilFraPart"
  Dorg<-FinnDesign(Fil,FGP=FGP,globs=globs)
  Dmod<-ModifiserDesign(Part,Dorg,globs=globs)
  RD<-FinnRedesign(Dorg,Dmod,globs=globs,echo=echo)
  return(OmkodFil(Fil,RD,rapport=rapport,globs=globs))
}


OmkodFilFraDesign<-function(Fil,Design,FGP=list(amin=0,amax=120),rapport=list(),globs=FinnGlobs()){
  rapport["KALL"]<-"OmkodFilFraDesign"
  Dorg<-FinnDesign(Fil,FGP=FGP,globs=globs)
  RD<-FinnRedesign(Dorg,Design,globs=globs)
  return(OmkodFil(Fil,RD,rapport=rapport,globs=globs))
}

# OmkodFilFraPart<-function(Fil,Part,FGP=list(amin=0,amax=120),globs=FinnGlobs()){
#   Dorg<-FinnDesign(Fil,FGP=FGP,globs=globs)
#   KB<-subset(FinnRedesign(Dorg,ModifiserDesign(Part,Dorg,globs=globs),globs=globs)$FULL,PRI==0)
#   return(OmkodFil(Fil,KB))
# }
# 
# 
# OmkodFilFraDesign<-function(Fil,Design,FGP=list(amin=0,amax=120),globs=FinnGlobs()){
#   Dorg<-FinnDesign(Fil,FGP=FGP,globs=globs)
#   KB<-subset(FinnRedesign(Dorg,Design,globs=globs)$FULL,PRI==0)
#   return(OmkodFil(Fil,KB))
# }



#####################################################################################


FinnDesign<-function(FIL,FGP=list(amin=0,amax=120),globs=FinnGlobs()){
  if (identical(class(FIL),"data.frame")){FIL<-data.table(FIL)}
  keyorg<-key(FIL)
  #Sett defdesign
  DelKols<-globs$DefDesign$DelKols
  UBeting<-globs$DefDesign$UBeting
  BetingOmk<-globs$DefDesign$BetingOmk
  BetingF<-globs$DefDesign$BetingF
  
  DesignKols<-globs$DefDesign$DesignKolsF[globs$DefDesign$DesignKolsF %in% names(FIL)]
  
  DesignKols<-globs$DefDesign$DesignKolsF[globs$DefDesign$DesignKolsF %in% names(FIL)]
  OmkKols<-globs$DefDesign$DesignKols[globs$DefDesign$DesignKols %in% names(FIL)]
  
  #Initier tomt resultat
  Design<-list()
  Design[["KolNavn"]]<-names(FIL)
  #Finn faktisk design
  setkeym(FIL,c(DesignKols))
  ObsDesign<-unique(FIL[,DesignKols,with=FALSE])
  #print(unique(FIL[,c("ALDERl","ALDERh"),with=FALSE]))
  #print(subset(FIL,GEOniv==1 & AARl==2009 & TAB1=="Total"))
  
  #Finn deler inneholdt i tabell
  Deler<-character()
  for (del in names(DelKols)){
    if(all(DelKols[[del]] %in% DesignKols)){
      Deler<-c(Deler,del)
    }
  }
  #Sjekk for evt ugyldig med bare ALDERl etc?
  
  #Sett omkodingskombinasjoner
  Design[["UBeting"]]<-UBeting[UBeting %in% Deler]
  Design[["BetingOmk"]]<-BetingOmk[BetingOmk %in% Deler]
  Design[["BetingF"]]<-BetingF[BetingF %in% Deler]
  Alle<-c(Design[["UBeting"]],Design[["BetingOmk"]],Design[["BetingF"]])
  Design[["OmkDeler"]]<-c(Design[["UBeting"]],Design[["BetingOmk"]])
  
  #Sett alle partielle tabuleringer (Gn,Y,K,A,T1,T2,T3), 
  for (del in Deler){
    kols<-DelKols[[del]]
    setkeyv(ObsDesign,kols)
    #SETT HAR
    Design[["Part"]][[del]]<-data.table(setNames(cbind(unique(ObsDesign[,kols,with=FALSE]),1),c(kols,paste(del,"_HAR",sep=""))),key=kols)
  }
  
  
  #Fyll evt hull i aldersintervaller 
  #Bør generaliserer til INT !!!
  if (globs$DefDesign$AMissAllow==TRUE){
    if ("A" %in% names(Design$Part)){
      mangler<-interval_difference(Intervals(c(FGP$amin,FGP$amax),type='Z'),Intervals(Design$Part$A[,DelKols$A,with=FALSE],type='Z'))
      if (nrow(mangler)>0){
        mangler<-setNames(cbind(as.data.frame(mangler),0),c("ALDERl","ALDERh","A_HAR"))
        #         if (max(mangler$ALDERl)>=95){
        #           mangler[ALDERl==max(mangler$ALDERl),A_HAR]<-1
        #         }
        Design[["Part"]][["A"]]<-rbind(Design[["Part"]][["A"]],mangler)
      }      
    }
  }
  
  #Finn fullt design, dvs kryssing av alle partielle. 
  delerlist<-paste("as.data.frame(Design[[\"Part\"]][[\"",Alle,"\"]])",sep="",collapse=",")
  FullDesign<-data.table(eval(parse(text=paste("expand.grid.df(",delerlist,")",sep=""))))
  setkeym(ObsDesign,names(ObsDesign))
  setkeym(FullDesign,names(ObsDesign))
  #Sett HAR=1 om denne finnes i fakttisk design
  FullDesign[,HAR:=0]
  FullDesign[ObsDesign,HAR:=1]
  Design[["Design"]]<-FullDesign
  
  #Utgått
  #Filtrer til bare den delen av designet som er aktuell for omkoding (dsv uten TAB1 etc)
  #setkeym(FullDesign,OmkKols)
  #Design[["OmkDesign"]]<-FullDesign[,list(HAR=max(HAR)),by=OmkKols]
  
  
  #Sett omkodingskombinasjone
  #Noen dimensjoner får variere fritt (UBeting). Andre må være fast for alle versjoner av UBeting
  #Def er at Gn og Y er frie, mens K og A må være fast for hver Gn,Y kombinasjon
  Beting<-c("",Design[["BetingOmk"]],Design[["BetingF"]])
  komb<-Design[["UBeting"]]
  for (del in Beting){
    if(del!=""){komb<-c(Design[["UBeting"]],del)}
    if (length(komb)>0){
      kols<-character(0)
      for (k in komb){kols<-c(kols,DelKols[[k]])}
      setkeyv(ObsDesign,kols)
      setkeyv(FullDesign,kols)
      kombFull<-data.table(unique(FullDesign[,kols,with=FALSE]))
      kombObs<-data.table(unique(ObsDesign[,kols,with=FALSE]))
      kombFull[,HAR:=0]
      kombFull[kombObs,HAR:=1]
      kombn<-paste("bet",del,sep="")
      Design[["SKombs"]][[kombn]]<-kombFull
    }
  }
  
  #Tilbakestill key
  setkeym(ObsDesign,names(ObsDesign))
  setkeym(FIL,keyorg)
  
  return(Design)
}



FinnRedesign<-function(DesFRA,DesTIL,SkalAggregeresOpp=character(),ReturnerFullFull=FALSE,globs=FinnGlobs(),prios=globs$DefDesign,KB=globs$KB,IntervallHull=globs$DefDesign$IntervallHull,AggPri=globs$DefDesign$AggPri,echo=0){
  
  
  #Merk assymtri mellom DesFRA og DesTIL. 
  #For DesTIL brukes bare DesTil$Part og DesTIL$OmkDesign.
  #DesFRA må derfor komme fra FinnDesign med alle egenskaper satt der, mens DesTIL kan være enklere og satt andre steder
  
  
  #   #Deler i DesFra som ikke er i DesTil må legegs til i DesTil (full kryss mot Part[del])
  #   #Merk at deler i DesTil som ikke er i DesFra går greit (all omkoding er indirekte "betinget" på disse)
  #   kryssdeler<-names(DesFRA$Part)[!(names(DesFRA$Part) %in% names(DesTIL$Part))]
  #   if (length(kryssdeler)>0){
  #     DesTIL<-ModifiserDesign(DesFRA$Part[kryssdeler],DesTIL,globs=globs)     
  #   }
  #   Redesign<-list()
  
  #Sett partiell omkoding
  #For intervaller kalles FinnKodebokINtervaller, elllers hentes fast kodebok som utgnagspunkt
  #Disse kodebøkene (KJONN etc) filtreres til de omkodingene som er aktuelle (bør gjøres her for å begrense kombinatorikk, selv om dette kunne vært utsatt)
  #Dvs omkodinger til en TIL som ikke har alle nødvendige deler i FRA filtreres bort 
  #Merk at noen deler i FRA ikke er obligatoriske (slik som KJONN=9 for omkoding til KJONN=0) 
  
  
  #Rydd DesTIL$Design (Kan variere litt mht HAR avhengig av hvor kallet på denne funksjonen er gjort fra. Skal ha 1 har felt)
  if (is.null(DesTIL$Design)){
    komblist<-paste("as.data.frame(DesTIL$Part[[\"",names(DesTIL$Part),"\"]])",sep="",collapse=",")
    FULL<-data.table(eval(parse(text=paste("expand.grid.df(",komblist,")",sep=""))))
    harkols<-names(FULL)[grepl("_HAR$",names(FULL))]
    if (length(harkols)>0){
      FULL[,(harkols):=NULL]  
    }    
  } else {
    FULL<-DesTIL$Design[HAR==1,]
    harkols<-names(FULL)[grepl("_HAR$|^HAR$",names(FULL))]
    FULL[,(harkols):=NULL]
  }
  setnames(FULL,names(FULL),paste(names(FULL),"_omk",sep=""))
  Udekk<-copy(FULL)
  
  betKols<-setdiff(names(DesFRA$SKombs$bet),"HAR")
  if (length(betKols)>0){
    FULL<-data.table(expand.grid.df(as.data.frame(FULL),as.data.frame(DesFRA$SKombs$bet[,betKols,with=FALSE])))
  }	
  for (del in DesFRA$UBeting){
    if (is.null(DesTIL$Part[[del]])){
      DesTIL$Part[[del]]<-copy(DesFRA$Part[[del]])
    }
  }
  
  
  Parts<-list()
  for (del in names(KB)){
    #if (del %in% names(DesTIL$Part)){
    
    
    if (del %in% names(DesTIL$Part) & del %in% names(DesFRA$Part)){
      DesTIL$Part[[del]]<-copy(as.data.table(DesTIL$Part[[del]])) #Får noen rare warnings uten copy, bør debugge dette
      delH<-paste(del,"_HAR",sep="")
      if (!delH %in% names(DesTIL)){
        DesTIL$Part[[del]][,(delH):=1]
      }
      KBD<-KB[[del]]
      kol<-globs$DefDesign$DelKolN[del]
      kolomk<-paste(kol,"_omk",sep="")
      kols<-globs$DefDesign$DelKols[[del]]
      kolsomk<-paste(kols,"_omk",sep="")
      
      #Sett 1-1 koding for T1,T2,.. dersom ikke annet gitt
      if (grepl("^T\\d$",del) & nrow(KBD)==0){
        tabN<-globs$DefDesign$DelKolN[del]
        tilTabs<-DesTIL$Part[[del]][,tabN,with=FALSE]
        KBD<-setNames(data.frame(tilTabs,tilTabs,0,1),c(tabN,paste(tabN,"_omk",sep=""),paste(del,c("_pri","_obl"),sep="")))
        Parts[[del]]<-KBD
      }
      #Behandling av enkle kolonner
      if (globs$DefDesign$DelType[del]=="COL"){
        if (nrow(KBD)>0){
          #Filtrer bort TIL-koder i global-KB som ikke er i desTIL 
          
          KBD<-KBD[KBD[,kolomk] %in% DesTIL$Part[[del]][[kol]],]
          omkcols<-c(kolomk,paste(del,"_pri",sep=""))
          kolsomkpri<-c(kolsomk,paste(del,"_pri",sep=""))
          KBD<-data.table(KBD,key=omkcols)
          #Sett HAR og Dekk
          eval(parse(text=paste(
            "KBD[,",del,"_HAR:=as.integer(",kol," %in% DesFRA$Part[[del]][[kol]])]",sep=""  
          )))
          eval(parse(text=paste(
            "KBD[,",del,"_Dekk:=as.integer(!any(",del,"_HAR==0 & ",del,"_obl==1)),by=kolsomkpri]",sep=""  
          )))
          #Kast omkodinger uten noen deler i FRA, behold de som dekkes helt og delvis
          eval(parse(text=paste(
            "KBD[,Kast:=!any(",del,"_HAR==1),by=kolsomkpri]",sep=""  
          )))
          KBD<-subset(KBD,Kast==FALSE)
          KBD$Kast<-NULL
          Parts[[del]]<-KBD          
        }
        
        #Behandling av intervaller (to kolonner etc)
      } else if (globs$DefDesign$DelType[del]=="INT"){
        #Global KB kan inneholde (fil)spesifikke koden "ALLE", må erstatte denne med "amin_amax" og lage intervall
        #Merk: dette gjelder typisk bare tilfellene der ukjent alder og evt tilsvarende skal settes inn under "ALLE" 
        Imin<-eval(parse(text=paste("min(DesFRA$Part[[del]][,",globs$DefDesign$DelKolN[[del]],"l])",sep="")))
        Imax<-eval(parse(text=paste("max(DesFRA$Part[[del]][,",globs$DefDesign$DelKolN[[del]],"h])",sep="")))
        alle<-paste(Imin,"_",Imax,sep="")
        if (nrow(KBD)>0){
          KBD[,kol]<-gsub("^(ALLE)$",alle,KBD[,kol])
          KBD[,kolomk]<-gsub("^(ALLE)$",alle,KBD[,kolomk])
          #KBD[,globs$DefDesign$DelKols[[del]]]<-as.integer(str_split_fixed(KBD[,kol],"_",2))
          KBD[,globs$DefDesign$DelKols[[del]]]<-matrix(as.integer(str_split_fixed(KBD[,kol],"_",2)),ncol=2)
          KBD[,paste(globs$DefDesign$DelKols[[del]],"_omk",sep="")]<-matrix(as.integer(str_split_fixed(KBD[,kolomk],"_",2)),ncol=2)
          #Kodebok ferdig mod
          
          #Filtrer KBD mot TIL!!
          #KBD<-KBD[KBD[,kolomk] %in% paste(DesTIL$Part[[del]][,kols,with=FALSE],sep="_"),]
          KBD<-KBD[KBD[,kolomk] %in% apply(DesTIL$Part[[del]][,kols,with=FALSE],1,paste,collapse="_"),]
        }
        #Må fjerne "del_HAR" inn i omkodintervall, fjerner dessuten del_HAR==0 i TIL
        
        delkols<-KHglobs$DefDesign$DelKols[[del]]
        IntFra<-DesFRA$Part[[del]][,delkols,with=FALSE]
        #IntTil<-DesTIL$Part[[del]][DesTIL$Part[[del]][[paste(del,"_HAR",sep="")]]==1,delkols,with=FALSE]
        #Merk: eneste som ikke har del_HAR er udekkede intervaller mellom amin og amax. 
        #Videre er disse bare med når TilDes er satt fra FinnDesign(FG), ikke når TilDes er fra Parts
        #Usikker på om det alltid er best å slippe disse gjennom.
        IntTil<-DesTIL$Part[[del]][,delkols,with=FALSE]    
        #Fjerner spesialkoder (dvs uoppgitt etc i KB) før intervallomregning
        IntFra<-IntFra[!apply(IntFra[,kols,with=FALSE],1,paste,collapse="_") %in% globs$LegKoder[[del]]$KODE]
        IntTil<-IntTil[!apply(IntTil[,kols,with=FALSE],1,paste,collapse="_") %in% globs$LegKoder[[del]]$KODE]
        #print("aksdløkaslødkøalsdkøkø")
        #print(IntFra)
        #print(IntTil)
        KBInt<-FinnKodebokIntervaller(as.data.frame(IntFra),as.data.frame(IntTil),deln=del)
        
        KBInt[,paste(del,"_obl",sep="")]<-1
        #DEVELOP:   DETTE ER TENMMELIG AD HOC!!!!!!!!!!
        if (del=="A"){
          KBInt[KBInt$ALDERl>=90,paste(del,"_obl",sep="")]<-0
        }
        
        KBInt[,paste(del,"_ok",sep="")]<-NULL  #Denne brukes bare ved filtrering rett fra KBint
        #Legg til spesialkoder igjen
        if (nrow(KBD)>0){
          KBD<-rbind(KBInt,KBD[,c(kols,kolsomk,paste(del,c("_pri","_obl"),sep=""))])
        } else {
          KBD<-KBInt
        }
        
        #Koble på "del_HAR"
        omkcols<-c(kolomk,paste(del,"_pri",sep=""))
        KBD<-data.table(KBD,key=kols)
        KBD<-data.table(DesFRA$Part[[del]],key=kols)[KBD]
        har<-paste(del,"_HAR",sep="")
        eval(parse(text=paste(
          "KBD[is.na(KBD[,",har,"]),",har,":=0]",sep=""  
        )))
        
        KBD<-SettPartDekk(KBD,del=del,IntervallHull=IntervallHull,globs=globs)
        #setnames(KBD,"DEKKok",paste(del,"_Dekk",sep=""))
        
        #Kast omkodinger uten noen deler i FRA, behold de som dekkes helt og delvis
        #USIKKER på om dette er optimalt. Det må ikke kastes for mye for riktig bruk fra FinnFellesTab
        #Egentlig er det jo unødvenig å kaste noe som helst. Dette er mest for rapport/lesing av KBD
        kolsomkpri<-c(kolsomk,paste(del,"_pri",sep=""))
        eval(parse(text=paste(
          "KBD[,Kast:=!any(",del,"_HAR==1 | ",del,"_obl==0),by=kolsomkpri]",sep=""  
        )))
        #         eval(parse(text=paste(
        #           "KBD[,Kast:=!any(",del,"_HAR==1),by=kolsomkpri]",sep=""  
        #         )))
        
        
        KBD<-KBD[Kast==FALSE,]
        KBD[,Kast:=NULL]
        Parts[[del]]<-KBD
      }
    }    
  }
  
  
  if(echo>=1){
    cat("Parts:\n")
    print(Parts)
  }
  
  SKombs<-list()
  KBs<-list() 
  Filters<-list()
  DelStatus<-list()
  
  #Må passe på rekkefølge (Ubeting til slutt), ellers kan det gå galt i FULL
  beting<-intersect(globs$DefDesign$AggPri[length(globs$DefDesign$AggPri):1],c(globs$DefDesign$BetingOmk,globs$DefDesign$BetingF))
  ubeting<-intersect(globs$DefDesign$AggPri[length(globs$DefDesign$AggPri):1],c(globs$DefDesign$UBeting))
  
  for (del in intersect(c(beting,ubeting),names(Parts))){
    delkols<-globs$DefDesign$DelKols[[del]]
    if (length(DesFRA[["UBeting"]])>0){
      if (del %in% DesFRA[["UBeting"]]){
        kombn<-"bet"
      } else {
        kombn<-paste("bet",del,sep="")
      }
      #Koble med DeSFRA
      setkeyv(Parts[[del]],delkols)
      setkeyv(DesFRA$SKombs[[kombn]],delkols)
      betD<-DesFRA$SKombs[[kombn]][Parts[[del]],allow.cartesian=TRUE]
      betD[is.na(HAR),HAR:=0]
      #Må kaste de som ikke har del_Dekk==1 hvis ikke kan de feilaktig 
      #få del_Dekk==1 under dersom del er i beting, da vil en annen del i beting få NA og by=betcols går galt!) 
      betD<-subset(betD,eval(parse(text=paste(del,"_Dekk==1",sep=""))))
      #Sett (betinget) dekning
      betcols<-unlist(globs$DefDesign$DelKols[setdiff(DesFRA[["UBeting"]],del)])
      betD<-SettPartDekk(betD,del=del,har="HAR",IntervallHull=IntervallHull,betcols=betcols,globs=globs)
    } else {
      betcols<-character()
      betD<-data.table(Parts[[del]])
    } 
    if(echo>=1){cat("betD 1:\n",kombn,"\n")
                print(betD)
                print(komblist)
    }
    
    
    #Finn beste alternativ
    OmkCols<-names(betD)[grepl("_(omk)$",names(betD))]
    bycols<-c(OmkCols,betcols)
    if (del %in% SkalAggregeresOpp){
      eval(parse(text=paste("betD[",del,"_Dekk==1,Bruk:=max(",del,"_pri),by=bycols]",sep="")))
    } else {
      eval(parse(text=paste("betD[",del,"_Dekk==1,Bruk:=min(",del,"_pri),by=bycols]",sep="")))
    }
    
    prid<-paste(del,"_pri",sep="")
    KB<-betD[eval(parse(text=paste("Bruk==",prid," & ",del,"_HAR==1",sep="")))]
    SKombs[[del]]<-betD
    
    
    
    #Sjekk om del kan omkodes helt partielt (fra Part) eller om må betinge (dvs KB)
    
    
    #Finner om en omk_kode bruker flere versjoner av partiell omkoding (hver versjon fra Part har ulik prid)
    #Om en slik finnes beholdes KB, ellers fjernes overlødig betinging
    maxBet<-KB[,eval(parse(text=paste("list(NOPri=length(unique(",prid,")))",sep=""))),by=OmkCols][,max(NOPri)]
    #Utgått se KB<- over
    #KB<-KB[[del]][eval(parse(text=paste(del,"_HAR==1",sep=""))),]
    if (maxBet==1){
      #brukcols<-setdiff(names(KB),betcols)
      brukcols<-c(gsub("_omk$","",OmkCols),OmkCols)
      setkeyv(KB,brukcols)
      KBs[[del]]<-unique(KB[,brukcols,with=FALSE])
      DelStatus[[del]]<-"P"
    } else {
      KB[,(names(KB)[grepl("(_obl|_{0,1}HAR|_Dekk|_pri|Bruk)$",names(KB))]):=NULL]
      KBs[[del]]<-KB
      DelStatus[[del]]<-"B"
    }
    
    if (del=="Y" & DelStatus[[del]]=="B"){
      KHerr("Har DelStatus[[Y]]==B, dette takles per nå ikke i FilOmkod og vil gi feil der!!!")
    }
    
    
    
    #Sett dekning i FULL
    #common<-intersect(names(FULL),names(KBs[[del]]))
    common<-intersect(names(FULL),names(KB))
    setkeyv(KB,common)
    setkeyv(FULL,common)
    FULL<-FULL[KB[,common,with=FALSE],nomatch=0,allow.cartesian=TRUE]
    
    # if (D_develop_predtype=="DIR"){
    #   delkols<-KHglobs$DefDesign$DelKols[[del]]
    # }
    
    #Ignorer KB der det ikke foregår reell omkoding
    if(all(KBs[[del]][,delkols,with=FALSE]==KBs[[del]][,paste(delkols,"_omk",sep=""),with=FALSE])){

      Filters[[del]]<-KBs[[del]][,names(KBs[[del]])[!grepl("_omk$",names(KBs[[del]]))],with=FALSE]
      KBs[del]<-NULL
      DelStatus[[del]]<-"F"

    }     
  }
  omkkols<-names(FULL)[grepl("_omk$",names(FULL))]
  setkeyv(FULL,omkkols)
  Dekk<-unique(FULL[,omkkols,with=FALSE])
  setnames(Dekk,names(Dekk),gsub("_omk$","",names(Dekk)))
  setkeyv(Udekk,names(Udekk))
  setkeyv(FULL,names(Udekk))
  Udekk<-Udekk[!FULL,allow.cartesian=TRUE]
  setnames(Udekk,names(Udekk),gsub("_omk$","",names(Udekk)))
  return(list(Parts=Parts,SKombs=SKombs,KBs=KBs,Filters=Filters,FULL=FULL,Dekk=Dekk,Udekk=Udekk,DelStatus=DelStatus))
}



SettPartDekk<-function(KB,del="",har=paste(del,"_HAR",sep=""),betcols=character(0),globs=FinnGlobs(),IntervallHull=globs$DefDesign$IntervallHull){
  OmkPriCols<-names(KB)[grepl("_(omk|pri)$",names(KB))]
  delKolN<-globs$DefDesign$DelKolN[del]
  bycols<-c(OmkPriCols,betcols)
  
  if (del %in% names(IntervallHull)){
    #haroblcond<-paste(del,"_HAR==1 & ",del,"_obl==1",sep="")
    haroblcond<-paste(har,"==1 | ",del,"_obl==0",sep="")
    DekkInt<-paste("sum((",haroblcond,")*(1+",delKolN,"h-",delKolN,"l))",sep="")
    NTOT<-paste("sum(",haroblcond,")",sep="")
    oblcond<-paste(del,"_obl==1",sep="")
    #TotInt<-paste("sum((",oblcond,")*(1+",delKolN,"h_omk-",delKolN,"l_omk))",sep="")
    TotInt<-paste("1+",delKolN,"h_omk-",delKolN,"l_omk",sep="")
    NHAR<-paste("sum(",oblcond,")",sep="")
    #Sett hjelpevariablene
    eval(parse(text=paste(
      "KB[,c(\"DekkInt\",\"FRADELER\",\"NHAR\",\"TotInt\",\"NTOT\"):=list(",DekkInt,",.N,",NTOT,",",TotInt,",",NHAR,"),by=bycols]",sep=""
    )))
    #Sjekk om dekning tilfrestiller krav satt i 
    eval(parse(text=paste(
      "KB[,\"",del,"_Dekk\":=as.integer(",IntervallHull[[del]],"),by=bycols]",sep=""
    )))
 IntRapp<-0
    if (IntRapp==1 & del=="A" & "AARl" %in% names(KB)){
      print("PARTDEKKK")
      print(IntervallHull[[del]])
      print(KB)
    }
    
    
    KB[,c("DekkInt","NHAR","TotInt","NTOT","FRADELER"):=NULL]
    #Kast hjelpekolonnner
    #KB[,c("DekkInt","NHAR","TotInt","NTOT","DEKKok"):=NULL]
  } else {
    eval(parse(text=paste(
      "KB[,",del,"_Dekk:=as.integer(!any(",har,"==0 & ",
      del,"_obl==1)),by=bycols]",sep="")))
  }  
  return(KB)
}



FinnKodebokIntervaller<-function(FRA,TIL,storst=TRUE,delnavn="INT",echo=0){
  #I tilfelle input er data.table
  FRA<-as.data.frame(FRA)
  TIL<-as.data.frame(TIL)
  #Bruk Intrevals-klassen
  utcolnavn<-c(names(FRA),paste(names(FRA),"_omk",sep=""),paste(delnavn,"_pri",sep=""))
  TILi<-Intervals(TIL,type='Z')
  FRAi<-Intervals(FRA,type='Z')
  if (storst==TRUE){
    #Gir høyest prioritet til å bruke/beholde store intervaller
    sorter<-order(size(FRAi),decreasing=TRUE)
  } else {
    #Gir høyest prioritet til å bruke små intervaller og aggregger opp. Brukes ved aldersstandardisering
    sorter<-order(size(FRAi))    
  }
  
  #sorter<-order(size(FRAi))
  FRAi<-FRAi[sorter]
  FRA<-FRA[sorter,]
   #Finn kandidater, dvs inkluderte "underintrevaller"
  KAND<-interval_included(TILi,FRAi)
  if(class(KAND)=="matrix"){  #Irriterende bug(?) i interval når TILi har dim 1 eller KAND er n*m
    #KAND<-list(KAND)
    KAND<-split(KAND, rep(1:ncol(KAND), each = nrow(KAND)))
  }
  if(echo==1){print(KAND)}
  
  #Finn intern overlapp i FRA
  OVLP<-interval_overlap(FRAi,FRAi) 
  if(echo==1){print(OVLP)}
    
  #Initier tom kodebok
  KODEBOK0<-as.data.frame(setNames(replicate(length(utcolnavn),integer(0), simplify = F), utcolnavn))
  KODEBOK<-KODEBOK0
  #Må loope over alle TIL. Kan dette vektoriseres? Kanskje ikke så mye å vinne?
  for (i in 1:nrow(TIL)){
    result<-list("pri"=0,"KODEBOK"=KODEBOK0)
    result<-FinnKodebokForEtIntervall(KAND[[i]],FRA,TIL[i,],OVLP,0,result,utcolnavn)
    #???????? ok==1???????  Dette skal vel aldri skje????
#     if (result$ok==0){
#       result$KODEBOK<-rbind(result$KODEBOK,as.data.frame(setNames(list(NA_integer_,NA_integer_,TIL[i,1],TIL[i,2],0,1),utcolnavn)))
#     }
    #print(result$KODEBOK)
    KODEBOK<-rbind(KODEBOK,result$KODEBOK)
  }
  return(KODEBOK)
}

FinnKodebokForEtIntervall<-function(Find,FRA,TILint,OVLP,letn,result,utcolnavn){
  if(DekkerInt(FRA[Find,],TILint)){
    jobb<-Find[Find>letn]
    if(length(jobb)>0){
      #Må unngå jobbing på alle esoteriske kombinasjoner ved mye overlap
      if (result$pri<6){
        letn<-jobb[1]
        #cat("Let videres med letn",letn,"\n")
        result<-FinnKodebokForEtIntervall(Find[!(Find %in% OVLP[[letn]]) | Find==letn],FRA,TILint,OVLP,letn,result,utcolnavn)
        #LEt videre uten letn
        result<-FinnKodebokForEtIntervall(Find[Find!=letn],FRA,TILint,OVLP,letn,result,utcolnavn)
      }
    } else {
      result$KODEBOK<-rbind(result$KODEBOK,setNames(cbind(FRA[Find,],TILint,result$pri),utcolnavn))
      result$pri<-result$pri+1
    }
  }
  else {
    #Sett diff/residual
    if (nrow(FRA[Find,])>0){
      miss<-setNames(as.data.frame(interval_difference(Intervals(TILint,type='Z'),Intervals(FRA[Find,],type='Z'))),names(FRA))
    } else {
      miss<-setNames(as.data.frame(TILint),names(FRA))
    }
    #Ta bare med dersom residual er ny
    miss<-miss[!apply(miss,1,paste,collapse="_") %in% apply(FRA,1,paste,collapse="_"),]
    if (nrow(miss)>0){
      FRA<-rbind(FRA,miss)
      Find<-c(Find,nrow(FRA))
      OVLP<-interval_overlap(Intervals(FRA,type='Z'),Intervals(FRA,type='Z')) 
      result<-FinnKodebokForEtIntervall(Find,FRA,TILint,OVLP,letn,result,utcolnavn)
    }
  } 
  return(result)
}


####################################################################################
#Tekniske hjelperutiner

DekkerInt<-function(FRA,TIL){
  #Sjekker at hele intervallet TIL[,1]-TIL[,2] dekkes av intervallene i FRA
  #Bryr seg ikke om overlapp her, det gjøres andre steder
  #Kompakt, men effektiv syntaks
  return(all(TIL[,1]:TIL[,2] %in% unlist(mapply(seq,FRA[,1],FRA[,2]))))
}

KHsum<-function(x){
  sum<-NA
  if (length(grep("\\d",x))>0){
    if ("." %in% x || ".." %in% x || ":" %in% x){
      sum<-"."
    } else {
      sum<-sum(as.numeric(x))
    }
  } else if (all(x=="..")){
    sum<-".."
  } else if (all(x==":")){
    sum<-":"
  } else {
    sum<-"."
  }  
  return(sum)
}

SettKHBatchDate<-function(){format(Sys.time(), "%Y-%m-%d-%H-%M")}

DFHeadToString <- function (innDF,topn=10){
  #Bruker data.table print for summary
  DT<-data.table(innDF)
  optdef<-getOption("width")  #Sett bred output
  options(width = 250)
  head<-paste(capture.output(print(print(DT,topn=topn))),collapse="\n")
  head<-sub("NULL$","",head)
  options(width = optdef)  
  return(head)  
}

DFHeadToString2 <- function (innDF){
  #return(paste(capture.output(print(head(innDF))),collapse="' & Chr(13) & Chr(10) & '"))
  return(paste(capture.output(print(head(innDF))),collapse="\n"))
}

expand.grid.df <- function(...) {
  #Hjelpefunksjon, se http://stackoverflow.com/questions/11693599/alternative-to-expand-grid-for-data-frames
  #Finnes også en i reshape, men ikke i reshape2, så bruker ikke denne
  #Skjønner ikke helt syntaksen, men funker utmerket
  Reduce(function(...) merge(..., by=NULL), list(...))
}

setkeym<-function(DTo,keys){
  #Forøsk på å speede opp når setkeyv brukes for å sikre key(DTo)=keys 
  if (!("data.table" %in% class(DTo) && identical(key(DTo),keys))){
    setDT(DTo)
    setkeyv(DTo,keys)
  }
}


KonverterRMappe<-function(Rmappe,Utmappe,Format="csv",globs=FinnFlobs){
  print(paste(globs$path,Rmappe,sep="/"))
  setwd(paste(globs$path,Rmappe,sep="/"))
  filer<-list.files()
  for (fil in filer[grepl("\\.rds$",filer)]){
    filn<-gsub("(.*)\\.rds$","\\1",fil)
    TABELL<-readRDS(fil)
    cat("Eksporterer ",filn,"\n")
    if (tolower(Format)=="csv"){
      filen<-paste(globs$path,"/",Utmappe,filn,".csv",sep="")
      write.table(TABELL,file=filen,sep=';',row.names = FALSE)
      cat("CSV til",filen,"\n")
    } else if (tolower(Format)=="stata"){
      write.dta(TABELL,file=paste(globs$path,"/",Utmappe,filn,".dta",sep=""))
    }      
  }
}

KonverterKUBER<-function(Format="CSV",versjonert=FALSE,globs=FinnGlobs()){
  #Rmappe<-paste(globs$path,"/",globs$KubeDirNy,sep="")
  Rmappe<-globs$KubeDirNy
  if (versjonert==TRUE){
    Rmappe<-globs$KubeDirDat
  }
  Utmappe<-gsub("/R/",paste("/",Format,"/",sep=""),Rmappe)
  KonverterRMappe(Rmappe=Rmappe,Utmappe=Utmappe,Format=Format,globs=globs)
}

KonverterStablaFilgrupper<-function(Format="CSV",globs=FinnGlobs()){
  #Rmappe<-paste(globs$path,"/",globs$KubeDirNy,sep="")
  Rmappe<-globs$StablaDirNy
  if (versjonert==TRUE){
    Rmappe<-globs$StablaDirDat
  }
  Utmappe<-gsub("(.*/)R(/.*)",paste("\\1",Format,"\\2",sep=""),Rmappe)
  KonverterRMappe(Rmappe=Rmappe,Utmappe=Utmappe,Format=Format,globs=globs)
}

YAlagLH<-function(FG,YL,AL,vals=FinnValKols(names(FG)),globs=FinnGlobs()){
  setDT(FG)
  FGl<-copy(FG)
  tabkols<-setdiff(names(FG),FinnValKolsF(names(FG)))
  FGl[,c("lAARl","lAARh","lALDERl","lALDERh"):=list(AARl+YL,AARh+YL,ALDERl+AL,ALDERh+AL)]
  FGl[,c("AARl","AARh","ALDERl","ALDERh"):=list(NULL)]
  setnames(FGl,c("lAARl","lAARh","lALDERl","lALDERh"),c("AARl","AARh","ALDERl","ALDERh"))
  lvals<-paste("YL",YL,"_AL",AL,"_",vals,c("",".f",".a"),sep="")
  setnames(FGl,paste(vals,c("",".f",".a"),sep=""),lvals)
  FGl<-FGl[,c(tabkols,lvals),with=FALSE]
  setkeyv(FG,tabkols)
  setkeyv(FGl,tabkols)
  FG<-FGl[FG]  
  return(FG)
}

YAlagMerge<-function(FG,YL,AL,vals=FinnValKols(names(FG)),globs=FinnGlobs()){
  setDT(FG)
  orgkols<-names(FG)
  FGl<-copy(FG)
  FGl[,c("lAARl","lALDERl"):=list(AARl+YL,ALDERl+AL)]
  FGl[,c("AARl","AARh","ALDERl","ALDERh"):=list(NULL)]
  setnames(FGl,c("lAARl","lALDERl"),c("AARl","ALDERl"))
  tabkols<-setdiff(names(FGl),FinnValKolsF(names(FG)))
  lvals<-paste("YL",YL,"_AL",AL,"_",vals,c("",".f",".a"),sep="")
  setnames(FGl,paste(vals,c("",".f",".a"),sep=""),lvals)
  FGl<-FGl[,c(tabkols,lvals),with=FALSE]
  setkeyv(FG,tabkols)
  setkeyv(FGl,tabkols)
  FG<-FGl[FG,allow.cartesian=TRUE][,c(orgkols,lvals),with=FALSE]
  return(FG)
}

YAlagVal<-function(FG,YL,AL,vals=FinnValKols(names(FG)),globs=FinnGlobs()){
  setDT(FG)
  orgkols<-names(FG)
  ltag<-function(lag){
    ltag<-""
    if (lag>0){
      ltag<-paste("m",abs(lag),sep="")
    } else if (lag<0) {
      ltag<-paste("p",abs(lag),sep="")
    }
    return(ltag)
  }
  FGl<-copy(FG)
  FGl[,c("lAARl","lALDERl"):=list(AARl+YL,ALDERl+AL)]
  FGl[,c("AARl","AARh","ALDERl","ALDERh"):=list(NULL)]
  setnames(FGl,c("lAARl","lALDERl"),c("AARl","ALDERl"))
  tabkols<-setdiff(names(FGl),FinnValKolsF(names(FG)))
  lvals<-paste("Y",ltag(YL),"_A",ltag(AL),"_",vals,c("",".f",".a"),sep="")
  setnames(FGl,unlist(lapply(vals,function(x){paste(x,c("",".f",".a"),sep="")})),lvals)
  FGl<-FGl[,c(tabkols,lvals),with=FALSE]
  setkeyv(FG,tabkols)
  setkeyv(FGl,tabkols)
  return(FGl)
}



#http://ctszkin.com/2012/03/11/generating-a-laglead-variables/
shift<-function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}

FinnValKols<-function(knames){
  gsub("^(.*?)\\.f$","\\1",knames[grepl("^(.*?)\\.f$",knames)])  
}
FinnValKolsF<-function(knames){
  vkolsN<-gsub("^(.*?)\\.f$","\\1",knames[grepl("^(.*?)\\.f$",knames)])
  vkolsNF<-unlist(lapply(vkolsN,function(x){paste(x,c("",".f",".a",".n"),sep="")}))
  return(intersect(knames,vkolsNF))
}

FinnTabKols<-function(knames){
  return(setdiff(knames,c(FinnValKolsF(knames),"KOBLID","ROW")))  
}


FinnTabKolsKUBE<-function(allnames,globs=FinnGlobs()){
  annet<-union(union(unlist(globs$NesstarOutputDef),FinnValKolsF(allnames)),c("NORMSMR","SMRtmp"))
  return(setdiff(allnames,annet))  
}


LagDesignRapportAlle<-function(){
  globs<-FinnGlobs()
  FILGRP<-unlist(sqlQuery(globs$dbh,"SELECT FILGRUPPE FROM FILGRUPPER"))
  for (FILgrp in FILGRP){
    LagDesignRapport(FILgrp)    
  }
}

LagFilgrupperForFilter<-function(filter,versjonert=FALSE){
  globs<-FinnGlobs()
  FILGRP<-unlist(sqlQuery(globs$dbh,paste("SELECT FILGRUPPE FROM FILGRUPPER WHERE ",filter,"='1'",sep=""),as.is=TRUE))
  print(FILGRP)
  #for (FILgrp in FILGRP){
    batchdate=SettKHBatchDate()
    LagFlereFilgrupper(filgrupper=FILGRP,batchdate=batchdate,globs=globs,versjonert=versjonert)
    #FG<-LagFilgruppe(FILgrp,versjonert=versjonert,globs=globs)    
  #}
}


LagDesignRapport<-function(filgrp){
  print(filgrp)
  FG<-FinnFilT(filgrp)
  FGd<-FinnDesign(FG)$Design
  Ubalans<-nrow(subset(FGd,HAR==0))
  cat("filgrp ",Ubalans,"/",nrow(FGd),"\n")
  return(FGd)
}


FinnNegVal<-function(){
  globs<-FinnGlobs()
  FILGRP<-unlist(sqlQuery(globs$dbh,"SELECT FILGRUPPE FROM FILGRUPPER"))
  for (FILgrp in FILGRP){
    FG<-FinnFilT(FILgrp)
    valkols<-FinnValKols(names(FG))
    valkols<-valkols[!grepl("\\.(f|a)$",valkols)]
    for (valk in valkols){
      cat(FILgrp,valk,sum(FG[,valk,with=FALSE]<0),"\n")
    }
  }
}

SjekkVersjoner<-function(commoncols=FALSE,dropcols=character(0)){
  globs<-FinnGlobs()
  path=paste(globs$path,"/",globs$KubeDirDat,sep="")
  setwd(path)
  Filer<-setNames(as.data.frame(list.files(include.dirs = FALSE),stringsAsFactors=FALSE),c("FILNAVN"))
  Filer$KUBE<-gsub("(.*)_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}\\.rds","\\1",Filer$FILNAVN)
  Filer$VERS<-0
  
  for (Kube in setdiff(unique(Filer$KUBE),"tmp")){
    versj=0
    filer<-Filer$FILNAVN[Filer$KUBE==Kube]
    filer<-filer[order(filer,decreasing=TRUE)]
    Filer$VERS[Filer$FILNAVN==filer[1]]<-versj
    VC<-readRDS(filer[1])
    i<-2
    while (i<=length(filer)){
      VN<-readRDS(filer[i])
      if (commoncols==TRUE | identical(names(VC),names(VN))){
        comparecols<-setdiff(intersect(names(VC),names(VN)),dropcols)
        if (!identical(VC[,comparecols,with=FALSE],VN[,comparecols,with=FALSE])){
          versj<-versj+1
          VC<-VN
        }
      }
      else {
        versj<-versj+1
        VC<-VN
      }
      Filer$VERS[Filer$FILNAVN==filer[i]]<-versj
      i<-i+1
    }
  }
  sqlQuery(globs$log,"DELETE * FROM FILVERSJONER")
  sqlSave(globs$log,Filer,"FILVERSJONER",append=TRUE)
}

FinnDatertKube<-function(KUBEid,batch=NA,silent=FALSE,hist=0){
  globs<-FinnGlobs()
  path<-paste(globs$path,"/",globs$KubeDirDat,sep="")
  #Finner nyeste daterte versjon om ikke batcdate er gitt
  if (is.na(batch)){
    orgwd<-getwd()
    setwd(path)
    Filer<-setNames(as.data.frame(list.files(include.dirs = FALSE),stringsAsFactors=FALSE),c("FILNAVN"))
    Filer$KUBE<-gsub("(.*)_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}\\.rds","\\1",Filer$FILNAVN)
    Filer<-subset(Filer,KUBE==KUBEid)
    Filer<-Filer[order(Filer$FILNAVN),]
    row<-nrow(Filer)-hist
    if (row>0){
      filn<-paste(path,Filer$FILNAVN[row],sep="")
    } else {
      if (silent==FALSE){cat("Finnes ikke hist=",hist,"eldre versjon\n")}
      filn<-NA
    }
    setwd(orgwd)
  } else {
    filn<-paste(path,KUBEid,"_",batch,".rds",sep="")
  }
  if (!is.na(filn)){
    cat("LESER inn",filn,"\n")
    KUBE<-readRDS(filn)
  } else {
    KUBE<-FALSE
  }
 
  return(KUBE)
}

SammenlignKuber<-function(V1,V2,FULL=TRUE,streng=TRUE,comparecols=character(0)){
  Tab1<-FinnTabKolsKUBE(names(V1))
  Tab2<-FinnTabKolsKUBE(names(V2))
  tabdiff<-union(setdiff(Tab1,Tab2),setdiff(Tab2,Tab1))
  check<-FALSE
  checkm<-FALSE
  mismatch<-integer(0)
  if (streng==TRUE & length(tabdiff)>0){
    cat("Kan ikke sammenligne KUBER når følgende kolonner ikker er i begge:",tabdiff,"\n")
    V12<-data.table(0)
  } else {
    if (length(tabdiff)>0){
      Tab<-intersect(Tab1,Tab2)
      cat("OBS! tabdiff:",tabdiff,"\n")
    } else {
      Tab<-Tab1
    }
    if (length(comparecols)==0){
      comparecols<-intersect(setdiff(names(V1),Tab1),setdiff(names(V2),Tab2))
    }
    setkeyv(V1,Tab)
    setkeyv(V2,Tab)
    V1<-V1[eval(parse(text=paste("order(",Tab,")",sep=""))),c(Tab,comparecols),with=FALSE]
    V2<-V2[eval(parse(text=paste("order(",Tab,")",sep=""))),c(Tab,comparecols),with=FALSE]
    
    if (streng==FALSE){
      V1<-V1[!grepl("99|9900$",GEO),]
      V2<-V2[!grepl("99|9900$",GEO),]  
      if ("ALDER" %in% Tab){
        V1<-V1[!ALDER %in% c("999_999","888_888"),]
        V2<-V2[!ALDER %in% c("999_999","888_888"),]
      } 
      if ("KJONN" %in% Tab){
        V1<-V1[!KJONN %in% c(8,9),]
        V2<-V2[!KJONN %in% c(8,9),]
      }
    }
    #comtabs<-V1[V2,nomatch=0][,Tab,with=F]
    #if (streng<0){
    #  check<-all.equal(V2[comtabs,],V1[comtabs,],check.attributes=FALSE,check.names=FALSE)
    #} else {
    check<-all.equal(V2,V1,check.attributes=FALSE,check.names=FALSE)  
    #}
    comp<-suppressWarnings(as.integer(gsub("^Component (\\d+):.*","\\1",check)))
    err<-gsub("^Component \\d+:(.*)","\\1",check)
    comp<-comp[!is.na(comp)]
    err<-err[!is.na(comp)]
    checkm<-paste(names(V1)[comp],":",err)
    
    V12<-data.table(0)
    if (FULL==TRUE){
      V12<-merge(V1,V2,all=TRUE,by=Tab,suffixes=c("_1","_2"))
      colorder<-c(Tab,unlist(lapply(comparecols,function(x){paste(x,c("_1","_2"),sep="")})))
      V12<-V12[,colorder,with=FALSE]
      mismatch<-which(V12[,paste(comparecols,"_1",sep=""),with=FALSE]!=V12[,paste(comparecols,"_2",sep=""),with=FALSE])
    }
  }
  return(list(check=check,checkm=checkm,V12=V12,V1=V1,V2=V2,cc=comparecols,mm=mismatch))  
}

PrintCompCheck<-function(TT,cols=c("TELLER","NEVNER","RATE","SMR","MEIS")){
  comp<-suppressWarnings(as.integer(gsub("^Component (\\d+):.*","\\1",TT$check)))
  err<-gsub("^Component \\d+:(.*)","\\1",TT$check)
  keepcomp<-which(names(TT$V1) %in% cols)
  compM<-comp[comp %in% keepcomp]
  err<-err[comp %in% keepcomp]
  checkm<-paste(names(TT$V1)[compM],":",err)
  print(checkm)
}


CompNyOgKlar<-function(KUBEid,streng=TRUE,globs=FinnGlobs()){
  okvers<-as.character(sqlQuery(globs$dbh,paste("SELECT VERSJON FROM KH2015_KUBESTATUS WHERE KUBE_NAVN='",KUBEid,"'",sep=""),as.is=TRUE))
  comp<-SammenlignKuber(FinnDatertKube(KUBEid),FinnDatertKube(KUBEid,batch=okvers),streng=streng)
  return(comp)
}

CompNyOgKlarAlle<-function(globs=FinnGlobs()){
  batchdate=SettKHBatchDate()
  KUBER<-sqlQuery(globs$dbh,"SELECT KUBE_NAVN FROM KUBER",as.is=TRUE)
  utfil<-paste(globs$path,"/",globs$KubeDir,"/LOGG/CompNyOgKlar_",batchdate,".txt",sep="")
  sink(utfil,split=TRUE)
  for (i in 1:nrow(KUBER)){
    comp<-try(CompNyOgKlar(KUBER[i,1],streng=FALSE,globs=globs))
    if (!inherits(comp,'try-error')){
      cat("__________________________________________\nKUBE: ",KUBER[i,1],"\n")
      print(comp$checkm)
      cat("___________________________________________________\n")
    }  
  }
  sink()
}


KHerr<-function(error){
  cat("***************************************************************************\n"
      ,"*KHFEIL!! ",error,"\n***************************************************************************\n")
}

KH2014v2015<-function(kube,batchdate=SettKHBatchDate(),globs=FinnGlobs(),echo=FALSE){
  KH2014dir<-"F:/Prosjekter/Kommunehelsa/Data og databehandling/Databehandling/2014/csvNESSTAR"
  setwd(KH2014dir)
  datef<-format(strptime(batchdate, "%Y-%m-%d-%H-%M"),"#%Y-%m-%d#")
  K15dscr<-FinnKubeInfo(kube)
  FellesTabs<-c("GEO","AAR","ALDER","KJONN")
  KLenke<-sqlQuery(globs$dbh,paste("SELECT KH2015v2014.*, KH2015_KUBESTATUS.VERSJON AS BATCH 
                                   FROM KH2015v2014 LEFT JOIN KH2015_KUBESTATUS
                                   ON KH2015v2014.KH2015_KUBE = KH2015_KUBESTATUS.KUBE_NAVN
                                   WHERE KH2015_KUBE='",kube,"'",sep=""),stringsAsFactors=FALSE)[1,]
  F2015<-FinnKubeT(kube,batch=KLenke$BATCH)
  F2014<-as.data.table(read.csv(KLenke$KH2014FIL,sep=';',stringsAsFactors=FALSE))
  if (echo==TRUE){
    print("F2014:")
    print(F2014)
    print("F2015:")
    print(F2015)
  }
  #print(F2014)
  #print(sapply(F2014,class))
  F2014$AAR<-as.character(F2014$AAR)
  #F2014$AAR<-gsub("^(\\d{4})$","\\1_\\1",F2014$AAR)
  F2015$AAR<-gsub("^(\\d{4})_(\\d{4})$","\\2",F2015$AAR)
  F2014$GEO<-as.character(F2014$GEO)
  F2014$GEO<-gsub("^(\\d{3})$","0\\1",F2014$GEO)
  F2014$GEO<-gsub("^([1-9])$","0\\1",F2014$GEO)
  if (!is.null(F2014$KJONN)){
    F2014$KJONN<-as.integer(F2014$KJONN)  
  } else if (!is.null(F2014$KJONN)){
    F2014$KJONN<-0
  }
  alle<-"0_120"
  if (!is.na(K15dscr$ALDER_ALLE)){
    alle<-K15dscr$ALDER_ALLE
  }
  allev<-unlist(str_split(alle,"_"))
  if (!is.null(F2014$ALDER)){
    F2014$ALDER<-as.character(F2014$ALDER)
    if (!is.na(KLenke$Alder14TOM)){
      int<-which(grepl("_",F2014$ALDER))
      atmp<-unlist(str_split(F2014$ALDER[int],"_"))
      F2014$ALDER[int]<-paste(atmp[1],"_",as.integer(atmp[2])-1,sep="") 
    }
    F2014$ALDER<-gsub("^_(\\d+)$",paste(allev[1],"_","\\1",sep=""),F2014$ALDER)
    F2014$ALDER<-gsub("^(\\d+)_$",paste("\\1","_",allev[2],sep=""),F2014$ALDER)
    F2014$ALDER<-gsub("ALLE",alle,F2014$ALDER)
  } else {
    F2014$ALDER<-alle
  }
  setnames(F2014,names(F2014),gsub("_MA\\d+$","",names(F2014)))
  setnames(F2014,names(F2014),gsub("RATE\\d+$","RATE",names(F2014)))
  Ftab15<-intersect(FellesTabs,names(F2015))
  Ftab14<-intersect(FellesTabs,names(F2014))
  Ftab<-intersect(Ftab15,Ftab14)
  tabs14<-KLenke[c("TAB1_14","TAB2_14")]
  tabs14<-tabs14[!(is.na(tabs14) | tabs14=="NA")]
  tabs15<-K15dscr[c("TAB1","TAB2","TAB3")]
  tabs15<-tabs15[!(is.na(tabs15) | tabs15=="NA")]
  
  if (length(tabs15)>0 & length(tabs14)==length(tabs15)){
    setnames(F2014,tabs14,tabs15)
  }
  
  for (tab in c("TAB1","TAB2")){
    tabT<-paste(tab,"map_14",sep="")
    if (!is.na(KLenke[1,tabT])){
      tab1415<-unlist(str_split(KLenke[1,tabT],","))
      tabT<-paste(tab,"org_14",sep="")
      tab1414<-unlist(str_split(KLenke[1,tabT],","))
      tab15n<-as.character(K15dscr[tab])
      F2014<-data.frame(F2014)
      print(tab15n)
      F2014[,tab15n]<-as.character(mapvalues(F2014[,tab15n],tab1414,tab1415))
      setDT(F2014)
    }
  }
  
  if(length(tabs15)>0){
    F2014[,(tabs15)]<-F2014[,lapply(.SD, as.character),.SDcols=tabs15]
  }
  
  
  Ftab<-c(Ftab,tabs15)
  
  if (!is.na(KLenke$TELLER14)){
    setnames(F2014,KLenke$TELLER14,"TELLER")
  }
  if (!is.na(KLenke$MALTALL14)){
    setnames(F2014,KLenke$MALTALL14,"MALTALL")
  }
  valcols<-c("TELLER","NEVNER","RATE","MALTALL","SMR","MEIS","SPVFLAGG")
  valcols14<-intersect(valcols,names(F2014))
  valcols15<-intersect(valcols,names(F2015))
  valcolsF<-intersect(valcols15,valcols14)
  
  cols<-c(Ftab,valcolsF)
  F2015<-F2015[,intersect(cols,names(F2015)),with=F]
  F2014<-F2014[,intersect(cols,names(F2015)),with=F]
  cat("Ftab:",Ftab,"\n")
  
  
  
  
  MM<-merge(F2015,F2014,by=Ftab,all=TRUE,suffixes=c("15","14"),allow.cartesian=T)
  utcolso<-c("GEO","AAR","ALDER","KJONN",tabs15,unlist(lapply(c("TELLER","NEVNER","RATE","MALTALL","SMR","MEIS","SPVFLAGG"),function(x){paste(x,c("15","14"),sep="")})))
  utcolso<-intersect(utcolso,names(MM))
  MM<-MM[,utcolso,with=F]
  utfil<-paste("F:/Prosjekter/Kommunehelsa/PRODUKSJON/VALIDERING/NESSTAR_KUBER/KH2014v2015/",kube,"_KH15v14_",KLenke$BATCH,".csv",sep="")
  write.table(MM,file=utfil,sep=";",row.names=FALSE)
  return(MM)  
}

FullKH2014v2015<-function(escape=character(0),globs=FinnGlobs()){
  kuber<-sqlQuery(globs$dbh,"SELECT KH2015_KUBE FROM KH2015v2014",stringsAsFactors=FALSE)[,1]  
  for (kube in setdiff(kuber[!is.na(kuber)],escape)){
    try(KH2014v2015(kube,globs=globs,echo=FALSE))
  }
}


DumpTabell <- function (TABELL,TABELLnavn,globs=FinnGlobs(),format=globs$DefDumpFormat){
  if (format=="CSV"){
    write.table(TABELL,file=paste(globs$path,"/",globs$DUMPdir,"/",TABELLnavn,".csv",sep=""),sep=';',na="",row.names = FALSE)
  } else if (format=="R"){
    .GlobalEnv$DUMPtabs[[TABELLnavn]]<-TABELL
    print(DUMPtabs)
  } else if (format=="STATA") {
    TABELL[TABELL==""]<-" "  #STATA støtter ikke "empty-string"
    names(TABELL)<-gsub("^(\\d.*)$","S_\\1",names(TABELL))   #STATA 14 tåler ikke numeriske kolonnenavn 
    write.dta(TABELL,paste(globs$path,"/",globs$DUMPdir,"/",TABELLnavn,".dta",sep=""))
  }
}

FinnKubeInfo<-function(kube){
  globs<-FinnGlobs()
  return(sqlQuery(globs$dbh,paste("
        SELECT DISTINCT KUBE_NAVN, TELLERKOL, NEVNERKOL, EKSTRAVARIABLE, FILGRUPPER.VAL1navn,FILGRUPPER.VAL2navn,FILGRUPPER.VAL3navn,
                                  FILGRUPPER.TAB1, FILGRUPPER.TAB2, FILGRUPPER.TAB3, FILGRUPPER.ALDER_ALLE
                                  FROM (KUBER INNER JOIN TNP_PROD ON KUBER.TNP = TNP_PROD.TNP_NAVN) 
                                  INNER JOIN FILGRUPPER ON TNP_PROD.TELLERFIL = FILGRUPPER.FILGRUPPE
                                  WHERE KUBER.KUBE_NAVN='",kube,"'",sep=""),stringsAsFactors=FALSE)[1,])
}


KjorStataSkript <- function(TABLE,script,tableTYP="DF",batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  tmpdir<-paste(globs$path,"/",globs$BUFFERdir,"/",sep="")
  wdOrg<-getwd()
  setwd(tmpdir)
  tmpdo<-paste("STATAtmp_",batchdate,".do",sep="")
  tmpdta<-paste("STATAtmp_",batchdate,".dta",sep="")
  tmplog<-paste("STATAtmp_",batchdate,".log",sep="")
  TABLE[TABLE==""]<-" "  #STATA støtter ikke "empty-string"
  names(TABLE)<-gsub("^(\\d.*)$","S_\\1",names(TABLE))   #STATA 14 tåler ikke numeriske kolonnenavn 
  #DEVELOP: STATAPRIKK slå av neste linje om det ikke funker
  names(TABLE)<-gsub("^(.*)\\.([afn])$","\\1_\\2",names(TABLE))   #Endre .a, .f, .n til _
  #DEVELOP: try(write.dta), if error then write.csv. Må da også sette tmpdta<-tmpcsv og evt opsjoner i STATAs use tmpdta
  write.dta(TABLE,tmpdta)
  #file.create(tmpdo,overwrite=TRUE,showWarnings=FALSE)
  sink(tmpdo)
  cat("use ",tmpdta,"\n",sep="")
  cat(script,"\n")
  #cat("save ",tmpdta,", replace\n",sep="")
  if (globs$StataVers<12){
    cat("save ",tmpdta,",  replace\n",sep="")
  } else if (globs$StataVers %in% 12:13) {
    cat("saveold ",tmpdta,", replace\n",sep="")
  } else {
    cat("saveold ",tmpdta,", version(11) replace\n",sep="")
  }
  sink()
  #system(paste("\"",globs$StataExe,"\" /e do",tmpdo,"\n",sep=""),intern=TRUE)
  statacall<-paste("\"",globs$StataExe,"\" /e do ",tmpdo," \n",sep="")
  system(statacall,intern=TRUE)
  #system(paste("\"C:\\Program Files (x86)\\Stata11\\StataSE-64.exe\"","/e do",tmpdo,"\n"),intern=TRUE)
  #system(paste("StataSE-64 /e do",tmpdo,"\n"),intern=TRUE)
  log<-readLines(tmplog)
  feil<-""
  if (log[length(log)]!="end of do-file"){
    log_start<-which(grepl(paste("do",tmpdo),log))
    feil<-paste(log[log_start:length(log)],collapse="\n")                     
  } else {
    TABLE<-read.dta(tmpdta)
  }
  #Reverserer omforminger for å kunne skrive til STATA
  TABLE[TABLE==" "]<-""
  names(TABLE)<-gsub("^S_(\\d.*)$","\\1",names(TABLE))
  #DEVELOP: STATAPRIKK slå av neste linje om det ikke funker
  names(TABLE)<-gsub("^(.*)_([afn])$","\\1.\\2",names(TABLE))   #Endre .a, .f, .n til _
  #file.remove(tmpdo,tmpdta,tmplog)
  setwd(wdOrg)
  if (tableTYP=="DT"){
    setDT(TABLE)
  }
  return(list(TABLE=TABLE,feil=feil))
}


#############################################
SammelignAarganger<-function(globs=FinnGlobs(),aar1=globs$KHaargang,aar2=globs$KHaargang-1,modus="KH"){
  KUBE1<-as.data.table(sqlQuery(globs$dbh,paste("SELECT KUBE_NAVN, VERSJON FROM ",modus,aar1,"_KUBESTATUS",sep=""),stringsAsFactors=F))
  KUBE2<-as.data.table(sqlQuery(globs$dbh,paste("SELECT KUBE_NAVN, VERSJON FROM ",modus,aar2,"_KUBESTATUS",sep=""),stringsAsFactors=F))
  setkey(KUBE1,"KUBE_NAVN")
  setkey(KUBE2,"KUBE_NAVN")
  PAR<-merge(KUBE1,KUBE2,by="KUBE_NAVN",suffixes=c("1","2"))
  setnames(PAR,c("KUBE_NAVN"),c("KUBE_NAVN1"))
  PAR[,KUBE_NAVN2:=KUBE_NAVN1]
  SammenlignKubePar(PAR,modus=modus,globs=globs)
}

SammenlignKubePar<-function(PAR,modus=NA,globs=FinnGlobs()){
  globs<-SettKubedirs(globs,modus=modus)
  for (i in 1:nrow(PAR)){
    paret<-PAR[i,]
    if (grepl("^\\d{4}",paret$VERSJON1) & grepl("^\\d{4}",paret$VERSJON2)){
      cat(paste("Skal merge",paret$KUBE_NAVN1,"\n"))
      KUBE1<-FinnKubeT(paret$KUBE_NAVN1,batch=paret$VERSJON1,globs=globs)
      KUBE2<-FinnKubeT(paret$KUBE_NAVN2,batch=paret$VERSJON2,globs=globs)
      tabs1<-FinnTabKolsKUBE(names(KUBE1))
      tabs2<-FinnTabKolsKUBE(names(KUBE2))
      if (nrow(KUBE1)>0 & nrow(KUBE2) & length(setdiff(tabs1,tabs2))==0){
        setkeyv(KUBE1,tabs1)
        setkeyv(KUBE2,tabs1)
        #VERSJON 1 INNER JOIN
        KOMP<-KUBE2[KUBE1]
        
        #VERSJON 2 FULL OUTER JOIN, RESUUSRKREVENDE!
        #KOMP<-merge(KUBE1,KUBE2,all=TRUE)
        
        #MÅ BAREBERE NED KOLONNNER OG EVT OMDØPE
        
        
        utfil<-paste("F:/Prosjekter/Kommunehelsa/PRODUKSJON/VALIDERING/NESSTAR_KUBER/KH2016v2015/",paret$KUBE_NAVN1,"_",paret$VERSJON1,"v",paret$VERSJON2,".csv",sep="")
        cat(paste("Skriver ut",utfil,"\n"))
        write.table(KOMP,file=utfil,sep=";",row.names=FALSE)
      } else {
        cat("!!!!!! ",paret$VERSJON1,"har ulike kolonner og kan ikke matches")
      }
    }
  }
}




SettKubedirs<-function(globs,modus=NA){
  if (modus=="KH"){
    globs$KubeDir<-globs$KubeDir_KH
    globs$KubeDirNy<-globs$KubeDirNy_KH
    globs$KubeDirDat<-globs$KubeDirDat_KH
    globs$FriskVDir<-globs$FriskVDir_KH
  } else {
    globs$KubeDir<-globs$KubeDir_NH
    globs$KubeDirNy<-globs$KubeDirNy_NH
    globs$KubeDirDat<-globs$KubeDirDat_NH
    globs$FriskVDir<-globs$FriskVDir_NH
  }
  return(globs)
}  



TmpRutineSammenlignKHkuber<-function(kubefilnavn1,kubefilnavn2,KUBENAVN,tabs=character(0),globs=FinnGlobs()){
  
  KUBE1<-as.data.table(read.csv(kube1filnavn,header=TRUE,sep=";"))
  KUBE2<-as.data.table(read.csv(kube2filnavn,header=TRUE,sep=";"))
  
  print(names(KUBE1))
  print(names(KUBE2))
  tabs<-unique(c(globs$FriskvikTabs,tabs))
  
  
  tabs1<-intersect(names(KUBE1),tabs)
  tabs2<-intersect(names(KUBE2),tabs)
    
  print(tabs1)
  print(tabs2)
  KHglobs$DefDesign$DesignKols

  ## Folder to keep the output if not allready there
  currYr <- as.integer(format(Sys.Date(),"%Y"))
  nextYr <- currYr + 1
  foldName <- paste0("Batch", currYr, "vs", nextYr)

  validDir <- file.path(defpaths[1], "VALIDERING/NESSTAR_KUBER", foldName)

  if (isFALSE(fs::dir_exists(validDir)))
    fs::dir_create(validDir)

  
  
  if (nrow(KUBE1)>0 & nrow(KUBE2)>0 & length(setdiff(tabs1,tabs2))==0){
    setkeyv(KUBE1,tabs1)
    setkeyv(KUBE2,tabs1)
    #VERSJON 1 INNER JOIN
    KOMP<-KUBE2[KUBE1]
    
    #VERSJON 2 FULL OUTER JOIN, RESUUSRKREVENDE!
    #KOMP<-merge(KUBE1,KUBE2,all=TRUE)
    
    #MÅ BAREBERE NED KOLONNNER OG EVT OMDØPE
    
    
    utfil<-paste(validDir, KUBENAVN,".csv",sep="")
    cat(paste("Skriver ut",utfil,"\n"))
    write.table(KOMP,file=utfil,sep=";",row.names=FALSE)
  } else {
    cat("!!!!!! tabellene har ulike kolonner og kan ikke matches")
  }
}


#############################################




## KHglobs<-FinnGlobs()


## ---------------------
## Backup filer
## ---------------------

## Backup filer
## ---------------
backup <- function(filename = c("KHfunctions.R", "KHELSA.mdb"), force = FALSE, ...){

  ## force : TRUE hvis man skal arkivere filen uansett ellers
  ## for KHFunction.R sjekkes det dato filen er lagret først

  if (isTRUE(grepl("function", filename))){valgFil <- "fun"}
  if (isTRUE(grepl("KHELSA", filename))){valgFil <- "mdb"}

  ## if (!require(RODBC)) {install.packages("RODBC")}
  ## require(RODBC)
  date<-format(Sys.time(), "%Y%m%d%H%M")

  switch(
    valgFil,

    ## Access Tabell
    ## -------------
    "mdb" = {
      styrpath <- file.path(defpaths[1], "STYRING")
      styrpath_b<-file.path(defpaths[1], "STYRING", "VERSJONSARKIV")
      styrvfiles<-list.files(path=styrpath_b)

      KHcFN<-paste(styrpath, filename, sep="/")
      KHvFN<-paste(styrpath_b,sort(styrvfiles[grepl("^KHELSA\\d+.mdb$",styrvfiles)],decreasing=TRUE)[1],sep="/")

      khc<-RODBC::odbcConnectAccess2007(KHcFN)
      khv<-RODBC::odbcConnectAccess2007(KHvFN)


      nytt<-0
      #Sammenlign
      for (TAB in c("INNLESING","KUBER","TNP_PROD","FILGRUPPER","ORGINNLESkobl","ORIGINALFILER")){
        TABc<-sqlQuery(khc,paste("SELECT * FROM",TAB))
        TABv<-sqlQuery(khv,paste("SELECT * FROM",TAB))
        if (!identical(TABc,TABv)){
          nytt<-1
        }
      }

      if (nytt==1){
        KHnFN<-sub(filename, paste("KHELSA",date,".mdb",sep=""),KHcFN)
        KHnFN<-sub(styrpath,styrpath_b,KHnFN)
        file.copy(KHcFN,KHnFN)
      }

    },

    ## KHFunction
    ## ----------
    "fun" = {

      binpath <- file.path(defpaths[1], "BIN")
      binpath_b <- file.path(defpaths[1], "BIN", "VERSJONSARKIV")
      binvfiles<-list.files(path=binpath_b)

      fil <- "KHfunctions"

      arkivFil <- sort(binvfiles[grepl(paste("^",fil,"\\d+\\.r",sep=""),
                                       binvfiles)],decreasing=TRUE)[1]

      ## Fil i BIN som brukes
      FILc<-paste(binpath, filename, sep="/")

      ## Fil i VERSJONSARKIV
      FILv<-paste(binpath_b, arkivFil, sep="/")

      if (file.info(FILc)$mtime>file.info(FILv)$mtime){
        FILn<-sub(filename,paste(fil, date, ".R", sep=""),FILc)
        FILn<-sub(binpath,binpath_b,FILn)
        file.copy(FILc,FILn)
      } else {

        cat("## --- Filen er ikke nyere enn i akrivet --- ##\n")

      }

      ## Arkiveres uansett
      if (isTRUE(force)) {
        FILn <- paste(binpath_b, "/", fil, date, ".R", sep = "")
        file.copy(FILc, FILn)
      }
    }
  )
}


###############################
## GODKJENT MAPPE
###############################
## inspak <- function(pkg){
##   nypkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
##   if (length(nypkg))
##     install.packages(nypkg, dependencies = TRUE)
## }

## pkg <- c("RODBC", "DBI", "data.table", "glue", "fs", "logger", "magrittr", "stringi")
## sapply(pkg, require, character.only = TRUE)
## ## inspak(pkg)


godkjent <- function(profil = c("FHP", "OVP"),
                     modus = globglobs$KHgeoniv,
                     aar = globglobs$KHaar, ...){

  
  bruker <- Sys.info()[["user"]]
  message("\n********\n  Kopiering av filer for ",
          profil[1], " og geonivå ", modus, " for ",
          aar, " begynner nå. Gjennomført av ", bruker, "\n********\n" )
  
  extra <- list(...)

  ## Get connection to DB
  mdb_file <- file.path(defpaths[1], globglobs$KHdbname)
  conn <- RODBC::odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                                 mdb_file))

  tblCols <- c("PROFILTYPE", "INDIKATOR", "KUBE_NAVN", "MODUS", "AARGANG")
  tblName <- "FRISKVIK"
  sqlFrisk <- glue::glue_sql("SELECT {`tblCols`*}
                      FROM {`tblName`}
                      WHERE {`tblCols[5]`} = {aar}", .con = DBI::ANSI())

  tbl_fsk <- RODBC::sqlQuery(conn, sqlFrisk)

  tblCols <- c("KUBE_NAVN", "VERSJON_PROFILAAR_GEO", "OK_PROFILAAR_GEO")
  tblName <- paste0("KH", aar, "_KUBESTATUS")
  sqlKube <- glue::glue_sql("SELECT {`tblCols`*} from {`tblName`}", .con = DBI::ANSI())

  tbl_kube <- RODBC::sqlQuery(conn, sqlKube)

  invisible(sapply(list(tbl_fsk, tbl_kube), setDT))

  ## merge tabels
  rawAlle <- tbl_fsk[tbl_kube, on = "KUBE_NAVN"]

  ## filter data
  utTYP <- profil[1]

  tblAlle <- rawAlle[PROFILTYPE == utTYP, ] %>%
    .[MODUS == modus, ] %>%
    .[OK_PROFILAAR_GEO == 1, ]


  ## Create filenames
  fileNames <- tblAlle[, filename := paste0(INDIKATOR, "_", VERSJON_PROFILAAR_GEO, ".csv")][["filename"]]

  ## Root folder where the file is
  pathRoot <- defpaths[1]
  
  ## Path for Profile
  pathProfil <- switch(utTYP,
                       "FHP" = c(
                         globglobs$FriskVDir_F,
                         globglobs$FriskVDir_K,
                         globglobs$FriskVDir_B
                       ),
                       "OVP" = c(
                         globglobs$ovpDir_F,
                         globglobs$ovpDir_K,
                         globglobs$ovpDir_B
                       ))
  
  
  ## Geolevels
  modeProfil <- c("F", "K", "B")
  indMode <- grep(modus, modeProfil, ignore.case = TRUE)
  
  ## Get correct path to profil
  pathDir <- pathProfil[indMode]
  
  ## Current date style to create folder
  batchdate<-SettKHBatchDate()

  fileRoot <- paste0(pathRoot, "/", pathDir, aar)
  fileFrom <- file.path(fileRoot, "CSV")
  fileTo <- file.path(fileRoot, "GODKJENT", batchdate)

  ## Check if folder exists else create
  if (!fs::dir_exists(fileTo)) fs::dir_create(fileTo)

  
  ## Group files to those that succeed or fail
  fileOK <- list()
  fileKO <- list()

  for (i in fileNames){

    outFile <- file.path(fileFrom, i)
    inFile <- file.path(fileTo, i)

    outMsg <- tryCatch(
    {fs::file_copy(outFile, inFile, overwrite = TRUE)},
    error = function(err) err)

    if (inherits(outMsg, "error")){
      message("\n --> OPS! Finner ikke filen: ", i, "\n")
      fileKO[i] <- i
      next
    } else {
      message("Kopierer filen: ", i)
      fileOK[i] <- i
    }

  }

  message("\n**********\n", " ",
      length(fileOK),
      " filer ble kopiert til ",
      fileTo, "\n")

  message("----------\n", " ",
      length(fileKO),
      " filer finnes ikke i ",
      fileFrom, "\n**********\n")

}

