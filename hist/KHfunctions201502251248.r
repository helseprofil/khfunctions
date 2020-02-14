require(RODBC)  #Brukes for kommunikasjon med Access-tabeller og lesing av xls/xlsx
require(foreign) #Brukes ved lesing av SPSS, dBF
#require(gdata)  #Brukes ved lesing av xls/xlsx filer
require(sas7bdat) #brukes ved lesing av SAS filer
require(XML)
require(reshape2)  #melt brukes til wide->long
require(zoo)  #na.locf for ? sette inn for NA i innrykket originaltabulering
require(plyr)  #mapvalues for omkoding
require(sqldf)
require(stringr)
require(intervals)
require(data.table) #Bruker data.table for rask merge

#GLOBAL FIXED PARAMETERS, leses bare av SettGlobs, bakes så inn i globs
#Merk at alle elementer angitt i denne lista vil være tilgjengelig i alle hovedrutiner, og evt (mindre robust) i KHglobs
#globsglobs<-list(....)
############################################## ----
globglobs<-list(
  KHaargang=2015,
  KHgeoniv="K",
  KHdbname="STYRING/KHELSA.mdb",
  KHlogg="STYRING/KHlogg.mdb",
  StablaDir="PRODUKTER/MELLOMPROD/R/STABLAORG/",
  StablaDirNy="PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE",
  StablaDirDat="PRODUKTER/MELLOMPROD/R/STABLAORG/DATERT",
  KubeDir="PRODUKTER/KUBER/KOMMUNEHELSA/",
  KubeDirNy="PRODUKTER/KUBER/KOMMUNEHELSA/NYESTE/R/",
  KubeDirDat="PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/R/",
  FriskVDir="PRODUKTER/KUBER/FRISKVIK_KOMM/",
  TNPDirNy="PRODUKTER/MELLOMPROD/R/TNP/NYESTE",
  TNPDirDat="PRODUKTER/MELLOMPROD/R/TNP/DATERT",  
  kolorgs=c("GEO","AAR","KJONN","ALDER","UTDANN","SIVST","LANDBAK","TAB1","TAB2","TAB3","VAL1","VAL2","VAL3"),
  taborgs=c("GEO","AAR","KJONN","ALDER","TAB1","TAB2","TAB3"),
  FriskvikKols=c("GEO","AAR","KJONN","ALDER","ETAB","sumTELLER","sumNEVNER","RATE","MALTALL","sumPREDTELLER","PREDTELLER","SMR","NORM","MEIS","RATE.n"),
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
SettGlobs<-function(path="",gibeskjed=FALSE) {
  #Setter globale parametre
  #Disse er faste over hver kjøring og endres normalt bare ved systemoppdatering/-migrering
  #Merk at globs$dbh ikke lukkes av seg selv, dermed kan det bli rot med gamle slike om FinnGlobs brukes for mye
  #Bruk evy odbcCloseAll() for å rydde absolutt alle 
    
  #Les globglobs (se topp av fil)
  globs<-globglobs
  KHdbname<-globs$KHdbname
  #Sett path om denne ikker er oppgitt:
  #Brukte pather under utvikling (NB: prioritert rekkefølge under)
  defpaths<-c("F:/Prosjekter/Kommunehelsa/PRODUKSJON",
              "F:/Prosjekter/Kommunehelsa/Data og databehandling/kbDEV",
              "J:/FHI/PRODUKSJON",
              "J:/kbDEV")
  
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
    #Sys.getenv("R_ARCH")   gir "/x64"eller "/i386" 
    KHOc<-odbcConnectAccess2007(paste(path,globs$KHdbname,sep="/"))
    #KHOc<-odbcConnectAccess(paste(path,KHdbname,sep="/"))
    KHLc<-odbcConnectAccess2007(paste(path,globs$KHlogg,sep="/"))
  }
  globs<-c(globs,list(dbh=KHOc,log=KHLc,path=path))
  
  GeoNavn<-data.table(sqlQuery(KHOc,"SELECT * from GeoNavn",as.is=TRUE))
  GeoKoder<-data.table(sqlQuery(KHOc,"SELECT * from GEOKoder",as.is=TRUE),key=c("GEO"))
  KnrHarm<-data.table(sqlQuery(KHOc,"SELECT * from KnrHarm",as.is=TRUE),key=c("GEO"))
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
  
  return(c(globs,list(GeoNavn=GeoNavn,GeoKoder=GeoKoder,KnrHarm=KnrHarm,GkBHarm=GkBHarm)))
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
LagFilgruppe<-function(gruppe,batchdate=SettKHBatchDate(),globs=FinnGlobs(),diagnose=0,printR=TRUE,printCSV=FALSE,printSTATA=FALSE,versjonert=FALSE){
  #Essensielt bare loop over alle delfiler/orignalfiler
  #For hver orignalfil kjøres LagTabellFraFil
  #Stables til tabellen FG
  #Finn filgruppeparametre
  FGP<-FinnFilgruppeParametre(gruppe,batchdate=batchdate,globs=globs)
 
  #Initier tom tabell
  Filgruppe<-data.frame()
  
  #Rydd gammel logg
  sqlQuery(globs$log,paste("DELETE * FROM KODEBOK_LOGG WHERE FILGRUPPE='",gruppe,"' AND SV='S'",sep=""))
  sqlQuery(globs$log,paste("DELETE * FROM INNLES_LOGG WHERE FILGRUPPE='",gruppe,"' AND SV='S'",sep=""))
  #Finn parameterbeskrivelse av delfilene
  delfiler<-FinnFilBeskGruppe(gruppe,batchdate=batchdate,globs=globs)
  if(nrow(delfiler)>0){
    for (i in 1:nrow(delfiler)){
      filbesk<-delfiler[i,]
      tm<-proc.time()
      filbesk$filn<-paste(globs$path,filbesk$FILNAVN,sep="/")
      filbesk$filn<-gsub("\\\\","/",filbesk$filn)
      #Sett evt default for år basert på aktuelt årstall
      filbesk$AAR<-gsub("<\\$y>",paste("<",filbesk$DEFAAR,">",sep=""),filbesk$AAR)
      #LagTabell
      DF<-LagTabellFraFil(filbesk,FGP,batchdate=batchdate,diagnose=diagnose,globs=globs,versjonert=versjonert)
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
  
  if (!is.na(FGP$RSYNT_PRE_FGLAGRING)){
    FGP$RSYNT_PRE_FGLAGRING<-gsub("\\\r","\\\n",FGP$RSYNT_PRE_FGLAGRING)
    rsynt1err<-try(eval(parse(text=FGP$RSYNT_PRE_FGLAGRING)),silent=TRUE)
    if(class(rsynt1err)=="try-error"){
      print(rsynt1err)
      ok<-0
    } 
  }
  
  
  
  #Datostempel
  sqlQuery(globs$dbh,paste("UPDATE FILGRUPPER SET PRODDATO='",format(Sys.time(), "%Y-%m-%d %X"),"' WHERE FILGRUPPE='",gruppe,"'",sep=""))  
  
  #SKRIV RESULTAT    
  path<-globs$path
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
#Slett dette, lag heller prosedyre for å konvertere (hele?) treet
#   if (printCSV){
#     utfiln<-paste(path,"/",stbdir,"/CSV/",gruppe,".csv",sep="")
#     write.table(Filgruppe,file=utfiln,sep=";",row.names=FALSE)
#   } 
#   if (printSTATA){
#     utfiln<-paste(path,"/",stbdir,"/STATA/",gruppe,".dta",sep="")
#     write.dta(Filgruppe,file=utfiln)
#   }
  
  return(Filgruppe)
}

#
LagFlereFilgrupper<-function(filgrupper=character(0),batchdate=SettKHBatchDate(),globs=FinnGlobs(),printR=TRUE,printCSV=FALSE,printSTATA=FALSE,versjonert=FALSE){
  #SKall rundt LagFilGruppe, lager og lagrer evt til fil
  #Default er å ta alle grupper, ellers angis ønsket batch i filgrupper-argumentet 
  if (length(filgrupper)==0){
    #filgrupper<-as.matrix(sqlQuery(globs$dbh,"SELECT DISTINCT Filgruppe from INNLESING WHERE Bruk=1",as.is=TRUE))
    filgrupper<-as.matrix(sqlQuery(globs$dbh,"SELECT DISTINCT Filgruppe from FILGRUPPER",as.is=TRUE))
  } 
  
  #HOVEDLOOP
  for (gruppe in filgrupper){
    FG<-LagFilgruppe(gruppe,batchdate=batchdate,globs=globs,versjonert=versjonert) 
  }
}

#
LagTabellFraFil<-function (filbesk,FGP,batchdate=SettKHBatchDate(),diagnose=0,globs=FinnGlobs(),versjonert=FALSE,echo=FALSE) {
  
  klokke<-proc.time()
  ######################################################
  #INNLESING
  filn<-filbesk$filn
  cat("\n#################\nLAGER TABELL FRA FIL:\n",filn,"\n")
  LestFil<-LesFil(filbesk,batchdate=batchdate,globs=globs)
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
    #if (!(is.na(filbesk$RESHAPEid) || filbesk$RESHAPEid=='')){
    if (!(is.na(filbesk$RESHAPEvar) || filbesk$RESHAPEvar=='')){
      rshpDF<-ReshapeTab(DF,filbesk,batchdate=batchdate,globs=globs)
      DF<-rshpDF$DF
      ok<-min(ok,rshpDF$ok)
      #cat("\nETTER RESHAPE\n#############################\n")
      #print(head(DF))
    }
    
    
    TilFilLogg(filbesk$KOBLID,"RESHAPEh",DFHeadToString(DF),batchdate=batchdate,globs=globs)    
  }
  if (ok==1){
        
    #Må splitte evt kolonne fra MULTIHEAD
    if (!is.na(filbesk$MULTIHEAD)){
      mhl<-LesMultiHead(filbesk$MULTIHEAD)
      DF[,mhl$colnames]<-str_split_fixed(DF[,mhl$varname],mhl$sep,2)
    }
    #EVT SPESIALBEHANDLING2
    if (!is.na(filbesk$RSYNT2)){
      filbesk$RSYNT2<-gsub("\\\r","\\\n",filbesk$RSYNT2)
      rsynt2err<-try(eval(parse(text=filbesk$RSYNT2)))
      if(class(rsynt2err)=="try-error"){
        TilFilLogg(filbesk$KOBLID,"RSYNT2ERR",rsynt2err,batchdate=batchdate,globs=globs)
        ok<-0
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
  sqlQuery(globs$log,paste("DELETE * FROM KODEBOK_LOGG WHERE KOBLID=",filbesk$KOBLID,sep=""))
  if (ok==1){
    colClass<-sapply(DF,class)
    if (any(colClass!="character")){
      cat("Advarsel! Kolonnene ",names(DF)[colClass!="character"]," er ikke character (",colClass[colClass!="character"],")\n",sep="")
      DF[,colClass!="character"]<-as.character(DF[,colClass!="character"])
    }
    DF[is.na(DF)]<-""
    
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
    
    #VASK AV TABx
    for (tab in c("TAB1","TAB2","TAB3")){
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
LesFil<-function (filbesk,batchdate=SettKHBatchDate(),globs=FinnGlobs()) {
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
    
    
    #DEV dette b?r v?re un?dvendig '' skal v?re lest inn som NA
    #DF[DF==""]<-NA
    #des<-lapply(DF,class)
    #if(length(des[des=="factor"])>0){
    #  cat("FACTOR i DF <",paste(names(des[des=="factor"]),collapse="><"),">, det er ugreit\n",sep="")
    #}
    
    
    TilFilLogg(filbesk$KOBLID,"modINNLESh",DFHeadToString(DF),batchdate=batchdate,globs=globs)
    
    ######################################################
    #EVT SPESIALBEHANDLING
    if (!is.na(filbesk$RSYNT1)){
      filbesk$RSYNT1<-gsub("\\\r","\\\n",filbesk$RSYNT1)
      rsynt1err<-try(eval(parse(text=filbesk$RSYNT1)),silent=TRUE)
      if(class(rsynt1err)=="try-error"){
        print(rsynt1err)
        TilFilLogg(filbesk$KOBLID,"RSYNT1ERR",rsynt1err,batchdate=batchdate,globs=globs)
        ok<-0
      } 
    }
  }
  #sink(file=paste(globs$path,"/hoder.txt",sep=""),append=TRUE)
  #cat("\n#################\nFIL: ")
  #cat(filn)
  #cat("\n")
  #print(head(T))
  #sink()
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
    csvT<-read.csv(filn,header=FALSE,skip=0,colClasses="character",sep=sep,quote=quote,dec=dec,fill=TRUE,encoding=encoding,blank.lines.skip=FALSE,na.strings=na.strings)
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
    #Bruk defaultnavn i celler der header mangler
    nonempty<-as.vector(which(DF[1,]!=""))
    names(DF)[nonempty]<-DF[1,nonempty]
    DF<-DF[-1,]
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
Xls2R.KH<-function(xlsfil,ark="",globs=FinnGlobs(),brukfread=TRUE,na.strings=c("NA"),ryddOpp=1,...){
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
      DELF<-melt(DELF,id.vars=idvars,measure.vars=mevars,variable.name=varname,value.name=valname,na.rm=FALSE)      
    } else {
      DELF<-melt(DELF,id.vars=idvars,variable.name=varname,value.name=valname,na.rm=FALSE)
    }
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
        subsant<-rbind(subsant,data.frame(ORG=KBsub$ORGKODE,KBOMK=paste("<",KBsub$NYKODE,">",sep=""),OMK=paste("<",KBsub$NYKODE,">",sep=""),FREQ=length(grepl(KBsub$ORGKODE,omk)),OK=1))
      }
      #omk<-sub(eval(parse(text=KBsub$ORGKODE)),eval(parse(text=KBsub$NYKODE)),omk)
      omk<-sub(KBsub$ORGKODE,KBsub$NYKODE,omk)
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
  geo$GEOniv[nchar(geo$OMK)==2]<-"F"
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
  geo$FYLKE[geo$GEOniv=="L"]<-"00"
  
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
  landbak$OMK[!(landbak$OMK %in% c(0,1,2,3,4,5,6,7,8,9,"-"))]<-globs$landbak_illeg
  landbak$OK[!(landbak$OMK %in% c(0,1,2,3,4,5,6,7,8,9,"-"))]<-0
  
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
  FGP<-as.list(sqlQuery(globs$dbh,paste("SELECT * FROM FILGRUPPER WHERE FILGRUPPE='",gruppe,"'",
                                        "AND VERSJONFRA<=",datef," AND VERSJONTIL>", datef,"
                                        ",sep=""),as.is=TRUE))
  #Sette endelig default alder ALLE
  #Default er 0_ALDinf    der ALDinf er global parameter i HOVEDPARAMETRE
  amin<-0
  amax<-as.numeric(sqlQuery(dbh,"SELECT ALDinf FROM HOVEDPARAMETRE")[1])
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
  
  return(c(FGP,list(vals=vals,amin=amin,amax=amax))) 
}

#
FinnFilBeskGruppe<-function(filgruppe,batchdate=NULL,globs=FinnGlobs()){
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
  return(fb)
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

LagFlereKuber<-function(KUBEidA,versjonert=FALSE,csvcopy=FALSE,globs=FinnGlobs()){
  batchdate=SettKHBatchDate()
  cat("BATCH:",batchdate,"\n")
  for (KUBEid in KUBEidA){
    KK<-LagKUBE(KUBEid,batchdate=batchdate,versjonert=versjonert,csvcopy=csvcopy,globs=globs)
  }
}

LagKubeDatertCsv<-function(KUBEID){
  invisible(LagFlereKuber(KUBEID,versjonert=TRUE,csvcopy=TRUE))
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
  
  #Denne er ikke så veldig robust for feilspesifkasjon, men den brukes ikke annet til de enkleste tilfellene
  PredFilter<-SettPredFilter(KUBEdscr$REFVERDI,globs=globs)
  
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
  FGPs<-list()
  FilDesL<-list()
  for (fil in unique(filer)){
    #Legg i BUFFER
    FGPs[[fil]]<-FinnFilgruppeParametre(fil,batchdate=batchdate,globs=globs)
    FilDesL[[fil]]<-FinnDesign(FinnFilT(fil,globs=globs),FGP=FGPs[[fil]],globs=globs)
    #FilDesL[[fil]]<-FinnDesign(FinnFilT(fil,batch=batchdate,globs=globs),FGP=FGPs[[fil]],globs=globs)  
  }
  return(list(KUBEdscr=KUBEdscr,TNPdscr=TNPdscr,filer=filer,PredFilter=PredFilter,STNPdscr=STNPdscr,FGPs=FGPs,FilDesL=FilDesL))
}

LagKUBE<-function(KUBEid,lagRapport=0,batchdate=SettKHBatchDate(),versjonert=FALSE,bare_TN=0,drop_TN=0,tmpbryt=0,FullUt=0,csvcopy=FALSE,globs=FinnGlobs(),echo=0){
  datef<-format(strptime(batchdate, "%Y-%m-%d-%H-%M"),"#%Y-%m-%d#")
  rapport<-list(KUBE=KUBEid,lagRapport=lagRapport)
  
  #Les inn nødvendig informasjon om filene involvert (skilt ut i egen funksjon for lesbarhet)
  ##################################################
  Finfo<-SettFilInfoKUBE(KUBEid,batchdate=batchdate,versjonert=versjonert,globs=globs)
  KUBEdscr<-Finfo$KUBEdscr
  TNPdscr<-Finfo$TNPdscr
  filer<-Finfo$filer
  PredFilter<-Finfo$PredFilter
  STNPdscr<-Finfo$STNPdscr
  FGPs<-Finfo$FGPs
  FilDesL<-Finfo$FilDesL
  
  #Sett Tab-filter
  #For å redusere ressursbruk er det viktig at lange lister med unødvendige ETAB blir barert bort tidlig
  TabConds<-character(0)
  TabFSub=""
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
  }
  TabFSub<-paste(TabConds,collapse=" & ")
   
  # TRINN 1 LAG TNF
  ####################################################
  if (drop_TN==0){
  
    cat("LAGER TNF\n")
    TNF<-LagTNtabell(filer,FilDesL,FGPs,TNPdscr,KUBEdscr=KUBEdscr,TabFSub=TabFSub,rapport=rapport,globs=globs)
    if(TNPdscr$NEVNERKOL!="-"){
      TNF<-LeggTilNyeVerdiKolonner(TNF,"RATE={TELLER/NEVNER}")
    }
    if(echo==1){
      cat("TNF:\n")
      print(TNF)
    }
    cat("FERDIG TNF\n")
  }
  
  if (bare_TN==1){
    RESULTAT=list(KUBE=TNF,TNPdscr=TNPdscr)
  }
  # Prediker referanseverdi om dette er etterspurt
  if (KUBEdscr$REFVERDI_VP=="P"  && bare_TN==0){
    #Finn design fra full merge av PT, PN, P
    print("PREDIKER!!!")
    #Må først finne design for (den syntetiske) koblinga ST, SN og PN
    ################################################
    #Design til s-TN er lik design til ST
    if (is.na(filer["SN"])){
      FT_TNd<-FilDesL[[filer["ST"]]]      
    } else  {
      #Finn felles tab for ST og SN, gir
      FT_TNf<-FinnFellesTab(FilDesL[[filer["ST"]]],FilDesL[[filer["SN"]]])
      FT_TN<-subset(FT_TNf$OmkF,DEKK1==1 & DEKK2==1)
      #Designet til S-TN
      FT_TNd<-FinnDesign(FT_TN,FGP=FGPs[[filer["ST"]]])  #Er det helt sikkert at FGP=FGPs[[filer[ST]]] er rett?
    }
    
    print("TRINN 1 FT_TN")
    #return(list(T=FilDesL[[filer["ST"]]],N=FilDesL[[filer["SN"]]],FT=FT_TNf))
    
    #Filtrer med PredFilter og fjern PredFilter dim (er det mer effektivt å gjøre dette på ST og SN før fellestab for disse?)
    
    ModDes<-ModifiserDesign(PredFilter$Design,FT_TNd,globs=globs)
    print("HAR ModDes")
    FT_TN<-subset(FinnRedesign(FT_TNd,ModDes,globs=globs)$TilDekk,Dekk==1)
    print(dim(FT_TN))
    print("HAR FT_TN etter mod")
    predKols<-PredFilter$Pkols
    FT_TN<-FT_TN[,!names(FT_TN) %in% predKols,with=FALSE]
    #Gjør om til design. Dette er nå design til beste ST-SN kobling (filtrert med PredFilter, slik at f.eks bare de aldersgrupper som kan lages på landsnivå er med)
    FT_TNd<-FinnDesign(FT_TN,FGP=FGPs[[filer["ST"]]])
    print("HAR FT_TNd bare PN igjen")
    #Finn felles tab med PN også
    FT_TNP_FT<-FinnFellesTab(FT_TNd,FilDesL[[filer["PN"]]])
    print(FT_TNd$OmkDesign)
    print(FilDesL[[filer["PN"]]]$OmkDesign)
    print(FT_TNP_FT)
    FT_TNP<-subset(FT_TNP_FT$OmkF,DEKK1==1 & DEKK2==1)
    
    
    FT_TNPd<-FinnDesign(FT_TNP,FGP=FGPs[[filer["PN"]]])
    FT_TNPd<-ModifiserDesign(PredFilter$Design,FT_TNPd,globs=globs)
    #FT_TNPd er nå det ferdige felles designet ST-SN-PN, som brukes først for STN og så for STNP
    
    print("DESIGN dilldall ferdig")
    
    STN<-LagTNtabell(filer,FilDesL,FGPs,STNPdscr,TT="ST",NN="SN",Design=FT_TNPd,TabFSub=TabFSub,rapport=rapport,globs=globs)
    STNc<-STN
    STN<-STN[,!names(STN) %in% predKols,with=FALSE]
    if(echo==1){
      cat("STNc:\n")
      print(STNc)
    }
    
    #SETT RATE HER er mest effektivt!
    STN[!(NEVNER==0 & NEVNER.f==0),c("PREDRATE","PREDRATE.f","PREDRATE.a"):=list(TELLER/NEVNER,pmax(TELLER.f,NEVNER.f),pmax(TELLER.a,NEVNER.a))]
    STN[NEVNER==0 & NEVNER.f==0,c("PREDRATE","PREDRATE.f","PREDRATE.a"):=list(0,pmax(TELLER.f,2),pmax(TELLER.a,NEVNER.a))]
    #MÅ JUKSE DET TIL LITT MED NEVNER 0. Bruken her er jo slik at dette er tomme celler, og ikke minst vil raten nesten garantert skulle ganges med et PREDTELLER=0
    #Tillater TELLER<=2 for å unngå evt numeriske problemer. Virker helt uskyldig gitt bruken
    STN[TELLER<=2 & TELLER.f==0 & NEVNER==0 & NEVNER.f==0,c("PREDRATE","PREDRATE.f","PREDRATE.a"):=list(0,0,pmax(TELLER.a,NEVNER.a))]
    
    print("FERDIG med STN")
   
    #Uklart hvorfor må kopiere her, men får uønsket warning ellers
    rapport["TRINN"]<-"PN"
    PN<-FinnFilT(filer["PN"],globs=globs)
    
    PNdscr<-sqlQuery(globs$dbh,paste("SELECT * FROM TNP_PROD WHERE TNP_NAVN='",filer["PN"],"' AND VERSJONFRA<=",datef," AND VERSJONTIL>",datef,sep=""),as.is=TRUE)  
    
    if (nrow(PNdscr)>0){
      if (!(is.na(PNdscr$NYEKOL_RAD) | PNdscr$NYEKOL_RAD=="")){
        PN<-LeggTilSumFraRader(PN,PNdscr$NYEKOL_RAD,FGP=FGPs[[filer["PN"]]],globs=globs)
      }
      if (!(is.na(PNdscr$NYEKOL_KOL) | PNdscr$NYEKOL_KOL=="")){
        PN<-LeggTilNyeVerdiKolonner(PN,PNdscr$NYEKOL_KOL)
      }
    }
    
    PN<-GeoHarm(PN,vals=FGPs[[filer["PN"]]]$vals,globs=globs)
   
    
    #print("UBAL")
    #print(subset(STN,PREDRATE.f>0))
    #print(subset(PN,(BEF.f>0 |TOTBEF.f>0) & GEOniv!="B"))
    print("SKAL SY SAMMEN TIL STNP, DETTE ER RESSURSKNEIKA!!")
    #STN blir sjef, så alle ST-rater får en PN-nevner, men ikke nødvendigvis omvendt. Hvis det ikke er en rate er denne implisitt 0.
    STNP<-SySammenTabeller(STN,PN,SJEF=1,FGP1=FGPs[[filer["ST"]]],FGP2=FGPs[[filer["PN"]]],rapport=rapport,globs=globs)
    
    print("STNP")
    #print(tables())
    #Sett PredNevnerKol
    if (!(is.na(TNPdscr$PREDNEVNERFIL) | TNPdscr$PREDNEVNERFIL=="")){
      PredNevnerKol<-gsub("^(.*):(.*)","\\2",TNPdscr$PREDNEVNERFIL)
    } else {
      PredNevnerKol<-TNPdscr$NEVNERKOL
    }
    STNPnames<-gsub(paste("^",PredNevnerKol,"(\\.f|\\.a|)$",sep=""),"PREDNEVNER\\1",names(STNP))
    setnames(STNP,names(STNP),STNPnames)
    STNP[,c("PREDTELLER","PREDTELLER.f","PREDTELLER.a"):=list(PREDRATE*PREDNEVNER,pmax(PREDRATE.f,PREDNEVNER.f),pmax(PREDRATE.a,PREDNEVNER.a))]
    #Kast overflødige kolonner
    desKols<-unlist(KHglobs$DefDesign$DelKolsF)
    desKols<-desKols[desKols %in% names(STNP)]
    print("STNP FERDIG")
    raaSTNP<-STNP
    
    STNP<-STNP[,c(desKols,"PREDTELLER","PREDTELLER.f","PREDTELLER.a"),with=FALSE]
    gc()
    #STNPc<-STNP
    if(echo==1){
      cat("STNPc:\n")
      print(STNP)      
    }
   
    
    #print(tables())
    rapport["TRINN"]<-"STNP"
    
    #print("NB FJERN MEG!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    #Det følgende er essensielt bare
    #KUBE<-SySammenTabeller(TNF,STNP,SJEF=1,FGP1=FGPs[[filer["ST"]]],FGP2=FGPs[[filer["PN"]]],SkalAggregeresOpp2=globs$DefDesign$AggVedStand,rapport=rapport,globs=globs)
    #men må tas her for å unngå kopieringa osv av STNP siden denne kan være svært stor
    
    #Dette blir dermed en nedstrippa variant av SysammenFiler, OmkodFIl og KHaggreger
    ##############################
    #Finn kodebok. NB: Aggregeres opp for standardisering!
    STNPd<-FinnDesign(STNP,FGP=FGPs[[filer["ST"]]])
    TNFd<-FinnDesign(TNF,FGP=FGPs[[filer[["T"]]]])
    RD<-FinnRedesign(STNPd,TNFd,SkalAggregeresOpp=globs$DefDesign$AggVedStand,globs=globs)
   
    #RD<-FinnRedesign(STNPd,TNFd,SkalAggregeresOpp=c("A","K"),globs=globs)
    KB<-subset(RD$FULL,PRId==0)
    omkcols<-names(KB)[grepl("_omk$",names(KB))]
    orgcols<-gsub("(.*)_omk$","\\1",omkcols)
    KB<-KB[,c(omkcols,orgcols),with=FALSE]
    #Merge
    setkeyv(STNP,orgcols)
    setkeyv(KB,orgcols)
    print("SISTE store merge, med den er stor")
    STNP<-KB[STNP,nomatch=0,allow.cartesian=TRUE]
    #Omkod geo
    STNP[GEOniv_omk=="K",GEO:=substr(GEO,0,4)]
    STNP[GEOniv_omk=="F",GEO:=FYLKE]
    STNP[GEOniv_omk=="L",c("GEO","FYLKE"):=list("0","00")]
    STNP[STNP$GEOniv_omk=="B" & STNP$GEOniv=="S" & !grepl("^(0301|1103|1201|1601)",STNP$GEO),c("GEO","FYLKE"):=list("0","00")]
    
    #Kast overflødige kolonner og omdøp omkcols tilbake til orgcols
    STNP[,eval(parse(text=paste("c(\"",paste(orgcols,collapse="\",\""),"\")",sep=""))):=NULL]
    setnames(STNP,omkcols,orgcols)
    cat("Ferdig, det ble dim STNP",dim(STNP),"\n")
    #Aggreger (alt er nødvendigvis sumerbart, og bare PRE)
    valkols<-FinnValKols(names(STNP))
    tabnames<-setdiff(names(STNP),FinnValKolsF(names(STNP)))
    STNP<-STNP[,list(PREDTELLER=sum(PREDTELLER),PREDTELLER.f=max(PREDTELLER.f),PREDTELLER.a=sum(PREDTELLER.a)),by=tabnames]
    
    
    #Merge STNP på TNF
    setkeyv(STNP,tabnames)
    setkeyv(TNF,tabnames)
    
    
    KUBE<-STNP[TNF]
    KUBE<-SettMergeNAs(KUBE) 
    ############  Ferdig med tilsvarende SySammenTabeller
    
  } 
  else {
    KUBE<-TNF
  }
  
  
  
  if (bare_TN==0){
    
    #Finn "snitt" for ma-år. 
    #DVs, egentlig lages forløpig bare summer, snitt settes etter prikking under
    #Snitt tolerer missing av type .f=1 ("random"), men bare noen få anonyme .f>1, se KHaggreger
    #Rapporterer variabelspesifikk VAL.n som angir antall år brukt i summen når NA holdt utenom
    ###################################################################3
   
    
    maprefix<-""  
    if (KUBEdscr$MOVAV>1){
      ma<-KUBEdscr$MOVAV
      maprefix<-paste("ma",ma,sep="")
      #Må "balansere" NA i teller og nevner slik sumrate og sumnevner balanserer  (Bedre/enklere å gjøre det her enn i KHaggreger)
      KUBE[TELLER.f>0,c("NEVNER","NEVNER.f"):=list(NA,2)]
      maKUBE<-FinnSnittOverAar(KUBE,ma=ma,globs=globs)
      #sett rate på nytt
      if(TNPdscr$NEVNERKOL!="-"){
        maRate<-paste(maprefix,"RATE={ma",ma,"TELLER/ma",ma,"NEVNER}",sep="")
        maKUBE<-LeggTilNyeVerdiKolonner(maKUBE,maRate)
      }
     
      #maKUBE<-maKUBE[,names(maKUBE)[!grepl(".n$",names(maKUBE))],with=FALSE]  #Kast .n kolonner, ferdig med disse
      KUBE<-maKUBE
      
      #Opsjon for ekstarrapport som tarmed også årlige tall, ikke bare snitt
      #Denen funker ikke særlig bra lenger og veldikeholdes ikke mht det som skjer under
      if (FullUt==1){
        tabkols<-setdiff(intersect(names(KUBE),KHglobs$DefDesign$DesignKolsFA),"AARl")
        setkeyv(KUBE,tabkols)
        setkeyv(maKUBE,tabkols)
        KUBE<-maKUBE[KUBE]
        tkols<-setdiff(names(KUBE),c("AARl","AARl.1"))
        KUBE<-KUBE[,c("AARl",tkols),with=FALSE]
      } 
    } else {
      #Må legge til VAL.n=1 for regning under
      valkols<-FinnValKols(names(KUBE))
      lp<-paste("KUBE[,c(\"",paste(valkols,".n",collapse="\",\"",sep=""),"\"):=list(1)]",sep="")
      KUBE[,eval(parse(text=lp))]
    }
    
    #Fjerner maprefix siden dette skaper unødvendig mye krøll med eval(parse) om TELLER etc er paste(maprefix,"TELLER")
    setnames(KUBE,names(KUBE),gsub(paste("^",maprefix,"(.*)",sep=""),"\\1",names(KUBE)))
    
    
    #Anonymiser og skjul
    ##################################################################
    
    #Anonymiser, trinn 1 Filtrer snitt som ikke skal brukes pga for mye anonymt
    #Se KHaggreger!
    raaKUBE<-copy(KUBE)
    
    #Anonymiser, trinn 2 Ekte anonymisering basert på liten teller, liten nevner og liten N-T
    if (!(is.na(KUBEdscr$PRIKK_T) | KUBEdscr$PRIKK_T=="")){
      #T<=PRIKK_T
      cat("T-PRIKKER",nrow(subset(KUBE,TELLER<=KUBEdscr$PRIKK_T)),"rader\n")
      KUBE[TELLER<=KUBEdscr$PRIKK_T,c("TELLER","TELLER.f","RATE","RATE.f"):=list(NA,3,NA,3)]
      #N-T<=PRIKK_T
      cat("N-T-PRIKKER",nrow(subset(KUBE,NEVNER-TELLER<=KUBEdscr$PRIKK_T)),"rader\n")
      KUBE[NEVNER-TELLER<=KUBEdscr$PRIKK_T,c("TELLER","TELLER.f","RATE","RATE.f"):=list(NA,3,NA,3)]    
    } 
    if (!(is.na(KUBEdscr$PRIKK_N) | KUBEdscr$PRIKK_N=="")){
      #N<PRIKK_N
      cat("N-PRIKKER",nrow(subset(KUBE,NEVNER<=KUBEdscr$PRIKK_N)),"rader\n")
      KUBE[NEVNER<=KUBEdscr$PRIKK_N,c("TELLER","TELLER.f","RATE","RATE.f"):=list(NA,3,NA,3)]
    }
   
    #Anonymiser trinn 3. Anonymiser naboer
    if (!(is.na(KUBEdscr$OVERKAT_ANO) | KUBEdscr$OVERKAT_ANO=="")){
      #DEVELOP: BRuk .f=4 her slik at ikke slår ut i HULL under
      KUBE<-AnonymiserNaboer(KUBE,KUBEdscr$OVERKAT_ANO,FGP=FGPs[[filer[["T"]]]],globs=globs)
    }
    
    raaKUBE2<-copy(KUBE)
    
    #Anonymiser trinn 4. Skjule svake og skjeve tidsserrier
    SvakAndelAvSerieGrense<-0.5
    HullAndelAvSerieGrense<-0.2
    
    if (!(is.na(KUBEdscr$STATTOL_T) | KUBEdscr$STATTOL_T=="")){
      tabkols<-setdiff(intersect(names(KUBE),globs$DefDesign$DesignKolsFA),c(globs$DefDesign$DelKols[["Y"]]))
      KUBE[,AntAar:=.N,by=tabkols]
      KUBE[,SVAK:=sum(is.na(TELLER) | TELLER<=KUBEdscr$STATTOL_T),by=tabkols]
      KUBE[,HULL:=sum(TELLER.f==3),by=tabkols]
      KUBE[,SKJUL:=ifelse(SVAK/AntAar>SvakAndelAvSerieGrense | HULL/AntAar>HullAndelAvSerieGrense,1,0)]
      
      cat("Skjuler",nrow(subset(KUBE,SKJUL==1)),"rader\n")
      KUBE[SKJUL==1,c("TELLER","TELLER.f"):=list(NA,3)]
      KUBE[SKJUL==1,c("RATE","RATE.f"):=list(NA,3)]
      KUBE[,c("SVAK","HULL","SKJUL","AntAar"):=NULL]
    }
    raaKUBE3<-copy(KUBE)
    
   
    ####################################################
    # LAYOUT
    ####################################################3
    
    
    #UTFØR EVT AD-HOC MANIPULERING
    if (!(is.na(KUBEdscr$SLUTTREDIGER) | KUBEdscr$SLUTTREDIGER=="")){
      KUBEdscr$SLUTTREDIGER<-gsub("\\\r","\\\n",KUBEdscr$SLUTTREDIGER)
      rsynt1err<-try(eval(parse(text=KUBEdscr$SLUTTREDIGER)),silent=TRUE)
      if(class(rsynt1err)=="try-error"){
        print(rsynt1err)
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
    
    #Sett skala for teller
    if (!(is.na(KUBEdscr$RATESKALA) | KUBEdscr$RATESKALA=="")){
      KUBE[,RATE:=RATE*as.numeric(KUBEdscr$RATESKALA)]
    }
    
    #Legg til manglende kolonner for homogen behandling under
    missKol<-setdiff(unlist(lapply(c("TELLER","NEVNER","RATE","PREDTELLER"),function(x){paste(x,c("",".f",".a",".n"),sep="")})),names(KUBE))
    if (length(missKol)>0){
      KUBE[,(missKol):=NA]
    }
    
    #Behold sum, disse sendes til Friskvik
    KUBE[,sumTELLER:=TELLER]
    KUBE[,sumNEVNER:=NEVNER]
    KUBE[,sumPREDTELLER:=PREDTELLER]
    
    #Ta snitt for alt annet enn RATE (der forholdstallet gjør snitt uønsket)
    #VAL:=VAL/VAL.n
    valkols<-setdiff(FinnValKols(names(KUBE)),c("NEVNER","RATE"))
    if (length(valkols)>0){
      lp<-paste("KUBE[,c(\"",paste(valkols,collapse="\",\""),"\"):=list(",
              paste(valkols,"=",valkols,"/",valkols,".n",
                    sep="",collapse=","),
              ")]",sep="")
      KUBE[,eval(parse(text=lp))]
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
    
    #SETT SMRtmp. For å lage NORMSMR under må denne settes før NORM. Derfor kan jeg ikke sette SMR=MALTALL/NORM nå. 
    #Men NORMSMR er selvsagt alltid 100 for REFVERDI_P="V"  
    if (KUBEdscr$REFVERDI_VP=="P"){
      KUBE[,SMRtmp:=TELLER/PREDTELLER*100]
    } else if (KUBEdscr$REFVERDI_VP=="V"){
      KUBE[,SMRtmp:=100]
    } else {
      KUBE[,SMRtmp:=NA]
    }
    
   
    #FINN "LANDSNORMAL". Merk at dette gjelder både ved REFVERDI_VP=P og =V  
    ########################################################################
    
    VF<-eval(parse(text=paste("subset(KUBE,",PredFilter$PfiltStr,")",sep="")))  
    #Evt hvis en eller flere element i PredFilter ikke er med i Design for TNF og må lages
    if(nrow(VF)==0){
      cat("************************************\nNOE RART MED LANDSNORM, IKKE I KUBEDESIGN, MÅ UT PÅ NY OMKODING.\nER DETTE RETT?\n")
      VF<-OmkodFilFraPart(TNF,PredFilter$Design,FGP=FGPs[[filer["T"]]],globs=globs)
    }
    
    
    VFtabkols<-setdiff(intersect(names(VF),globs$DefDesign$DesignKolsFA),PredFilter$Pkols)
    if (maltall %in% c("TELLER","RATE")){
      setnames(VF,c(paste(maltall,c("",".f",".a",".n"),sep=""),"SMRtmp"),c(paste("NORM",c("",".f",".a",".n"),sep=""),"NORMSMR"))
      VF<-VF[,c(VFtabkols,paste("NORM",c("",".f",".a",".n"),sep=""),"NORMSMR"),with=FALSE]
    } else {
      setnames(VF,c(maltall,"SMRtmp"),c("NORM","NORMSMR"))
      VF<-VF[,c(VFtabkols,"NORM","NORMSMR"),with=FALSE]
    }
    
    setkeyv(KUBE,VFtabkols)
    setkeyv(VF,VFtabkols)
      
    KUBE<-VF[KUBE]
    
    #Juster SMR proporsjonalt slik at NORM (landet) alltid har SMR=100
    #SMR>100 kan oppstå dersom det f.eks. er noen med ukjent alder/kjønn. 
    #Ratene for ukjent alder/kjønn vil ikke matche nevner fra BEF, derfor vil det predikeres for får døde relativt til observert
    if (KUBEdscr$REFVERDI_VP=="P"){
      KUBE[,SMR:=TELLER/PREDTELLER*100]
    } else if (KUBEdscr$REFVERDI_VP=="V"){
      KUBE[,SMR:=MALTALL/NORM*100]
    } else {
      KUBE[,SMR:=NA]
    }
    
    KUBE[,SMR:=100*(SMR/NORMSMR)]
    
    KUBE[,MEIS:=SMR*NORM/100]
    
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
    NesstarOutputDef<-c(MT="MALTALL",T="TELLER",N="NEVNER",RATE="RATE",SMR="SMR",MEIS="MEIS",ST="sumTELLER",SN="sumNEVNER")
    OutVar<-NesstarOutputDef[NstarTup]
  
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
    
    
    #LAYOUT
    utkols<-c(tabs,OutVar)
    NESSTAR<-copy(KUBE)
    #SKJUL HELE TUPPELET  
    #FLAGG PER VARIABEL KAN/BØR VURDERES!
    fvars<-intersect(names(NESSTAR),c(OrgKubeKolNames[grepl(",f$",OrgKubeKolNames)],"NORM.f","SMR.f"))
    NESSTAR[,SPVFLAGG:=0]
    cat("ASKLJDKLASKLDJLASKJDLK FVARS", fvars, "|",OrgKubeKolNames,"\n")
    if (length(fvars)>0){
      NESSTAR[,SPVFLAGG:=eval(parse(text=paste("pmax(",paste(fvars,collapse=","),")",sep="")))]
      NESSTAR[SPVFLAGG>0,eval(parse(text=paste("c(\"",paste(OutVar,collapse="\",\""),"\"):=list(NA)",sep="")))]
    }
    
    #Filtrer bort uønskede tabs
    #Dette er helt ad hoc nå, ma parametriseres mot KHkodebok/LegKoder
    NESSTAR<-NESSTAR[!grepl("99|9900$",GEO),]
    if ("ALDER" %in% names(KUBE)){
      NESSTAR<-NESSTAR[!ALDER %in% c("999_999","888_888"),]
    } 
    if ("KJONN" %in% names(NESSTAR)){
      NESSTAR<-NESSTAR[!KJONN %in% c(8,9),]
    }
    
    LagAlleFriskvikIndikatorerForKube(KUBEid=KUBEid,KUBE=NESSTAR,aargang=globs$KHaargang,FGP=FGP[["T"]],versjonert=versjonert,batchdate=batchdate,globs=globs)
    
    NESSTAR<-NESSTAR[,c(utkols,"SPVFLAGG"),with=FALSE]
    
    
    if (tmpbryt==1){
      print("TMPBRYT")
      return(list(raaKUBE=raaKUBE,raaKUBE2=raaKUBE2,raaKUBE3=raaKUBE3,KUBE=KUBE))
    }
    
  
    #Ad hoc redigering
    ##################################################################
    
    #FRISKVIK
    ################################################################
    
    
    #RESULTAT<-(list(TNF=TNF,STN=STNc,STNP=STNP,KUBE=KUBE))
    RESULTAT<-list(KUBE=NESSTAR)
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
      cat("Kopierer datert",utfild, "jasdkljasl",globs$KubeDirDat,"\n")
      file.copy(utfiln,utfild)
      if (csvcopy==TRUE){
        utfild<-gsub("(.*)/R/(.*)","\\1/csv/\\2",utfild)
        utfild<-gsub("(.*)\\.rds$","\\1.csv",utfild)
        print(utfild)
        write.table(NESSTAR,file=utfild,sep=';',row.names = FALSE)
      } 
    }
  }
  cat("-------------------------KUBE",KUBEid,"FERDIG--------------------------------------\n")
  return(RESULTAT)
  

}


LagAlleFriskvikIndikatorerForKube<-function(KUBEid,aargang,globs=FinnGlobs(),...){
  indikatorer<-unlist(sqlQuery(globs$dbh,paste("SELECT INDIKATOR FROM FRISKVIK",aargang," WHERE KUBE_NAVN='",KUBEid,"'",sep=""),as.is=TRUE))
  if (length(indikatorer)>0){
    for (indikator in indikatorer){
      cat("Lager Friskvikfil for ",indikator,"\n")
      LagFriskvikIndikator(indikator=indikator,aargang=aargang,globs=globs,...)
    }
  }
}

LagFriskvikIndikator<-function(indikator,KUBE=data.table(),FGP=FGP,GEOfilter=c("K","F","L"),versjonert=FALSE,aargang=format(Sys.time(), "%Y"),batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  #FVdscr<-sqlQuery(globs$dbh,paste("SELECT * FROM ",aargang," WHERE INDIKATOR='",indikator,"' AND VERSJONFRA<=",datef," AND VERSJONTIL>",datef,sep=""),as.is=TRUE)
  FVdscr<-sqlQuery(globs$dbh,paste("SELECT * FROM FRISKVIK",aargang," WHERE INDIKATOR='",indikator,"'",sep=""),as.is=TRUE)
  
  
  #FILTRER RADER
  filterA<-"(GEOniv %in% GEOfilter)"
  if (grepl("\\S",FVdscr$ALDER) & FVdscr$ALDER !="-"){
    FVdscr$ALDER<-gsub("\\(d+)_$",paste("\\1_",FGP$amax,sep=""),FVdscr$ALDER)
    FVdscr$ALDER<-gsub("^_(\\d+)$",paste(FGP$amin,"_\\1",sep=""),FVdscr$ALDER)
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
  
  MissKol2<-setdiff(globs$FriskvikKols,names(FRISKVIK))
  if (length(MissKol2)>0) {
    KHerr(paste("FEIL: Kolonnene", MissKol2, "mangler i Friskvik!"))
    FRISKVIK[,(MissKol2):=NA]
  }
  FRISKVIK<-FRISKVIK[,globs$FriskvikKols,with=FALSE]
  
  #SKRIV UT
  if (versjonert==TRUE){
    #utfiln<-paste(globs$path,"/",globs$FriskVDir,aargang,"/stata/",indikator,"_",batchdate,".dta",sep="")
    utfiln<-paste(globs$path,"/",globs$FriskVDir,aargang,"/csv/",indikator,"_",batchdate,".csv",sep="")
    cat("FRISKVIK EKSPORT:",utfiln,"\n")
    write.table(FRISKVIK,file=utfiln,sep=';',row.names = FALSE)
    #write.dta(FRISKVIK,file=utfiln)
  }
}


FinnSumOverAar<-function(KUBE,per=0,AntYMiss=0,na.rm=FALSE,globs=FinnGlobs()){
  UT<-KUBE[0,]
  tabs<-setdiff(FinnTabKols(names(KUBE)),c("AARl","AARh"))
  valkols<-FinnValKols(names(KUBE))
  #Utrykk for KH-aggregering (med hjelpestørrelses for snitt)
  lpv<-paste(valkols,"=sum(",valkols,",na.rm=",na.rm,"),",
            valkols,".f=max(",valkols,".f),",
            valkols,".a=sum(",valkols,".a*(!is.na(",valkols,") & ",valkols,"!=0))",
            sep="",collapse=",")
  lps<-paste(valkols,".fn1=sum(",valkols,".f==1),",
             valkols,".fn3=sum(",valkols,".f>1),",
             valkols,".n=.N",
             sep="",collapse=",")
  if (snitt==TRUE) {
    lpv<-paste(lpv,lps,sep=",")
    lpsvars<-paste(valkols,c(".fn1",".fn3",".n"),sep="")
    UT[,(lpsvars):=NA]
  }
  aara<-unique(KUBE$AARh)
  aara<-intersect((min(aara)+per-1):max(aara),aara)
  cat("Tar snitt for ")
  for (aar in aara){
    cat(aar," ")
    lp<-paste("list(AARl=",aar-per+1,",AARh=",aar,",",lpv,")",sep="")
    UT<-rbind(UT,KUBE[AARh %in% c((aar-per+1):aar),eval(parse(text=lp)), by=tabs][,names(UT),with=FALSE])
  }
  #KHaggrege
  cat("\n")
  #RYDD na.rm
  
  
  return(UT)
}

# #Anonymiser, trinn 1
# #Filtrer snitt som ikke skal brukes pga for mye anonymt
# anon_tot_tol<-0.2
# lp<-paste("FILa[,':='(",
#           paste(valkols,"=ifelse(",valkols,".fn3/",valkols,".n>=",anon_tot_tol,",NA,",valkols,"),",
#                 valkols,".f=ifelse(",valkols,".fn3/",valkols,".n>=",anon_tot_tol,",3,",valkols,".f)",
#                 sep="",collapse=","),
#           ")]",sep="")
# eval(parse(text=lp))   


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
SettNaboAnoSpec<-function(ovkatstr,FGP=list(amin=0,amax=120),globs=FinnGlobs()){
  overkat<-list()
  if (!(is.null(ovkatstr) || is.na(ovkatstr))){
    ovkatstr<-gsub("([^=]+)=([^=]+)","\\1==\\2",ovkatstr)
    ovkatstr<-gsub("(.*)ALDER=='*ALLE'*(.*)",paste("\\1","ALDER==",FGP$amin,"_",FGP$amax,"\\2",sep=""),ovkatstr)
    ovkatstr<-gsub("(.*)ALDER=='*(\\d+)_('| )(.*)",paste("\\1","ALDER==\\2_",FGP$amax,"\\3\\4",sep=""),ovkatstr)
    for (del in names(globs$DefDesign$DelKolN)){
      delN<-globs$DefDesign$DelKolN[del]
      if (globs$DefDesign$DelType[del]=="COL"){
        if (grepl(paste("(^|\\&) *",delN," *== *'*(.*?)'* *(\\&|$)",sep=""),ovkatstr)){
          over<-gsub(paste(".*(^|\\&) *(",delN," *== *'*.*?)'* *(\\&|$).*",sep=""),"\\2",ovkatstr)
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
  }
  return(overkat)  
}

AnonymiserNaboer<-function(FG,ovkatstr,FGP=list(amin=0,amax=120),globs=FinnGlobs()){
  FG<-copy(FG)
  overkats<-SettNaboAnoSpec(ovkatstr,FGP=FGP,globs=globs)
  vals<-FinnValKols(names(FG))
  alletabs<-setdiff(names(FG),FinnValKolsF(names(FG)))
  for (val in vals){
    eval(parse(text=paste(
      "FG[,",val,".na:=0]",sep=""
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
          "FG[!(",substr,"),",val,".na:=ifelse((",val,".na==1 | any(",val,".f==3)),1,0),by=bycols]",sep=""
        )))
      }
      #FG[substr,VAL.na:=ifelse(any(VAL.f==3),1,0),by=setdiff(alletabs,overtabs)]
    }
  }
  for (val in vals){
    eval(parse(text=paste(
      "FG[",val,".na==1,",val,".f:=3]",sep=""
    )))
    eval(parse(text=paste(
      "FG[,",val,".na:=NULL]",sep=""
    )))
  }
  return(FG)
}

RektangulariserKUBE<-function(orgnames,KubeD,vals=list(),batchdate=SettKHBatchDate(),GEOstdAAR=substr(batchdate,1,4),globs=FinnGlobs()){
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
    GEOK<-subset(globs$GeoKoder,FRA<=GEOstdAAR & TIL>GEOstdAAR & GEOniv==Gn)$GEO
    DELERg<-subset(DELER,GEOniv==Gn)
    REKT<-rbind(data.table(expand.grid.df(data.frame(DELERg),data.frame(GEO=GEOK))),REKT)
  }
  return(REKT)
}



LagTNtabell<-function(filer,FilDesL,FGPs,TNPdscr,TT="T",NN="N",Design=NULL,KUBEdscr=NULL,TabFSub="",rapport=list(),globs=FinnGlobs()){
  KubeD<-list()
  
  #ALT 1/3: Nevner er i tellerfil, trenger ikke sammsying
  #######################################################
  if (is.na(filer[NN])){
    #EVT REDESIGN
    rapport["TRINN"]<-TT
    TTFil<-FinnFilT(filer[TT],globs=globs)
    
    if (TabFSub!=""){
      TTFil<-eval(parse(text=paste("subset(TTFil,",TabFSub,")",sep="")))
    }
    if (is.null(KUBEdscr) & is.null(Design)){
      TNF<-TTFil
    } else if (is.null(KUBEdscr)){
      TNF<-OmkodFilFraDesign(TTFil,Design,FGP=FGPs[[filer[TT]]],rapport=rapport,globs=globs)
    } else {
      KubeD<-FinnKubeDesignB(KUBEdscr,FilDesL[[filer[TT]]],FGP=FGPs[[filer[TT]]],globs=globs)
      TNF<-OmkodFilFraPart(TTFil,KubeD$TMP,FGP=FGPs[[filer[TT]]],rapport=rapport,globs=globs)
    }
    
    
    #Nye kolonner fra kolonner og rader, geoharm og Tabfilter
    if (!(is.na(TNPdscr$NYEKOL_KOLpreRAD) | TNPdscr$NYEKOL_KOLpreRAD=="")){
      TNF<-LeggTilNyeVerdiKolonner(TNF,TNPdscr$NYEKOL_KOLpreRAD,slettInf=TRUE)
    }
    TNF<-GeoHarm(TNF,vals=FGPs[[filer[TT]]]$vals,globs=globs)
    if (!(is.na(TNPdscr$NYETAB) |TNPdscr$NYETAB=="")){
      TNF<-AggregerRader(TNF,TNPdscr$NYETAB,FGP=FGPs[[filer[TT]]])
    }
    if (!(is.na(TNPdscr$NYEKOL_RAD) | TNPdscr$NYEKOL_RAD=="")){
      FGPtnf<-FGPs[[filer[TT]]]
      FGPtnf$vals<-c(FGPs[[filer[TT]]]$vals,FGPs[[filer[NN]]]$vals)
      TNF<-LeggTilSumFraRader(TNF,TNPdscr$NYEKOL_RAD,FGP=FGPs[[filer[TT]]],globs=globs)
    }
    if (!(is.na(TNPdscr$NYEKOL_KOL) | TNPdscr$NYEKOL_KOL=="")){
      TNF<-LeggTilNyeVerdiKolonner(TNF,TNPdscr$NYEKOL_KOL,slettInf=TRUE)
    }
    if (length(KubeD)>0){
      TNF<-FiltrerTab(TNF,KubeD$MAIN,globs=globs)
    }
          
  } 
  #ALT 2/3: "Standard" sammsying av T og N til TNF
  #######################################################
  else if (!TNPdscr$MERGESJEF %in% c(TT,NN)){
    KubeD<-list()
    #Underalt 1/3: Fri sammsying uten eksterne designføringer. 
    #Finner best mulige felles design (jfr FinnFellesTab), dette kan være vilkårlig ubalansert
    #Designelement som ikke støttes fra begge sider faller bort
    print("TN 2/3")
    if (is.null(KUBEdscr) & is.null(Design)){
      rapport["F1"]<-TT
      rapport["F2"]<-NN
      #Geoharmonisering, inkl GEO-rektangularisering 
      #(merk at om det trengs å gjøres kolonneoperasjoner før radoperasjoner slik som GeoHarm, må man bruke MERGESJEF (hovvedalt 3 under))
      TTFil<-FinnFilT(filer[TT],globs=globs)
      if (TabFSub!=""){
        TTFil<-eval(parse(text=paste("subset(TTFil,",TabFSub,")",sep="")))
      }
      TF<-GeoHarm(TTFil,vals=FGPs[[filer[TT]]]$vals,globs=globs)
      NF<-GeoHarm(FinnFilT(filer[NN],globs=globs),vals=FGPs[[filer[TT]]]$vals,globs=globs)
      
      #Sysammen med bruk av FinnFellesTab
      TNF<-SySammenTabeller(TF,NF,SJEF=0,FGP1=FGPs[[filer[TT]]],FGP2=FGPs[[filer[NN]]],rapport=rapport,globs=globs) 
      #Utgått?: Sett tabfilter til KubeD for filtrering under
      #if (length(TabF)>0){
      #  KubeD<-list(MAIN=TabF)
      #}
    } 
    else {
      
      #Underalt 2/3: Omkoding til (felles) KubeD  
      if (!is.null(KUBEdscr)){
        #Finn "Initialt design" for TN slik at KubeD også kan settes fra default der uspesifisert
        FTab<-FinnFellesTab(FilDesL[[filer[NN]]],FilDesL[[filer[TT]]])
        FTab<-subset(FTab$OmkF,DEKK1==1 & DEKK2==1)
        TNdesT<-FinnDesign(FTab,FGP=FGPs[[filer[TT]]],globs=globs) #Er det helt sikkert at FGP=FGPs[[filer[TT]]] er rett?
        #Merk: TAB1 etc forsvinner ved FinnFellesTab, så TNdesT og KubeDuT er uten disse 
        #Må legge til disse i TNdes og så finne KubeD
        KubeDuT<-FinnKubeDesign(KUBEdscr,TNdesT,FGP=FGPs[[filer[TT]]],globs=globs) #Er det helt sikkert at FGP=FGPs[[filer[TT]]] er rett?
        TNdes<-TNdesT
        TNdes$Part<-c(TNdes$Part,FilDesL[[filer[TT]]]$Part[FilDesL[[filer[TT]]]$BetingF],FilDesL[[filer[NN]]]$Part[FilDesL[[filer[NN]]]$BetingF])
        KubeD<-FinnKubeDesignB(KUBEdscr,TNdes,FGP=FGPs[[filer[TT]]],globs=globs)      
        #Omkod filer til KubeDuT (kan ikke bruke KubeD her)
        rapport["TRINN"]<-TT
        TTFil<-FinnFilT(filer[TT],globs=globs)
        if (TabFSub!=""){
          TTFil<-eval(parse(text=paste("subset(TTFil,",TabFSub,")",sep="")))
        }
        TF<-OmkodFilFraPart(TTFil,KubeDuT,FGP=FGPs[[filer[TT]]],rapport=rapport,globs=globs)
        TF<-FiltrerTab(TF,KubeD$TMP,globs=globs)   #Må bare bruke KubeD$TMP fordi kolonnelaging ikke er ferdig
        rapport["TRINN"]<-NN
        NF<-OmkodFilFraPart(FinnFilT(filer[NN],globs=globs),KubeDuT,FGP=FGPs[[filer[NN]]],rapport=rapport,globs=globs)
        NF<-FiltrerTab(NF,KubeD$TMP,globs=globs)
      } 
      #Underalt 3/3: Omkoding til (felles) ekstern gitt Design  
      else if (!is.null(Design)){
        print("UNDERALT 3/3, HER BURDE JEG VEL TABFILTRE!!!!!!!!!!!!!!!!!!!!!!!!!")
        rapport["TRINN"]<-TT
        TTFil<-FinnFilT(filer[TT],globs=globs)
        if (TabFSub!=""){
          TTFil<-eval(parse(text=paste("subset(TTFil,",TabFSub,")",sep="")))
        }
        TF<-OmkodFilFraDesign(TTFil,Design,FGP=FGPs[[filer[TT]]],rapport=rapport,globs=globs)
        rapport["TRINN"]<-NN
        NF<-OmkodFilFraDesign(FinnFilT(filer[NN],globs=globs),Design,FGP=FGPs[[filer[NN]]],rapport=rapport,globs=globs)        
      } 
      else {
        print("HER SKAL DET VÆRE UMULIG Å VÆRE!!!!!!!!!!!!!!")  #Tas med her for å hjelpe lesing av litt krøkkete logiske forgreninger 
      }
      #Felles for underalt 2 og 3
      
      #Geoharmonisering, inkl GEO-rektangularisering 
      #(merk at om det trengs å gjøres kolonneoperasjoner før radoperasjoner slik som GeoHarm, må man bruke MERGESJEF (hovvedalt 3 under))
      TF<-GeoHarm(TF,vals=FGPs[[filer[TT]]]$vals,globs=globs)
      NF<-GeoHarm(NF,vals=FGPs[[filer[TT]]]$vals,globs=globs)
      if (!(is.na(TNPdscr$NYETAB) |TNPdscr$NYETAB=="")){
        TF<-AggregerRader(TF,TNPdscr$NYETAB,FGP=FGPs[[filer[TT]]])
      }
      
      
      #Merge av T og N til TNF
      if (length(KubeD)>0){
        KubeDRekt<-RektangulariserKUBE(names(TF),KubeD$TMP,globs=globs)
        setkeyv(KubeDRekt,intersect(names(KubeDRekt),names(TF)))
        setkeyv(TF,intersect(names(KubeDRekt),names(TF)))
        TF<-TF[KubeDRekt]
        setkeyv(KubeDRekt,intersect(names(KubeDRekt),names(NF)))
        setkeyv(NF,intersect(names(KubeDRekt),names(NF)))
        NF<-NF[KubeDRekt]
        setkeyv(TF,intersect(names(TF),names(NF)))
        setkeyv(NF,intersect(names(TF),names(NF)))
        TNF<-TF[NF]
        cat("REktangularisering, dim KUBEd", dim(KubeDRekt),"dim TF", dim(TF), "dim NF", dim(NF), "dim TNF", dim(TNF),"\n")
      } else {
        kolsT<-unlist(globs$DefDesign$DelKolsF[FilDesL[[filer[TT]]]$OmkDeler])
        kolsN<-unlist(globs$DefDesign$DelKolsF[FilDesL[[filer[NN]]]$OmkDeler])
        kols<-intersect(kolsT,kolsN)
        setkeyv(TF,kols)
        setkeyv(NF,kols)
        #Trenger ikke ta hensyn til implisitte null når KubeD=NULL, derfor bare join TF->NF
        TNF<-NF[TF]
        
        #TNF<-merge(TF,NF,by=kols,all=TRUE,suffixes=c(1:2))
        #UTGÅTTTTT når ikke full join
        ####################################
        #Merk at TF og NF ikke trenger å være rektangularisert på alle kolonner. Dette er ikke ønskelig generelt 
        #siden filer med mange implisitte nuller kan bli blåst veldig opp (særlig med lange TAB1 lister slik som i KUHR).
        #Ved bruk av KUBEd kunen dette vært påført og endog ønkskelig, men trenger også å kunne brukes generelt.
        #Det følgende er et kompromiss. 
        #Dersom f.eks. 1988 tall fro 10 åringer mangler i DÅR designet (for alle)
        
        #Merk at TAB1 etc er holdt utenom og ikke trenger balansere!!!
        #Må Sjekke mismatch på designkolonner som ikke er i begge, og krysse disse med missing
        unikKols<-unique(setdiff(names(TF),names(NF)))
        unikKols<-intersect(unikKols,globs$DefDesign$DesignKolsF)
        for (kol in unikKols){
          MM<-eval(parse(text=paste("TNF[is.na(",kol,"),names(TNF)!=kol,with=FALSE]",sep="")))
          if (nrow(MM)>0){
            cat("%%%%%%%%%%%%%%%%%%%%%%%\n I MISMATCH!!!!!!!! DET ER FEIL NÅ!!!!\n%%%%%%%%%%%%%%%%%%%%%%%%%\n")
            eval(parse(text=paste("TNF<-TNF[!is.na(",kol,"),]",sep="")))
            print(MM)
            MM<-setDT(expand.grid.df(as.data.frame(MM),as.data.frame(unique(TNF[,kol,with=FALSE]))))
            MM<-MM[,names(TNF),with=FALSE]
            print(MM)
            TNF<-rbind(TNF,MM)
          }
        }
        #################################3#UNØDVENDIG HIT????
      }     
      TNF<-SettMergeNAs(TNF,c(FGPs[[filer[TT]]]$vals,FGPs[[filer[NN]]]$vals)) 
    }
    #Felles for underalt 1,2,3: Lag nye kolonner fra kolonner og rader
    
    
    #NYEKOL_KOLpreRAD kan vel ikke brukes meningsfullt her?
    #if (!(is.na(TNPdscr$NYEKOL_KOLpreRAD) | TNPdscr$NYEKOL_KOLpreRAD=="")){
    #  TNF<-LeggTilNyeVerdiKolonner(TNF,TNPdscr$NYEKOL_KOLpreRAD,slettInf=TRUE)
    #}
   
    if (!(is.na(TNPdscr$NYEKOL_RAD) | TNPdscr$NYEKOL_RAD=="")){
      FGPtnf<-FGPs[[filer[TT]]]
      FGPtnf$vals<-c(FGPs[[filer[TT]]]$vals,FGPs[[filer[NN]]]$vals)
      TNF<-LeggTilSumFraRader(TNF,TNPdscr$NYEKOL_RAD,FGP=FGPs[[filer[TT]]],globs=globs)
    }
    #GEOHARM???????
    if (!(is.na(TNPdscr$NYEKOL_KOL) | TNPdscr$NYEKOL_KOL=="")){
      TNF<-LeggTilNyeVerdiKolonner(TNF,TNPdscr$NYEKOL_KOL,slettInf=TRUE)
    }
    #Endelig filtrering, trenger ikke lenger de ekstra elementene i DEL0
    TNF<-FiltrerTab(TNF,KubeD$MAIN,globs=globs)
  }
  #Alternativ 3/3. En fil er sjef for merge. 
  #Brukes primært (bare) der det er behov for å lage kolonner fra andre kolonner før 
  #redesign, geoharm og andre radoperasjoner
  ##########################################################################################
  else if (TNPdscr$MERGESJEF %in% c(TT,NN)){
    #Sy sammen med sjef
    sjefTN<-TNPdscr$MERGESJEF
    if (sjefTN==TT){
      rapport["TRINN"]<-NN
      TNF<-SySammenTabeller(FinnFilT(filer[TT],globs=globs),FinnFilT(filer[NN],globs=globs),SJEF=1,FGP1=FGPs[[filer[TT]]],FGP2=FGPs[[filer[NN]]],rapport=rapport,globs=globs) 
    } else {
      rapport["TRINN"]<-TT
      TNF<-SySammenTabeller(FinnFilT(filer[NN],globs=globs),FinnFilT(filer[TT],globs=globs),SJEF=1,FGP1=FGPs[[filer[NN]]],FGP2=FGPs[[filer[TT]]],rapport=rapport,globs=globs)  
    } 
    
    #Lag nye kolonner fra kolonner
    if (!(is.na(TNPdscr$NYEKOL_KOLpreRAD) | TNPdscr$NYEKOL_KOLpreRAD=="")){
      TNF<-LeggTilNyeVerdiKolonner(TNF,TNPdscr$NYEKOL_KOLpreRAD,slettInf=TRUE)
    }
    #Nå først kan radoperasjonen GEO-harm og redesign tas
    TNF<-GeoHarm(TNF,vals=c(FGPs[[filer[TT]]]$vals,FGPs[[filer[NN]]]$vals),globs=globs)
      rapport["TRINN"]<-"TNF"
    if (!is.null(KUBEdscr)){
      TNdes<-FinnDesign(TNF,FGP=FGPs[[filer[TT]]],globs=globs)
      KubeD<-FinnKubeDesignB(KUBEdscr,TNdes,FGP=FGPs[[filer[TT]]],globs=globs)
      TNF<-OmkodFilFraPart(TNF,KubeD$TMP,FGP=FGPs[[filer[TT]]],rapport=rapport,globs=globs)
    } else if (!is.null(Design)){
      TNF<-OmkodFilFraDesign(TNF,Design,FGP=FGPs[[filer[TT]]],rapport=rapport,globs=globs)
    }
    #Lag nye kolonner fra rader
    if (!(is.na(TNPdscr$NYEKOL_RAD) | TNPdscr$NYEKOL_RAD=="")){
      FGPtnf<-FGPs[[filer[TT]]]
      FGPtnf$vals<-c(FGPs[[filer[TT]]]$vals,FGPs[[filer[NN]]]$vals)
      TNF<-LeggTilSumFraRader(TNF,TNPdscr$NYEKOL_RAD,FGP=FGPs[[filer[TT]]],globs=globs)
    } 
    if (length(KubeD)>0){
      NF<-FiltrerTab(NF,KubeD$MAIN,globs=globs)
    }
  }
  
  #Siste felles trinn for alle
  #SETT TELLER OG NEVNER navn
  TNnames<-names(TNF)
  TNnames<-gsub(paste("^",TNPdscr$TELLERKOL,"(\\.f|\\.a|)$",sep=""),"TELLER\\1",TNnames)
  TNnames<-gsub(paste("^",TNPdscr$NEVNERKOL,"(\\.f|\\.a|)$",sep=""),"NEVNER\\1",TNnames)
  #NEVNERKOL=='-' gir TELLER->MALTALL ??
  setnames(TNF,names(TNF),TNnames)
  
  return (TNF)
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


FiltrerTabGml<-function(FT,KubeD,globs=FinnGlobs()){
  print("FILTRER TAB")
  #print(unique(FT$TAB1))
  print(KubeD)
  for (TABd in names(KubeD)[grepl("^T\\d$",names(KubeD))]){
    TABn<-globs$DefDesign$DelKolN[TABd]
    if (TABn %in% names(FT)){
      print(paste("subset(FT,",TABn," %in% unlist(KubeD[['",TABd,"']]))",sep=""))
      print(unlist(KubeD[[TABd]]))
      FT<-eval(parse(text=paste("subset(FT,",TABn," %in% unlist(KubeD[['",TABd,"']]))",sep="")))
    }      
  }
  #print(unique(FT$TAB1))
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
        setkeyv(listDT,key(ORGd$Part[[del]]))
        if (minus==TRUE){
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


FinnFilN<-function(filstr,batch=NA,TYP="STABLAORG",globs=FinnGlobs()){
  fila<-unlist(str_split(filstr,":"))
  if (length(fila)==1){
    fila<-c(TYP,fila)
  }
  if(is.na(batch)){
    filn<-paste(globs$path,"/PRODUKTER/MELLOMPROD/R/",fila[1],"/NYESTE/",fila[2],".rds",sep="")
  } else {
    filn<-paste(globs$path,"/PRODUKTER/MELLOMPROD/R/",fila[1],"/DATERT/",fila[2],"_",batch,".rds",sep="")
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
  return(list(filn=filn,ok=ok,err=err))
}

FinnFilT<-function(filstr,batch=NA,ROLLE="",TYP="STABLAORG",IDKOLS=FALSE,globs=FinnGlobs()){
  #BYGG UT MED MAPPEFUNKSJONALITET
  FILID<-filstr
  filn<-FinnFilN(filstr,batch=batch,globs=globs)
  FT<-data.frame()
  if (filn$ok==0){
    print(filn$err)
  } else {
    if (exists("BUFFER") && FILID %in% names(BUFFER)){
      FT<-BUFFER[[FILID]]
    } else {
      FT<-readRDS_KH(filn$filn,IDKOLS=IDKOLS)
    }
    cat("Lest inn ", ROLLE,"FIL ", FILID," (",dim(FT)[1]," x ",dim(FT)[2],")\n",sep="")
  }
  return(as.data.table(FT))
}


FinnKubeT<-function(fila,batch=NA,TYP="STABLAORG",globs=FinnGlobs()){
  if(is.na(batch)){
    filn<-paste(globs$path,"/",globs$KubeDirNy,fila,".rds",sep="")  
  } else {
    filn<-paste(globs$path,"/",globs$KubeDirDat,fila,batch,".rds",sep="")
  }
  print(filn)
  KUBE<-readRDS(filn)
  return(KUBE)
}


GeoHarm<-function(FIL,vals=list(),rektiser=TRUE,FDesign=list(),batchdate=SettKHBatchDate(),GEOstdAAR=substr(batchdate,1,4),globs=FinnGlobs()){
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
 
  
  FIL[,FYLKE:=ifelse(GEOniv=="L","00",substr(GEO,1,2))]
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


FinnFellesTab<-function(DF1,DF2,SkalAggregeresOpp1=character(),SkalAggregeresOpp2=character(),globs=FinnGlobs(),echo=0){
  #Diff<-union(setdiff(names(DF1$Part),names(DF2$Part)),setdiff(names(DF2$Part),names(DF1$Part)))
  omkDel1<-names(DF1$Part)[names(DF1$Part) %in% globs$DefDesign$OmkDel]
  omkDel2<-names(DF2$Part)[names(DF2$Part) %in% globs$DefDesign$OmkDel]
  FellesDel<-intersect(omkDel1,omkDel2)
  
  ok<-1
  lengst<-1
  if (length(omkDel1)>=length(omkDel2)){
    ekstDel<-setdiff(omkDel1,omkDel2)
    DF1m<-DF1
    DF2m<-DF2
  } else {
    ekstDel<-setdiff(omkDel2,omkDel1)
    DF1m<-DF2
    DF2m<-DF1
    lengst<-2
  } 
  if (length(setdiff(omkDel1,omkDel2))>0 & length(setdiff(omkDel2,omkDel1))>0){
    ok<-0
  }
  
  
  if (ok==1){
    
    
    #Finn alle (unike) kombinasjoner i "DF1m union DF2m"
    #Trenger bare gjøre det for PART$Y,PART$A og OmkDesign
    DFF1m<-list()
    #OmkDesignF<-data.frame()
    for (del in FellesDel){
      DFF1m$Part[[del]]<-unique(rbind(DF1m$Part[[del]],DF2m$Part[[del]]))
      setkeym(DFF1m$Part[[del]],key(DF1m$Part[[del]]))
      #OmkDesignF<-expand.grid.df(as.data.frame(DF1m$Part[[del]]),OmkDesignF)
    }
    delerlist<-paste("as.data.frame(DFF1m$Part[[\"",FellesDel,"\"]])",sep="",collapse=",")
    OmkDesignF<-data.table(eval(parse(text=paste("expand.grid.df(",delerlist,")",sep=""))))
    #Rydd i HAR
    harcols<-names(OmkDesignF)[grepl("(|_)HAR(|1|2)$",names(OmkDesignF))]
    OmkDesignF[,(harcols):=NULL]
    OmkDesignF[,HAR:=1]  #Forsøk å kode til alle
    
    DFF1m$OmkDesign<-OmkDesignF
    DFF2m<-copy(DFF1m)
    
    #Må krysse fellestab som bare er i DF1m før med ekstratab for å få full balanse 
    #Dette er riktignok bare nødvendig for å få full tabulering også av de kombinasjoner som ikke dekkes av begge
    #OmkDesignekst<-as.data.frame(DF2m$OmkDesign)
    print(ekstDel)
    if (length(ekstDel)>0){
      
      OmkDesignekst<-as.data.frame(DF2m$OmkDesign)
      for (del in ekstDel){
        DFF1m$Part[[del]]<-DF1m$Part[[del]]
      }
      delerlist<-paste("as.data.frame(OmkDesignF),",paste("as.data.frame(DFF1m$Part[[\"",ekstDel,"\"]])",sep="",collapse=","),sep="")
      OmkDesignF<-data.table(eval(parse(text=paste("expand.grid.df(",delerlist,")",sep=""))))
      
#       setDT(OmkDesignekst)
#       print(OmkDesignekst)
#       comtabs<-setdiff(intersect(names(OmkDesignekst),names(OmkDesignF)),"HAR")
#       print(comtabs)
#       setkeym(OmkDesignekst,comtabs)      
#       setkeym(OmkDesignF,comtabs)      
#       OmkDesignF<-merge(DFF1m$OmkDesign,OmkDesignekst,by=comtabs,all=TRUE,suffixes=c(1:2),allow.cartesian=TRUE)
      harcols<-names(OmkDesignF)[grepl("(|_)HAR(|1|2)$",names(OmkDesignF))]
      OmkDesignF[,(harcols):=NULL]
      OmkDesignF[,HAR:=1]  
      DFF1m$OmkDesign<-copy(OmkDesignF)
    }
    
    
    if (echo==1){
      print("OmkDesignF")
      print(OmkDesignF)      
    }
    
    if (echo==1){
      print("DFF2m$OmkDesign")
      print(DFF2m$OmkDesign)
    }
  
    #Finn omkoding fra original- til fellestab fra DF1m og DF2m hhv        
      
    RD12<-FinnRedesign(DF1m,DFF1m,SkalAggregeresOpp=SkalAggregeresOpp1,globs=globs)
    KB12<-RD12$FULL
    if (echo==1){print("++++++++++++++++++++++++")}
    RD21<-FinnRedesign(DF2m,DFF2m,SkalAggregeresOpp=SkalAggregeresOpp2,globs=globs)
    KB21<-RD21$FULL
    print("RD21 OK")
    
    if (echo==1){
      print("KB12")
      print(KB12)
      print("KB21")
      print(KB21)      
    }
   
    KB12<-subset(KB12,PRId==0)
    KB21<-subset(KB21,PRId==0)
    
    #Finn de fellestab som er dekket av omkoding fra DF1m og DF2m
    OmkCols1<-names(KB12)[grepl("_omk",names(KB12))]
    setkeym(KB12,OmkCols1)
    Omk1<-unique(KB12[KB12$Dekk==1,OmkCols1,with=FALSE])
    
    OmkCols2<-names(KB21)[grepl("_omk",names(KB21))]
    setkeym(KB21,OmkCols2)
    Omk2<-unique(KB21[KB21$Dekk==1,OmkCols2,with=FALSE])
    
    #Sett DEKK1m DEKK2
    OmkDesignF[,DEKK1:=0]
    OmkDesignF[,DEKK2:=0]
    setkeym(OmkDesignF,gsub("_omk","",key(Omk1)))
    
    OmkDesignF[Omk1,DEKK1:=1]
    setkeym(OmkDesignF,gsub("_omk","",key(Omk2)))
    OmkDesignF[Omk2,DEKK2:=1]
    
    
    #print("TMPBRYT!!!!!")
    #return(list(RD12=RD12,RD21=RD21,ODF=OmkDesignF))
    OmkF<-subset(OmkDesignF,DEKK1==1 & DEKK2==1)
   
    #Filtrer kolonner
    OmkF1<-OmkF[,gsub("_omk","",OmkCols1),with=FALSE]
    OmkF2<-OmkF[,gsub("_omk","",OmkCols2),with=FALSE]
    setkeym(OmkF1,gsub("_omk","",OmkCols1))
    setkeym(OmkF2,gsub("_omk","",OmkCols2))
    OmkF2<-unique(OmkF2)
    
    
    #Filtrer KB til de som er dekket
    KB12<-KB12[OmkF1]
    KB21<-KB21[OmkF2]
    
    
    if (lengst==2){
      DFF<-list(ok=1,lengst=lengst,OmkF=OmkF,KB12=KB21,KB21=KB12)
    } else {
      DFF<-list(ok=1,lengst=lengst,OmkF=OmkF,KB12=KB12,KB21=KB21)
    }
  } else {
    DFF<-list(ok=0)
  }
  print("FERDIG DFF")
  return(DFF)
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
    FIL<-OmkodFilFraPart(FIL,OmkParts,globs=globs)
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

LeggTilNyeVerdiKolonner<-function(TNF,NYEdscr,slettInf=TRUE){
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
OmkodFil<-function(FT,RD,snitt=FALSE,rapport=list(),globs=FinnGlobs()){
  
  #Lag rapport
  if (!is.null(rapport[["lagRapport"]]) && rapport[["lagRapport"]]==1){
    sqlQuery(globs$log,paste("DELETE * FROM OMKOD WHERE KUBE='",rapport[["KUBE"]],"' AND TRINN='",rapport[["TRINN"]],"'",sep=""))
    tmp<-sqlQuery(globs$log,"SELECT * FROM OMKOD WHERE KUBE='-1'")
    tmp2<-tmp
    tmp[1:nrow(RD$FULL),]<-NA
    tmp[,c(names(RD$FULL),"KUBE","TRINN","KALL")]<-cbind(RD$FULL,rapport[["KUBE"]],rapport[["TRINN"]],rapport[["KALL"]])
    FellesKol<-names(RD$TilDekk)[names(RD$TilDekk)!="Dekk"]
    setkeym(RD$TilDekk,FellesKol)
    setkeym(RD$FULL,paste(FellesKol,"_omk",sep=""))
    heltUdekk<-RD$TilDekk[!RD$FULL]
    if (nrow(heltUdekk)>0){
      setnames(heltUdekk,FellesKol,paste(FellesKol,"_omk",sep=""))
      tmp2[1:nrow(heltUdekk),]<-NA
      tmp2[,c(names(heltUdekk),"KUBE","TRINN","KALL")]<-cbind(heltUdekk,rapport[["KUBE"]],rapport[["TRINN"]],rapport[["KALL"]])
      tmp<-rbind(tmp,tmp2)
    }
    print("sqlSave starter, kan ta tid. Gir beskejd når ferdig")
    sqlSave(globs$log, tmp, "OMKOD", rownames = FALSE,append=TRUE)
    print("sqlSave ferdig")
  }
  cat("***OMKODFIL, TRINN:",rapport[["TRINN"]],"(kall:",rapport[["KALL"]],")\n")
  #print(RD$FULL)
  
  #Merk: RD kan enten være bare KB, eller full RD. Siste gir påliming av udekkede.
  if (identical(class(FT),"data.frame")){FT<-data.table(FT)}
  orgkeys<-key(FT)
  orgNames<-names(FT)
  
  #Finn kodebok, betinget på class(RD)
  if (is.data.frame(RD)){
    KB<-subset(RD,PRId==0)    
  } else {
    KB<-subset(RD$FULL,PRId==0)  
  }
  Omktabs<-intersect(globs$DefDesign$DesignKolsF,names(KB))
  nonOmktabs<-setdiff(orgNames,Omktabs)
  
  
  #Finn del av design som ikke omkodes (bare når RD er list med RD$TilDekk)
  Udekk<-data.frame()
  if (!is.data.frame(RD)){
    designKols<-intersect(globs$DefDesign$DesignKolsF,names(FT))
    
    Udekk<-subset(RD$TilDekk,Dekk==0)
    Udekk$Dekk<-NULL
    if (nrow(Udekk)>0){
      print("UDEKK!!!")
      print(subset(Udekk,GEOniv!="B"))
      #Må evt. krysse med den delen av design som ikke omkodes og geo
      if (length(setdiff(designKols,Omktabs))>0){
        UDKols<-setdiff(designKols,Omktabs)
        setkeym(FT,UDKols)
        UDtab<-unique(FT[,UDKols,with=FALSE])
        Udekk<-expand.grid.df(as.data.frame(Udekk),as.data.frame(UDtab))
      }
      
      #Må krysse med default GEO-koder  
      if ("GEOniv" %in% names(Udekk)){
        UdekkT<-data.frame()
        for (GeoN in unique(Udekk$GEOniv)){
          Geo<-data.frame("GEO"=globs$GeoKoder[globs$GeoKoder$GEOniv==GeoN,c("GEO"),with=FALSE],stringsAsFactors=FALSE)
          if (GeoN=="L"){
            Geo$FYLKE<-"00"
          } else {
            Geo$FYLKE<-substr(Geo$GEO,1,2)
          }
          UdekkT<-rbind(expand.grid.df(as.data.frame(subset(Udekk,GEOniv==GeoN)),Geo),UdekkT)
        }
        #Sett Miss-verdi
        valKols<-setdiff(names(FT),globs$DefDesign$DesignKolsFA)
        #FinnValKols(names(FT))
        valKolsDF<-setNames(as.data.frame(matrix(0,1,length(valKols))),valKols)
        valKolsDF[,grepl(".f$",valKols)]<-2  #Dvs '.', ikke'..'
        valKolsDF[,grepl(".a$",valKols)]<-1  
        UdekkT[,valKols]<-valKolsDF
        UdekkT<-UdekkT[,orgNames] #Sett til riktig kolonnerekkefølge for effektiv rbind
        Udekk<-setDT(UdekkT)
        
      }
    } else {
      Udekk<-data.frame()
    }    
  }
 
  
  #Merge med kodebok
  #NB: Dette er en krtisk ressursbruksfase for store filer, særlig dersom det omkodes K->F,K->L og til overlappende alder
  #De vil FT bli blåst opp i KB[FT] trinnet
  
  setkeym(FT,Omktabs)
  setkeym(KB,Omktabs)
  dimP<-dim(FT)
  FT<-KB[FT,nomatch=0,allow.cartesian=TRUE]    
  cat("I OMKOD med dim(KB):",dim(KB),"og dim(FT) før",dimP,"og etter",dim(FT),"\n")

  
  #Omkod geo
  FT[FT$GEOniv_omk=="K",GEO:=substr(GEO,0,4)]
  FT[FT$GEOniv_omk=="F",GEO:=FYLKE]
  FT[FT$GEOniv_omk=="L",c("GEO","FYLKE"):=list("0","00")]
  FT[FT$GEOniv_omk=="B" & FT$GEOniv=="S" & !grepl("^(0301|1103|1201|1601)",FT$GEO),c("GEO","FYLKE"):=list("0","00")]
  
  
  #Kutt bort kolonner fra gammelt design
  Omktabs_omk<-paste(Omktabs,"_omk",sep="")
  FT<-FT[,c(Omktabs_omk,nonOmktabs),with=FALSE]  #:=NULL FOR Å unngå kopiering!???
  setnames(FT,c(Omktabs,nonOmktabs))
  
  #Aggreger til nytt design
  #sumexp<-KHaggregerM("FT",snitt=snitt,)
  
    
  FT<-KHaggreger(FT,snitt=snitt,globs=globs)   #Her kopieres FT???? Det kan være kritisk
  cat("I OMKOD er dim(FT) etter aggreger",dim(FT),"\n")
  
  #Sett tilbake til gammel layout. OM snitt=1 må ogs VAL.n slippe gjennom
  if (snitt==FALSE){
    FT<-rbind(FT[,orgNames,with=FALSE],Udekk)
    setkeym(FT,orgkeys)
  }
  return(FT)  
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
  
  
  #Filtrer til bare den delen av designet som er aktuell for omkoding (dsv uten TAB1 etc)
  setkeym(FullDesign,OmkKols)
  Design[["OmkDesign"]]<-FullDesign[,list(HAR=max(HAR)),by=OmkKols]
  
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
      if (del %in% BetingF){
        Design[["FKombs"]][[kombn]]<-kombFull
      } else {
        Design[["SKombs"]][[kombn]]<-kombFull
      }
    }
  }
  
  #Tilbakestill key
  setkeym(ObsDesign,names(ObsDesign))
  setkeym(FIL,keyorg)
  
  return(Design)
}

FinnRedesign<-function(DesFRA,DesTIL,SkalAggregeresOpp=character(),ReturnerFullFull=FALSE,globs=FinnGlobs(),prios=globs$DefDesign,KB=globs$KB,echo=0){
  
  #Default for prioritetsrekkefølge ved omkoding
  #Dette er vel utdatert???
  if (is.null(prios)){
    if (KAagg==FALSE){
      prios<-list(Y="keep",Gn="keep",K="keep",A="keep")
    } else {
      prios<-list(Y="keep",Gn="keep",K="agg",A="keep")
    }
  }  
  
  
  #Merk assymtri mellom DesFRA og DesTIL. 
  #For DesTIL brukes bare DesTil$Part og DesTIL$OmkDesign.
  #DesFRA må derfor komme fra FinnDesign med alle egenskaper satt der, mens DesTIL kan være enklere og satt andre steder
  
  if(echo>=2){
    print("FRA:")
    print(DesFRA)
    cat("______________________\n")
    print("TIL:")
    print(DesTIL)
    cat("______________________\n")
  }
  
  #Deler i DesFra som ikke er i DesTil må legegs til i DesTil (full kryss mot Part[del])
  #Merk at deler i DesTil som ikke er i DesFra går greit (all omkoding er indirekte "betinget" på disse)
  kryssdeler<-names(DesFRA$Part)[!(names(DesFRA$Part) %in% names(DesTIL$Part))]
  if (length(kryssdeler)>0){
    DesTIL<-ModifiserDesign(DesFRA$Part[kryssdeler],DesTIL,globs=globs)     
  }
  Redesign<-list()
  
  #Sett partiell omkoding
  #For intervaller kalles FinnKodebokINtervaller, elllers hentes fast kodebok som utgnagspunkt
  #Disse kodebøkene (KJONN etc) filtreres til de omkodingene som er aktuelle (bør gjøres her for å begrense kombinatorikk, selv om dette kunne vært utsatt)
  #Dvs omkodinger til en TIL som ikke har alle nødvendige deler i FRA filtreres bort 
  #Merk at noen deler i FRA ikke er obligatoriske (slik som KJONN=9 for omkoding til KJONN=0) 
  
  Parts<-list()
  for (del in names(KB)){
    if (del %in% names(DesFRA$Part)){
      KBD<-KB[[del]]
      kol<-globs$DefDesign$DelKolN[del]
      kolomk<-paste(kol,"_omk",sep="")
      kols<-globs$DefDesign$DelKols[[del]]
      kolsomk<-paste(kols,"_omk",sep="")
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
        
        KBD<-SettIntervallDekk(KBD,del=del,globs=globs)
        setnames(KBD,"DEKKok",paste(del,"_Dekk",sep=""))
        
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
  
  
  #Sett FULL fra alle kombinasjoner av partielle. Foreløoig uavhengig av om disse er støttet av desFRA
  delerlist<-paste("as.data.frame(Parts[[\"",DesFRA[["OmkDeler"]],"\"]])",sep="",collapse=",")
  FULL<-data.table(eval(parse(text=paste("expand.grid.df(",delerlist,")",sep=""))))
    

  #Initier Dekk modifiseres under
  FULL[,Dekk:=1]
  
  

  #Rydd DesTIL$OmkDesign (Kan variere litt mht HAR avhengig av hvor kallet på denne funksjonen er gjort fra. Skal ha 1 har felt)
  hars<-names(DesTIL$OmkDesign)[grepl("^HAR(|1|2)$",names(DesTIL$OmkDesign))]
  if (length(hars)>1){
    eval(parse(text=paste("DesTIL$OmkDesign[,HAR:=pmax(",hars,")]",sep="")))
  } else if (length(hars)==0){
    DesTIL$OmkDesign[,HAR:=1]
  }
  
  #Slett de omk i FULL som ikke er i DesTIL
  tilKols<-names(DesTIL$OmkDesign)[!grepl("(^|_)HAR\\d*$",names(DesTIL$OmkDesign))] 
  tilKols<-intersect(tilKols,gsub("^(.*)_omk$","\\1",names(FULL)))
  setkeyv(DesTIL$OmkDesign,tilKols)
  setkeyv(FULL,paste(tilKols,"_omk",sep=""))
  
  

  FULL<-FULL[!DesTIL$OmkDesign[HAR==0,],allow.cartesian=TRUE]
  #FULL<-DesTIL$OmkDesign[FULL]
  
  if (echo>=1){
    cat("***************\nFinnRedesign1:\n")
    print(FULL)
  }
  #Må ta den ubetingede delen først 
  if (length(DesFRA[["UBeting"]])>0){
    if (any(DesFRA[["UBeting"]] %in% names(globs$DefDesign$IntervallHull))){
      print("ADVARSEL: Kan ikke tillate hull i intervaller for deler i UBeting!!")
    }
    
    #Lag full komb av KB for alle deler i DesFRA[["BetingOmk"]]
    komblist<-paste("as.data.frame(Parts[[\"",DesFRA[["UBeting"]],"\"]])",sep="",collapse=",")
    
    bet<-data.table(eval(parse(text=paste("expand.grid.df(",komblist,")",sep=""))))
    
    setkeyv(bet,key(DesFRA[["SKombs"]][["bet"]]))
    bet<-DesFRA[["SKombs"]][["bet"]][bet,allow.cartesian=TRUE]
    bet$HAR[is.na(bet$HAR)]<-0
    
    oblkols<-paste(DesFRA[["UBeting"]],"_obl",sep="")
    #Bør også bygges opp!
    OmkPriCols<-names(bet)[grepl("_(omk|pri)$",names(bet))]
    
    #bet<-SettIntervallDekk(bet,del=del,har="HAR",globs=globs)
    #Må fa brukt den for Y!!!!!
    eval(parse(text=paste(
      "bet[,UDEKK:=any(HAR==0 & any(",
        paste(oblkols,"==1",sep="",collapse=" & "),
      ")),by=OmkPriCols]",sep="")))
    bet<-subset(bet,UDEKK=FALSE)
    bet[,c("HAR","UDEKK")]<-NULL
    
    
    
    #Kolonner for omkoding. Nb: betinget på pri
    #Mer robuust om denne settes/bygges fra del og defdesign???
    OmkPriCols<-names(bet)[grepl("_(omk|pri)$",names(bet))]
    #SETT FULL DEKK=0!!!!
    setkeym(FULL,OmkPriCols)
    setkeym(bet,OmkPriCols)  
    FULL[!bet,Dekk:=0,allow.cartesian=TRUE]
  }
  
  #Resterende deler tas beinget på ovenstående
  
  for (del in DesFRA[["BetingOmk"]]){
    kombn<-paste("bet",del,sep="")
    if (length(DesFRA[["UBeting"]])>0){
      betD<-data.table(expand.grid.df(as.data.frame(bet),as.data.frame(Parts[[del]])))
    } else {
      betD<-data.table(Parts[[del]])
    }  
    if(echo>=1){cat("betD 1:\n",kombn,"\n")
      print(betD)
      print(komblist)
    }
    #Koble med DeSFRA, limer på HAR
    setkeyv(betD,key(DesFRA[["SKombs"]][[kombn]]))
    betD<-DesFRA[["SKombs"]][[kombn]][betD,allow.cartesian=TRUE]
    betD$HAR[is.na(betD$HAR)]<-0
    if(echo>=1){cat("betD 2:\n",kombn,"\n")
      print(betD)
      cat("*************\n")
    }
    #Kolonner for omkoding. Nb: betinget på pri
    #Mer robuust om denne settes/bygges fra del og defdesign???
    OmkPriCols<-names(betD)[grepl("_(omk|pri)$",names(betD))]
    delKolN<-globs$DefDesign$DelKolN[del]
    betD<-SettIntervallDekk(betD,del=del,har="HAR",globs=globs)
    
    #Sett Dekk=0 for udekket 
    setkeym(FULL,OmkPriCols)
    setkeym(betD,OmkPriCols)
    FULL[!betD[DEKKok==1,],Dekk:=0,allow.cartesian=TRUE]
    
   
    
  }
  #FULL[,PRI:=rank(order(Gn_pri,A_pri))]
  #FULL<-subset(FULL,Dekk==1)
  
  if(echo==1){
    print(FULL)
    print("%%%%%%%%%%")
  }
  
 

  #Velg beste metode (av evt alternativer) for omkoding til hver del av TIL  
  if (nrow(FULL)>0){  
    OmkCols<-names(FULL)[grepl("_(omk)$",names(FULL))]
    PriCols<-names(FULL)[grepl("_(pri)$",names(FULL))]
    BetCols<-c(OmkCols,PriCols)
    setkeym(FULL,c(BetCols,"Dekk"))
    
    aggpri<-globs$DefDesign$AggPri[globs$DefDesign$AggPri %in% c(DesFRA[["UBeting"]],DesFRA[["BetingOmk"]])]
    aggpri[aggpri %in% SkalAggregeresOpp]<-paste("-",aggpri[aggpri %in% SkalAggregeresOpp],sep="")
    prioritering<-paste(aggpri,"_pri",collapse=",",sep="")
    
    alternativer<-unique(FULL[,c(BetCols,"Dekk"),with=FALSE])
    
    eval(parse(text=paste(
      "alternativer[,PRI:=order(",prioritering,")-1,by=OmkCols]",sep=""
    )))
    alternativer[,PRId:=NA_integer_]
    eval(parse(text=paste(
      "alternativer[Dekk==1,PRId:=as.integer(order(",prioritering,")-1),by=OmkCols]",sep=""
    )))
    
    FULL<-alternativer[FULL]
    #print(subset(FULL,AARl==2005 & KJONN==0 & PRId==0))
    if (ReturnerFullFull==FALSE){
      FULL<-subset(FULL,PRId==0)
    }
  } else {
    FULL$PRId<-NA
  }
  #Rapporter de som er dekket
  TilDekk<-DesTIL$OmkDesign[HAR==1,tilKols,with=FALSE]
  TilDekk$Dekk<-1   
  setkeyv(TilDekk,tilKols)
  setkeyv(FULL,paste(tilKols,"_omk",sep=""))
  #SKjønner ikke hvorfor jeg må ha allow.cartesian=TRUE og ser ikke helt konsekvensene
  nroworg<-nrow(TilDekk)
  TilDekk[!subset(FULL,PRId==0),Dekk:=0,allow.cartesian=TRUE]
  if (nrow(TilDekk)!=nroworg){
    cat("***********************************\n****NOE ER RART HER MED SETTING AV DEKK I TILLDEKK\n*************************\n")
  }  

  return(list(FULL=FULL,TilDekk=TilDekk,Parts=Parts))
}

SettIntervallDekk<-function(KB,del="",har=paste(del,"_HAR",sep=""),globs=FinnGlobs()){
  OmkPriCols<-names(KB)[grepl("_(omk|pri)$",names(KB))]
  delKolN<-globs$DefDesign$DelKolN[del]
  
  if (del %in% names(globs$DefDesign$IntervallHull)){
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
      "KB[,c(\"DekkInt\",\"FRADELER\",\"NHAR\",\"TotInt\",\"NTOT\"):=list(",DekkInt,",.N,",NTOT,",",TotInt,",",NHAR,"),by=OmkPriCols]",sep=""
    )))
    
    #Sjekk om dekning tilfrestiller krav satt i 
    eval(parse(text=paste(
      "KB[,\"DEKKok\":=as.integer(",globs$DefDesign$IntervallHull[[del]],"),by=OmkPriCols]",sep=""
    )))
   
    
    KB[,c("DekkInt","NHAR","TotInt","NTOT"):=NULL]
    #Kast hjelpekolonnner
    #KB[,c("DekkInt","NHAR","TotInt","NTOT","DEKKok"):=NULL]
  } else {
    eval(parse(text=paste(
      "KB[,DEKKok:=as.integer(!any(",har,"==0 & ",
      del,"_obl==1)),by=OmkPriCols]",sep="")))
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

FinnKodebokForEtIntervallGml<-function(Find,FRA,TILint,OVLP,letn,result,utcolnavn){
  print(FRA)
  if(DekkerInt(FRA[Find,],TILint)){
    jobb<-Find[Find>letn]
    if(length(jobb)>0){
      letn<-jobb[1]
      #cat("Let videres med letn",letn,"\n")
      result<-FinnKodebokForEtIntervall(Find[!(Find %in% OVLP[[letn]]) | Find==letn],FRA,TILint,OVLP,letn,result,utcolnavn)
      #LEt videre uten letn
      result<-FinnKodebokForEtIntervall(Find[Find!=letn],FRA,TILint,OVLP,letn,result,utcolnavn)
    } else {
      result$KODEBOK<-rbind(result$KODEBOK,setNames(cbind(FRA[Find,],TILint,1,result$pri),utcolnavn))
      result$pri<-result$pri+1
      result$ok<-1
    }
  }
  else {
    #Sett diff
    #print(Intervals(FRA[Find,],type='Z'))
    #print(Intervals(TILint,type='Z'))
    if (nrow(FRA[Find,])>0){
      miss<-setNames(as.data.frame(interval_difference(Intervals(TILint,type='Z'),Intervals(FRA[Find,],type='Z'))),names(FRA))
    } else {
      miss<-setNames(as.data.frame(TILint),names(FRA))
    }
    #Må stoppe om en (en holder) i miss er i FRA (om ikke replikeres samme kodebok!)
    if (!any(apply(miss,1,paste,collapse="_") %in% apply(FRA,1,paste,collapse="_"))){
      result$KODEBOK<-rbind(result$KODEBOK,setNames(cbind(rbind(FRA[Find,],miss),TILint,0,result$pri),utcolnavn))
      result$pri<-result$pri+1
      result$ok<-0
      print("Sett opsjon på om ekstra int skal settes her, eller bare int med ok=1!!!!!")
    }
    #cat("Dekker ikke\n")
    #ok<-0
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
  setnames(FGl,paste(vals,c("",".f",".a"),sep=""),lvals)
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
  vkolsNF<-unlist(lapply(vkolsN,function(x){paste(x,c("",".f",".a"),sep="")}))
  return(intersect(knames,vkolsNF))
}

FinnTabKols<-function(knames){
  return(setdiff(knames,c(FinnValKolsF(knames),"KOBLID","ROW")))  
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
    LagFlereFilgrupper(filgrupper=FILGRP,batchdate=batchdate,globs=globs,versjonert=TRUE)
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

SjekkVersjoner<-function(){
  globs<-FinnGlobs()
  path="F:/Prosjekter/Kommunehelsa/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/R"
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
      if (!identical(VC,VN)){
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
KHerr<-function(error){
  cat("***************************************************************************\n"
      ,"*KHFEIL!! ",error,"\n***************************************************************************\n")
}

KH2014v2015<-function(kube,batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  KH2014dir<-"F:/Prosjekter/Kommunehelsa/Data og databehandling/Databehandling/2014/csvNESSTAR"
  setwd(KH2014dir)
  datef<-format(strptime(batchdate, "%Y-%m-%d-%H-%M"),"#%Y-%m-%d#")
  K15dscr<-FinnKubeInfo(kube)
  FellesTabs<-c("GEO","AAR","ALDER","KJONN")
  KLenke<-sqlQuery(globs$dbh,paste("SELECT * FROM KH2015v2014 WHERE KH2015_KUBE='",kube,"'",sep=""),stringsAsFactors=FALSE)[1,]
  F2015<-FinnKubeT(kube)
  F2014<-as.data.table(read.csv(KLenke$KH2014FIL,sep=';',stringsAsFactors=FALSE))
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
  print(unique(F2014$ALDER))
  if (!is.null(F2014$ALDER)){
    F2014$ALDER<-as.character(F2014$ALDER)
    if (!is.na(KLenke$Alder14TOM)){
      int<-which(grepl("_",F2014$ALDER))
      atmp<-unlist(str_split(F2014$ALDER[int],"_"))
      F2014$ALDER[int]<-paste(atmp[1],"_",as.integer(atmp[2])-1,sep="") 
    }
    F2014$ALDER<-gsub("^_(\\d+)$",paste(allev[1],"_","\\1",sep=""),F2014$ALDER)
    F2014$ALDER<-gsub("^(\\d+)_$",paste("\\1","_",allev[2],sep=""),F2014$ALDER)
    print(F2014)
    F2014$ALDER<-gsub("ALLE",alle,F2014$ALDER)
  } else {
    F2014$ALDER<-alle
  }
  print(unique(F2014$ALDER))
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
      F2014[,tab15n]<-mapvalues(F2014[,tab15n],tab1414,tab1415)
      setDT(F2014)
    }
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
  utfil<-paste("F:/Prosjekter/Kommunehelsa/PRODUKSJON/VALIDERING/NESSTAR_KUBER/KH2014v2015/",kube,"_KH15v14.csv",sep="")
  write.table(MM,file=utfil,sep=";",row.names=FALSE)
  return(MM)  
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

KHglobs<-FinnGlobs()

