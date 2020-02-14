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
  KHdbname="STYRING/KHELSA.mdb",
  KHlogg="STYRING/KHlogg.mdb",
  StablaDir="PRODUKTER/MELLOMPROD/R/STABLAORG/",
  StablaDirNy="PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE",
  StablaDirDat="PRODUKTER/MELLOMPROD/R/STABLAORG/DATERT",
  kolorgs=c("GEO","AAR","KJONN","ALDER","TAB1","TAB2","TAB3","VAL1","VAL2","VAL3"),
  taborgs=c("GEO","AAR","KJONN","ALDER","TAB1","TAB2","TAB3"),  
  DesignKols=c("GEOniv","AARl","AARh","KJONN","ALDERl","ALDERh","TAB1","TAB2","TAB3"),
  OmkKols=c("GEOniv","AARl","AARh","KJONN","ALDERl","ALDERh"),
  TabKols=c("AARl","AARh","GEOniv","ALDERl","ALDERh","KJONN","GEO","FYLKE","TAB1","TAB2","TAB3"),
  binDir="bin",
  tmpfilerpath="bin\tmpfiler",
  geoukjent="GGG",
  alderukjent="888_888",
  kjonnukjent="8",
  aarukjent="8888_8888",
  SisteBatch="9999-01-01-01-01",
  stjstr="************************************************************\n",
  XLScols=as.vector(sapply(c("",as.vector(paste(sapply(c("",LETTERS[]),paste,LETTERS[],sep="")))),paste,LETTERS[],sep=""))
)



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
    KHOc<-odbcConnectAccess2007(paste(path,globs$KHdbname,sep="/"))
    #KHOc<-odbcConnectAccess(paste(path,KHdbname,sep="/"))
    KHLc<-odbcConnectAccess2007(paste(path,globs$KHlogg,sep="/"))
  }
  GeoNavn<-sqlQuery(KHOc,"SELECT * from GeoNavn",as.is=TRUE)
  GeoKoder<-sqlQuery(KHOc,"SELECT * from GEOKoder",as.is=TRUE)
  KnrHarm<-sqlQuery(KHOc,"SELECT * from KnrHarm",as.is=TRUE)
  globs$DefDesign<-SettDefDesignKH()
  
  return(c(globs,list(dbh=KHOc,log=KHLc,path=path,GeoNavn=GeoNavn,GeoKoder=GeoKoder,KnrHarm=KnrHarm)))
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
KHglobs<-FinnGlobs()

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
LagFilgruppe<-function(gruppe,batchdate=SettKHBatchDate(),globs=FinnGlobs(),printR=TRUE,printCSV=FALSE,printSTATA=FALSE,versjonert=FALSE){
  #Essensielt bare loop over alle delfiler/orignalfiler
  #For hver orignalfil kjøres LagTabellFraFil
  #Stables til tabellen FG
    
  #Finn filgruppeparametre
  FGP<-FinnFilgruppeParametre(gruppe,globs)
  
  #Initier tom tabell
  Filgruppe<-data.frame()
  
  #Rydd gammel logg
  #sqlQuery(globs$log,"DELETE * FROM KODEBOK_LOGG WHERE FILGRUPPE")
  
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
      DF<-LagTabellFraFil(filbesk,FGP,batchdate=batchdate,globs=globs,versjonert=versjonert)
      #Stable delfiler
      Filgruppe<-rbind.fill(Filgruppe,DF)
      
      #Stopp klokke, skriv tid og feillogg
      tid<-proc.time()-tm
      stid<-format(Sys.time(), "%Y-%m-%d %X")
    }
  } else {
    #Må gi fornuftig tilbakemelding
  }
  
  #Diagnostisering og rapportering på hele filgruppa under ett
  diagnose<-0
  if(nrow(Filgruppe)>0 & diagnose==1){
    #Finn og rapporter duplikater
    #HarDuplikater<-SjekkDuplikater(Filgruppe,batchdate=batchdate,filgruppe=gruppe,versjonert=versjonert,globs=KHglobs)
    
    #Sjekk design
    GYKAT<-FinnDesign(Filgruppe)
    sqlQuery(globs$log,paste("DELETE * FROM GYKAT WHERE FILGRUPPE='",gruppe,"' AND SV='S'",sep=""))
    #Legg til resterende kolonner 
    tmp<-sqlQuery(globs$log,"SELECT * FROM GYKAT WHERE FILGRUPPE=''")
    tmp[1:nrow(GYKAT$GYKATfull),]<-NA
    tmp[,names(GYKAT$GYKATfull)]<-GYKAT$GYKATfull
    tmp$FILGRUPPE<-gruppe
    tmp$BATCH<-batchdate
    tmp$SV<-"S"
    sqlSave(KHglobs$log,tmp,"GYKAT",rownames=FALSE,append=TRUE)
    if (versjonert==TRUE){
      tmp$SV<-"V"
      sqlSave(KHglobs$log,tmp,"GYKAT",rownames=FALSE,append=TRUE)
    }
    sqlQuery(globs$dbh,paste("UPDATE FILGRUPPER SET DUPLIKATER='",HarDuplikater,"' WHERE FILGRUPPE='",gruppe,"'",sep=""))  
    #DEV: SPESIALBEHANDLING AV FILGRUPPE HER!! F.EKS. IMPUTER NPR
  }
  
  #Sett (eksterne) kolonnenavn
  for (val in c("VAL1","VAL2","VAL3")){
    if (val %in% names(Filgruppe)){
      valn<-paste(val,"navn",sep="")
      if (grepl("\\S",FGP[[valn]])){
        names(Filgruppe)<-gsub(paste("^",val,"(\\.[fa]|)$",sep=""),paste(FGP[[valn]],"\\1",sep=""),names(Filgruppe))    
      }
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
      utfild<-paste(path,"/",globs$StablaDirDat,"/",gruppe,"_",batchdate,".rData",sep="")
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
    FG<-LagFilgruppe(gruppe,batchdate=batchdate,globs=globs) 
  }
}

#
LagTabellFraFil<-function (filbesk,FGP,batchdate=SettKHBatchDate(),globs=FinnGlobs(),versjonert=FALSE) {
  
  klokke<-proc.time()
  ######################################################
  #INNLESING
  filn<-filbesk$filn
  cat("\n#################\nLAGER TABELL FRA FIL:\n",filn,"\n")
  LestFil<-LesFil(filbesk,batchdate=batchdate,globs=globs)
  ok<-LestFil$ok
  DF<-LestFil$DF
  
  
  #cat("\nETTER INNLES\n#############################\n")
  #print(head(DF))
  
#   ######################################################
#   #EVT SPESIALBEHANDLING
#   if (!is.na(filbesk$RSYNT1)){
#     filbesk$RSYNT1<-gsub("\\\r","\\\n",filbesk$RSYNT1)
#     eval(parse(text=filbesk$RSYNT1))
#   }
  #cat("\nETTER INNLES\n#############################\n")
  #print(head(DF))
  
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
    
    
    #RESHAPE
    if (!(is.na(filbesk$RESHAPEid) || filbesk$RESHAPEid=='')){
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
  
  if (ok==1){    
        
    #Merge GEO delt i to
    if (filbesk$GEOd2!="-" & !is.na(filbesk$GEOd2)){
      DF$GEO<-paste(DF$GEO,sprintf("%02d",as.numeric(DF[,filbesk$GEOd2])),sep="")
    }
    
    #KAST USPESIFISERTE KOLONNER
    DF<-DF[,names(DF)[names(DF) %in% kolorgs]]
    
  }

  TilFilLogg(filbesk$KOBLID,"INNLES_OK",ok,batchdate=batchdate,globs=globs)

  

  ######################################################
  #SKILL EVT UT SOM EGEN FUNKSJON
  #Nullstill logg
  sqlQuery(globs$log,paste("DELETE * FROM KODEBOK_LOGG WHERE KOBLID=",filbesk$KOBLID,sep=""))
  if (ok==1){
    #RENSK GEO (Alle er legit inntil videre??? Eller kod til 9999???)
    if ("GEO" %in% names(DF)){
      org<-setNames(as.data.frame(table(DF$GEO,useNA="ifany"),stringsAsFactors=FALSE),c("ORG","FREQ"))
      geo<-GEOvask(org,filbesk=filbesk,batchdate=batchdate,globs=globs)
      
      SkrivKBLogg(KB=geo,type="GEO",filbesk=filbesk,batchdate=batchdate,globs=globs)
      TilFilLogg(filbesk$KOBLID,"GEO_ok",ifelse(0 %in% geo$OK,0,1),batchdate=batchdate,globs=globs)
      
      DF$GEOniv<-as.numeric(mapvalues(DF$GEO,geo$ORG,geo$GEOniv,warn_missing = FALSE))
      DF$FYLKE<-mapvalues(DF$GEO,geo$ORG,geo$FYLKE,warn_missing = FALSE)
      DF$GEO<-mapvalues(DF$GEO,geo$ORG,geo$OMK,warn_missing = FALSE)  #NB: rekkefølge har betydning   
    }
   
    #RENSK ALDER
    #Sett intervall for alder ALLE
    if ("ALDER" %in% names(DF)){
      DF$ALDER<-gsub(" \\Wr\\b"," år",DF$ALDER,perl=TRUE)   #Problem med codebook i dbf
      
      org<-setNames(as.data.frame(table(DF$ALDER,useNA="ifany"),stringsAsFactors=FALSE),c("ORG","FREQ"))
      alder<-ALDERvask(org,FGP=FGP,filbesk=filbesk,batchdate=batchdate,globs=globs)
      
      SkrivKBLogg(KB=alder,type="ALDER",filbesk=filbesk,batchdate=batchdate,globs=globs)
      TilFilLogg(filbesk$KOBLID,"ALDER_ok",ifelse(globs$alderukjent %in% alder$OMK,0,1),batchdate=batchdate,globs=globs)
      
      DF$ALDERl<-as.numeric(mapvalues(DF$ALDER,alder$ORG,alder$LO,warn_missing = FALSE))
      DF$ALDERh<-as.numeric(mapvalues(DF$ALDER,alder$ORG,alder$HI,warn_missing = FALSE))
    }
    
    
    #RENSK KJ?NN
    if ("KJONN" %in% names(DF)){
      
      org<-setNames(as.data.frame(table(DF$KJONN,useNA="ifany"),stringsAsFactors=FALSE),c("ORG","FREQ"))
      kjonn<-KJONNvask(org,filbesk=filbesk,batchdate=batchdate,globs=globs)
      
      SkrivKBLogg(KB=kjonn,type="KJONN",filbesk=filbesk,batchdate=batchdate,globs=globs)
      TilFilLogg(filbesk$KOBLID,"KJONN_ok",ifelse(globs$kjonnukjent %in% kjonn$OMK,0,1),batchdate=batchdate,globs=globs)
      
      DF$KJONN<-as.numeric(mapvalues(DF$KJONN,kjonn$ORG,kjonn$OMK,warn_missing = FALSE))
    }
    
    #AAR TIL INTERVALL
    if ("AAR" %in% names(DF)){
      org<-setNames(as.data.frame(table(DF$AAR,useNA="ifany"),stringsAsFactors=FALSE),c("ORG","FREQ"))
      
      aar<-AARvask(org,filbesk=filbesk,batchdate=batchdate,globs=globs)
        
      SkrivKBLogg(KB=aar,type="AAR",filbesk=filbesk,batchdate=batchdate,globs=globs)
      TilFilLogg(filbesk$KOBLID,"AAR_ok",ifelse(globs$aarukjent %in% aar$OMK,0,1),batchdate=batchdate,globs=globs)
      
      DF$AARl<-as.numeric(mapvalues(DF$AAR,aar$ORG,aar$LO,warn_missing = FALSE))
      DF$AARh<-as.numeric(mapvalues(DF$AAR,aar$ORG,aar$HI,warn_missing = FALSE))
    }
    
    #VASK AV TABx
    for (tab in c("TAB1","TAB2","TAB3")){
      if (tab %in% names(DF)){
        tabKB<-setNames(as.data.frame(table(DF[,tab],useNA="ifany"),stringsAsFactors=FALSE),c("ORG","FREQ"))
        tabKB$KBOMK<-KBomkod(tabKB$ORG,type=tab,filbesk=filbesk,batchdate=batchdate,globs=globs)
        tabKB$OMK<-gsub("^-$","XXXKASTXXX",tabKB$KBOMK) #Dirty tircks. Beskytter '-' mot uttrykket nedenfor, uten å gjøre regexp unødvendig komplisert
        tabKB$OMK<-gsub("[- ,\\/]","_",tabKB$KBOMK)
        tabKB$OMK<-gsub("XXXKASTXXX","-",tabKB$KBOMK)
        tabKB$OK<-1
        SkrivKBLogg(KB=tabKB,type=tab,filbesk=filbesk,batchdate=batchdate,globs=globs)
        DF[,tab]<-mapvalues(DF[,tab],tabKB$ORG,tabKB$OMK,warn_missing = FALSE)
      }
    }
    
    
    #DROPP ALLE MED '-' I TABULERING
    DF<-subset(DF,rowSums(DF[,names(DF) %in% globs$taborgs]=="-")==0)
    
    
    #VASK VERDIER. Litt annen prosess, bruker KB, men tabulerer bare ikke-numeriske. 
    #Setter numerisk, med flagg for type NA
    
    for (val in c("VAL1","VAL2","VAL3")){
    #Bedre, men funker ikke i forhold til logg
    #for (val in names(DF)[grepl("VAL\\d+$",names(DF))]){  
      if (val %in% names(DF)){
        valok<-1
        valf<-paste(val,".f",sep="")
        vala<-paste(val,".a",sep="")
        valomk<-paste(val,"omk",sep="")
        
        #Lag omkodet verdi med numerisk. Ikke numerisk blir foreløpig NA
        suppressWarnings(DF[,valomk]<-as.numeric(DF[,val]))
        DF[,valf]<-0
        DF[,vala]<-1
                
        #Behandle ikke-numeriske
        nonNum<-which(is.na(DF[,valomk]))
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
          
          #if(valok==0){print(valKB)}
          
          #DEV: komprimer output for \d,\d??, må da nedenfor først, (eller?) blir litt krøll
          SkrivKBLogg(KB=valKB,type=val,filbesk=filbesk,batchdate=batchdate,globs=globs)
          
          #Internt, regnbart format med numerisk flagg i "VAL1f" etc
          #".." = 1, "." = 2, ":" = 3  
          valKB$kbNUM<-kbNUM
          valKB$FLAG<-0
          valKB$FLAG[valKB$OMK==".."]<-1
          valKB$FLAG[valKB$OMK=="."]<-2
          valKB$FLAG[valKB$OMK==":"]<-3
          valKB$FLAG[valKB$OK==0]<-8
          
          
          #if(valok==0){print(valKB)}
          
          DF[nonNum,valomk]<-as.numeric(mapvalues(DF[nonNum,val],valKB$ORG,valKB$kbNUM,warn_missing = FALSE))
          #DF[nonNum,valomk]<-suppressWarnings(as.numeric(mapvalues(DF[nonNum,val],valKB$ORG,valKB$kbNUM,warn_missing = FALSE)))
          DF[nonNum,valf]<-mapvalues(DF[nonNum,valf],valKB[,"ORG"],valKB[,"FLAG"],warn_missing = FALSE)
        }
        
        DF[,val]<-NULL
        DF<-setNames(DF,mapvalues(names(DF),valomk,val))
      }
      
      TilFilLogg(filbesk$KOBLID,paste(val,"OK",sep="_"),valok,batchdate=batchdate,globs=globs)
    }
   
    default.stringsAsFactors=TRUE 
    Kols<-c(globs$TabKols[globs$TabKols %in% names(DF)],names(DF)[grepl("^VAL\\d+(\\.(f|a)|)$",names(DF))])
    #print(filbesk)
    #kAN KRÆSJE VED UKJENT KOLNAVN!
    #print(FGP)
    DF<-DF[,Kols]
    
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
  sqlQuery(globs$log,paste("INSERT INTO INNLES_LOGG ( KOBLID,BATCH, SV) SELECT =",filbesk$KOBLID,",'",batchdate,"', 'S'",sep=""))
  
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
KHCsvread <- function (filn,header=FALSE,skip=0,colClasses="character",sep=";",quote = "\"",dec = ".",fill=FALSE,encoding = "unknown",blank.lines.skip=FALSE,na.strings=c("NA"),brukfread=TRUE,...) {
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
Xls2R.KH <- function(xlsfil,ark="",globs=FinnGlobs(),brukfread=TRUE,na.strings=c("NA"),ryddOpp=1,...){
  err<-""
  ok<-1
  DF<-data.frame()
  #step 1: Validate sheetname with fuzzy match
  rdbh<-odbcConnectExcel2007(xlsfil)
  #rdbh<-odbcConnectExcel(xlsfil)
  tables<-sqlTables(rdbh)$TABLE_NAME
  close(rdbh)
  tables<-gsub("\'","",tables)  
  tables<-gsub(".$","",tables)  #Something is strange with resepct to $ in R's regexp-syntax, but should work
  if (ark=="" | is.na(ark)){
    ark<-tables[1]
  } else if (!(ark %in% tables)){
    kand<-tables[grep(paste("^",ark,sep=""),tables,ignore.case=TRUE)]
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
ReshapeTab <- function (DELF,filbesk,batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  #Reshape av DELF basert på parametre i filbesk
  ok<-1
  idvars<-eval(parse(text=paste("c(",filbesk$RESHAPEid,")")))
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

KBomkod<-function(org,type,filbesk,batchdate=NULL,globs=FinnGlobs()) {
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
  if (nrow(kbok)>0){
    KBsubs<-subset(kbok,TYPE=="SUB")   #Regulæruttrykk
    KB<-subset(kbok,TYPE=="KB")       #Oppslagsliste
    i<-1
    while (i<=nrow(KBsubs)){
      KBsub<-KBsubs[i,]
      omk<-sub(KBsub$ORGKODE,KBsub$NYKODE,omk)
      i<-i+1
    } 
    omk<-mapvalues(omk,KB$ORGKODE,KB$NYKODE,warn_missing = FALSE)
  }
  
return(omk)
}

GEOvask<-function (geo,filbesk,batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  geo$KBOMK<-KBomkod(geo$ORG,type="GEO",filbesk=filbesk,batchdate=batchdate,globs=globs)
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
  
  #Finn ukjente koder. Sett til ukjent (99) under fylke eller by om mulig, ellers 
  #TMP<-globs$GeoKoder
  #ukjent<-sqldf("SELECT OMK FROM geo LEFT JOIN TMP ON geo.OMK=TMP.GEO WHERE TMP.ID Is NULL")
  #print(head(globs$GeoKoder))
  #print(geo[1:50,])
  #print(which(!(geo[,"OMK"] %in% globs$GeoKoder$GEO)))
  ukjent<-geo$OMK[!(geo$OMK %in% c(globs$GeoKoder$GEO,"-"))]
  
  ukjent99<-sub("^\\d{2}$",99,ukjent) #Ukjent fylke
  ukjent99<-sub("^(\\d{2})\\d{2}$",paste("\\1","99",sep=""),ukjent) #Ukjent kommune
  ukjent99<-sub("^(\\d{4})\\d{2}$",paste("\\1","99",sep=""),ukjent) #Ukjent bydel
  
  #Sjekk om legitime 99-ukjente
  ukjent<-ukjent[ukjent99 %in% globs$GeoKoder$GEO]
  ukjent99<-ukjent99[ukjent99 %in% globs$GeoKoder$GEO]
  geo$OMK<-mapvalues(geo$OMK,ukjent,ukjent99,warn_missing = FALSE)
  
    
  #Sett GEOniv
  geo$GEOniv<-NA
  geo$GEOniv[nchar(geo$OMK)==6]<--1
  geo$GEOniv[nchar(geo$OMK)==4]<-1
  geo$GEOniv[nchar(geo$OMK)==2]<-2
  geo$GEOniv[geo$OMK==0]<-3
  geo$GEOniv[geo$OMK=="-"]<-9
  geo$GEOniv[is.na(geo$GEOniv)]<-8      
  
  #Ekte ulegit
  geo$OK[geo$GEOniv==8]<-0
  #Fil har bare kommunedata -> bruker 8888
  if (sum(c(-1,2,3) %in% geo$GEOniv)==0){
    geo$OMK[geo$OK==0]<-"8888"
    geo$GEOniv[geo$OK==0]<-1
  } else {
    geo$OMK[geo$OK==0]<-globs$geoukjent
  }    
  
  #Sett fylke
  geo$FYLKE<-NA
  geo$FYLKE[geo$GEOniv<3]<-substr(geo$OMK[geo$GEOniv<3],1,2)
  geo$FYLKE[geo$GEOniv==3]<-"00"
  
  return(geo)
}

ALDERvask<-function(alder,filbesk,FGP,batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  amax<-FGP$amax
  amin<-FGP$amin
  alder$KBOMK<-KBomkod(alder$ORG,type="ALDER",filbesk=filbesk,batchdate=batchdate,globs=globs)

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
  alder$OMK<-sub("^ *under (\\d+)( +år| *$)","_\\1",alder$OMK,ignore.case = TRUE)
  alder$OMK<-sub("^ *(\\d+)( ?r|) *(og|eller) (yngre|under)","_\\1",alder$OMK,ignore.case = TRUE)
  alder$OMK<-sub("^ *(\\d+) *( +år| *$)","\\1_\\1",alder$OMK,ignore.case = TRUE)
  alder$OMK<-sub("^ *(Alle( *aldre.*|)|Totalt{0,1}) *$",paste(amin,"_",amax,sep=""),alder$OMK,ignore.case = TRUE)
  alder$OMK[is.na(alder$OMK)]<-"999_999"
  alder$OMK<-sub("^(\\d+)_$",paste("\\1_",amax,sep=""),alder$OMK)
  alder$OMK<-sub("^_(\\d+)$",paste(amin,"_\\1",sep=""),alder$OMK)
  
  #Ukjent????????
  #!Må ha to amax, en for ukjent som er høyere, se også ulest under!!!
  
  okformat<-grepl("^\\d+_\\d+$|^-$",alder$OMK)
  #Ugyldig verdi/ukjent kode
  alder$OMK[!okformat]<-globs$alderukjent
  alder$OK[!okformat]<-0
  
  #Sett intervall
  alder[,c("LO","HI")]<-suppressWarnings(matrix(as.numeric(str_split_fixed(alder$OMK,"_",2)),ncol=2))
  
  #Ugyldig intervall
  alder$OMK[alder$HI<alder$LO]<-globs$alderukjent
  alder[,c("LO","HI")]<-suppressWarnings(matrix(as.numeric(str_split_fixed(alder$OMK,"_",2)),ncol=2))
  
  return(alder)
}

KJONNvask<-function (kjonn,filbesk,batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  kjonn$KBOMK<-KBomkod(kjonn$ORG,type="KJONN",filbesk=filbesk,batchdate=batchdate,globs=globs)
  kjonn$OK<-1
  
  kjonn$OMK<-kjonn$KBOMK
  kjonn$OMK<-sub("^ *(M|Menn|Mann|gutt(er|)|g|1) *$","1",kjonn$OMK,ignore.case = TRUE)
  kjonn$OMK<-sub("^ *(K|F|Kvinner|Kvinne|jente(r|)|j|2) *$","2",kjonn$OMK,ignore.case = TRUE)
  kjonn$OMK<-sub("^ *(Tot(alt{0,1}|)|Begge([ \\._]*kjønn|)|Alle|A|0|M\\+K) *$","0",kjonn$OMK,ignore.case = TRUE)
  kjonn$OMK<-sub("^ *(Uspesifisert|Uoppgitt|Ikke spesifisert|Ikke oppgitt|Ukjent|) *$","9",kjonn$OMK,ignore.case = TRUE)
  kjonn$OMK[is.na(kjonn$ORG)]<-9
  
  #Ugyldig verdi/ukjent kode
  kjonn$OMK[!(kjonn$OMK %in% c(0,1,2,9,"-"))]<-globs$kjonnukjent
  kjonn$OK[!(kjonn$OMK %in% c(0,1,2,9,"-"))]<-0
  
  return(kjonn)
}

AARvask<-function (aar,filbesk,batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  aar$KBOMK<-KBomkod(aar$ORG,type="AAR",filbesk=filbesk,batchdate=batchdate,globs=globs)
  aar$OMK<-aar$KBOMK
  aar$OK<-1
  
  aar$OMK<-sub("^Høsten ","",aar$OMK)
  aar$OMK<-sub("^(\\d+) *[_-] *(\\d+)$","\\1_\\2",aar$OMK)
  aar$OMK<-sub("^ *(\\d+) *$","\\1_\\1",aar$OMK)
  
  #Ugyldig verdi/ukjent kode
  okformat<-grepl("^\\d+_\\d+$|^-$",aar$OMK)
  aar$OMK[!okformat]<-globs$aarukjent
  aar$OK[!okformat]<-0
  
  #Sett intervall
  aar[,c("LO","HI")]<-suppressMessages(matrix(as.numeric(str_split_fixed(aar$OMK,"_",2)),ncol=2))
  #Ugyldig intervall
  aar$OMK[aar$HI<aar$LO]<-globs$aarukjent
  aar[,c("LO","HI")]<-suppressMessages(matrix(as.numeric(str_split_fixed(aar$OMK,"_",2)),ncol=2))
  return(aar)
}
  


#Diverse prosedyrer for INNLESING-trinnet
###########################################

SjekkDuplikater<-function(FG,filgruppe,batchdate=SettKHBatchDate(),versjonert=FALSE,globs=FinnGlobs()){
  HarDuplikater<-0
  if (identical(class(FG),"data.frame")){FG<-data.table(FG)}
  orgkeys<-key(FG)
  tabkols<-globs$TabKols
  tabkols<-tabkols[tabkols %in% names(FG)]
  valkols<-names(FG)[grepl("^VAL\\d+(f|)$",names(FG))]
  setkeym(FG,tabkols)
  #DT[unique(DT[duplicated(DT)]),which=T]
  
  #FG2<-FG[FG[unique(FG[duplicated(FG[,c(tabkols),with=FALSE]),c(tabkols),with=FALSE]),which=T],]
  DUB<-FG[FG[unique(FG[duplicated(FG)]),which=T],]
  
  #D1<-duplicated(FG[,tabkols,with=FALSE])
  if (length(DUB)>0){
    HarDuplikater<-1
    DUB[,LIK:=0]
    DUB[,INTERN:=0]
    setkeym(DUB,c(tabkols,valkols))
    likVerdi<-DUB[unique(DUB[duplicated(DUB)]),which=T]
    if(all(is.na(likVerdi))){DUB[likVerdi,LIK:=1]}
    #D2<-duplicated(FG[,c(tabkols,valkols),with=FALSE])
    setkeym(DUB,c(tabkols,"KOBLID"))
    intern<-DUB[unique(DUB[duplicated(DUB)]),which=T]
    if(all(!is.na(intern))){DUB[intern,INTERN:=1]}
    #D3<-duplicated(FG[,c(tabkols,"KOBLID"),with=FALSE])  
    #DD<-cbind(ROW=which(D1),LIK=D2[D1],INTERN=D3[D1])
    #DD<-cbind(FG[D1,],LIK=D2[D1],INTERN=D3[D1])
    
    #Skriv dubletter til logg
    sqlQuery(globs$log,paste("DELETE * FROM DUBLETT WHERE FILGRUPPE='",filgruppe,"' AND SV='S'",sep=""))
    #Legg til resterende kolonner 
    tmp<-sqlQuery(globs$log,"SELECT * FROM DUBLETT WHERE KOBLID=-1")
    tmp[1:nrow(DUB),]<-NA
    tmp[,names(DUB)]<-DUB
    tmp$FILGRUPPE<-filgruppe
    tmp$BATCHID<-batchdate
    tmp$SV<-"S"
    sqlSave(KHglobs$log,tmp,"DUBLETT",rownames=FALSE,append=TRUE)
    if (versjonert==TRUE){
      tmp$SV<-"V"
      sqlSave(KHglobs$log,tmp,"DUBLETT",rownames=FALSE,append=TRUE)
    }
  }
  setkeym(FG,orgkeys)
  return(HarDuplikater)
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
LagTabellFraFilNo <-function(id,y="",globs=FinnGlobs()){
  filbesk<-FinnFilBesk(id,aar=y,globs)
  FGP<-FinnFilgruppeParametre(filbesk$FILGRUPPE,globs)
  return(LagTabellFraFil(filbesk,FGP=FGP,globs=globs))
}

#
FinnFilgruppeParametre<-function(gruppe,globs=FinnGlobs()){
  dbh<-globs$dbh
  FGP<-as.list(sqlQuery(globs$dbh,paste("SELECT * FROM FILGRUPPER WHERE FILGRUPPE='",gruppe,"'",sep=""),as.is=TRUE))
  #Sette endelig default alder ALLE
  #Default er 0_ALDinf    der ALDinf er global parameter i HOVEDPARAMETRE
  amin<-0
  amax<-as.numeric(sqlQuery(dbh,"SELECT ALDinf FROM HOVEDPARAMETRE")[1])
  #Evt egen def for filgruppe fra ALDER_ALLE i tabell FILGRUPPER
  alle_aldre<-FGP$ALDER_ALLE
  if(grepl("\\d",alle_aldre)){
    alle_aldre<-unlist(strsplit(alle_aldre,"_"))                       
    if (length(alle_aldre)==1){
      amin<-as.numeric(alle_aldre[1])
    } else if (length(alle_aldre)==2){
      amin<-as.numeric(alle_aldre[1])
      amax<-as.numeric(alle_aldre[2])
    }
  }
  return(c(FGP,list(amin=amin,amax=amax))) 
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
  fb<-sqlQuery(globs$dbh,sqlt,stringsAsFactors=FALSE)
  return(fb)
}

#
TilFilLogg<-function (koblid,felt,verdi,batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  #Sjekk om finnes rad for filid, eller lag ny
  if (nrow(sqlQuery(globs$log,paste("SELECT * FROM INNLES_LOGG WHERE KOBLID=",koblid," AND SV='S' AND BATCH='",batchdate,"'",sep="")))==0){
    print("**************Hvorfor er jeg egentlig her?*********************'")
    sqlQuery(globs$log,paste("DELETE * FROM INNLES_LOGG WHERE KOBLID=",koblid,"AND SV='S'",sep=""))
    upd<-paste("INSERT INTO INNLES_LOGG ( KOBLID, BATCH, SV ) SELECT=",koblid,",'",batchdate,"', 'S'",sep="")
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
SkrivKBLogg<-function(KB,type,filbesk,batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  sqlQuery(globs$log,paste("DELETE * FROM KODEBOK_LOGG WHERE KOBLID=",filbesk$KOBLID," AND TYPE='",type,"' AND SV='S'",sep=""))
  sqlSave(globs$log,cbind(KOBLID=filbesk$KOBLID,TYPE=type, SV="S", KB[,c("ORG","KBOMK","OMK","FREQ","OK")],BATCHDATE=batchdate),"KODEBOK_LOGG",rownames=FALSE,append=TRUE)
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

#BUFFER<-SetBuffer(filer=c("NPR","DAAR","BEFOLK"))

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

readRDS_KH<-function(file,...){
  FIL<-readRDS(file,...)
  FIL$KOBLID<-NULL
  FIL$ROW<-NULL
  return(FIL)
}



LagTNPFil<-function(TNPid,batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  ok<-1
  TNF<-LagTnFil(TNPid,batchdate=batchdate,globs=globs)
  
  #Finn landstall
  Part<-list(Gn=data.frame(GEOniv=3))
  TNL<-OmkodFilFraPart(TNL,Part,globs=KHglobs)
  
  
  
}

LagTNFil<-function(TNPid,batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  ok<-1
  datef<-format(strptime(batchdate, "%Y-%m-%d-%H-%M"),"#%Y-%m-%d#")
  TNPdscr<-sqlQuery(globs$dbh,paste("SELECT * FROM TNP_PROD WHERE TNP_NAVN='",TNPid,"' AND VERSJONFRA<=",datef," AND VERSJONTIL>",datef,sep=""),as.is=TRUE)
  
  if (!(is.na(TNPdscr$NEVNERFIL) | TNPdscr$NEVNERFIL=="")){
    TNF<-SySammenFiler(FIL1=list(FILID=TNPdscr$TELLERFIL,ROLLE="TELLER"),FIL2=list(FILID=TNPdscr$NEVNERFIL,ROLLE="NEVNER"))
  } else {
    fil1<-FinnFilN(TNPdscr$TELLERFIL,batch=NA,globs=globs)
    if(fil1$ok==0){
      print(fil1$err)
      ok<-0
    } else {
      if (exists("BUFFER") && TNPdscr$TELLERFIL %in% names(BUFFER)){
        TNF<-BUFFER[[TNPdscr$TELLERFIL]]
      } else {
        TNF<-readRDS_KH(fil1$filn)
      }
    }
  }
  print(sapply(TNF,class))
  print(head(TNF))
  
  #FGP<-FinnFilgruppeParametre(gruppe,globs)
  FGP<-list(amin=0,amax=120)
  
  if (!(is.na(TNPdscr$NEVNERFRARADER) | TNPdscr$NEVNERFRARADER=="")){
    print("*************")
    NF<-EkstraherRadSummer(TNF,TNPdscr$NEVNERFRARADER,FGP=FGP,globs=globs)
    print("_____________")
  }
  #EkstraherRadSummer
  
  return(NF)
}


SySammenFiler<-function(FIL1,FIL2,globs=FinnGlobs()){
  fil1<-FinnFilN(FIL1$FILID,batch=NA,globs=globs)
  fil2<-FinnFilN(FIL2$FILID,batch=NA,globs=globs)
  if(fil1$ok==0 | fil2$ok==0){
    #LOGG FEIL!
    print(fil1$err)
    print(fil2$err)    
    ok<-0
  } else {
    if (exists("BUFFER") && FIL1$FILID %in% names(BUFFER)){
      F1<-BUFFER[[FIL1$FILID]]
    } else {
      F1<-readRDS_KH(fil1$filn)
    }
    cat("Lest inn ",FIL1$ROLLE,"FIL ",FIL1$FILID," (",dim(F1)[1],"x",dim(F1)[2],")\n",sep="")
    if(exists("BUFFER") && FIL2$FILID %in% names(BUFFER)){
      F2<-BUFFER[[FIL2$FILID]]
    } else { 
      F2<-readRDS_KH(fil2$filn)
    }
    cat("Lest inn ",FIL2$ROLLE,"FIL ",FIL2$FILID," (",dim(F2)[1],"x",dim(F2)[2],")\n",sep="")
    cat("Begynner sammmensying av\n")
    SF<-SySammenTabeller(F1,F2)
  }
  
}


FinnFilN<-function(filstr,batch=NA,TYP="STABLAORG",globs=FinnGlobs()){
  fila<-str_split(filstr,":")
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



LagAvledTab<-function(avledid,globs=FinnGlobs()){
  
  ok<-1
  #Bygg ut til datert
  ATspec<-as.list(sqlQuery(globs$dbh,paste("SELECT * FROM AVLEDTAB WHERE AVLEDID=",avledid),stringsAsFactors=FALSE)[1,])
  #Bygg ut til i AVLEDA vs STABLAORG
  #Bygg ut til datert
  filn1<-paste(globs$path,"/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE",ATspec$TAB1,".rds",sep="")
  filn2<-paste(globs$path,"/PRODUKTER/MELLOMPROD/R/STABLAORG/NYESTE",ATspec$TAB2,".rds",sep="")
  if (file.access(filn1,mode=0)==-1){
    print(paste("FEIL!",filn1,"finnes ikke!"))
    ok<-0
  }
  if (file.access(filn2,mode=0)==-1){
    print(paste("FEIL!",filn2,"finnes ikke!"))
    ok<-0
  }
  #Bygg ut til at BEFOLK med geoharm er klar til bruk
  if (ok==1){
    FIL1<-readRDS(filn1)
    if (ATspec$GEOH_PRE==1){FIL1<-GeoHarm(FIL1,globs=globs)}
    if (ATspec$TAB2=="KLARGEO"){
      FIL2<-globs$askdjalsd
    } else {
      FIL2<-readRDS(filn2)
      if (ATspec$GEOH_PRE==1){FIL2<-GeoHarm(FIL2,globs=globs)}    
    }
  }
  SF<-SySammenFiler(FIL1,FIL2,globs)
  AF<-SF$AF
  ok<-SF$OK
  
}


GeoHarm<-function(FIL,batchdate=SettKHBatchDate(),globs=FinnGlobs()){
  if (identical(class(FIL),"data.frame")){FIL<-data.table(FIL)}
  keyorg<-key(FIL)
  
  tabnames<-names(FIL)[!names(FIL) %in% c("VAL1","VAL2","VAL3","VAL1f","VAL2f","VAL3f")]
  Vals<-names(FIL)[names(FIL) %in% c("VAL1","VAL2","VAL3","VAL1f","VAL2f","VAL3f")]
  setkeym(FIL,tabnames)
  
  geoomk<-globs$KnrHarm
  FIL$GEO<-mapvalues(FIL$GEO,geoomk$KNRorg,geoomk$KNRharm,warn_missing = FALSE)
  FIL<-KHaggreger(FIL,globs=globs)  
  FIL$FYLKE[FIL$GEOniv<3]<-substr(FIL$GEO[FIL$GEOniv<3],1,2)
  setkeym(FIL,keyorg)
  return(FIL)
}


KHaggreger<-function(FIL,globs=FinnGlobs()){
  print(head(FIL))
  if (identical(class(FIL),"data.frame")){FIL<-data.table(FIL)}
  orgkeys<-key(FIL)
  tabnames<-globs$TabKols[globs$TabKols %in% names(FIL)]
  #tabnames<-names(FIL)[!grepl("^VAL\\d+(f|)$",names(FIL))]
  vals<-names(FIL)[!names(FIL) %in% tabnames]
  vals<-vals[!grepl("\\.(f|a)",vals)]
  vals<-vals[!vals %in% c("KOBLID","ROW")]
  setkeym(FIL,tabnames)  #Sjekk om key ok for å effektivisere?
  lp<-paste("list(",
        paste(vals,"=sum(",vals,",na.rm=TRUE),",vals,".f=max(",vals,".f),",vals,".a=sum(",vals,".a)",sep="",collapse=","),
        ")",sep="")
  FILa<-FIL[, eval(parse(text=lp)), by=tabnames]
  setkeym(FIL,orgkeys)
  return(FILa)
}


SySammenTabeller<-function(F1,F2,globs=FinnGlobs(),GYKA2=NULL){
  if (identical(class(F1),"data.frame")){F1<-data.table(F1)}
  if (identical(class(F2),"data.frame")){F2<-data.table(F2)}
  orgkey1<-key(F1)
  orgkey2<-key(F2)
  
  FU<-data.table()
  ok<-1
  
  GYKAtabs<-globs$OmkKols
  GYKAtabs1<-GYKAtabs[GYKAtabs %in% names(F1)]
  GYKAtabs2<-GYKAtabs[GYKAtabs %in% names(F2)]
  
  AllTabs<-globs$TabKols
  AllTabs1<-AllTabs[AllTabs %in% names(F1)]
  AllTabs2<-AllTabs[AllTabs %in% names(F2)]
  
  
  
  if (identical(GYKAtabs1,GYKAtabs2)){
    GYKAtabs<-GYKAtabs1
    setkeym(F1,AllTabs1)
    setkeym(F2,AllTabs2)
    
    DF1<-FinnDesign(F1)
    #tab1<-as.data.frame(unique(F1[,tabnames1,with=FALSE]))
    #Åpner for å kunne sende en ferdig tabulering (se særlig: BEFOLK brukes mange ganger og tar tid å tabulere)
    if (is.null(GYKA2)){
      DF2<-FinnDesign(F2)
      #tab2<-as.data.frame(unique(F2[,tabnames2,with=FALSE]))  
    }
    
    
    #Sett felles TAB, trenger bare gjøre det for PART$Y,PART$A og OmkDesign
    DFF<-list()
    for (del in names(DF1$Part)){
      DFF$Part[[del]]<-unique(rbind(DF1$Part[[del]],DF2$Part[[del]]))
      setkeym(DFF$Part[[del]],key(DF1$Part[[del]]))      
    }
    
    OmkDesignF<-merge(DF1$OmkDesign,DF2$OmkDesign,all=TRUE,suffixes=c(1:2))
    OmkDesignF[is.na(OmkDesignF$HAR1),HAR1:=0]
    OmkDesignF[is.na(OmkDesignF$HAR2),HAR2:=0]
    DFF$OmkDesign<-OmkDesignF
    
#      OmkDesignF<-rbind(DF1$OmkDesign,DF2$OmkDesign)
#     setkeym(OmkDesignF,key(DF1$OmkDesign))
#     DFF$OmkDesign<-OmkDesignF[,list(HAR=max(HAR)),by=key(OmkDesignF)]
#     
    KB12<-FinnRedesign(DF1,DFF)$FULL
    KB21<-FinnRedesign(DF2,DFF)$FULL
    
    OmkCols<-names(KB12)[grepl("_omk",names(KB12))]
    setkeym(KB12,OmkCols)
    setkeym(KB21,OmkCols)
    
    Omk1<-unique(KB12[KB12$Dekk==1,OmkCols,with=FALSE])
    Omk2<-unique(KB21[KB21$Dekk==1,OmkCols,with=FALSE])
    
    OmkDesignF[,DEKK1:=0]
    OmkDesignF[,DEKK2:=0]
    OmkDesignF[Omk1,DEKK1:=1]
    OmkDesignF[Omk2,DEKK2:=1]

    OmkF<-subset(OmkDesignF,DEKK1==1 & DEKK2==1)
    
    #OmkF<-Omk1[Omk2,nomatch=0]
    
        
    KB12<-KB12[OmkF]
    KB21<-KB21[OmkF]
         
        
    F1<-OmkodFil(F1,KB12)
    F2<-OmkodFil(F2,KB21)
    
    
    #Merge
    FU<-F2[F1,allow.cartesian=TRUE]
    
    setkeym(F1,orgkey1)
    setkeym(F2,orgkey2)
    
  } else {
    cat("Kan ikke\n")
  } 
  return(list(SF=FU,ok=ok))
}


#
SySammenFilerG<-function(F1,F2,globs=FinnGlobs(),GYKA2=NULL){
  if (identical(class(F1),"data.frame")){F1<-data.table(F1)}
  if (identical(class(F2),"data.frame")){F2<-data.table(F2)}
  orgkey1<-key(F1)
  orgkey2<-key(F2)
  
  FU<-data.table()
  ok<-1
  
  GYKAtabs<-globs$OmkKols
  GYKAtabs1<-GYKAtabs[GYKAtabs %in% names(F1)]
  GYKAtabs2<-GYKAtabs[GYKAtabs %in% names(F2)]
  
  AllTabs<-globs$TabKols
  AllTabs1<-AllTabs[AllTabs %in% names(F1)]
  AllTabs2<-AllTabs[AllTabs %in% names(F2)]
  
  
  
  if (identical(GYKAtabs1,GYKAtabs2)){
    GYKAtabs<-GYKAtabs1
    setkeym(F1,AllTabs1)
    setkeym(F2,AllTabs2)
         
    DF1<-FinnDesign(F1)
    #tab1<-as.data.frame(unique(F1[,tabnames1,with=FALSE]))
    #Åpner for å kunne sende en ferdig tabulering (se særlig: BEFOLK brukes mange ganger og tar tid å tabulere)
    if (is.null(GYKA2)){
      DF2<-FinnDesign(F2)
      #tab2<-as.data.frame(unique(F2[,tabnames2,with=FALSE]))  
    }
    
    
    
    
    
    KB12<-FinnRedesign(DF1,DF2)$FULL
    KB21<-FinnRedesign(DF2,DF1)$FULL
    
    
    #Konsolider kodebøker til felles GYKA
    ##fellestab<-unique(c(KB12$omkTAB,KB21$omkTAB))
    omktabs<-paste(GYKAtabs,"_omk",sep="")
    GYKAfelles<-unique(rbindlist(list(KB12[,omktabs,with=FALSE],KB21[,omktabs,with=FALSE])))
    print(GYKAfelles)
    setkeym(GYKAfelles,omktabs)
    setkeym(KB12,omktabs)
    setkeym(KB21,omktabs)
    
        
    KB12<-KB12[GYKAfelles,allow.cartesian=TRUE]
    KB21<-KB21[GYKAfelles,allow.cartesian=TRUE]
    
    #print(sapply(KB12,class))
    
    KB12[KB12$Dekk==0,c(GYKAtabs):=KB12[KB12$Dekk==0,omktabs,with=FALSE]]
    KB12[KB12$Dekk==0,Dekk:=1]
    KB21[KB21$Dekk==0,c(GYKAtabs):=KB21[KB21$Dekk==0,omktabs,with=FALSE]]
    KB21[KB21$Dekk==0,Dekk:=1]
    
    print(subset(KB12,Dekk==1))
    print(subset(KB21,Dekk==1))
    
    
    F1<-OmkodFil(F1,KB12)
    F2<-OmkodFil(F2,KB21)
    
    print(F1)
    print(F2)
    
    
    #Merge
    FU<-F2[F1]
    
    setkeym(F1,orgkey1)
    setkeym(F2,orgkey2)
   
  } else {
    cat("Kan ikke\n")
  } 
  return(list(SF=FU,ok=ok))
}


#pstrorg="ALDER %in% \"ALLE\" & TAB1 %in% c(\"eur33\",\"eur34\")"
EkstraherRadSummer<-function(FIL,pstrorg,FGP=list(amin=0,amax=120),globs=FinnGlobs()){
  amin<-FGP$amin
  amax<-FGP$amax
  #NB: takler ikke "|" (or) i pstrorg
  
  
  #Rydd i "feil syntaks"
  pstrorg<-gsub("(?<!=)=(?!=)","==",pstrorg,perl=TRUE)
  pstrorg<-gsub(" *== *(?=c\\()"," %in% ",pstrorg,perl=TRUE)
  
  #Finn kolonner involvert
  alletabs<-str_replace(unlist(str_split(pstrorg," *& *")),"^(\\w*?) *(%in%.*|==.*| *$)","\\1")
  
  
  #Standard "alle"-verdier
  pstrorg<-gsub("(^ *|& *)ALDER( *&| *$)","ALDER==\\1\"ALLE\"\\2",pstrorg)
  pstrorg<-gsub("(^ *|& *)(KJONN|UTD|LAND|SIVST)( *&| *$)","\\1\\2==0\\3",pstrorg)
 
  #Intervaller
  pstrorg<-gsub("ALDER *(={1,2}|%in%) *\"ALLE\"",paste("ALDERl==",amin," & ALDERh==",amax,sep=""),pstrorg)
  pstrorg<-gsub("ALDER *(={1,2}|%in%) *(\\d+)$","ALDERl==\\2 & ALDERh==\\2",pstrorg)
  pstrorg<-gsub("AAR *(={1,2}|%in%) *(\\d+)$","AARl==\\2 & AARh==\\2",pstrorg)
  #Fjern de som ikke er del av subset betingelse
  substr<-gsub("(?<=[&^]) *\\w+ *(?=[&$])","",pstrorg,perl=TRUE)
  substr<-gsub("&+","&",substr)
  
  #Splitt i kolonnenavn og verdi
  subtabs<-str_replace(unlist(str_split(substr," *& *")),"^(\\w+) *(%in%.*|==.*)","\\1")
  subvals<-str_replace(unlist(str_split(substr," *& *")),"^(.*%in% *|.*== *)(\\w+)","\\2")
  #Filtrer til de som er aktuelle for omkoding  
  subtabs<-subtabs[names(subtabs) %in% globs$OmkKols]
  subvals<-subvals[names(subtabs) %in% globs$OmkKols]
  
  if (length(subtabs)>0){
    
    
    omk<-as.data.frame(pp[names(pp) %in% globs$OmkKols],stringsAsFactors=FALSE)  
    omk[,]<-as.numeric(omk[,])
    omkD<-FinnDesign(omk,amin=amin,amax=amax,globs=globs)
    FIL<-OmkodFilFraPart(FIL,omkD$Part,globs=globs)
  }
  FIL<-eval(parse(text=paste("subset(FIL,",substr,")",sep="")))
  FIL<-KHaggreger(FIL[,names(FIL)[!names(FIL) %in% tabs]],globs=globs)
  print(head(FIL))
  #Summer!!!!
  return(FIL)  
}

#
OmkodFil<-function(FT,KB,aggreger=1,globs=FinnGlobs()){
  if (identical(class(FT),"data.frame")){FT<-data.table(FT)}
  orgkeys<-key(FT)
  orgNames<-names(FT)
  
  Omktabs<-globs$DesignKols
  Omktabs<-Omktabs[Omktabs %in% names(KB)]
  nonOmktabs<-orgNames[!orgNames %in% Omktabs]
  
  #Merge med kodebok
  setkeym(FT,Omktabs)
  setkeym(KB,Omktabs)
  
  KB<-subset(KB,Dekk==1)
  
  
  FT<-KB[FT,nomatch=0,allow.cartesian=TRUE]
  
  
  #Geo aggregering
#   FT[,GEO_omk:=GEO]
#   FT[FT$GEOniv==1 & FT$GEOniv_omk==2,GEO_omk:=FYLKE]
#   FT[FT$GEOniv==1 & FT$GEOniv_omk==3,c("GEO_omk","FYLKE"):=list("0","00")]
#   FT[FT$GEOniv==2 & FT$GEOniv_omk==3,c("GEO_omk","FYLKE"):=list("0","00")]
#   FT[FT$GEOniv==1 & FT$GEOniv_omk==2,GEO_omk:=FYLKE]
  FT[FT$GEOniv==1 & FT$GEOniv_omk==2,GEO:=FYLKE]
  FT[FT$GEOniv==1 & FT$GEOniv_omk==3,c("GEO","FYLKE"):=list("0","00")]
  FT[FT$GEOniv==2 & FT$GEOniv_omk==3,c("GEO","FYLKE"):=list("0","00")]
  
  #Kutt bort gamle tab-verdier
  Omktabs_omk<-paste(Omktabs,"_omk",sep="")
  FT<-FT[,c(Omktabs_omk,nonOmktabs),with=FALSE]
  setnames(FT,c(Omktabs,nonOmktabs))
  
  
  FT<-KHaggreger(FT,globs=globs)
  
  #Sett tilbake til gammel layout
  FT<-FT[,orgNames,with=FALSE]
  setkeym(FT,orgkeys)
  #FT[, lapply(.SD, KHsum), by=c(Omktabs_omk,Etabs,"FYLKE"), .SDcols=Vals]
  #FT2<-aggregate(FT[,Vals],by=FT[,c(Omktabs_omk,Etabs,"FYLKE")],FUN=sum)
  return(FT)  
}

ModifiserDesign<-function(Nytt,Org=list(),globs=FinnGlobs()){
  for (del in names(Nytt)){
    Org$Part[[del]]<-Nytt[[del]]  
  }
  delerlist<-paste("as.data.frame(Org[[\"Part\"]][[\"",names(Org$Part),"\"]])",sep="",collapse=",")
  #delerlist<-paste("as.data.frame(Org[[\"Part\"]][[\"",names(Org$Part)[names(Org$Part) %in% c("Gn","Y","K","A")],"\"]])",sep="",collapse=",")
  FullDesign<-data.table(eval(parse(text=paste("expand.grid.df(",delerlist,")",sep=""))))
  FullDesign[,HAR:=1]
  OmkKols<-globs$OmkKols[globs$OmkKols %in% names(FullDesign)]
  setkeym(FullDesign,OmkKols)
  Org[["OmkDesign"]]<-FullDesign[,list(HAR=max(HAR)),by=OmkKols]
  return(Org)
}


#Part<-list(Gn=data.frame(GEOniv=3))
OmkodFilFraPart<-function(Fil,Part,globs=FinnGlobs()){
  Dorg<-FinnDesign(Fil,globs=globs)
  KB<-FinnRedesign(Dorg,ModifiserDesign(Part,Dorg,globs=globs))$FULL
  return(OmkodFil(Fil,KB))
}


#####################################################################################

SettDefDesignKH<-function(){
  DelKols=list(Gn=c("GEOniv"),Y=c("AARl","AARh"),K=c("KJONN"),A=c("ALDERl","ALDERh"),
          T1=c("TAB1"),T2=c("TAB2"),T3=c("TAB3"))
  KolsDel<-list()
  for (del in names(DelKols)){
    for (kol in DelKols[[del]]){
      KolsDel[[kol]]<-del
    }
  }
  
  return(
  list(DelKols=DelKols,
       KolsDel=KolsDel,
       UBeting=c("Gn","Y"),
       BetingOmk=c("K","A"),
       BetingF=c("T1","T2","T3"),
       AMissAllow=TRUE
  ))
}

BalanserDesign<-function(FG){
  #FRA<-sqldf("SELECT DISTINCT AARl,AARh,ALDERl,ALDERh,GEOniv,KJONN FROM FG")
  tabs<-character()
    
  if (all(c("AARl","AARh","GEOniv") %in% names(FG))){
    AARt<-unique(FG[,c("AARl","AARh")])
    GEOt<-data.frame(GEOniv=unique(FG[,c("GEOniv")]))   #Spesial for bydel?!
    tabs<-c(tabs,c("AARl","AARh","GEOniv"))
    BALANSE<-expand.grid.df(AARt,GEOt)
  }
  #else:   KRITISK FEIL, ABORT!!
      
  if (all(c("ALDERl","ALDERh") %in% names(FG))){
    ALDERt<-unique(FG[,c("ALDERl","ALDERh")])
    tabs<-c(tabs,c("ALDERl","ALDERh"))
    BALANSE<-expand.grid.df(BALANSE,ALDERt)
  }
  if (all(c("KJONN") %in% names(FG))){
    KJONNt<-data.frame(KJONN=unique(FG[,c("KJONN")]))
    tabs<-c(tabs,c("KJONN"))
    BALANSE<-expand.grid.df(BALANSE,KJONNt)
  }
    
  FRA<-unique(FG[,tabs])
    
  return(FinnBesteKodebokGYKA(FRA,BALANSE))
}






FinnDesign<-function(FIL,amin=0,amax=120,globs=FinnGlobs()){
  if (identical(class(FIL),"data.frame")){FIL<-data.table(FIL)}
  keyorg<-key(FIL)
  
  #Sett defdesign
  DelKol<-globs$DefDesign$DelKol
  UBeting<-globs$DefDesign$UBeting
  BetingOmk<-globs$DefDesign$BetingOmk
  BetingF<-globs$DefDesign$BetingF
  DesignKols<-globs$DesignKols[globs$DesignKols %in% names(FIL)]
  OmkKols<-globs$OmkKols[globs$OmkKols %in% names(FIL)]
  #setkeym(FIL,c(DesignKols))
  
  #Initier tomt resultat
  Design<-list()
  #Finn faktisk design
  ObsDesign<-unique(FIL[,DesignKols,with=FALSE])
  
  #Finn deler inneholdt i tabell
  Deler<-character()
  for (del in names(DelKol)){
    if(all(DelKol[[del]] %in% names(FIL))){
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
    kols<-DelKol[[del]]
    setkeyv(ObsDesign,kols)
    Design[["Part"]][[del]]<-data.table(unique(ObsDesign[,kols,with=FALSE]),key=kols)
  }
  
  
  #Fyll evt hull i aldersintervaller 
  if (globs$DefDesign$AMissAllow==TRUE){
    if ("A" %in% names(Design$Part)){
      mangler<-interval_difference(Intervals(c(amin,amax),type='Z'),Intervals(Design[["Part"]][["A"]],type='Z'))
      Design[["Part"]][["A"]]<-rbind(Design[["Part"]][["A"]],as.matrix(mangler))
    }
  }
  
  #Finn fullt design, dvs kryssing av alle partielle. 
  delerlist<-paste("as.data.frame(Design[[\"Part\"]][[\"",Alle,"\"]])",sep="",collapse=",")
  FullDesign<-data.table(eval(parse(text=paste("expand.grid.df(",delerlist,")",sep=""))))
  setkeym(ObsDesign,names(ObsDesign))
  setkeym(FullDesign,names(FullDesign))
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
      for (k in komb){kols<-c(kols,DelKol[[k]])}
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




FinnRedesign<-function(DesFRA,DesTIL){
  #Merk assymtri mellom DesFRA og DesTIL. 
  #For DesTIL brukes bare DesTil$Part og DesTIL$OmkDesign.
  #DesFRA må derfor komme fra FinnDesign med alle egenskaper satt der, mens DesTIL kan være enklere og satt andre steder
  
  
  Redesign<-list()
  #Finner ikke noen mer generisk måte å sette dette på
  #GEOniv
  Parts<-list()
  if ("Gn" %in% names(DesFRA$Part)){
    KBGn<-data.frame("GEOniv"=c(1,2,1,3,2,1,-1),"GEOniv_omk"=c(1,2,2,3,3,3,-1),"GEOniv_ok"=1,"GEOniv_pri"=c(1,1,2,1,2,3,1))
    Parts[["Gn"]]<-KBGn[KBGn$GEOniv_omk %in% DesTIL$Part$Gn$GEOniv,]
  } 
  #AAR
  if ("Y" %in% names(DesFRA$Part)){
    Parts[["Y"]]<-FinnKodebokIntervaller(DesFRA$Part$Y,DesTIL$Part$Y)
  }
  #KJONN
  if ("K" %in% names(DesFRA$Part)){
    KBK<-data.frame("KJONN"=c(0,1,2,1,2,9),"KJONN_omk"=c(0,0,0,1,2,9),"KJONN_ok"=1,"KJONN_pri"=c(1,2,2,1,1,1))
    Parts[["K"]]<-KBK[KBK$KJONN_omk %in% DesTIL$Part$K$KJONN,]
  }
  #ALDER
  if ("A" %in% names(DesFRA$Part)){
    Parts[["A"]]<-FinnKodebokIntervaller(DesFRA$Part$A,DesTIL$Part$A)
  }
  
  
  #Full GnYKA, alle kombinasjoner. Foreløoig uavhengig av om disse er støttet av desFRA
  delerlist<-paste("Parts[[\"",DesFRA[["OmkDeler"]],"\"]]",sep="",collapse=",")
  FULL<-data.table(eval(parse(text=paste("expand.grid.df(",delerlist,")",sep=""))))
  FULL[,Dekk:=1]
  
  for (del in c("",DesFRA[["BetingOmk"]])){
    komb<-c(DesFRA[["UBeting"]],del)
    kombn<-paste("bet",del,sep="")
    if (!identical(komb,"")){
      komblist<-paste("Parts[[\"",komb,"\"]]",sep="",collapse=",")
      betD<-data.table(eval(parse(text=paste("expand.grid.df(",delerlist,")",sep=""))))
      
      #Koble med DeSFRA, limer på HAR
      setkeyv(betD,key(DesFRA[["SKombs"]][[kombn]]))
      betD<-DesFRA[["SKombs"]][[kombn]][betD,allow.cartesian=TRUE]
      betD$HAR[is.na(betD$HAR)]<-0
                   
      #Kolonner for omkoding. Nb: betinget på pri      
      OmkPriCols<-names(betD)[grepl("_(omk|pri)$",names(betD))]
      
      #Hiv bort omkodinger som ikke har HAR==1 for alle fra, dvs de som ikke støttes av fra-siden
      setkeyv(betD,OmkPriCols)
      
      #Sjekk i hvilken grad omkodingene støttes av DesFRa. 
      if (del=="A"){
        #DekkH<-betA[,list(DEKK=sum(1+ALDERh-ALDERl)/(1+ALDERh_omk-ALDERl_omk),NH=.N),by=c(OmkPriCols,"HAR")]
        DekkH<-betD[betD$HAR==1,list(DekkAAR=sum(1+ALDERh-ALDERl),NHAR=.N),by=OmkPriCols]
        Dekk<-betD[,list(TotAAR=(1+ALDERh_omk-ALDERl_omk),NTOT=.N),by=OmkPriCols]
        setkeyv(DekkH,OmkPriCols)
        setkeyv(Dekk,OmkPriCols)    
        Dekk<-Dekk[DekkH]
       
        #Filtrer til de som er OK gitt eksterne kritereier 
        DekkOK<-subset(Dekk, DekkAAR/TotAAR>0.999 | (NTOT>=10 & NHAR/NTOT>0.8) | (TotAAR<=20 & DekkAAR>=10) | TotAAR<=10) 
        setkeyv(betD,OmkPriCols) 
        
        #Behold bare de med OK dekning
        betD<-betD[DekkOK]    
      } else {
        betD<-betD[!betD[HAR==0,],allow.cartesian=TRUE]
        betD$HAR<-NULL
      }
      #Filtrer FULL
      setkeyv(FULL,OmkPriCols)
      FULL[!betD,Dekk:=0,allow.cartesian=TRUE]
    }
  }
  FULL<-subset(FULL,Dekk==1)
  
 
  if (nrow(FULL)>0){  
    prios<-list(AAR="keep",GEOniv="keep",KJONN="keep",ALDER="keep")
    OmkCols<-names(FULL)[grepl("_(omk)$",names(FULL))]
    PriCols<-names(FULL)[grepl("_(pri)$",names(FULL))]
    setkeym(FULL,c(OmkCols,PriCols))
    for (del in names(prios)){
      #if (del in names)
      del_pri<-paste(del,"_pri",sep="")
      if (prios[[del]]=="keep"){
          Topp<-eval(parse(text=paste("FULL[,list(",del_pri,"=min(",del_pri,")),by=OmkCols]",sep="")))
      } else if (prios[[del]]=="agg"){
        Topp<-eval(parse(text=paste("FULL[,list(",del_pri,"=max(",del_pri,")),by=OmkCols]",sep="")))
      }
      setkeym(Topp,names(Topp))
      setkeym(FULL,names(Topp))
      FULL<-FULL[Topp,names(FULL),with=FALSE]
    }
  }
  #Sett inn udekket
  orgtilkey<-key(DesTIL$OmkDesign)
  tilkols<-names(DesTIL$OmkDesign)[names(DesTIL$OmkDesign) %in% names(FULL)]
  omkkols<-paste(tilkols,"_omk",sep="")
  setkeym(FULL,omkkols)
  
  setkeym(DesTIL$OmkDesign,tilkols)
  FULL<-FULL[DesTIL$OmkDesign,allow.cartesian=TRUE]
  FULL[is.na(FULL$Dekk),Dekk:=0]
  setkeym(DesTIL$OmkDesign,orgtilkey)
  
   
  ekols<-c("HAR","HAR1","HAR2","Dekk")
  ekols<-ekols[ekols %in% names(FULL)]
  FULL<-FULL[,c(tilkols,omkkols,ekols),with=FALSE]
  
   
  return(list(FULL=FULL,Parts=Parts))
}


FinnKodebokGYKA<-function(FRA,TIL){
  #Sjekk at samme kolonner!!
  #Må gi bedre respons!!!
  if(!all(names(FRA)==names(TIL))){
    print("KRITISK FEIL FinnKodebokGYKA!!!!")
    print(names(FRA))
    print(names(TIL))
    
  }  
  
  #Lag KBGYKA for fullt balansert design, dvs. kryssing av kodebøker for GEOniv,AAR,KJONN,ALDER separat 
  orgtabs<-character(0)
  deler<-character()
  KBGYKA<-data.frame()
  
  #AAR
  if (!is.null(FRA$Y)){
    orgtabs<-c(orgtabs,"AARl","AARh")
    KBY<-FinnKodebokIntervaller(FRA$Y,TIL$Y)
    deler<-c(deler,"KBY")
  } 
  
  #GEOniv
  if (!is.null(FRA$Gn)){
    orgtabs<-c(orgtabs,"GEOniv")
    KBG<-data.frame("GEOniv"=c(1,2,1,3,2,1,-1),"GEOniv_omk"=c(1,2,2,3,3,3,-1),"GEOniv_ok"=1,"GEOniv_pri"=c(1,1,2,1,2,3,1))
    deler<-c(deler,"KBG")
  } 
  
  #ALDER
  if (!is.null(FRA$A)){
    orgtabs<-c(orgtabs,c("ALDERl","ALDERh"))
    KBA<-FinnKodebokIntervaller(FRA$A,TIL$A)
    KBA<-subset(KBA,ALDER_ok==1)
    deler<-c(deler,"KBA")
  } 
  
  #KJONN
  if (!is.null(FRA$K)){
    orgtabs<-c(orgtabs,c("KJONN"))
    KBK<-data.frame("KJONN"=c(0,1,2,1,2,9),"KJONN_omk"=c(0,0,0,1,2,9),"KJONN_ok"=1,"KJONN_pri"=c(1,2,2,1,1,1))
    deler<-c(deler,"KBK")
  } 
  
  delstr<-paste(deler,collapse=",")
  KBGYKA<-eval(parse(text=paste("expand.grid.df(",delstr,")",sep="")))
  
#   if (is.null(FRA$Y)){
#     KBGYKA$AAR_pri<-1
#   }
#   if (is.null(FRA$Gn)){
#     KBGYKA$GEOniv_pri<-1
#   }
#   if (is.null(FRA$A)){
#     KBGYKA$ALDER_pri<-1
#   }  
#   if (is.null(FRA$K)){
#     KBGYKA$KJONN_pri<-1
#   }
  
  #Veier pri mot hverandre. "Smådirty" triks for å få leksikografisk veiing ved å gange med 10 per lag i hierarkiet.
  #Altså: aggregerer først/helst over år, deretter kjønn, alder og geo
  #Ikke helt sikekr på om dette alltid er best, men usikker på om mye å vinne på å gjøre mer avansert
  
  #KBGYKA$PRI<-1000*KBGYKA$AAR_pri+100*KBGYKA$KJONN_pri+10*KBGYKA$ALDER_pri+KBGYKA$GEOniv_pri
  KBGYKA$PRI<-1000*ifelse(is.null(FRA$Y),1,KBGYKA$AAR_pri)+100*ifelse(is.null(FRA$K),1,KBGYKA$KJONN_pri)+10*ifelse(is.null(FRA$A),1,KBGYKA$ALDER_pri)+ifelse(is.null(FRA$Gn),1,KBGYKA$GEOniv_pri)
  
  
  #Rydd kolonne
  omktabs<-paste(orgtabs,"_omk",sep="")
  KBGYKA<-KBGYKA[,c(orgtabs,omktabs,"PRI")]   
  
  setDT(KBGYKA)
  setkeyv(KBGYKA,c(orgtabs,omktabs))
  
    
  #Finn omkodinger som faktisk er mulige fra evt ubalansert design i FRA
  if (nrow(KBGYKA)>0){
    #Legg til attributtet HAR fra FRA$GYKATfull
    KBGYKA<-FRA$GYKATfull[KBGYKA]
    print(KBGYKA)
    print(key(KBGYKA))
    
    #Finner alle til-omkodninger som mangler en eller flere orgTAB i FRA
    umuligKBGYKA <- KBGYKA[!FRA$GYKA]
    setkeyv(umuligKBGYKA,c(omktabs,"PRI"))
    umuligeOmk <- unique(umuligKBGYKA[,c(omktabs,"PRI"),with=FALSE])
    setkeyv(KBGYKA,c(omktabs,"PRI"))
    KBGYKA<-KBGYKA[!umuligeOmk]  
  } 


  setkeyv(KBGYKA,c(omktabs,"PRI"))
  
  #Dev skal det være opsjon for denne? Dvs skal også delsummer ikke i TIL kunne lages som default? 
  KBGYKA<-KBGYKA[TIL$GYKA,allow.cartesian=TRUE] #Merge on omktabs
  #KBGYKA<-merge(KBGYKA,setNames(TIL$GYKA,omktabs),by=omktabs,all=TRUE)
  #Rydd kolonne
  KBGYKA<-KBGYKA[,c(orgtabs,omktabs,"PRI"),with=FALSE]    
  #ok<-ifelse(nrow(subset(KBGYKA,is.na(PRI)))==0,1,0)
  return(KBGYKA)
}

FinnBesteKodebokGYKA <- function(FRA,TIL,globs=FinnGlobs()){
  KBGYKA<-FinnKodebokGYKA(FRA,TIL)
  kols<-names(KBGYKA)
  omktabs<-paste(globs$DesignKols,"_omk",sep="")
  omktabs<-omktabs[omktabs %in% names(KBGYKA)]
  if (nrow(KBGYKA)>0){
    Beste<-KBGYKA[,list(PRI=min(PRI)),by=omktabs]
    setkeym(Beste,c(omktabs,"PRI"))
    setkeym(KBGYKA,c(omktabs,"PRI"))
    #Reduserer full kodebok til bare de valgte/beste omkodingene
    KBGYKA<-KBGYKA[Beste]
  }
  #Sett original kolonnerekkefølge etter merge
  return(KBGYKA[,kols,with=FALSE])
}
 
FinnUbalanse<-function(FG){
  GYKAT<-FinnDesign(FG)
  
}



Test<-function () {
  FRA<-sqlQuery(KHglobs$dbh,"SELECT * FROM TEST")
  TIL<-sqlQuery(KHglobs$dbh,"SELECT * FROM TEST2")
  
  SySammenFiler(FRA,TIL)
  #FinnKodebokGYKA(FinnDesign(FRA),FinnDesign(TIL))
}


FinnKodebokIntervaller<-function(FRA,TIL,storst=TRUE){
  #I tilfelle input er data.table
  FRA<-as.data.frame(FRA)
  TIL<-as.data.frame(TIL)
  #Bruk Intrevals-klassen
  typ<-sub("^(.*)l$","\\1",names(FRA[1]),ignore.case = TRUE)
  utcolnavn<-c(names(FRA),paste(names(FRA),"_omk",sep=""),paste(typ,"_ok",sep=""),paste(typ,"_pri",sep=""))
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
  if(class(KAND)=="matrix"){  #Irriterende bug(?) i interval når TILi har dim 1
    KAND<-list(KAND)
  }
  
  #Finn intern overlapp i FRA
  OVLP<-interval_overlap(FRAi,FRAi) 
  #Initier tom kodebok
  KODEBOK0<-as.data.frame(setNames(replicate(length(utcolnavn),numeric(0), simplify = F), utcolnavn))
  KODEBOK<-KODEBOK0
  #Må loope over alle TIL. Kan dette vektoriseres? Kanskje ikke så mye å vinne?
  for (i in 1:nrow(TIL)){
    result<-list("ok"=0,"pri"=1,"KODEBOK"=KODEBOK0)
    result<-FinnKodebokForEtIntervall(KAND[[i]],FRA,TIL[i,],OVLP,0,result,utcolnavn)
    #???????? ok==1???????
    if (result$ok==0){
      result$KODEBOK<-rbind(result$KODEBOK,as.data.frame(setNames(list(NA_integer_,NA_integer_,TIL[i,1],TIL[i,2],0,1),utcolnavn)))
    }
    #print(result$KODEBOK)
    KODEBOK<-rbind(KODEBOK,result$KODEBOK)
  }
  return(KODEBOK)
}

FinnKodebokForEtIntervall<-function(Find,FRA,TILint,OVLP,letn,result,utcolnavn){
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





