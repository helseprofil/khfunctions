
filn <-
  "F:/Prosjekter/Kommunehelsa/PRODUKSJON/ORGDATA/KUHR/ORG/2019/2017 Lege - affektive og depressive lidelser.xlsx "

LagTabellFraFil()

LagTabellFraFil<-function (filbesk,FGP,batchdate=SettKHBatchDate(),diagnose=0,globs=FinnGlobs(),versjonert=FALSE,echo=FALSE,dumps=list()) {
  
  klokke<-proc.time()
  ######################################################
  #INNLESING
  ## filn<-filbesk$filn
  filn <- "F:/Prosjekter/Kommunehelsa/PRODUKSJON/ORGDATA/KUHR/ORG/2019/2017 Lege - affektive og depressive lidelser.xlsx "
  
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
