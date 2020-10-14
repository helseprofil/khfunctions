

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

### FinnRedesign
### -------------

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




LagKubeDatertCsv("ENPERSON_NH")
