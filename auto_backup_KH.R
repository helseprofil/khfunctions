require(RODBC)
date<-format(Sys.time(), "%Y%m%d%H%M")

styrpath<-"F:/Prosjekter/Kommunehelsa/PRODUKSJON/STYRING"
styrpath_b<-"F:/Prosjekter/Kommunehelsa/PRODUKSJON/STYRING/VERSJONSARKIV"
styrvfiles<-list.files(path=styrpath_b)

KHcFN<-paste(styrpath,"KHELSA.mdb",sep="/")
KHvFN<-paste(styrpath_b,sort(styrvfiles[grepl("^KHELSA\\d+.mdb$",styrvfiles)],decreasing=TRUE)[1],sep="/")

khc<-odbcConnectAccess2007(KHcFN)
khv<-odbcConnectAccess2007(KHvFN)


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
  KHnFN<-sub("KHELSA.mdb",paste("KHELSA",date,".mdb",sep=""),KHcFN)
  KHnFN<-sub(styrpath,styrpath_b,KHnFN)
  file.copy(KHcFN,KHnFN)
}

###############

binpath<-"F:/Prosjekter/Kommunehelsa/PRODUKSJON/BIN"
binpath_b<-"F:/Prosjekter/Kommunehelsa/PRODUKSJON/BIN/VERSJONSARKIV"
binvfiles<-list.files(path=binpath_b)

for (fil in c("KHfunctions")){
  FILc<-paste(binpath,"/",fil,".r",sep="")
  FILv<-paste(binpath_b,sort(binvfiles[grepl(paste("^",fil,"\\d+\\.r",sep=""),binvfiles)],decreasing=TRUE)[1],sep="/")
  if (file.info(FILc)$mtime>file.info(FILv)$mtime){
    FILn<-sub("\\.r",paste(date,".r",sep=""),FILc)
    FILn<-sub(binpath,binpath_b,FILn)
    file.copy(FILc,FILn)
  }  
}


