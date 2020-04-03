## Backup filer
## ---------------
backup <- function(filename = c("KHfunctions.R", "KHELSA.mdb"), force = FALSE, ...){

  ## force : TRUE hvis man skal arkivere filen uansett ellers
  ## for KHFunction.R sjekkes det dato filen er lagret først

  if (isTRUE(grepl("function", filename))){valgFil <- "fun"}
  if (isTRUE(grepl("KHELSA", filename))){valgFil <- "mdb"}

  if (!require(RODBC)) {install.packages("RODBC")}
  require(RODBC)
  date<-format(Sys.time(), "%Y%m%d%H%M")

  switch(
    valgFil,

    ## Access Tabell
    ## -------------
    "mdb" = {
      styrpath<-"F:/Prosjekter/Kommunehelsa/PRODUKSJON/STYRING"
      styrpath_b<-"F:/Prosjekter/Kommunehelsa/PRODUKSJON/STYRING/VERSJONSARKIV"
      styrvfiles<-list.files(path=styrpath_b)

      KHcFN<-paste(styrpath, filename, sep="/")
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
        KHnFN<-sub(filename, paste("KHELSA",date,".mdb",sep=""),KHcFN)
        KHnFN<-sub(styrpath,styrpath_b,KHnFN)
        file.copy(KHcFN,KHnFN)
      }

    },

    ## KHFunction
    ## ----------
    "fun" = {

      binpath<-"F:/Prosjekter/Kommunehelsa/PRODUKSJON/BIN"
      binpath_b<-"F:/Prosjekter/Kommunehelsa/PRODUKSJON/BIN/VERSJONSARKIV"
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


## ## Slik brukes den
## ## -----------------
## backup("KHfunctions.R")
## backup("KHELSA.mdb")
