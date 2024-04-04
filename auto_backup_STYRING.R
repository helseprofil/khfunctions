## Backup av styringsdatabasen
## ---------------------------
backup <- function(profilaargang, filename = "KHELSA.mdb", force = FALSE, ...){

  ## Skriv inn nyeste årstall for KUBESTATUS-tabell (KH og NH).
  ## force : TRUE hvis man skal arkivere filen uansett, ellers
  ## sjekkes det for endringer i de to nyeste KUBESTATUS, og i et utvalg andre tabeller.

      # if (isTRUE(grepl("function", filename))){valgFil <- "fun"}
      # if (isTRUE(grepl("KHELSA", filename))){valgFil <- "mdb"}

  if (!require(RODBC)) {install.packages("RODBC")}
  require(RODBC)
  date <- format(Sys.time(), "%Y%m%d%H%M")

  styrpath <- "F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/STYRING"
  styrpath_b <- "F:/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/STYRING/VERSJONSARKIV"
  styroldfiles <- list.files(path = styrpath_b)

  KHkildeFN <- paste(styrpath, filename, sep = "/")
  KHtargetFN <- paste(styrpath_b, sort(styroldfiles[grepl("^KHELSA\\d+.mdb$", styroldfiles)], decreasing = TRUE)[1], sep="/")

  khkilde <- odbcConnectAccess2007(KHkildeFN)
  khtarget <- odbcConnectAccess2007(KHtargetFN)

      
# LEGGE INN: Hvis force = TRUE, kopier filen. ELSE, kjør resten av prosessen.  
if (isTRUE(force)){valg <- "TRUE"}
if (!isTRUE(force)){valg <- "FALSE"}  
      
switch(
  valg,
  "TRUE" = {
    ## Arkiveres uansett
    KHnewFN <- sub(filename, paste("KHELSA", date, ".mdb", sep=""), KHkildeFN)
    KHnewFN <- sub(styrpath, styrpath_b, KHnewFN)
    file.copy(KHkildeFN, KHnewFN)
  },
  
  "FALSE" = {
    ## Analyserer om backup trengs
    nytt <- 0
    
    #Lage tabellnavn for de to siste KUBESTATUS for begge stat.banker
    ifjor <- profilaargang -1
    KUBESTATUS_1 <- paste("KH", profilaargang, "_KUBESTATUS", sep = "")
    KUBESTATUS_2 <- paste("KH", ifjor, "_KUBESTATUS", sep = "")
    KUBESTATUS_3 <- paste("NH", profilaargang, "_KUBESTATUS", sep = "")
    KUBESTATUS_4 <- paste("NH", ifjor, "_KUBESTATUS", sep = "")
    
    #Sammenlign
    for (TAB in c("INNLESING", "KUBER", "TNP_PROD", "FILGRUPPER", "ORGINNLESkobl", "ORIGINALFILER",
                  KUBESTATUS_1, KUBESTATUS_2, KUBESTATUS_3, KUBESTATUS_4)){
      TABkilde <- sqlQuery(khkilde, paste("SELECT * FROM", TAB))
      TABtarget <- sqlQuery(khtarget, paste("SELECT * FROM", TAB))
      if (!identical(TABkilde, TABtarget)){
        nytt <- 1
      }
    }

    if (nytt == 1){
      KHnewFN <- sub(filename, paste("KHELSA", date, ".mdb", sep=""), KHkildeFN)
      KHnewFN <- sub(styrpath, styrpath_b, KHnewFN)
      file.copy(KHkildeFN, KHnewFN)
    }

  }  
)
# Rydde opp: Stenge forbindelser til databasen.
odbcCloseAll()

}

## Slik brukes den
## -----------------
## backup(2024)
## backup(2024, "KHELSA.mdb")
