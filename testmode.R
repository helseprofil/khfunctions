## Test mode
## ---------
rm(list = ls())


BUFFER<-list(BEF_GKa=KlargjorFil("BEF_GKa",versjonert=TRUE)$FIL)
#BUFFER<-list(BEF_GKa=FinnFilT("BEF_GK_Ta"))

#Denne m? ogs? ofte kj?res ved oppstart. Setter opp ODBC.
KHglobs<-SettGlobs()


## List of Filgruppenavn to check
## ------------------------------
gpnavn <- c("ELEVUNDER", "ARBLEDIGE", "REGNEFERD_NH", "DODE_GK")



## tesfil = TRUE is to choose file where TESTING is 1 in ORGINALFILERse
LagFilgruppe(gpnavn[4])

LagFilgruppe("ELEVUNDER") #CSV fil
LagFilgruppe("ELEVUNDER", test = TRUE) #CSV fil testfile
LagFilgruppe("INNTULIKHET", test = TRUE) #CSV fil testfile



tm <- R6::R6Class(
  "TestMode",
  public = list(
    id = NULL,
    initialize = function(){
      assign("test_files", TRUE, envir = .GlobalEnv)
      source("KHfunctions.R")
      message("\n -->>>  Test modus aktivert  <<<--")
    },
    add_id = function(val){
      self$id <- val
      assign("test_files", self$id, envir = .GlobalEnv)
    },
    empty = function(){
      message(">>> Slett test KOBLID <<<")
      self$id <- NULL
      rm(test_files, envir = .GlobalEnv)
    }
  )
)


ts <- tm$new()
ts$id
test_files
ts$add_id(3534)
ts$empty()
