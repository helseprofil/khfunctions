# Set encoding for R>=4.2
if(version$major >= 4 & version$minor >= 2){
  Sys.setlocale("LC_ALL", "nb-NO.UTF-8")
}

#' .updatelockfile (VL)
#' Helper function to automatically fetch the lastest renv.lock from github
.updatelockfile <- function(){
  
  # Check if the master branch is active
  b <- system("git branch --show-current", intern = TRUE)
  if(b != "master"){
    message("You are not on the master branch, keep on the good dev work!")
    return(invisible(NULL))
  }
  
  # Check if an update is necessary
  tryCatch({
    new_lockfile <- readLines("https://raw.githubusercontent.com/helseprofil/khfunctions/master/renv.lock")
    if (identical(readLines("renv.lock"), new_lockfile)) {
      return(invisible(NULL))
    } else {
      choice <- menu(choices = c("Yes", "No"), 
                     title = "renv.lock update available. Overwrite local file?")
      if (choice == 1) {
        writeLines(new_lockfile, "renv.lock") 
        message("renv.lock updated, update to latest package versions.")
        renv::restore()
      } else {
        message("Skipping renv.lock update, your package versions differ from the project.")
      }
    }
  }, error = function(e) {
    message("Could not fetch renv.lock from the repository. Check URL and connectivity.")
  })
}

.updatelockfile()

# Load packages
library(conflicted)
library(epitools) # Egentlig ikke i bruk, bare i direkte_stdz_jome.R
library(httr2)
library(RODBC)
library(DBI)
library(foreign)
library(sas7bdat)
library(XML)
library(zoo)
library(plyr) 
library(sqldf)
library(stringr)
library(intervals)
library(data.table)
library(readxl)
library(fs)
library(collapse)

# Solve conflicts
conflicted::conflict_prefer("as.Date", "zoo", quiet = T)
conflicted::conflict_prefer("as.Date.numeric", "zoo", quiet = T)
conflicted::conflict_prefer("empty", "intervals", quiet = T)
conflicted::conflict_prefer("join", "collapse", quiet = T)
conflicted::conflict_prefer("D", "collapse", quiet = T)

# Set debugging to inactive
show_functions <- FALSE
show_arguments <- FALSE

# Load setup scripts (paths, global parameters, functions) to enable all functionality
source("https://raw.githubusercontent.com/helseprofil/khfunctions/master/R/KHmisc.R", encoding = "latin1")
source("https://raw.githubusercontent.com/helseprofil/khfunctions/master/R/KHpaths.R", encoding = "latin1")
source("https://raw.githubusercontent.com/helseprofil/khfunctions/master/R/KHglobs.R", encoding = "latin1")
source("https://raw.githubusercontent.com/helseprofil/khfunctions/master/R/KHfilgruppefunctions.R", encoding = "latin1")
source("https://raw.githubusercontent.com/helseprofil/khfunctions/master/R/KHfilgruppe.R", encoding = "latin1")
source("https://raw.githubusercontent.com/helseprofil/khfunctions/master/R/KHkubefunctions.R", encoding = "latin1")
source("https://raw.githubusercontent.com/helseprofil/khfunctions/master/R/KHkube.R", encoding = "latin1")
source("https://raw.githubusercontent.com/helseprofil/khfunctions/master/R/KHother.R", encoding = "latin1")
