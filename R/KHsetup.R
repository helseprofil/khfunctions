# Set encoding for R>=4.2
if(version$major >= 4 & version$minor >= 2){
  Sys.setlocale("LC_ALL", "nb-NO.UTF-8")
}

# Look for updates to renv.lock and update packages
source("https://raw.githubusercontent.com/helseprofil/khfunctions/master/R/KHupdate.R", encoding = "latin1")
renv::restore(prompt = F)

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
