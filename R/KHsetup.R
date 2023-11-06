# Set encoding for R>=4.2
if(version$major >= 4 & version$minor >= 2){
  Sys.setlocale("LC_ALL", "nb-NO.UTF-8")
}

# Load packages
library(conflicted)
library(epitools) # Egentlig ikke i bruk, bare i direkte_stdz_jome.R
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

# Load misc/utils.R if not available (e.g. when using rm(list = ls()))
if(isFALSE(exists("kh_source"))){
  source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
}

# Load setup scripts (paths, global parameters, functions) to enable all functionality
kh_source(repo = "khfunctions", branch = "master", file = "R/KHmisc.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "master", file = "R/KHpaths.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "master", file = "R/KHglobs.R", encoding = "latin1")
# kh_source(repo = "khfunctions", branch = "master", file = "R/KHfunctions.R", encoding = "latin1")
#kh_source(repo = "khfunctions", branch = "master", file = "R/KHgraveyard.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "master", file = "R/KHfilgruppefunctions.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "master", file = "R/KHfilgruppe.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "master", file = "R/KHkubefunctions.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "master", file = "R/KHkube.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "master", file = "R/KHother.R", encoding = "latin1")