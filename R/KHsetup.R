# Load packages
library(epitools)
library(RODBC)
library(DBI)
library(foreign)
library(sas7bdat)
library(XML)
library(zoo)
library(plyr) # count, 
library(sqldf)
library(stringr)
library(intervals)
library(data.table)
library(readxl)
library(fs) # dir_exists, dir_create, file_copy, file_exists, file_delete
library(collapse)

# Set debugging to inactive
show_functions <- FALSE
show_arguments <- FALSE

# Load misc/utils.R if not available (e.g. when using rm(list = ls()))
if(isFALSE(exists("kh_source"))){
  source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
}

# Load setup scripts (paths, global parameters, functions) to enable all functionality
kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHmisc.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHpaths.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHglobs.R", encoding = "latin1")
# kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHfunctions.R", encoding = "latin1")
#kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHgraveyard.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHfilgruppefunctions.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHfilgruppe.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHkubefunctions.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHkube.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHother.R", encoding = "latin1")