# Set encoding for R>=4.2
# if(version$major >= 4 & version$minor >= 2){
#   Sys.setlocale("LC_ALL", "nb-NO.UTF-8")
# }
# 
# invisible(system("git fetch"))
# 
# githubpath <- "https://raw.githubusercontent.com/helseprofil/khfunctions/"
# branch <- "master"

# Load packages
# library(conflicted)
# library(epitools) # Egentlig ikke i bruk, bare i direkte_stdz_jome.R
# library(httr2)
# library(RODBC)
# library(DBI)
# library(foreign)
# library(haven)
# # library(sas7bdat)
# library(XML)
# library(zoo)
# library(plyr) 
# # library(sqldf)
# library(stringr)
# library(rlang)
# library(intervals)
# library(data.table)
# library(readxl)
# library(fs)
# library(collapse)

# Solve conflicts
# conflicted::conflict_prefer("as.Date", "zoo", quiet = T)
# conflicted::conflict_prefer("as.Date.numeric", "zoo", quiet = T)
# conflicted::conflict_prefer("empty", "intervals", quiet = T)
# conflicted::conflict_prefer("join", "collapse", quiet = T)
# conflicted::conflict_prefer("D", "collapse", quiet = T)
# conflicted::conflict_prefer(":=", "data.table", quiet = T)

# Set debugging to inactive
# show_functions <- FALSE
# show_arguments <- FALSE

# Load setup scripts (paths, global parameters, functions) to enable all functionality
# source(paste0(githubpath, branch, "/R/KHmisc.R"), encoding = "latin1")
# KH_options()
# rfiles <- grep("KHmisc.R|KHsetup.R", list_files_github(branch = branch), value = T, invert = T)
# for(file in rfiles){
#   source(paste0(githubpath, branch, "/R/", file), encoding = "latin1")
# }
# rm(githubpath, file, rfiles)