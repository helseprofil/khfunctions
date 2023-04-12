# Load packages
library(epitools)
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

# Load setup scripts
kh_source(repo = "khfunctions", branch = "master", file = "KHpaths.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "master", file = "KHglobs.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "master", file = "KHfunctions.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "master", file = "KHkube.R", encoding = "latin1")


# Set debugging to inactive
show_functions = FALSE
show_arguments = FALSE
