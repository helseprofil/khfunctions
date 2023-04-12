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


# Set debugging to inactive
show_functions = FALSE
show_arguments = FALSE

# Load setup scripts
kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHpaths.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHglobs.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHfunctions.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHkube.R", encoding = "latin1")

