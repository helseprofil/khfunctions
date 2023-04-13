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

# Define debugging function, set to inactive by default
show_functions <- FALSE
show_arguments <- FALSE

#' is_kh_debug (ybk)
#' 
#' Debugging all other functions in the project
#' To use, set show_functions or show_arguments = TRUE
is_kh_debug <- function(fun = show_functions, arg = show_arguments, show = FALSE){
  
  # If both are TRUE then show_arguments will be deactivated automatically.
  if (fun & arg)
    arg <- FALSE
  
  if (arg) {
    show = show_arguments
    out = sys.calls()[[1]]
  }
  
  if (fun) {
    show = show_functions
    out = sys.calls()[[1]][1]
  }
  
  if (show) {
    message("Execute: ", deparse(out))
  }
  
  invisible()
}

# Load setup scripts (paths, global parameters, functions) to enable all functionality
kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHpaths.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHglobs.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHfunctions.R", encoding = "latin1")
kh_source(repo = "khfunctions", branch = "allviskube", file = "R/KHkube.R", encoding = "latin1")
