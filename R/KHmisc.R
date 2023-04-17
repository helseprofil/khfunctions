# Helper functions used in the data processing, both in LagKUBE and LagFilgruppe

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

#' SettKHBatchDate
#' 
#' Used to set date tag in file names
SettKHBatchDate <- function() {
  is_kh_debug()
  
  format(Sys.time(), "%Y-%m-%d-%H-%M")
}

