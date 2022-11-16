# If both are TRUE then show_arguments will be deactivated automatically.
show_functions = FALSE
show_arguments = FALSE

is_kh_debug <- function(fun = show_functions, arg = show_arguments, show = FALSE){

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

