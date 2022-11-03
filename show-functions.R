
# If both are TRUE then show_arguments will be deactivated automatically.
show_functions = FALSE
show_arguments = FALSE

is_kh_debug <- function(fun = show_functions, arg = show_arguments, show = FALSE){

  if (fun & arg)
    arg <- FALSE

  if (arg) {
    show = show_arguments
    what = "arg"
    args <- sys.call(sys.parent())
  }

  if (fun) {
    show = show_functions
    what = "fun"
    args = NULL
  }

  if (show) {
    is_show_time(what, args)
  }

  invisible()
}

is_show_time <- function(what = c("fun", "arg"), args){
  switch(what,
         fun = {
           fnc <- sys.calls()[[sys.nframe() - 2]][1]
           if (requireNamespace("orgdata", quietly = TRUE)){
             orgdata:::is_colour_txt(x = deparse(fnc),
                                     msg = "Execute:",
                                     type = "debug",
                                     emoji = TRUE,
                                     symbol = "mark")
           } else {
             message("Execute: ", deparse(fnc))
           }
         },
         arg = {
           fnc <- args
           if (requireNamespace("orgdata", quietly = TRUE)){
             orgdata:::is_colour_txt(x = deparse(fnc),
                                     msg = "Execute:",
                                     type = "debug",
                                     emoji = TRUE,
                                     symbol = "mark")
           } else {
             message("Execute: ", deparse(fnc))
           }
         })

  invisible()
}
