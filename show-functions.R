
show_functions = FALSE
show_arguments = FALSE

if (show_functions & show_arguments){
  warning("Use either `show_arguments` or `show_functions`, but not both!")
}

is_kh_debug <- function(fun = NULL, arg = NULL){
  if (is.null(fun)) {
    show = show_functions
    what = "fun"
    args = NULL
  }

  if (is.null(arg)) {
    show = show_arguments
    what = "arg"
    args <- sys.call(sys.parent())
  }

  if (show) {
    is_show_time(what, args)
  }

  invisible()
}

is_show_time <- function(what = c("fun", "arg"), args){

  msg <- switch(what,
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
