## Global options
opt.khfunctions <- orgdata:::is_globs("khfunctions")

.onLoad <- function(libname, pkgname) {
  optkhfunctions <- orgdata:::is_globs("khfunctions")
  orgDT <- !(names(optkhfunctions) %in% names(options()))
  if (any(orgDT)) options(optkhfunctions[orgDT])
  
  corrglobs <- orgdata:::is_correct_globs(optkhfunctions)
  if(!isTRUE(corrglobs)){
    x <- utils::menu(title = "Options are not the same as in the config file, update options now?",
                     choices = c("Yes", "No"))
    if(x == 1){
      orgdata::update_globs("khfunctions")
    }
  }
  invisible()
}

.onAttach <- function(libname, pkgname){
  # khfunctions must be added to orgdata functions, and deployed to github master branch before activating this. 
  # newver <- suppressMessages(orgdata:::is_latest_version("khfunctions"))
  # if (newver){
  #   x <- utils::askYesNo("Update khfunctions now?")
  #   if (isTRUE(x)){
  #     orgdata::update_khpackage("khfunctions")
  #   }
  # }
  packageStartupMessage("khfunctions version: ",
                        utils::packageDescription("khfunctions")[["Version"]])
  check_connection_folders()
}

check_connection_folders <- function(){
  if(!dir.exists(getOption("khfunctions.root"))) stop(paste0(getOption("khfunctions.root"), " ikke funnet, Har du tilgang til O:/?"))
  if(!file.exists(file.path(getOption("khfunctions.root"), getOption("khfunctions.db")))) stop(getOption("khfunctions.db"), " ikke funnet i ", getOption("khfunctions.root"))
  invisible(NULL)
}