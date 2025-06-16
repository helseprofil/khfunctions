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
  newversion <- check_for_new_version()
  if(newversion){
    x <- utils::askYesNo("Update khfunctions now?")
    if(x) remotes::install_github("helseprofil/khfunctions@master")
  }
    
  packageStartupMessage("khfunctions version: ",
                        utils::packageDescription("khfunctions")[["Version"]])
  check_connection_folders()
}

check_for_new_version <- function(){
  out <- FALSE
  localversion <- utils::packageDescription("khfunctions")[["Version"]]
  desc <- paste("https://raw.githubusercontent.com/helseprofil/khfunctions/master/DESCRIPTION")
  isOn <- orgdata:::is_online(desc)
  if(!isOn){
    cat("No online connection, cannot check for updated package version")
    return(invisible(out))
  } 
  gitversion <- data.table::fread(desc, nrows = 4, fill = TRUE)[grepl("Version", V1), V2]
  new <- numeric_version(gitversion) > numeric_version(localversion)
  
  if(new){
    cat("\nNew version available:", gitversion)
    cat("\nYour installed version:", localversion)
    return(invisible(TRUE))
  }
}

check_connection_folders <- function(){
  if(!dir.exists(getOption("khfunctions.root"))) stop(paste0(getOption("khfunctions.root"), " ikke funnet, Har du tilgang til O:/?"))
  if(!file.exists(file.path(getOption("khfunctions.root"), getOption("khfunctions.db")))) stop(getOption("khfunctions.db"), " ikke funnet i ", getOption("khfunctions.root"))
  invisible(NULL)
}

utils::globalVariables(c("HAR", "..betKols", "..kol", "keep", "..kols", "..outnames", "Bruk",
                         ".", "NOPri", "..brukcols", "..common", "..kolsomk", "..omkkols",
                         "filbesk"))