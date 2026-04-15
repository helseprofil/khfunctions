## Global options
opt.khfunctions <- orgdata:::is_globs("khfunctions")

.onLoad <- function(libname, pkgname){
  ensure_kh_options()
  invisible()
}

.onAttach <- function(libname, pkgname){
  newversion <- orgdata:::is_latest_version("khfunctions", "master")
  if(interactive() & newversion){
    x <- utils::menu(title = "Update khfunctions now?", choices = c("Yes", "No"))
    if(x == 1){
        packageStartupMessage("Please restart your R session and then run:")
        packageStartupMessage('remotes::install_github("helseprofil/khfunctions@master")')
    }
  }
  
  opt_ok <- ensure_kh_options()
  if(interactive() & !opt_ok){
    x <- utils::menu(title = "Options are not the same as in the config file, update options now?",
                     choices = c("Yes", "No"))
    if(x == 1){
      orgdata::update_globs("khfunctions")
    }
  }
 
  packageStartupMessage("khfunctions version: ", utils::packageDescription("khfunctions")[["Version"]])
  check_connection_folders()
}

check_connection_folders <- function(){
  if(!dir.exists(getOption("khfunctions.root"))) stop(paste0(getOption("khfunctions.root"), " ikke funnet, Har du tilgang til O:/?"))
  if(!file.exists(file.path(getOption("khfunctions.root"), getOption("khfunctions.db")))) stop(getOption("khfunctions.db"), " ikke funnet i ", getOption("khfunctions.root"))
  invisible(NULL)
}

get_snutt_list <- function(){
  req <- httr2::request("https://api.github.com/repos/helseprofil/backend/git/trees/main?recursive=1")
  resp <- httr2::req_perform(req)
  tree <- httr2::resp_body_json(resp, simplifyDataFrame = TRUE)$tree$path
  files <- grep("^snutter/.*.R$", tree, value = T)
  paste0("https://raw.githubusercontent.com/helseprofil/backend/refs/heads/main/", files)
}

ensure_kh_options <- function(){
  optkhfunctions <- orgdata:::is_globs("khfunctions")
  missing <- !(names(optkhfunctions) %in% names(options()))
  if (any(missing)) options(optkhfunctions[missing])
  options("khfunctions.snutter" = get_snutt_list())
  corrglobs <- orgdata:::is_correct_globs(c(optkhfunctions, "khfunctions.snutter"))
  isTRUE(corrglobs)
}

utils::globalVariables(c("HAR", "..betKols", "..kol", "keep", "..kols", "..outnames", "Bruk",
                         ".", "NOPri", "..brukcols", "..common", "..kolsomk", "..omkkols",
                         "filbesk"))

