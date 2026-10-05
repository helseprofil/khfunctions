.onLoad <- function(libname, pkgname){
  khtools:::load_package_options("khfunctions")
  options("khfunctions.snutter" = get_snutt_list())
  invisible()
}

.onAttach <- function(libname, pkgname){
  khtools:::check_package_update(package = "khfunctions", branch = "master")
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

utils::globalVariables(c("HAR", "..betKols", "..kol", "keep", "..kols", "..outnames", "Bruk",
                         ".", "NOPri", "..brukcols", "..common", "..kolsomk", "..omkkols",
                         "filbesk"))

