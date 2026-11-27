.onLoad <- function(libname, pkgname) {
  khtools:::load_package_options(
    package = "khfunctions",
    dynamic_options = list(
      "khfunctions.snutter" = get_snutt_list
    )
  )
  
  invisible(NULL)
}

.onAttach <- function(libname, pkgname){
  khtools:::check_package_update(package = "khfunctions", branch = "master")
  packageStartupMessage("khfunctions version: ", utils::packageDescription("khfunctions")[["Version"]])
  check_connection_folders()
  
  options_are_default <- tryCatch(
    khtools:::package_options_are_default(package = "khfunctions",
                                          dynamic_options = list("khfunctions.snutter" = get_snutt_list)),
    error = function(e) {
      packageStartupMessage(
        "Kunne ikke kontrollere khfunctions-options mot gjeldende defaults: ",
        conditionMessage(e)
      )
      TRUE
    }
  )
  
  if(!options_are_default){
    packageStartupMessage("Options for khfunctions avviker fra gjeldende defaults.")
    if (interactive()) {
      choice <- utils::menu(choices = c("Reset options til gjeldende defaults",
                                        "Behold nåværende options"),
        title = "Vil du resette options for khfunctions?"
      )
      
      if(identical(choice, 1L)) {
        khtools:::load_package_options(package = "khfunctions",
                                       dynamic_options = list("khfunctions.snutter" = get_snutt_list),
                                       overwrite = TRUE)
        packageStartupMessage("Options for khfunctions er reset til gjeldende defaults.")
      }
    }
  }
  
  invisible(NULL)
}

check_connection_folders <- function(){
  root <- getOption("kh.paths")$root
  db_khelsa <- getOption("kh.paths")$database$khelsa
  
  if(is.null(root) || length(root) != 1L || is.na(root) || !nzchar(root)) {
    stop("Elementet 'root' mangler i option  'kh.paths'.", call. = FALSE)
  }
  
  if(!dir.exists(root)) {
    stop(root, " ikke funnet. Har du tilgang til O:/?", call. = FALSE)
  }
  
  if(is.null(db_khelsa) || length(db_khelsa) != 1L || is.na(db_khelsa) || !nzchar(db_khelsa)) {
    stop("Elementet 'database$khelsa' mangler i option 'kh.paths'.", call. = FALSE)
  }
  
  if(!file.exists(db_khelsa)) {
    stop("KHELSA-databasen ble ikke funnet: ", db_khelsa, call. = FALSE)
  }
  
  invisible(NULL)
}

get_snutt_list <- function(){
  req <- httr2::request("https://api.github.com/repos/helseprofil/backend/git/trees/main?recursive=1")
  resp <- httr2::req_perform(req)
  tree <- httr2::resp_body_json(resp, simplifyDataFrame = TRUE)$tree$path
  files <- grep("^snutter/.*.R$", tree, value = T)
  paste0("https://raw.githubusercontent.com/helseprofil/backend/refs/heads/main/", files)
}
