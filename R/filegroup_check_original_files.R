filegroup_check_original_files_and_spec <- function(parameters){
  cat("\n* SJEKK AV ORIGINALFILER OG PARAMETRE")
  parameters$read_parameters
  checks <- list()
  checks[["FILER_FINNES"]] <- check_if_files_exists_and_are_readable(files = parameters$read_parameters$filepath)
  checks[["FORMAT_OK"]] <- check_if_format_is_ok(read_parameters = parameters$read_parameters)
  
  
  # Hvis noe feil oppdages, skriv en feilrapport som .txt-fil ved hjelp av sink(), 
  # og lagre denne et sted. For alle elementer over som ikke er NULL, limes disse inn 
  # i rapporten. Stopp så kjøringen og vis til denne rapporten for hva som må fikses i ACCESS.
  
  if(length(checks) > 0){
    # feilrapport <- file.path("STI TIL FEILRAPPORTMAPPE", paste0("filgruppe_feil_", parameters$batchdate, ".txt"))
    # sink(feilrapport)
    for(i in 1:length(checks)){
      cat("*", names(checks[i]), ":\n\n")
      cat(checks[[1]])
    }
    # sink()
    stop("FEIL funnet i originalfiler eller innlesingsspecs, se feillogg for oversikt")
  } else {
    cat("\n* ALLE SJEKKER FERDIG OG OK!")
  }
}




check_if_files_exists_and_are_readable <- function(files){
  not_exist <- character()
  not_readable <- character()
  for(file in files){
    if(file.access(file, mode = 0) != 0){
      not_exist <- c(not_exist, sub(getOption("khfunctions.root"), "", file))
    } else if(file.access(file, mode = 4) != 0){
      not_readable <- c(not_readable, sub(getOption("khfunctions.root"), "", file))
    }
  }
  if(length(not_exist) == 0 || length(not_readable) == 0){
    cat("\n** Alle filer eksisterer og kan leses")
    return(invisible(NULL))
  } 
  msg <- character()
  if(length(not_exist) > 0) msg <- paste0(msg, "\n** Følgende fil(er) eksisterer ikke:\n", paste(" - ", not_exist, collapse = "\n"))
  if(length(not_readable) > 0) msg <- paste0(msg,"\n\n** Følgende fil(er) eksisterer men kan ikke leses:\n", paste(" - ", not_readable, collapse = "\n"))
  return(msg)
}

check_if_format_is_ok <- function(read_parameters){
  read_parameters[, FORMAT := toupper(FORMAT)]
  not_valid_format <- read_parameters[FORMAT %notin% c(c("CSV", "XLS", "XLSX", "SPSS"))]
  mismatch_extension <- read_parameters[FORMAT == "CSV" & !grepl(".csv$", FILNAVN) |
                                  FORMAT %in% c("XLS", "XSLX") & !grepl(".xls$|.xlsx$", FILNAVN)|
                                  FORMAT == "SPSS" & !grepl(".sav$", FILNAVN)]
  if(nrow(not_valid_format) == 0 | nrow(mismatch_extension) == 0){
    cat("\n** Alle filformater ok og korresponderer med filnavn")
    return(invisible(NULL))
  }
  
  msg <- character()
  if(nrow(not_valid_format)) msg <- paste0(msg, "\n** Følgende fil(er) har ugyldig FORMAT:\n", paste(" - ", not_valid_format$FILNAVN))
  if(nrow(mismatch_extension)) msg <- paste0(msg, "\n\n** Følgende fil(er) har mismatch mellom FORMAT og filtype", paste(" - ", mismatch_extension$FILNAVN))
  return(msg)
}
