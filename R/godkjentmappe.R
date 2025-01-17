#' @title godkjent
#' @param profil 
#' @param geoniv 
#' @param year
godkjent <- function(profil = c("FHP", "OVP"),
                     geoniv = c("K", "F", "B"),
                     year = getOption("khfunctions.year")) {
  profil <- match.arg(profil)
  geoniv <- match.arg(geoniv)
  message("Finner godkjente filer for ", profil, " (", geoniv, ") for ", year)
  
  con <- connect_khelsa()
  on.exit(RODBC::odbcCloseAll(), add = T)
  
  friskvik <- read_friskvik(con, profil, geoniv, year)
  kubestatus <- read_kubestatus(con, year)
  alle <- collapse::join(friskvik, kubestatus, how = "inner", on = "KUBE_NAVN", verbose = 0)
  godkjent_summary_msg(friskvik, alle)
  
  files <- alle[, friskvikfile := paste0(INDIKATOR, "_", DATOTAG_KUBE, ".csv")][["friskvikfile"]]
  if(length(files) < 1) return(message("\nIngen godkjente filer funnet, avbryter"))
  
  paths <- get_godkjent_paths(profil, geoniv, year)
  filelist <- godkjent_files_exist(files, paths)
  
  do_godkjent_copy_files(filelist$found, paths)
  if(length(filelist$notfound) > 0) report_godkjent_files_notfound(filelist$notfound, year)
}
  
#' @noRd
read_friskvik <- function(con, profil, geoniv, year){
  friskvik_columns <- paste(c("PROFILTYPE", "INDIKATOR", "KUBE_NAVN", "MODUS", "AARGANG"), collapse = ", ")
  friskvik_sql <- paste("SELECT", friskvik_columns, "FROM FRISKVIK WHERE AARGANG =", year, 
                        "AND PROFILTYPE =", paste0("'", profil, "'"),
                        "AND MODUS =", paste0("'", geoniv, "'"))
  friskvik <- data.table::setDT(RODBC::sqlQuery(con, friskvik_sql))
  return(friskvik)
}

#' @noRd
read_kubestatus <- function(con, year){
  kubestatus_columns <- paste0(c("KUBE_NAVN", "DATOTAG_KUBE", "QC_OK"), collapse = ", ")
  kubestatus_table <- paste0("KUBESTATUS_", year)
  kubestatus_sql <- paste("SELECT", kubestatus_columns, "FROM", kubestatus_table, "WHERE QC_OK = '1'")
  kubestatus <- data.table::setDT(RODBC::sqlQuery(con, kubestatus_sql))
  return(kubestatus)
}

#' @noRd
godkjent_summary_msg <- function(friskvik, alle){
  all <- friskvik[, length(unique(KUBE_NAVN))]
  complete <- alle[, length(unique(KUBE_NAVN))]
  remaining <- all - complete
  message("Per nå er ", complete, " av ", all, " kuber godkjent, ", remaining, " kuber gjenstår")
}

#' @noRd
get_godkjent_paths <- function(profil, geoniv, year){
  batchdate <- SettKHBatchDate()
  base <- file.path(getOption("khfunctions.root"),
                    getOption("khfunctions.kubedir"),
                    getOption(paste0("khfunctions.", tolower(profil), geoniv)),
                    year)
  paths <- list(from = file.path(base, "csv"),
                to = file.path(base, "GODKJENT", batchdate))
  return(paths)
}

#' @noRd
godkjent_files_exist <- function(files, paths){
  found <- notfound <- character()
  for(file in files){
    fromfile <- file.path(paths$from, file)
    if(fs::file_exists(fromfile)){
      found <- c(found, file)
    } else {
      notfound <- c(notfound, file)
    }
  }
  return(list(found = found, notfound = notfound))
}

#' @noRd
do_godkjent_copy_files <- function(files, paths){
  if(length(files) == 0){
    message("\nIngen friskvikfiler funnet i csv, godkjentmappe ikke opprettet")
    return(invisible(NULL))
  }
  
  fs::dir_create(paths$to)
  message("\nKopierer friskvikfiler fra csv til GODKJENT:\n")
  for(file in files){
      fs::file_copy(file.path(paths$from, file), file.path(paths$to, file), overwrite = T)
      message(" - ", file)
  }
}

#' @noRd
report_godkjent_files_notfound <- function(notfoundfiles, year){
  message("\n********\n\n",
          length(notfoundfiles), " friskvikfiler ikke funnet i csv\n",
          "Sjekk at riktig datotag er lagt inn i KUBESTATUS_", year, "\n",
          paste0(" - ", notfoundfiles, collapse = "\n"))
}


