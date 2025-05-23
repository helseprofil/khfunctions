.SetKubeParameters <- function(cube){
  user_args <<- list(cube_name = cube,
                     year = getOption("khfunctions.year"),
                     dumps = list(),
                     write = FALSE,
                     geonaboprikk = TRUE)
}

.SetFilgruppeParameters <- function(filgruppenavn){
  user_args <<- list(filegroup_name = filgruppenavn,
                     write = FALSE,
                     dumps = list())
}

fg_get_all_read_args <- function(globs = get_global_parameters()){
  orginnleskobl <- data.table::setDT(RODBC::sqlQuery(globs$dbh, query = paste0("SELECT * FROM ORGINNLESkobl"), as.is = TRUE))
  originalfiler <- data.table::setDT(RODBC::sqlQuery(globs$dbh, query = paste0("SELECT * FROM ORIGINALFILER WHERE ", gsub("VERSJON", "IBRUK", globs$validdates)), as.is = TRUE))
  innlesing <- data.table::setDT(RODBC::sqlQuery(globs$dbh, query = paste0("SELECT * FROM INNLESING WHERE ", globs$validdates), as.is = TRUE))
  
  outcols <- c("KOBLID", "FILID", "FILNAVN", "FORMAT", "DEFAAR", setdiff(names(innlesing), "KOMMENTAR"))
  out <- collapse::join(orginnleskobl, originalfiler, how = "i", on = "FILID", overid = 2, verbose = 0)
  out <- collapse::join(out, innlesing, how = "i", on = c("FILGRUPPE", "DELID"), overid = 2, verbose = 0)
  out <- out[, .SD, .SDcols = outcols]
  out[, let(FILNAVN = gsub("\\\\", "/", FILNAVN))]
  out[, let(filepath = file.path(getOption("khfunctions.root"), FILNAVN), FORMAT = toupper(FORMAT))]
  out[AAR == "<$y>", let(AAR = paste0("<", DEFAAR, ">"))]
  return(out)
}
