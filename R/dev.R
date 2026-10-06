SetKubeParameters <- function(cube){
  user_args <<- list(name = cube,
                     year = getOption("khfunctions.year"),
                     dumps = list(),
                     write = FALSE,
                     ramlimit = NULL,
                     qualcontrol = TRUE)
  parameters <<- get_cubeparameters(user_args = user_args)
}

SetFilgruppeParameters <- function(filgruppenavn){
  user_args <<- list(name = filgruppenavn,
                     write = FALSE,
                     ramlimit = NULL, 
                     dumps = list(), 
                     qualcontrol = TRUE)
  parameters <<- get_filegroup_parameters(user_args = user_args)
}

# Sammenligne mot et fasitdatasett:

comparefasit <- function(dt, fasit){
  fasitcompare <- data.table::copy(fasit)
  fix_fasit_colorder(dt, fasitcompare)
  fix_fasit_order(dt, fasitcompare)
  rm_fasit_extracolumn(dt, fasitcompare)
  all.equal(dt, fasitcompare)
}

# Setter kolonner i fasit i samme rekkefølge som dt
fix_fasit_colorder <- function(dt, fasit){
  data.table::setcolorder(fasit, names(dt))
}

# Setter samme rekkefølge i dt og fasit for sammenligning
fix_fasit_order <- function(dt, fasit){
  keyvar <- identify_dimensions(names(dt))
  data.table::setkeyv(dt, keyvar)
  data.table::setkeyv(fasit, keyvar)
}

rm_fasit_extracolumn <- function(dt, fasit){
  extracol <- setdiff(names(fasit), names(dt))
  if(length(extracol)){
    warning(paste("Sletter ekstrakolonner i fasit:", paste(extracol, collapse = ", ")))
    fasit[, extracol := NULL]
  }
}


fg_get_all_read_args <- function(globs = get_global_parameters()){
  orginnleskobl <- data.table::setDT(DBI::dbGetQuery(globs$dbh, paste0("SELECT * FROM ORGINNLESkobl")))
  originalfiler <- data.table::setDT(DBI::dbGetQuery(globs$dbh, paste0("SELECT * FROM ORIGINALFILER WHERE ", gsub("VERSJON", "IBRUK", globs$validdates))))
  innlesing <- data.table::setDT(DBI::dbGetQuery(globs$dbh, paste0("SELECT * FROM INNLESING WHERE ", globs$validdates)))
  
  outcols <- c("KOBLID", "FILID", "FILNAVN", "FORMAT", "DEFAAR", setdiff(names(innlesing), "KOMMENTAR"))
  out <- collapse::join(orginnleskobl, originalfiler, how = "i", on = "FILID", overid = 2, verbose = 0)
  out <- collapse::join(out, innlesing, how = "i", on = c("FILGRUPPE", "DELID"), overid = 2, verbose = 0)
  out <- out[, .SD, .SDcols = outcols]
  out[, let(FILNAVN = gsub("\\\\", "/", FILNAVN))]
  out[, let(filepath = file.path(getOption("khfunctions.root"), FILNAVN), FORMAT = toupper(FORMAT))]
  out[AAR == "<$y>", let(AAR = paste0("<", DEFAAR, ">"))]
  return(out)
}

# Teste dumppunkter
get_all_fg_dumps <- function(fg){
  LagFilgruppe(fg, write = F, 
               dumps = list(RSYNT1pre = "R", RSYNT1post = "R", 
                            RESHAPEpre = "R", RESHAPEpost = "R", 
                            RSYNT2pre = "R", RSYNT2post = "R", 
                            KODEBOKpre = "R", KODEBOKpost = "R", 
                            RSYNT_PRE_FGLAGRINGpre = "R", RSYNT_PRE_FGLAGRINGpost = "R"))
}

get_all_cube_dumps <- function(cube){
  LagKUBE(cube, write = F,
          dumps = list(MOVAVpre = "R", MOVAVpost = "R",
                       SLUTTREDIGERpre = "R", SLUTTREDIGERpost = "R",
                       PRIKKpre = "R", PRIKKpost = "R",
                       RSYNT_POSTPROSESSpre = "R", RSYNT_POSTPROSESSpost = "R"))
}
