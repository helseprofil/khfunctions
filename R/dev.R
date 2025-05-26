.SetKubeParameters <- function(cube){
  user_args <<- list(name = cube,
                     year = getOption("khfunctions.year"),
                     dumps = list(),
                     write = FALSE,
                     geonaboprikk = TRUE)
}

.SetFilgruppeParameters <- function(filgruppenavn){
  user_args <<- list(name = filgruppenavn,
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
