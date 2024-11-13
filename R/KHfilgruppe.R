# TRINN 1: STABLING AV ORIGINALFILER I FILGRUPPE-FILER
#         Gir ferdige stablede filer i \\StablaFilGrupper

#' LagFilgruppe (kb)
#'
#' @param gruppe 
#' @param batchdate 
#' @param globs 
#' @param diagnose 
#' @param printR 
#' @param printCSV 
#' @param printSTATA 
#' @param versjonert 
#' @param dumps 
#' @param test 
#' @param idtest 
#' @param localDir 
LagFilgruppe <- function(gruppe,
                         batchdate = SettKHBatchDate(),
                         globs = FinnGlobs(),
                         diagnose = 0,
                         printR = TRUE,
                         printCSV = FALSE,
                         printSTATA = FALSE,
                         versjonert = FALSE,
                         dumps = list(),
                         test = runtest,
                         idtest = testfiles,
                         localDir = setLocal) {
  is_kh_debug()
  
  globs$dbh <- RODBC::odbcConnectAccess2007(file.path(getOption("khfunctions.root"), getOption("khfunctions.db")))
  globs$log <- RODBC::odbcConnectAccess2007(file.path(getOption("khfunctions.root"), getOption("khfunctions.logg")))
  on.exit(RODBC::odbcCloseAll(), add = TRUE)
  
  ## test is TRUE when column 'TESTING' in ORIGINALFILER is used
  ## for selecting the file to be processed
  lineMsg <- "\n---------------------------\n"
  if (test) {
    message(
      lineMsg,
      "\n** ---- Test Modus ---- **"
    )
  }
  
  ## To see which DB is currently used
  whichPath <- ifelse(localDir, setLocalPath, getOption("khfunctions.root"))
  whichDB <- ifelse(localDir, setDBFile, getOption("khfunctions.db"))
  message(
    lineMsg,
    "  DB name: ", whichDB, "\n",
    "  DB path: ", whichPath,
    lineMsg
  )
  
  if (test && is.null(idtest)) {
    stop("Ingen KOBLID nr. er spesifisert")
  }
  
  if (test) {
    cat("  Test filer KOBLID: ", idtest, lineMsg)
  }
  
  ## testfiles global value is needed by FinnFilBeskGruppe() for filtering
  assign("testfiles", idtest, envir = .GlobalEnv)
  
  
  # Essensielt bare loop over alle delfiler/orignalfiler
  # For hver orignalfil kjoeres LagTabellFraFil
  # Stables til tabellen FG
  # Finn filgruppeparametre
  FGP <- FinnFilgruppeParametre(gruppe, batchdate = batchdate, globs = globs)
  # Initier tom tabell
  Filgruppe <- data.frame()
  if (FGP$ok == 1) {
    # Rydd gammel logg
    RODBC::sqlQuery(globs$log, paste("DELETE * FROM KODEBOK_LOGG WHERE FILGRUPPE='", gruppe, "' AND SV='S'", sep = ""))
    RODBC::sqlQuery(globs$log, paste("DELETE * FROM INNLES_LOGG WHERE FILGRUPPE='", gruppe, "' AND SV='S'", sep = ""))
    # Finn parameterbeskrivelse av delfilene
    delfiler <- FinnFilBeskGruppe(gruppe, batchdate = batchdate, globs = globs)
    if (nrow(delfiler) > 0) {
      for (i in 1:nrow(delfiler)) {
        
        ## Need root path for raw files
        filbesk <- delfiler[i, ]
        tm <- proc.time()
        filbesk$filn <- file.path(getOption("khfunctions.root"), filbesk$FILNAVN)
        ## filbesk$filn<-paste(getOption("khfunctions.root"),filbesk$FILNAVN,sep="/")
        filbesk$filn <- gsub("\\\\", "/", filbesk$filn)
        # Sett evt default for aar basert paa aktuelt aarstall
        filbesk$AAR <- gsub("<\\$y>", paste("<", filbesk$DEFAAR, ">", sep = ""), filbesk$AAR)
        # LagTabell
        DF <- LagTabellFraFil(filbesk, FGP, batchdate = batchdate, diagnose = diagnose, globs = globs, versjonert = versjonert, dumps = dumps)
        # Stable delfiler
        Filgruppe <- plyr::rbind.fill(Filgruppe, DF)
        
        
        # Stopp klokke, skriv tid og feillogg
        tid <- proc.time() - tm
        stid <- format(Sys.time(), "%Y-%m-%d %X")
      }
    } else {
      # Maa gi fornuftig tilbakemelding
    }
    
    # DEV: SPESIALBEHANDLING AV FILGRUPPE HER!! F.EKS. IMPUTER NPR
    
    # Diagnostisering og rapportering paa hele filgruppa under ett
    
    if (nrow(Filgruppe) > 0 & diagnose == 1) {
      # Finn og rapporter duplikater
      HarDuplikater <- SjekkDuplikater(Filgruppe, batchdate = batchdate, filgruppe = gruppe, versjonert = versjonert, globs = KHglobs)
      RODBC::sqlQuery(globs$dbh, paste("UPDATE FILGRUPPER SET DUPLIKATER='", HarDuplikater, "' WHERE FILGRUPPE='", gruppe, "'", sep = ""))
      
      # Sjekk design
      FGd <- FinnDesign(Filgruppe, FGP = FGP)
      
      # Er ubalansert?
      subset(FGd$Design, HAR != 1)
      
      FGdT <- FGd$Design
      RODBC::sqlQuery(globs$log, paste("DELETE * FROM DESIGN WHERE FILGRUPPE='", gruppe, "' AND SV='S'", sep = ""))
      # Legg til resterende kolonner
      tmp <- RODBC::sqlQuery(globs$log, "SELECT * FROM DESIGN WHERE FILGRUPPE=''")
      tmp[1:nrow(FGdT), ] <- NA
      tmp[, names(FGdT)] <- FGdT
      tmp$FILGRUPPE <- gruppe
      tmp$BATCH <- batchdate
      tmp$SV <- "S"
      RODBC::sqlSave(KHglobs$log, tmp, "DESIGN", rownames = FALSE, append = TRUE)
      if (versjonert == TRUE) {
        tmp$SV <- "V"
        RODBC::sqlSave(KHglobs$log, tmp, "DESIGN", rownames = FALSE, append = TRUE)
      }
    }
    # Sett (eksterne) kolonnenavn
    for (val in names(Filgruppe)[grepl("^VAL\\d+$", names(Filgruppe))]) {
      valn <- paste(val, "navn", sep = "")
      if (grepl("\\S", FGP[[valn]])) {
        names(Filgruppe) <- gsub(paste("^", val, "(\\.[fa]|)$", sep = ""), paste(FGP[[valn]], "\\1", sep = ""), names(Filgruppe))
      }
    }
    FGP1 <- data.table::copy(Filgruppe)
    
    if ("RSYNT_PRE_FGLAGRINGpre" %in% names(dumps)) {
      for (format in dumps[["RSYNT_PRE_FGLAGRINGpre"]]) {
        DumpTabell(Filgruppe, paste(filbesk$FILGRUPPE, "RSYNT_PRE_FGLAGRINGpre", sep = "_"), globs = globs, format = format)
      }
    }
    
    # EVT SPESIALBEHANDLING
    if (!is.na(FGP$RSYNT_PRE_FGLAGRING)) {
      synt <- gsub("\\\r", "\\\n", FGP$RSYNT_PRE_FGLAGRING)
      error <- ""
      ok <- 1
      if (grepl("<STATA>", synt)) {
        synt <- gsub("<STATA>[ \n]*(.*)", "\\1", synt)
        RES <- KjorStataSkript(Filgruppe, synt, batchdate = batchdate, globs = globs)
        if (RES$feil != "") {
          stop("Noe gikk galt i kjoering av STATA \n", RES$feil)
          ok <- 0
        } else {
          Filgruppe <- RES$TABLE
        }
      } else {
        rsynterr <- try(eval(parse(text = synt)), silent = TRUE)
        if ("try-error" %in% class(rsynterr)) {
          ok <- 0
          error <- rsynterr
        }
      }
      if (ok == 0) {
        print(error)
      }
    }
    
    if ("RSYNT_PRE_FGLAGRINGpost" %in% names(dumps)) {
      for (format in dumps[["RSYNT_PRE_FGLAGRINGpost"]]) {
        DumpTabell(Filgruppe, paste(filbesk$FILGRUPPE, "RSYNT_PRE_FGLAGRINGpost", sep = "_"), globs = globs, format = format)
      }
    }
    
    # Datostempel
    RODBC::sqlQuery(globs$dbh, paste("UPDATE FILGRUPPER SET PRODDATO='", format(Sys.time(), "%Y-%m-%d %X"), "' WHERE FILGRUPPE='", gruppe, "'", sep = ""))
    
    # SKRIV RESULTAT
    path <- getOption("khfunctions.root")
    
    if (!exists("testmappe")) testmappe <- file.path(getOption("khfunctions.root"), "TEST")
    
    if (test) {
      printR <- FALSE
      testFile <- paste0(gruppe, batchdate, ".rds")
      testPath <- file.path(testmappe, testFile)
      message("Testfil: ", testPath)
      saveRDS(Filgruppe, file = testPath)
    }
    
    if (printR) {
      utfiln <- paste(path, "/", getOption("khfunctions.filegroups.ny"), "/", gruppe, ".rds", sep = "")
      # save(Filgruppe,file=utfiln)
      print(utfiln)
      saveRDS(Filgruppe, file = utfiln)
      if (versjonert == TRUE) {
        utfild <- paste(path, "/", getOption("khfunctions.filegroups.dat"), "/", gruppe, "_", batchdate, ".rds", sep = "")
        file.copy(utfiln, utfild)
      }
    }
  }
  
  return(Filgruppe)
  # ht2(Filgruppe)
}

#' LagFlereFilgrupper (kb)
#'
#' @param filgrupper 
#' @param batchdate 
#' @param globs 
#' @param printR 
#' @param printCSV 
#' @param printSTATA 
#' @param versjonert 
LagFlereFilgrupper <- function(filgrupper = character(0), batchdate = SettKHBatchDate(), globs = FinnGlobs(), printR = TRUE, printCSV = FALSE, printSTATA = FALSE, versjonert = FALSE) {
  is_kh_debug()
  
  # SKall rundt LagFilGruppe, lager og lagrer evt til fil
  # Default er aa ta alle grupper, ellers angis oensket batch i filgrupper-argumentet
  if (length(filgrupper) == 0) {
    # filgrupper<-as.matrix(RODBC::sqlQuery(globs$dbh,"SELECT DISTINCT Filgruppe from INNLESING WHERE Bruk=1",as.is=TRUE))
    filgrupper <- as.matrix(RODBC::sqlQuery(globs$dbh, "SELECT DISTINCT Filgruppe from FILGRUPPER", as.is = TRUE))
  }
  cat("BATCH:", batchdate, "\n")
  # HOVEDLOOP
  for (gruppe in filgrupper) {
    FG <- LagFilgruppe(gruppe, batchdate = batchdate, globs = globs, versjonert = versjonert)
  }
}
