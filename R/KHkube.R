#' LagKUBE
#' 
#' The main function of the production line, producing the files going to FHI Statistikk and public health profiles
#'
#' @param KUBEid Name of kube, corresponding to KUBE_NAVN in ACCESS
#' @param batchdate 
#' @param versjonert 
#' @param csvcopy Save a CSV-copy?
#' @param globs global parameters, defaults to SettGlobs()
#' @param dumps list of required dumps
#' @param write should results be written to files, default = TRUE. Set to FALSE for testing (only save to global envir)
#' @param alarm if TRUE, plays a sound when done
#' @param ... 
LagKUBE <- function(KUBEid,
                    versjonert = FALSE,
                    csvcopy = FALSE,
                    dumps = list(), 
                    write = FALSE,
                    alarm = FALSE,
                    geonaboprikk = TRUE,
                    ...) {
  # Because temporary files are generated with standard names, parallell processing
  # see check_if_system_available() function for description. 
  system_available_file <- file.path(fs::path_home(), getOption("khfunctions.lagkube_guardfile"))
  on.exit(fs::file_delete(system_available_file))
  system_available <- check_if_system_available(file = system_available_file)
  if(!system_available) stop("LagKUBE stoppet grunnet parallellkjøring, vent til den andre kuben er ferdig")
  
  is_kh_debug()
  batchdate <- SettKHBatchDate()
  globs <- SettGlobs()
  on.exit(RODBC::odbcCloseAll(), add = TRUE)
  
  sink(file = file.path(getOption("khfunctions.root"), getOption("khfunctions.dumpdir"), paste0("KUBELOGG/", KUBEid, "_", batchdate, "_LOGG.txt")), split = TRUE)
  on.exit(sink(), add = TRUE)
  if(!geonaboprikk) message("OBS! naboprikking på GEO er deaktivert!")
  
  parameters <- get_cubeparameters(KUBEid = KUBEid, batchdate = batchdate, globs = globs)
  load_and_format_files(parameters, batchdate = batchdate, versjonert = versjonert, globs = globs)
  parameters[["filedesign"]] <- get_filedesign(parameters = parameters, globs = globs)
  
  KUBEinformation <- parameters$CUBEinformation
  TNPinformation <- parameters$TNPinformation
  STNPinformation <- parameters$STNPinformation
  fileinformation <- parameters$fileinformation
  filedesign <- parameters$filedesign
  filer <- parameters$files
  PredFilter <- parameters$PredFilter
  D_develop_predtype <- parameters$PredFilter$D_develop_predtype
  
  stataspec <- kube_spec(spec = KUBEinformation)
  
  # If write = TRUE, save ACCESS specs
  if(isTRUE(write)){
    cat("Saving ACCESS specs to file:\n")
    utfils <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kubedir"), getOption("khfunctions.kube.specs"), paste0("spec_", KUBEid, "_", batchdate, ".csv"))
    specs <- GetAccessSpecs(KUBEid = KUBEid, kuber = KUBEinformation, tnp = TNPinformation, stnp = STNPinformation, filgrupper = fileinformation, batchdate = batchdate, globs = globs)
    data.table::fwrite(specs, file = utfils, sep = ";")
    cat("\n", utfils)
  }
  
  # LAG TNF ----
  
  cat("\n******LAGER TNF\n")
  TNtab <- merge_teller_nevner(files = filer, filedesigns = filedesign, fileparameters = fileinformation, TNPparameters = TNPinformation, KUBEparameters = KUBEinformation, globs = globs)
  TNF <- TNtab$TNF
  KUBEd <- TNtab$KUBEd
  rm(TNtab)
  if (TNPinformation$NEVNERKOL != "-") TNF <- LeggTilNyeVerdiKolonner(TNF, "RATE={TELLER/NEVNER}")
  cat("------FERDIG TNF\n")
  gc()
  
  # LAG STNF for prediksjon ----
  if (KUBEinformation$REFVERDI_VP == "P") {
    print("*****PREDIKER!!!")
    # Maa foerst finne design for (den syntetiske) koblinga ST, SN og PN
    # Bruk PREDfilter paa ST og evt SN
    # Finn saa FellesTab for disse
    cat("***Skal finne felles design for STANDARDTELLER, STANDARDNEVNER og PREDNEVNER\n")
    STFd <- FinnDesignEtterFiltrering(filedesign[[filer$STANDARDTELLER]], PredFilter$Design, PredFilter$Pkols, FGP = fileinformation[[filer$STANDARDTELLER]], globs = globs)
    if (!is.null(filer$STANDARDNEVNER) && !is.na(filer$STANDARDNEVNER)) {
      SNFd <- FinnDesignEtterFiltrering(filedesign[[filer$STANDARDNEVNER]], PredFilter$Design, PredFilter$Pkols, FGP = fileinformation[[filer$STANDARDNEVNER]], globs = globs)
      STNFd <- FinnFellesTab(STFd, SNFd, globs = globs)$FDes
    } else {
      STNFd <- STFd
    }
    # Finn FellesTab ogsaa med PN, denne gjelder som Til-design for PN
    STNPFd <- FinnFellesTab(STNFd, filedesign[[filer$PREDNEVNER]], globs = globs)$FDes
    # Maa filtrere STNPFd med Predfilter igjen for aa finne ny STNFd som gir til-design for ST og SN
    # (merge med PN kan ha endra fra STNFd-versjonen over
    STNFd <- FinnDesignEtterFiltrering(STNPFd, PredFilter$Design, FGP = fileinformation[[filer$STANDARDTELLER]], globs = globs)
    cat("---Satt felles design for STANDARDTELLER, STANDARDNEVNER og PREDNEVNER\n")
    
    STN <- merge_teller_nevner(files = filer, filedesigns = filedesign, fileparameters = fileinformation, TNPparameters = STNPinformation, TELLERFIL = "STANDARDTELLER", NEVNERFIL = "STANDARDNEVNER", Design = STNFd, KUBEparameters = NULL, globs = globs)
    STN <- STN$TNF
    
    # Fjern PredFilter$Pkols
    STN[, (PredFilter$Pkols) := NULL]
    
    # SETT RATE HER er mest effektivt!
    STN[!(NEVNER == 0 & NEVNER.f == 0), c("PREDRATE", "PREDRATE.f", "PREDRATE.a") := list(TELLER / NEVNER, pmax(TELLER.f, NEVNER.f), pmax(TELLER.a, NEVNER.a))]
    STN[NEVNER == 0 & NEVNER.f == 0, c("PREDRATE", "PREDRATE.f", "PREDRATE.a") := list(0, pmax(TELLER.f, 2), pmax(TELLER.a, NEVNER.a))]
    # Maa JUKSE DET TIL LITT MED NEVNER 0. Bruken her er jo slik at dette er tomme celler, og ikke minst vil raten nesten garantert skulle ganges med et PREDTELLER=0
    # Tillater TELLER<=2 for aa unngaa evt numeriske problemer. Virker helt uskyldig gitt bruken
    STN[TELLER <= 2 & TELLER.f == 0 & NEVNER == 0 & NEVNER.f == 0, c("PREDRATE", "PREDRATE.f", "PREDRATE.a") := list(0, 0, pmax(TELLER.a, NEVNER.a))]
    
    ukurante <- nrow(subset(STN, is.na(TELLER) | is.na(NEVNER)))
    if (ukurante > 0) {
      cat("!!! NAs i ST og/eller SN (", ukurante, "), dette vil gi problemer i PREDTELLER\n")
    }
    soppelkols <- setdiff(names(STN), c(get_dimension_columns(names(STN)), paste0("PREDRATE", c("", ".f", ".a"))))
    if (length(soppelkols) > 0) {
      STN[, (soppelkols) := NULL]
    }
    
    cat("------FERDIG med STN\n")
    cat("***Lager PN\n")
    PNrd <- FinnRedesign(filedesign[[filer$PREDNEVNER]], STNPFd, globs = globs)
    PN <- OmkodFil(FinnFilT(filer$PREDNEVNER), PNrd, globs = globs)
    
    if (!(is.na(TNPinformation$PREDNEVNERFIL) | TNPinformation$PREDNEVNERFIL == "")) {
      PredNevnerKol <- gsub("^(.*):(.*)", "\\2", TNPinformation$PREDNEVNERFIL)
    } else {
      PredNevnerKol <- TNPinformation$NEVNERKOL
    }
    PNnames <- gsub(paste0("^", PredNevnerKol, "(\\.f|\\.a|)$"), "PREDNEVNER\\1", names(PN))
    data.table::setnames(PN, names(PN), PNnames)
    soppelkols <- setdiff(names(PN), c(get_dimension_columns(names(PN)), paste0("PREDNEVNER", c("", ".f", ".a"))))
    if (length(soppelkols) > 0) {
      PN[, (soppelkols) := NULL]
    }
    cat("---Ferdig PN\n")
    cat("******Lager STNP, dette kan bli en stor tabell foer kollaps til PT\n")
    commonkols <- intersect(get_dimension_columns(names(PN)), get_dimension_columns(names(STN)))
    data.table::setkeyv(STN, commonkols)
    data.table::setkeyv(PN, commonkols)
    mismatch <- nrow(STN[!PN, allow.cartesian = TRUE])
    if (mismatch > 0) {
      cat("!!!!!ADVARSEL: Mismatch i STN[PN,] paa ", mismatch, "kolonner\n")
    }
    
    # Finn omkoding til KUBEd, dvs design for TNF
    # NB: Her maa det aggregeres opp for standardisering
    PNd <- FinnDesign(PN, FGP = fileinformation[[filer$PREDNEVNER]], globs = globs)
    # Burde kanskje bruke STNFd i stedet, men da maa den faa paa PredFilterDimensjonene. Maa uansett sende til FinDesigmm
    RD <- FinnRedesign(PNd, list(Part = KUBEd$MAIN), SkalAggregeresOpp = globs$DefDesign$AggVedStand, globs = globs)
    cat("Foer merge: dim(PN)", dim(PN), " og dim(STN)", dim(STN))
    STNP <- STN[PN, allow.cartesian = TRUE]
    STNP[, c("PREDTELLER", "PREDTELLER.f", "PREDTELLER.a") := list(PREDRATE * PREDNEVNER, pmax(PREDRATE.f, PREDNEVNER.f), pmax(PREDRATE.a, PREDNEVNER.a))]
    # Kast overfloedige kolonner
    kastkols <- setdiff(names(STNP), c(get_dimension_columns(names(STNP)), "PREDTELLER", "PREDTELLER.f", "PREDTELLER.a"))
    STNP[, (kastkols) := NULL]
    cat(" og etter mergre dim(STNP)", dim(STNP), "\n")
    PT <- OmkodFil(STNP, RD, globs = globs)
    cat("-----PREDTELLER (PT) ferdig med dim(PT)", dim(PT), "\n")
    cat("***Merger med TNF\n")
    orgdim <- dim(TNF)
    
    # Merge PT med TNF til ferdig kube
    tabkols <- get_dimension_columns(names(TNF))
    data.table::setkeyv(TNF, tabkols)
    data.table::setkeyv(PT, tabkols)
    KUBE <- PT[TNF]
    KUBE <- set_implicit_null_after_merge(KUBE, fileinformation[[filer[["TELLER"]]]]$vals)
    cat("Foer merge KUBE<-PT[TNF] er dim(TNF)", orgdim, " og etter merge dim(KUBE)", dim(KUBE), "\n")
    cat("------FERDIG MED PREDIKSJON\n")
  } else {
    KUBE <- data.table::copy(TNF)
  }
  
  .GlobalEnv$BUFFER <- NULL
  
  if ("raaKUBE0" %in% names(dumps)) {
    for (format in dumps[["raaKUBE0"]]) {
      DumpTabell(KUBE, paste0(KUBEid, "_raaKUBE0"), globs = globs, format = format)
    }
  }
  
  # STANDARDISERING ----
  
  if (D_develop_predtype == "DIR") {
  # Sett skala for teller (maa gjoeres foer rate brukes i MEISskala)
    if (!(is.na(KUBEinformation$RATESKALA) | KUBEinformation$RATESKALA == "")) {
      KUBE[, RATE := RATE * as.numeric(KUBEinformation$RATESKALA)]
    }
    
    # FINN MEISskala. Merk at dette gjelder baade ved REFVERDI_VP=P og =V
    if (KUBEinformation$REFVERDI_VP == "P") {
      VF <- KUBE[eval(rlang::parse_expr(PredFilter$PfiltStr))]
      # Evt hvis en eller flere element i PredFilter ikke er med i Design for TNF og maa lages
      if (nrow(VF) == 0) {
        cat("************************************\nNOE RART MED PredFilter, IKKE I KUBEDESIGN, MAA UT PAA NY OMKODING.\nER DETTE RETT?\n")
        VF <- OmkodFilFraPart(TNF, PredFilter$Design, FGP = fileinformation[[filer$TELLER]], globs = globs)
      }
      
      VF[, MEISskala := RATE]
      VFtabkols <- setdiff(intersect(names(VF), globs$DefDesign$DesignKolsFA), PredFilter$Pkols)
      VF <- VF[, c(VFtabkols, "MEISskala"), with = FALSE]
      
      data.table::setkeyv(KUBE, VFtabkols)
      data.table::setkeyv(VF, VFtabkols)
      KUBE <- VF[KUBE]
    } else {
      KUBE[, MEISskala := NA_real_]
    }
  }
  
  # AGGREGER PERIODE ----
  # Finn "snitt" for ma-aar.
  # DVs, egentlig lages forloepig bare summer, snitt settes etter prikking under
  # Snitt tolerer missing av type .f=1 ("random"), men bare noen faa anonyme .f>1, se KHaggreger
  # Rapporterer variabelspesifikk VAL.n som angir antall aar brukt i summen naar NA holdt utenom
  
  data.table::setkeyv(KUBE, c("AARl", "AARh"))
  aar <- unique(KUBE[, c("AARl", "AARh"), with = FALSE])
  int_lengde <- as.integer(unique(KUBE[, AARh - AARl + 1]))
  if (length(int_lengde) > 1) {
    KHerr("!!!!!!HAR ULIKE LENGDER PAA INTERVALLER!!")
  }
  
  # Maa "balansere" NA i teller og nevner slik sumrate og sumnevner balanserer  (Bedre/enklere aa gjoere det her enn i KHaggreger)
  # Kunne med god grunn satt SPVFLAGG her og saa bare operert med denne som en egenskap for hele linja i det som kommer
  # Men for aa ha muligheten for aa haandtere de forskjellige varibalene ulikt og i full detalj lar jeg det staa mer generelt
  # Slik at dataflyten stoetter en slik endring
  
  tuppel <- intersect(c("TELLER", "NEVNER", "RATE"), names(KUBE))
  tuppel.f <- paste0(tuppel, ".f")
  fmax <- rlang::parse_expr(paste0("pmax(", paste(tuppel.f, collapse = ","), ")"))
  if (length(tuppel) > 0) {
    KUBE[eval(fmax) > 0, (tuppel) := list(NA)]
    KUBE[eval(fmax) > 0, (tuppel.f) := eval(fmax)]
    # Om enkeltobservasjoner ikke skal brukes, men samtidig tas ut av alle summeringer
    # kan man ha satt VAL=0,VAL.f=-1
    # Dette vil ikke oedelegge summer der tallet inngaar. Tallet selv, eller sumemr av kun slike tall, settes naa til NA
    # Dette brukes f.eks naar SVANGERROYK ekskluderer Oslo, Akershus. Dette er skjuling, saa VAL.f=3
    KUBE[eval(fmax) == -1, (tuppel) := list(NA)]
    KUBE[eval(fmax) == -1, (tuppel) := list(3)]
  }
  
  ma_satt <- 0
  orgintMult <- 1
  if (KUBEinformation$MOVAV > 1) {
    if (any(aar$AARl != aar$AARh)) {
      KHerr(paste0("Kan ikke sette snitt (ma=", ma, ") naar det er intervaller i originaldata"))
    } else {
      ma <- KUBEinformation$MOVAV
      
      # Finner evt hull i aar for hele designet
      AntYMiss <- max(aar$AARl) - min(aar$AARl) + 1 - length(aar$AARl)
      if (AntYMiss > 0) {
        cat("Setter SumOverAar med AntYMiss=", AntYMiss, "\n")
      }
      maKUBE <- FinnSumOverAar(KUBE, per = ma, FyllMiss = TRUE, AntYMiss = AntYMiss, globs = globs)
      
      # sett rate paa nytt
      if (TNPinformation$NEVNERKOL != "-") {
        maKUBE <- LeggTilNyeVerdiKolonner(maKUBE, "RATE={TELLER/NEVNER}")
        if (D_develop_predtype == "DIR") {
          if (!(is.na(KUBEinformation$RATESKALA) | KUBEinformation$RATESKALA == "")) {
            maKUBE[, RATE := RATE * as.numeric(KUBEinformation$RATESKALA)]
          }
        }
      }
      ma_satt <- 1
      KUBE <- maKUBE
    }
  } else {
    # Maa legge til VAL.n for regning under naar orignale periodesummer, evt n=1 naar originale snitt
    valkols <- get_value_columns(names(KUBE))
    orgint_n <- int_lengde[1]
    n <- orgint_n
    if (!is.na(fileinformation[[filer$TELLER]]$ValErAarsSnitt)) {
      n <- 1
      orgintMult <- orgint_n
    }
    KUBE[, paste0(names(.SD), ".n") := list(n), .SDcols = valkols]
  }
  
  # Fikser BYDEL_STARTAAR, DK2020START og AALESUND/HARAM 2020-23
  fix_geo_special(d = KUBE, specs = fileinformation[[filer$TELLER]], id = KUBEid)
  
  if ("maKUBE0" %in% names(dumps)) {
    for (format in dumps[["maKUBE0"]]) {
      DumpTabell(KUBE, paste0(KUBEid, "_maKUBE0"), globs = globs, format = format)
    }
  }
  
  # Anonymiser og skjul ----
  
  # Anonymiser, trinn 1   Filtrer snitt som ikke skal brukes pga for mye anonymt fra original
  if (ma_satt == 1) {
    valkols <- get_value_columns(names(KUBE))
    anon_tot_tol <- getOption("khfunctions.anon_tot_tol") #0.2
    
    for(kol in valkols){
    kol.f <- paste0(kol, ".f")
    kol.n <- paste0(kol, ".n")
    kol.fn3 <- paste0(kol, ".fn3")
    data.table::set(KUBE,
                    i = KUBE[get(kol.n) > 0 & get(kol.fn3)/get(kol.n) >= anon_tot_tol, which = TRUE],
                    j = c(kol, kol.f),
                    value = list(NA, 3))
    }
  }
  
  if ("anoKUBE1" %in% names(dumps)) {
    for (format in dumps[["anoKUBE1"]]) {
      DumpTabell(KUBE, paste0(KUBEid, "_anoKUBE1"), globs = globs, format = format)
    }
  }
  
  # Anonymiser, trinn 2 Ekte anonymisering basert paa liten teller, liten nevner og liten N-T
  if (!(is.na(KUBEinformation$PRIKK_T) | KUBEinformation$PRIKK_T == "")) {
    cat("T-PRIKKER", nrow(subset(KUBE, TELLER <= KUBEinformation$PRIKK_T)), "rader\n")
    KUBE[TELLER <= KUBEinformation$PRIKK_T & TELLER.f >= 0, c("TELLER", "TELLER.f", "RATE", "RATE.f") := list(NA, 3, NA, 3)]
    cat("N-T-PRIKKER", nrow(subset(KUBE, NEVNER - TELLER <= KUBEinformation$PRIKK_T)), "rader\n")
    KUBE[NEVNER - TELLER <= KUBEinformation$PRIKK_T & TELLER.f >= 0 & NEVNER.f >= 0, c("TELLER", "TELLER.f", "RATE", "RATE.f") := list(NA, 3, NA, 3)]
  }
  
  if (!(is.na(KUBEinformation$PRIKK_N) | KUBEinformation$PRIKK_N == "")) {
    # N<PRIKK_N
    cat("N-PRIKKER", nrow(subset(KUBE, NEVNER <= KUBEinformation$PRIKK_N)), "rader\n")
    KUBE[NEVNER <= KUBEinformation$PRIKK_N & NEVNER.f >= 0, c("TELLER", "TELLER.f", "RATE", "RATE.f") := list(NA, 3, NA, 3)]
  }
  
  if ("anoKUBE2" %in% names(dumps)) {
    for (format in dumps[["anoKUBE2"]]) {
      DumpTabell(KUBE, paste0(KUBEid, "_anoKUBE2"), globs = globs, format = format)
    }
  }
  # Anonymiser trinn 3. Anonymiser naboer
  if (!(is.na(KUBEinformation$OVERKAT_ANO) | KUBEinformation$OVERKAT_ANO == "")) {
    # DEVELOP: BRuk .f=4 her slik at ikke slaar ut i HULL under
    KUBE <- AnonymiserNaboer(KUBE, KUBEinformation$OVERKAT_ANO, FGP = fileinformation[[filer$TELLER]], D_develop_predtype, globs = globs)
  }
  if ("anoKUBE3" %in% names(dumps)) {
    for (format in dumps[["anoKUBE3"]]) {
      DumpTabell(KUBE, paste0(KUBEid, "_anoKUBE3"), globs = globs, format = format)
    }
  }
  
  # Anonymiser trinn 4. Skjule svake og skjeve tidsserrier
  SvakAndelAvSerieGrense <- getOption("khfunctions.anon_svakandel")
  HullAndelAvSerieGrense <- getOption("khfunctions.anon_hullandel")
  
  if (!(is.na(KUBEinformation$STATTOL_T) | KUBEinformation$STATTOL_T == "")) {
    tabkols <- setdiff(intersect(names(KUBE), globs$DefDesign$DesignKolsFA), c(globs$DefDesign$DelKols[["Y"]]))
    KUBE[TELLER.f < 9, AntAar := .N, by = tabkols]
    KUBE[TELLER.f < 9, SVAK := sum(is.na(TELLER) | TELLER <= KUBEinformation$STATTOL_T), by = tabkols]
    KUBE[TELLER.f < 9, HULL := sum(TELLER.f == 3), by = tabkols]
    KUBE[TELLER.f < 9, SKJUL := ifelse(SVAK / AntAar > SvakAndelAvSerieGrense | HULL / AntAar > HullAndelAvSerieGrense, 1, 0)]
    
    
    cat("Skjuler", nrow(subset(KUBE, SKJUL == 1)), "rader\n")
    KUBE[SKJUL == 1, c("TELLER", "TELLER.f") := list(NA, 3)]
    KUBE[SKJUL == 1, c("RATE", "RATE.f") := list(NA, 3)]
    KUBE[, c("SVAK", "HULL", "SKJUL", "AntAar") := NULL]
  }

  if ("anoKUBE4" %in% names(dumps)) {
    for (format in dumps[["anoKUBE4"]]) {
      DumpTabell(KUBE, paste0(KUBEid, "_anoKUBE4"), globs = globs, format = format)
    }
  }
  
  # LAYOUT ----
  
  if ("KUBE_SLUTTREDIGERpre" %in% names(dumps)) {
    for (format in dumps[["KUBE_SLUTTREDIGERpre"]]) {
      DumpTabell(KUBE, paste0(KUBEid, "_KUBE_SLUTTREDIGERpre"), globs = globs, format = format)
    }
  }
  
  ## RSYNT_SLUTTREDIGER ---- 
  
  if (!(is.na(KUBEinformation$SLUTTREDIGER) | KUBEinformation$SLUTTREDIGER == "")) {
    synt <- gsub("\\\r", "\\\n", KUBEinformation$SLUTTREDIGER)
    error <- ""
    ok <- 1
    if (grepl("<STATA>", synt)) {
      synt <- gsub("<STATA>[ \n]*(.*)", "\\1", synt)
      RES <- KjorStataSkript(KUBE, synt, tableTYP = "DT", batchdate = batchdate, globs = globs)
      if (RES$feil != "") {
        stop("Something went wrong in STATA, SLUTTREDIGER", RES$feil, sep = "\n")
      } else {
        KUBE <- RES$TABLE
      }
    } else {
      rsynterr <- try(eval(parse(text = synt)), silent = TRUE)
      if ("try-error" %in% class(rsynterr)) {
        print(rsynterr)
        stop("Something went wrong in R, SLUTTREDIGER")
      }
    }
  }
  
  if ("KUBE_SLUTTREDIGERpost" %in% names(dumps)) {
    for (format in dumps[["KUBE_SLUTTREDIGERpost"]]) {
      DumpTabell(KUBE, paste0(KUBEid, "_KUBE_SLUTTREDIGERpost"), globs = globs, format = format)
    }
  }
  
  ## KOLONNER ----
  
  OrgKubeKolNames <- names(KUBE)
  
  # Alle kolonner settes for alle KUBER uavhengig av om TELLER, NEVNER, RATE, MALTALL, PRED=V/P etc
  if (!"NEVNER" %in% names(KUBE)) {
    KUBE[, NEVNER := NA]
  }
  if (!"RATE" %in% names(KUBE)) {
    KUBE[, RATE := NA]
  }
  
  if (D_develop_predtype != "DIR") {
    # Sett skala for teller
    if (!(is.na(KUBEinformation$RATESKALA) | KUBEinformation$RATESKALA == "")) {
      KUBE[, RATE := RATE * as.numeric(KUBEinformation$RATESKALA)]
    }
  }
  
  # Legg til manglende kolonner for homogen behandling under
  missKol <- setdiff(unlist(lapply(c("TELLER", "NEVNER", "RATE", "PREDTELLER"), function(x) {
    paste0(x, c("", ".f", ".a", ".n"))
  })), names(KUBE))
  if (length(missKol) > 0) {
    KUBE[, (missKol) := NA]
  }
  
  ## sumTELLER-NEVNER-PREDTELLER ----
  # Behold sum, disse sendes til Friskvik
  KUBE[, sumTELLER := orgintMult * TELLER]
  KUBE[, sumNEVNER := orgintMult * NEVNER]
  KUBE[, sumPREDTELLER := orgintMult * PREDTELLER]
  
  ## Gjennomsnitt ---- 
  # Ta snitt for alt annet enn RATE (der forholdstallet gjoer snitt uoensket)
  valkols <- setdiff(get_value_columns(names(KUBE)), c("RATE", "SMR"))
  if (length(valkols) > 0) {
    lp <- paste0("KUBE[,c(\"", paste(valkols, collapse = "\",\""), "\"):=list(",
                 paste0(valkols, "=", valkols, "/", valkols, ".n", collapse = ","
                       ),
                 ")]")
    KUBE[, eval(parse(text = lp))]
  }
  
  ## Nye verdikolonner ----
  if (!(is.na(TNPinformation$NYEKOL_RAD_postMA) | TNPinformation$NYEKOL_RAD_postMA == "")) {
    KUBE <- LeggTilNyeVerdiKolonner(KUBE, TNPinformation$NYEKOL_RAD_postMA, slettInf = TRUE, postMA = TRUE)
  }
  
  ## MALTALL ----
  if (grepl("\\S", KUBEinformation$MTKOL)) {
    maltall <- KUBEinformation$MTKOL
    KUBE[, eval(parse(text = paste0("MALTALL:=", KUBEinformation$MTKOL)))]
  } else if (TNPinformation$NEVNERKOL == "-") {
    maltall <- "TELLER"
    KUBE[, MALTALL := TELLER]
  } else {
    maltall <- "RATE"
    KUBE[, MALTALL := RATE]
  }
  
  ## SETT SMR og MEIS ----
  if (D_develop_predtype == "DIR") {
    
    if (KUBEinformation$REFVERDI_VP == "P") {
      KUBE[, SMR := sumTELLER / sumPREDTELLER * 100]
      KUBE[, MEIS := (sumTELLER / sumPREDTELLER) * MEISskala]
    } else if (KUBEinformation$REFVERDI_VP == "V") {
      KUBE[, SMR := NA_real_]
      KUBE[, MEIS := MALTALL]
    } else {
      KUBE[, SMR := NA_real_]
      KUBE[, MEIS := MALTALL]
    }
  } else {
    # SETT SMRtmp. For aa lage NORMSMR under maa denne settes foer NORM. Derfor kan jeg ikke sette SMR=MALTALL/NORM naa.
    # Men NORMSMR er selvsagt alltid 100 for REFVERDI_P="V"
    if (KUBEinformation$REFVERDI_VP == "P") {
      KUBE[, SMRtmp := sumTELLER / sumPREDTELLER * 100]
    } else if (KUBEinformation$REFVERDI_VP == "V") {
      KUBE[, SMRtmp := 100]
    } else {
      KUBE[, SMRtmp := NA]
    }
  }
  
  ## LANDSNORMAL ----
  # FINN "LANDSNORMAL". Merk at dette gjelder baade ved REFVERDI_VP=P og =V
  
  if (D_develop_predtype == "DIR") {
    # Midlertidig dirty loesning
    # Det boer saa lages en ny kolonne KUBER:REFGEOn som har GEOniv for referanseverdi. Denne brukes primaert for aa sette SMR i modus=V
    RefGEOn <- "L"
    RefGEOnFilt <- paste0("GEOniv=='", RefGEOn, "'")
    VF <- eval(parse(text = paste0("subset(KUBE,", RefGEOnFilt, ")")))
  } else {
    VF <- KUBE[eval(rlang::parse_expr(PredFilter$PfiltStr))]
  }
  
  # Evt hvis en eller flere element i PredFilter ikke er med i Design for TNF og maa lages
  if (nrow(VF) == 0) {
    cat("************************************\nNOE RART MED LANDSNORM, IKKE I KUBEDESIGN, MAA UT PAA NY OMKODING.\nER DETTE RETT?\n")
    VF <- OmkodFilFraPart(TNF, PredFilter$Design, FGP = fileinformation[[filer$TELLER]], globs = globs)
  }
  
  if (D_develop_predtype == "IND") {
    VFtabkols <- setdiff(intersect(names(VF), globs$DefDesign$DesignKolsFA), PredFilter$Pkols)
    if (maltall %in% c("TELLER", "RATE")) {
      data.table::setnames(VF, c(paste0(maltall, c("", ".f", ".a", ".n")), "SMRtmp"), c(paste0("NORM", c("", ".f", ".a", ".n")), "NORMSMR"))
      VF <- VF[, c(VFtabkols, paste0("NORM", c("", ".f", ".a", ".n")), "NORMSMR"), with = FALSE]
    } else {
      data.table::setnames(VF, c(maltall, "SMRtmp"), c("NORM", "NORMSMR"))
      VF <- VF[, c(VFtabkols, "NORM", "NORMSMR"), with = FALSE]
    }
  } else {
    VF[, lopendeMEISref := MEIS]
    VFtabkols <- setdiff(intersect(names(VF), globs$DefDesign$DesignKolsFA), c("GEOniv", "GEO", "FYLKE"))
    VF <- VF[, c(VFtabkols, "lopendeMEISref"), with = FALSE]
  }
  data.table::setkeyv(KUBE, VFtabkols)
  data.table::setkeyv(VF, VFtabkols)
  
  KUBE <- VF[KUBE]
  
  if (D_develop_predtype == "IND") {
    # Juster SMR proporsjonalt slik at NORM (landet) alltid har SMR=100
    # SMR>100 kan oppstaa dersom det f.eks. er noen med ukjent alder/kjoenn.
    # Ratene for ukjent alder/kjoenn vil ikke matche nevner fra BEF, derfor vil det predikeres for faar doede relativt til observert
    if (KUBEinformation$REFVERDI_VP == "P") {
      KUBE[, SMR := sumTELLER / sumPREDTELLER * 100]
    } else if (KUBEinformation$REFVERDI_VP == "V") {
      KUBE[, SMR := MALTALL / NORM * 100]
    } else {
      KUBE[, SMR := NA]
    }
    
    KUBE[, SMR := 100 * (SMR / NORMSMR)]
    
    KUBE[, MEIS := SMR * NORM / 100]
  } else {
    KUBE[, lopendeFORHOLDSVERDI := MEIS / lopendeMEISref * 100]
    
    # D-develop
    # Dirty tricks i denne midlertidige loesninga, bruker gamle utnavn
    KUBE[, SMR := lopendeFORHOLDSVERDI]
    KUBE[, NORM := lopendeMEISref]
  }
  
  # Bytt til eksterne TAB-navn for ekstradimensjoner
  FGP <- fileinformation[[filer$TELLER]]
  etabs <- character(0)
  for (etab in names(KUBE)[grepl("^TAB\\d+$", names(KUBE))]) {
    if (grepl("\\S", FGP[[etab]])) {
      data.table::setnames(KUBE, etab, FGP[[etab]])
      etabs <- c(etabs, FGP[[etab]])
    }
  }
  
  # SETT UTKOLONNER FOR ALLVISKUBE
  if (!(is.na(KUBEinformation$NESSTARTUPPEL) | KUBEinformation$NESSTARTUPPEL == "")) {
    NstarTup <- unlist(stringr::str_split(KUBEinformation$NESSTARTUPPEL, ","))
  } else if (KUBEinformation$REFVERDI_VP == "P") {
    NstarTup <- c("T", "RATE", "SMR", "MEIS")
  } else {
    NstarTup <- character(0)
  }
  OutVar <- as.character(getOption("khfunctions.valcols")[NstarTup])
  
  if (!(is.na(KUBEinformation$EKSTRAVARIABLE) | KUBEinformation$EKSTRAVARIABLE == "")) {
    hjelpeVar <- unlist(stringr::str_split(KUBEinformation$EKSTRAVARIABLE, ","))
    OutVar <- c(OutVar, hjelpeVar)
  }
  
  KHtabs <- getOption("khfunctions.khtabs")
  tabs <- c(KHtabs, etabs)
  if (!(is.na(KUBEinformation$DIMDROPP) | KUBEinformation$DIMDROPP == "")) {
    dimdropp <- unlist(stringr::str_split(KUBEinformation$DIMDROPP, ","))
    tabs <- setdiff(tabs, dimdropp)
  }
  
  KUBE[, AAR := paste0(AARl, "_", AARh)]
  if (all(c("ALDERl", "ALDERh") %in% names(KUBE))) {
    KUBE[, ALDER := paste0(ALDERl, "_", ALDERh)]
  } else {
    tabs <- setdiff(tabs, "ALDER")
  }
  if (!"KJONN" %in% names(KUBE)) {
    tabs <- setdiff(tabs, "KJONN")
  }
  
  # Filtrer bort GEO, ALDER og KJONN som ikke skal rapporteres
  KUBE <- KUBE[GEO %in% globs$UtGeoKoder]
  
  # EVT SPESIALBEHANDLING
  
  if ("STATAPRIKKpre" %in% names(dumps)) {
    for (format in dumps[["STATAPRIKKpre"]]) {
      DumpTabell(KUBE, paste0(KUBEid, "_STATAPRIKKpre"), globs = globs, format = format)
    }
  }

  # STATAPRIKKING ---- 
  # Lage stataspec og overskrive helseprofil/kubespec.csv inkludert DIMS 
  dims <- find_dims(dt = KUBE, spec = fileinformation)
  # geoprikk_triangles <- get_geonaboprikk_triangles(geoniv = unique(KUBE$GEOniv))
  stataspec <- kube_spec(spec = KUBEinformation, dims = dims, geonaboprikk = geonaboprikk, geoprikktriangel = get_geonaboprikk_triangles())

  KUBE <- do_stata_prikk(dt = KUBE, spc = stataspec, batchdate = batchdate, geonaboprikk = geonaboprikk, globs = globs)

  if ("STATAPRIKKpost" %in% names(dumps)) {
    for (format in dumps[["STATAPRIKKpost"]]) {
      DumpTabell(KUBE, paste0(KUBEid, "_STATAPRIKKpost"), globs = globs, format = format)
    }
  }
  
  # RSYNT_POSTPROSESS ---- 
  if (!(is.na(KUBEinformation$RSYNT_POSTPROSESS) | KUBEinformation$RSYNT_POSTPROSESS == "")) {
    synt <- gsub("\\\r", "\\\n", KUBEinformation$RSYNT_POSTPROSESS)
    if (grepl("<STATA>", synt)) {
      synt <- gsub("<STATA>[ \n]*(.*)", "\\1", synt)
      RES <- KjorStataSkript(KUBE, synt, tableTYP = "DT", batchdate = batchdate, globs = globs)
      if (RES$feil != "") {
        stop("Something went wrong in STATA, RSYNT_POSTPROSESS", RES$feil, sep = "\n")
      } else {
        KUBE <- RES$TABLE
      }
    } else {
      rsynterr <- try(eval(parse(text = synt)), silent = TRUE)
      if ("try-error" %in% class(rsynterr)) {
        print(rsynterr)
        stop("Something went wrong in R, RSYNT_POSTPROSESS")
      }
    }
  }
  
  if ("RSYNT_POSTPROSESSpost" %in% names(dumps)) {
    for (format in dumps[["RSYNT_POSTPROSESSpost"]]) {
      DumpTabell(KUBE, paste0(KUBEid, "_RSYNT_POSTPROSESSpost"), globs = globs, format = format)
    }
  }
  
  if ("ALDER" %in% names(KUBE)) {
    KUBE <- KUBE[!ALDER %in% c("999_999", "888_888"), ]
  }
  if ("KJONN" %in% names(KUBE)) {
    KUBE <- KUBE[!KJONN %in% c(8, 9), ]
  }
  
  ## ---- TODO: REKTANGULARISERE MANGLENDE RADER FOR BYDEL ----
  ## Der bydel starter senere enn andre mÃ¥ disse radene genereres, da ALLVIS ikke takler manglende rader.
  
  # LAYOUT
  utkols <- c(tabs, OutVar)
  ALLVIS <- data.table::copy(KUBE)
  
  # SKJUL HELE TUPPELET
  # FLAGG PER VARIABEL KAN/BoeR VURDERES!
  # Litt tricky aa finne riktig ".f"-kolloner. Maa ikke ta med mBEFc f.eks fra BEF fila dersom denne er irrelevant
  fvars <- intersect(names(ALLVIS), paste0(union(getOption("khfunctions.valcols"), OutVar), ".f"))
  ALLVIS[, SPVFLAGG := 0]
  if (length(fvars) > 0) {
    # Dette er unoedvendig krongelete. Men dersom f.eks RATE.f=2 pga TELLER.f=1, oenskes SPVFLAGG=1
    ALLVIS[, tSPV1 := eval(parse(text = paste0("pmax(", paste(lapply(fvars, function(x) {
      paste0(x, "*(", x, "!=2)")
    }), collapse = ","), ",na.rm = TRUE)")))]
    ALLVIS[, tSPV2 := eval(parse(text = paste0("pmax(", paste(fvars, collapse = ","), ",na.rm = TRUE)")))]
    ALLVIS[, SPVFLAGG := ifelse(tSPV1 == 0, tSPV2, tSPV1)]
    ALLVIS[, c("tSPV1", "tSPV2") := NULL]
    ALLVIS[SPVFLAGG > 0, eval(parse(text = paste0("c(\"", paste(OutVar, collapse = "\",\""), "\"):=list(NA)")))]
  }
  ALLVIS[is.na(SPVFLAGG), SPVFLAGG := 0]
  ALLVIS[, SPVFLAGG := plyr::mapvalues(SPVFLAGG, c(-1, 9, 4), c(3, 1, 3), warn_missing = FALSE)]
  
  if(isTRUE(write)){
    LagAlleFriskvikIndikatorerForKube(KUBEid = KUBEid, KUBE = ALLVIS, FGP = fileinformation[[filer$TELLER]], modus = KUBEinformation$MODUS, batchdate = batchdate, globs = globs)
  }
  
  ALLVIS <- ALLVIS[, c(..utkols, "SPVFLAGG")]
  QC <- LagQCKube(allvis = ALLVIS, allvistabs = tabs, kube = KUBE)
  
  cat("---------------------KUBE FERDIG\n\n")
  
  RESULTAT <<- list(KUBE = KUBE, ALLVIS = ALLVIS, QC = QC)

  if(isTRUE(write)){
    basepath <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kubedir"))
    cat("SAVING OUTPUT FILES:\n")

    if(!geonaboprikk) KUBEid <- paste0("ikkegeoprikket_", KUBEid)

    utfiln <- file.path(basepath, getOption("khfunctions.kube.ny"), paste0(KUBEid, ".rds"))
    saveRDS(KUBE, file = utfiln)
    cat("\n", utfiln)
    
    if (versjonert == TRUE) {
      utfilv <- file.path(basepath, getOption("khfunctions.kube.dat"), "R", paste0(KUBEid, "_", batchdate, ".rds"))
      file.copy(utfiln, utfilv)
      cat("\n", utfilv)
    }
    
    if (csvcopy == TRUE) {
      utfild <- file.path(basepath, getOption("khfunctions.kube.dat"), "csv", paste0(KUBEid, "_", batchdate, ".csv"))
      data.table::fwrite(ALLVIS, file = utfild, sep = ";")
      cat("\n", utfild)
      
      utfilq <- file.path(basepath, getOption("khfunctions.kube.qc"), paste0("QC_", KUBEid, "_", batchdate, ".csv"))
      data.table::fwrite(QC, file = utfilq, sep = ";")
      cat("\n", utfilq)
    }
  }
  
  cat("-------------------------KUBE", KUBEid, "FERDIG--------------------------------------\n")
  cat("Se output med RESULTAT$KUBE (full), RESULTAT$ALLVIS (utfil) eller RESULTAT$QC (kvalkont)")
  if(alarm) try(beepr::beep(1))
}

#' LagKubeDatertCsv
#' 
#' Wrapper around LagKUBE, with default options to save output files
#'
LagKubeDatertCsv <- function(KUBEID, 
                             dumps = list(), 
                             versjonert = TRUE,
                             csvcopy = TRUE,
                             write = TRUE,
                             alarm = FALSE) {
  invisible(LagKUBE(KUBEid = KUBEID, versjonert = versjonert, csvcopy = csvcopy, dumps = dumps, write = write, alarm = alarm))
}

#' LagFlereKuber
#' 
#' Wrapper aroung LagKUBE, allowing for more than one KUBE to be made simultaneously
#' 
#' @param KUBEid_ALLE
#' @param ... Optional, can set versjonert, csvcopy, write arguments if TRUE not wanted
#'
LagFlereKuber <- function(KUBEid_ALLE, 
                          dumps = list(), 
                          alarm = FALSE,
                          ...) {
  for (KUBEid in KUBEid_ALLE) {
    LagKubeDatertCsv(KUBEid, dumps = dumps, alarm = alarm, ...)
  }
  sink()
}

