#' LagKUBE
#' 
#' The main function of the production line, producing the files going to FHI Statistikk and public health profiles
#'
#' @param KUBEid Name of kube, corresponding to KUBE_NAVN in ACCESS
#' @param versjonert 
#' @param csvcopy Save a CSV-copy?
#' @param globs global parameters, defaults to SettGlobs()
#' @param dumps list of required dumps
#' @param write should results be written to files, default = TRUE. Set to FALSE for testing (only save to global envir)
#' @param alarm if TRUE, plays a sound when done
#' @param geonaboprikk  should the file be secondary censored on geographical codes? default = TRUE
#' @param ... 
LagKUBE <- function(KUBEid,
                    versjonert = FALSE,
                    csvcopy = FALSE,
                    dumps = list(), 
                    write = FALSE,
                    alarm = FALSE,
                    geonaboprikk = TRUE,
                    ...) {
  is_kh_debug()
  check_if_lagkube_available()
  on.exit(lagkube_cleanup(), add = TRUE)
  
  batchdate <- SettKHBatchDate()
  globs <- SettGlobs()
  sink(file = file.path(getOption("khfunctions.root"), getOption("khfunctions.dumpdir"), paste0("KUBELOGG/", KUBEid, "_", batchdate, "_LOGG.txt")), split = TRUE)
  if(!geonaboprikk) message("OBS! GEO-naboprikking er deaktivert!")
  
  parameters <- get_cubeparameters(KUBEid = KUBEid, batchdate = batchdate, globs = globs)
  load_and_format_files(parameters, batchdate = batchdate, versjonert = versjonert, globs = globs)
  parameters[["filedesign"]] <- get_filedesign(parameters = parameters, globs = globs)
  parameters[["standardmethod"]] <- parameters$PredFilter$D_develop_predtype
  
  save_kubespec_csv(spec = parameters$CUBEinformation)
  if(isTRUE(write)) save_access_specs(KUBEid = KUBEid, parameterlist = parameters, batchdate = batchdate, globs = globs)
  
  # LAG TNF ----
  cat("******LAGER TNF\n")
  TNtab <- merge_teller_nevner(parameterlist = parameters, globs = globs)
  TNF <- TNtab$TNF
  KUBEd <- TNtab$KUBEd
  rm(TNtab)
  if (parameters$TNPinformation$NEVNERKOL != "-") TNF <- LeggTilNyeVerdiKolonner(TNF, "RATE={TELLER/NEVNER}")
  cat("------FERDIG TNF\n")
  gc()
  
  # LAG STNF for prediksjon ----
  if (parameters$CUBEinformation$REFVERDI_VP == "P") {
    print("*****PREDIKER!!!")
    # Maa foerst finne design for (den syntetiske) koblinga ST, SN og PN
    # Bruk parameters$PredFilter paa ST og evt SN
    # Finn saa FellesTab for disse
    cat("***Skal finne felles design for STANDARDTELLER, STANDARDNEVNER og PREDNEVNER\n")
    STFd <- FinnDesignEtterFiltrering(parameters$filedesign[[parameters$files$STANDARDTELLER]], parameters$PredFilter$Design, parameters$PredFilter$Pkols, FGP = parameters$fileinformation[[parameters$files$STANDARDTELLER]], globs = globs)
    if (!is.na(parameters$files$STANDARDNEVNER)) {
      SNFd <- FinnDesignEtterFiltrering(parameters$filedesign[[parameters$files$STANDARDNEVNER]], parameters$PredFilter$Design, parameters$PredFilter$Pkols, FGP = parameters$fileinformation[[parameters$files$STANDARDNEVNER]], globs = globs)
      STNFd <- FinnFellesTab(STFd, SNFd, globs = globs)$FDes
    } else {
      STNFd <- STFd
    }
    # Finn FellesTab ogsaa med PN, denne gjelder som Til-design for PN
    STNPFd <- FinnFellesTab(STNFd, parameters$filedesign[[parameters$files$PREDNEVNER]], globs = globs)$FDes
    # Maa filtrere STNPFd med parameters$PredFilter igjen for aa finne ny STNFd som gir til-design for ST og SN
    # (merge med PN kan ha endra fra STNFd-versjonen over
    STNFd <- FinnDesignEtterFiltrering(STNPFd, parameters$PredFilter$Design, FGP = parameters$fileinformation[[parameters$files$STANDARDTELLER]], globs = globs)
    cat("---Satt felles design for STANDARDTELLER, STANDARDNEVNER og PREDNEVNER\n")
    
    STN <- merge_teller_nevner(parameterlist = parameters, standardfiles = TRUE, design = STNFd, globs = globs)
    STN <- STN$TNF
    
    # Fjern parameters$PredFilter$Pkols
    STN[, (parameters$PredFilter$Pkols) := NULL]
    
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
    PNrd <- FinnRedesign(parameters$filedesign[[parameters$files$PREDNEVNER]], STNPFd, globs = globs)
    PN <- OmkodFil(FinnFilT(parameters$files$PREDNEVNER), PNrd, globs = globs)
    
    if (!(is.na(parameters$TNPinformation$PREDNEVNERFIL) | parameters$TNPinformation$PREDNEVNERFIL == "")) {
      PredNevnerKol <- gsub("^(.*):(.*)", "\\2", parameters$TNPinformation$PREDNEVNERFIL)
    } else {
      PredNevnerKol <- parameters$TNPinformation$NEVNERKOL
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
    # PNd <- FinnDesign(PN, FGP = parameters$fileinformation[[parameters$files$PREDNEVNER]], globs = globs)
    PNd <- find_filedesign(file = PN, fileparameters = parameters$fileinformation[[parameters$files$PREDNEVNER]], globs = globs)
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
    KUBE <- set_implicit_null_after_merge(KUBE, parameters$fileinformation[[parameters$files[["TELLER"]]]]$vals)
    cat("Foer merge KUBE<-PT[TNF] er dim(TNF)", orgdim, " og etter merge dim(KUBE)", dim(KUBE), "\n")
    cat("------FERDIG MED PREDIKSJON\n")
  } else {
    KUBE <- data.table::copy(TNF)
  }
  
  rydd <- setdiff(names(.GlobalEnv$BUFFER), c("BEF_GKa", "BEF_GKu"))
  .GlobalEnv$BUFFER[rydd] <- NULL
  
  if ("raaKUBE0" %in% names(dumps)) {
    for (format in dumps[["raaKUBE0"]]) {
      DumpTabell(KUBE, paste0(KUBEid, "_raaKUBE0"), globs = globs, format = format)
    }
  }
  
  # STANDARDISERING ----
  
  if (parameters$standardmethod == "DIR") {
  # Sett skala for teller (maa gjoeres foer rate brukes i MEISskala)
    if (!(is.na(parameters$CUBEinformation$RATESKALA) | parameters$CUBEinformation$RATESKALA == "")) {
      KUBE[, RATE := RATE * as.numeric(parameters$CUBEinformation$RATESKALA)]
    }
    
    # FINN MEISskala. Merk at dette gjelder baade ved REFVERDI_VP=P og =V
    if (parameters$CUBEinformation$REFVERDI_VP == "P") {
      VF <- KUBE[eval(rlang::parse_expr(parameters$PredFilter$PfiltStr))]
      # Evt hvis en eller flere element i parameters$PredFilter ikke er med i Design for TNF og maa lages
      if (nrow(VF) == 0) {
        cat("************************************\nNOE RART MED parameters$PredFilter, IKKE I KUBEDESIGN, MAA UT PAA NY OMKODING.\nER DETTE RETT?\n")
        VF <- OmkodFilFraPart(TNF, parameters$PredFilter$Design, FGP = parameters$fileinformation[[parameters$files$TELLER]], globs = globs)
      }
      
      VF[, MEISskala := RATE]
      VFtabkols <- setdiff(intersect(names(VF), globs$DefDesign$DesignKolsFA), parameters$PredFilter$Pkols)
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
  if (parameters$CUBEinformation$MOVAV > 1) {
    if (any(aar$AARl != aar$AARh)) {
      KHerr(paste0("Kan ikke sette snitt (ma=", ma, ") naar det er intervaller i originaldata"))
    } else {
      ma <- parameters$CUBEinformation$MOVAV
      
      # Finner evt hull i aar for hele designet
      AntYMiss <- max(aar$AARl) - min(aar$AARl) + 1 - length(aar$AARl)
      if (AntYMiss > 0) {
        cat("Setter SumOverAar med AntYMiss=", AntYMiss, "\n")
      }
      maKUBE <- FinnSumOverAar(KUBE, per = ma, FyllMiss = TRUE, AntYMiss = AntYMiss, globs = globs)
      
      # sett rate paa nytt
      if (parameters$TNPinformation$NEVNERKOL != "-") {
        maKUBE <- LeggTilNyeVerdiKolonner(maKUBE, "RATE={TELLER/NEVNER}")
        if (parameters$standardmethod == "DIR") {
          if (!(is.na(parameters$CUBEinformation$RATESKALA) | parameters$CUBEinformation$RATESKALA == "")) {
            maKUBE[, RATE := RATE * as.numeric(parameters$CUBEinformation$RATESKALA)]
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
    if (!is.na(parameters$fileinformation[[parameters$files$TELLER]]$ValErAarsSnitt)) {
      n <- 1
      orgintMult <- orgint_n
    }
    KUBE[, paste0(names(.SD), ".n") := list(n), .SDcols = valkols]
  }
  
  # Fikser BYDEL_STARTAAR, DK2020START og AALESUND/HARAM 2020-23
  fix_geo_special(d = KUBE, specs = parameters$fileinformation[[parameters$files$TELLER]], id = KUBEid)
  
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
  if (!(is.na(parameters$CUBEinformation$PRIKK_T) | parameters$CUBEinformation$PRIKK_T == "")) {
    cat("T-PRIKKER", nrow(subset(KUBE, TELLER <= parameters$CUBEinformation$PRIKK_T)), "rader\n")
    KUBE[TELLER <= parameters$CUBEinformation$PRIKK_T & TELLER.f >= 0, c("TELLER", "TELLER.f", "RATE", "RATE.f") := list(NA, 3, NA, 3)]
    cat("N-T-PRIKKER", nrow(subset(KUBE, NEVNER - TELLER <= parameters$CUBEinformation$PRIKK_T)), "rader\n")
    KUBE[NEVNER - TELLER <= parameters$CUBEinformation$PRIKK_T & TELLER.f >= 0 & NEVNER.f >= 0, c("TELLER", "TELLER.f", "RATE", "RATE.f") := list(NA, 3, NA, 3)]
  }
  
  if (!(is.na(parameters$CUBEinformation$PRIKK_N) | parameters$CUBEinformation$PRIKK_N == "")) {
    # N<PRIKK_N
    cat("N-PRIKKER", nrow(subset(KUBE, NEVNER <= parameters$CUBEinformation$PRIKK_N)), "rader\n")
    KUBE[NEVNER <= parameters$CUBEinformation$PRIKK_N & NEVNER.f >= 0, c("TELLER", "TELLER.f", "RATE", "RATE.f") := list(NA, 3, NA, 3)]
  }
  
  if ("anoKUBE2" %in% names(dumps)) {
    for (format in dumps[["anoKUBE2"]]) {
      DumpTabell(KUBE, paste0(KUBEid, "_anoKUBE2"), globs = globs, format = format)
    }
  }
  # Anonymiser trinn 3. Anonymiser naboer
  if (!(is.na(parameters$CUBEinformation$OVERKAT_ANO) | parameters$CUBEinformation$OVERKAT_ANO == "")) {
    # DEVELOP: BRuk .f=4 her slik at ikke slaar ut i HULL under
    KUBE <- AnonymiserNaboer(KUBE, parameters$CUBEinformation$OVERKAT_ANO, FGP = parameters$fileinformation[[parameters$files$TELLER]], parameters$standardmethod, globs = globs)
  }
  if ("anoKUBE3" %in% names(dumps)) {
    for (format in dumps[["anoKUBE3"]]) {
      DumpTabell(KUBE, paste0(KUBEid, "_anoKUBE3"), globs = globs, format = format)
    }
  }
  
  # Anonymiser trinn 4. Skjule svake og skjeve tidsserrier
  SvakAndelAvSerieGrense <- getOption("khfunctions.anon_svakandel")
  HullAndelAvSerieGrense <- getOption("khfunctions.anon_hullandel")
  
  if (!(is.na(parameters$CUBEinformation$STATTOL_T) | parameters$CUBEinformation$STATTOL_T == "")) {
    tabkols <- setdiff(intersect(names(KUBE), globs$DefDesign$DesignKolsFA), c(globs$DefDesign$DelKols[["Y"]]))
    KUBE[TELLER.f < 9, AntAar := .N, by = tabkols]
    KUBE[TELLER.f < 9, SVAK := sum(is.na(TELLER) | TELLER <= parameters$CUBEinformation$STATTOL_T), by = tabkols]
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
  
  if (!(is.na(parameters$CUBEinformation$SLUTTREDIGER) | parameters$CUBEinformation$SLUTTREDIGER == "")) {
    synt <- gsub("\\\r", "\\\n", parameters$CUBEinformation$SLUTTREDIGER)
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
  
  if (parameters$standardmethod != "DIR") {
    # Sett skala for teller
    if (!(is.na(parameters$CUBEinformation$RATESKALA) | parameters$CUBEinformation$RATESKALA == "")) {
      KUBE[, RATE := RATE * as.numeric(parameters$CUBEinformation$RATESKALA)]
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
  if (!(is.na(parameters$TNPinformation$NYEKOL_RAD_postMA) | parameters$TNPinformation$NYEKOL_RAD_postMA == "")) {
    KUBE <- LeggTilNyeVerdiKolonner(KUBE, parameters$TNPinformation$NYEKOL_RAD_postMA, slettInf = TRUE, postMA = TRUE)
  }
  
  ## MALTALL ----
  if (grepl("\\S", parameters$CUBEinformation$MTKOL)) {
    maltall <- parameters$CUBEinformation$MTKOL
    KUBE[, eval(parse(text = paste0("MALTALL:=", parameters$CUBEinformation$MTKOL)))]
  } else if (parameters$TNPinformation$NEVNERKOL == "-") {
    maltall <- "TELLER"
    KUBE[, MALTALL := TELLER]
  } else {
    maltall <- "RATE"
    KUBE[, MALTALL := RATE]
  }
  
  ## SETT SMR og MEIS ----
  if (parameters$standardmethod == "DIR") {
    
    if (parameters$CUBEinformation$REFVERDI_VP == "P") {
      KUBE[, SMR := sumTELLER / sumPREDTELLER * 100]
      KUBE[, MEIS := (sumTELLER / sumPREDTELLER) * MEISskala]
    } else if (parameters$CUBEinformation$REFVERDI_VP == "V") {
      KUBE[, SMR := NA_real_]
      KUBE[, MEIS := MALTALL]
    } else {
      KUBE[, SMR := NA_real_]
      KUBE[, MEIS := MALTALL]
    }
  } else {
    # SETT SMRtmp. For aa lage NORMSMR under maa denne settes foer NORM. Derfor kan jeg ikke sette SMR=MALTALL/NORM naa.
    # Men NORMSMR er selvsagt alltid 100 for REFVERDI_P="V"
    if (parameters$CUBEinformation$REFVERDI_VP == "P") {
      KUBE[, SMRtmp := sumTELLER / sumPREDTELLER * 100]
    } else if (parameters$CUBEinformation$REFVERDI_VP == "V") {
      KUBE[, SMRtmp := 100]
    } else {
      KUBE[, SMRtmp := NA]
    }
  }
  
  ## LANDSNORMAL ----
  # FINN "LANDSNORMAL". Merk at dette gjelder baade ved REFVERDI_VP=P og =V
  
  if (parameters$standardmethod == "DIR") {
    # Midlertidig dirty loesning
    # Det boer saa lages en ny kolonne KUBER:REFGEOn som har GEOniv for referanseverdi. Denne brukes primaert for aa sette SMR i modus=V
    RefGEOn <- "L"
    RefGEOnFilt <- paste0("GEOniv=='", RefGEOn, "'")
    VF <- eval(parse(text = paste0("subset(KUBE,", RefGEOnFilt, ")")))
  } else {
    VF <- KUBE[eval(rlang::parse_expr(parameters$PredFilter$PfiltStr))]
  }
  
  # Evt hvis en eller flere element i parameters$PredFilter ikke er med i Design for TNF og maa lages
  if (nrow(VF) == 0) {
    cat("************************************\nNOE RART MED LANDSNORM, IKKE I KUBEDESIGN, MAA UT PAA NY OMKODING.\nER DETTE RETT?\n")
    VF <- OmkodFilFraPart(TNF, parameters$PredFilter$Design, FGP = parameters$fileinformation[[parameters$files$TELLER]], globs = globs)
  }
  
  if (parameters$standardmethod == "IND") {
    VFtabkols <- setdiff(intersect(names(VF), globs$DefDesign$DesignKolsFA), parameters$PredFilter$Pkols)
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
  
  if (parameters$standardmethod == "IND") {
    # Juster SMR proporsjonalt slik at NORM (landet) alltid har SMR=100
    # SMR>100 kan oppstaa dersom det f.eks. er noen med ukjent alder/kjoenn.
    # Ratene for ukjent alder/kjoenn vil ikke matche nevner fra BEF, derfor vil det predikeres for faar doede relativt til observert
    if (parameters$CUBEinformation$REFVERDI_VP == "P") {
      KUBE[, SMR := sumTELLER / sumPREDTELLER * 100]
    } else if (parameters$CUBEinformation$REFVERDI_VP == "V") {
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
  FGP <- parameters$fileinformation[[parameters$files$TELLER]]
  etabs <- character(0)
  for (etab in names(KUBE)[grepl("^TAB\\d+$", names(KUBE))]) {
    if (grepl("\\S", FGP[[etab]])) {
      data.table::setnames(KUBE, etab, FGP[[etab]])
      etabs <- c(etabs, FGP[[etab]])
    }
  }
  
  # SETT UTKOLONNER FOR ALLVISKUBE
  if (!(is.na(parameters$CUBEinformation$NESSTARTUPPEL) | parameters$CUBEinformation$NESSTARTUPPEL == "")) {
    NstarTup <- unlist(stringr::str_split(parameters$CUBEinformation$NESSTARTUPPEL, ","))
  } else if (parameters$CUBEinformation$REFVERDI_VP == "P") {
    NstarTup <- c("T", "RATE", "SMR", "MEIS")
  } else {
    NstarTup <- character(0)
  }
  OutVar <- as.character(getOption("khfunctions.valcols")[NstarTup])
  
  if (!(is.na(parameters$CUBEinformation$EKSTRAVARIABLE) | parameters$CUBEinformation$EKSTRAVARIABLE == "")) {
    hjelpeVar <- unlist(stringr::str_split(parameters$CUBEinformation$EKSTRAVARIABLE, ","))
    OutVar <- c(OutVar, hjelpeVar)
  }
  
  KHtabs <- getOption("khfunctions.khtabs")
  tabs <- c(KHtabs, etabs)
  if (!(is.na(parameters$CUBEinformation$DIMDROPP) | parameters$CUBEinformation$DIMDROPP == "")) {
    dimdropp <- unlist(stringr::str_split(parameters$CUBEinformation$DIMDROPP, ","))
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
  geoprikk_triangles <- get_geonaboprikk_triangles(geoniv = unique(KUBE$GEOniv))
  stataspec <- kube_spec(spec = KUBEinformation, dims = dims, geonaboprikk = geonaboprikk, geoprikktriangel = geoprikk_triangles)

  KUBE <- do_stata_prikk(dt = KUBE, spc = stataspec, batchdate = batchdate, geonaboprikk = geonaboprikk, globs = globs)

  if ("STATAPRIKKpost" %in% names(dumps)) {
    for (format in dumps[["STATAPRIKKpost"]]) {
      DumpTabell(KUBE, paste0(KUBEid, "_STATAPRIKKpost"), globs = globs, format = format)
    }
  }
  
  # RSYNT_POSTPROSESS ---- 
  if (!(is.na(parameters$CUBEinformation$RSYNT_POSTPROSESS) | parameters$CUBEinformation$RSYNT_POSTPROSESS == "")) {
    synt <- gsub("\\\r", "\\\n", parameters$CUBEinformation$RSYNT_POSTPROSESS)
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
  ## Der bydel starter senere enn andre mÃÂ¥ disse radene genereres, da ALLVIS ikke takler manglende rader.
  
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
    LagAlleFriskvikIndikatorerForKube(KUBEid = KUBEid, KUBE = ALLVIS, FGP = parameters$fileinformation[[parameters$files$TELLER]], modus = parameters$CUBEinformation$MODUS, batchdate = batchdate, globs = globs)
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
#' Wrapper around LagKUBE, with default options to save output files
LagKubeDatertCsv <- function(KUBEID, 
                             dumps = list(), 
                             versjonert = TRUE,
                             csvcopy = TRUE,
                             write = TRUE,
                             alarm = FALSE) {
  invisible(LagKUBE(KUBEid = KUBEID, versjonert = versjonert, csvcopy = csvcopy, dumps = dumps, write = write, alarm = alarm))
}

#' LagFlereKuber
#' Wrapper aroung LagKUBE, allowing for more than one KUBE to be made simultaneously
#' @param KUBEid_ALLE
#' @param ... Optional, can set versjonert, csvcopy, write arguments if TRUE not wanted
LagFlereKuber <- function(KUBEid_ALLE, 
                          dumps = list(), 
                          alarm = FALSE,
                          ...) {
  for (KUBEid in KUBEid_ALLE) {
    LagKubeDatertCsv(KUBEid, dumps = dumps, alarm = alarm, ...)
  }
  sink()
}

#' @title check_if_lagkube_available
#' @description
#' Checks if guardfile exists, indicating that the system is already running.
#' If file doesn't exist, or if user overrides and force continue, the file
#' is generated and TRUE is returned indicating that the function may continue. 
#' If the file exists and the user does not override, FALSE is returned indicating 
#' that the system is busy and data processing stops.
#' 
#' An on.exit call must be included in the main function to delete
#' the file when the function finish or crash. This function checks if the file already exists, 
#' and generate the file if not (or overridden by user). 
check_if_lagkube_available <- function(){
  file <- get_lagkube_guardfile_path()
  continue <- TRUE
  if(file.exists(file)){
    force_continue <- utils::menu(choices = c("YES", "NO"),
                                  title = paste0("It appears that another cube is already being processed on this computer. ",
                                                 "To avoid errors due to parallell processing, this is not allowed.\n\n",
                                                 "If you are not running another file, you can continue.\n\n",
                                                 "Continue?"))
    if(force_continue == 2) stop("LagKUBE() stopped due to paralell processing. Wait until the other file is done and start again")
  }
  if(continue) fs::file_create(file)
}

get_lagkube_guardfile_path <- function(){
  file.path(fs::path_home(), getOption("khfunctions.lagkube_guardfile"))
}

lagkube_cleanup <- function(){
  fs::file_delete(get_lagkube_guardfile_path())
  RODBC::odbcCloseAll()
  sink()
}
