#' LagKUBE
#' 
#' The main function of the production line, producing the files going to FHI Statistikk and public health profiles
#'
#' @param KUBEid 
#' @param lagRapport 
#' @param batchdate 
#' @param versjonert 
#' @param bare_TN 
#' @param drop_TN 
#' @param tmpbryt 
#' @param csvcopy 
#' @param globs global parameters, defaults to FinnGlobs()
#' @param echo 
#' @param dumps
#' @param write should results be written to files, default = TRUE. Set to FALSE for testing (only save to global envir)
#' @param ... 
#'
#' @examples
#' LagKUBE("ENEFHIB")
LagKUBE <- function(KUBEid,
                    lagRapport = 0,
                    batchdate = SettKHBatchDate(),
                    versjonert = FALSE,
                    bare_TN = 0,
                    drop_TN = 0,
                    tmpbryt = 0,
                    csvcopy = FALSE,
                    globs = FinnGlobs(),
                    echo = 0, 
                    dumps = list(), 
                    write = TRUE) {
  
  is_kh_debug()
  
  datef <- format(strptime(batchdate, "%Y-%m-%d-%H-%M"), "#%Y-%m-%d#")
  rapport <- list(KUBE = KUBEid, lagRapport = lagRapport)
  
  # Les inn nødvendig informasjon om filene involvert (skilt ut i egen funksjon for lesbarhet)
  Finfo <- SettFilInfoKUBE(KUBEid, batchdate = batchdate, versjonert = versjonert, globs = globs)
  KUBEdscr <- Finfo$KUBEdscr
  TNPdscr <- Finfo$TNPdscr
  filer <- Finfo$filer
  PredFilter <- Finfo$PredFilter
  D_develop_predtype <- Finfo$PredFilter$D_develop_predtype
  STNPdscr <- Finfo$STNPdscr
  FGPs <- Finfo$FGPs
  FilDesL <- Finfo$FilDesL
  KUBEd <- list()
  
  if (KUBEdscr$MODUS == "KH") {
    globs$KubeDir <- globs$KubeDir_KH
    globs$KubeDirNy <- globs$KubeDirNy_KH
    globs$KubeDirDat <- globs$KubeDirDat_KH
    globs$KubeDirQc <- globs$KubeDirQC_KH
  } else {
    globs$KubeDir <- globs$KubeDir_NH
    globs$KubeDirNy <- globs$KubeDirNy_NH
    globs$KubeDirDat <- globs$KubeDirDat_NH
    globs$KubeDirQc <- globs$KubeDirQC_NH
  }
  
  # Lage og eksportere USER/helseprofil/kubespec.csv
  kube_spec(spec = KUBEdscr, dims = NA)
  
  # If write = TRUE, save ACCESS specs
  if(isTRUE(write)){
    cat("Saving ACCESS specs to file:\n")
    utfils <- paste(globs$path, "/", globs$KubeDir, "/SPECS/spec_", KUBEid, "_", batchdate, ".csv", sep = "")
    specs <- GetAccessSpecs(kuber = KUBEdscr, tnp = TNPdscr, stnp = STNPdscr, filgrupper = FGPs, datef = datef, globs = globs)
    data.table::fwrite(specs, file = utfils, sep = ";")
    cat("\n", utfils)
  }
  
  # TRINN 1 LAG TNF
  if (drop_TN == 0) {
    cat("******LAGER TNF\n")
    TNtab <- LagTNtabell(filer, FilDesL, FGPs, TNPdscr, KUBEdscr = KUBEdscr, rapport = rapport, globs = globs)
    TNF <- TNtab$TNF
    
    KUBEd <- TNtab$KUBEd
    if (TNPdscr$NEVNERKOL != "-") {
      TNF <- LeggTilNyeVerdiKolonner(TNF, "RATE={TELLER/NEVNER}")
    }
    if (echo == 1) {
      cat("TNF:\n")
      print(TNF)
    }
    cat("------FERDIG TNF\n")
  }
  
  if (bare_TN == 1) {
    RESULTAT <- list(KUBE = TNF, TNPdscr = TNPdscr)
  }
  # Prediker referanseverdi om dette er etterspurt
  if (KUBEdscr$REFVERDI_VP == "P" && bare_TN == 0) {
    print("*****PREDIKER!!!")
    # Må først finne design for (den syntetiske) koblinga ST, SN og PN
    
    # Bruk PREDfilter på ST og evt SN
    # Finn så FellesTab for disse
    # STdFilt<-FinnRedesignForFilter(FilDesL[[filer["ST"]]],PredFilter$Design,globs=globs)$Dekk
    # STdFilt<-STdFilt[,setdiff(names(STdFilt),PredFilter$Pkols),with=FALSE]
    # STFd<-FinnDesign(STdFilt,FGP=FGPs[[filer["ST"]]],globs=globs)
    cat("***Skal finne felles design ST,SN,PN\n")
    STFd <- FinnDesignEtterFiltrering(FilDesL[[filer["ST"]]], PredFilter$Design, PredFilter$Pkols, FGP = FGPs[[filer["ST"]]], globs = globs)
    if (!is.na(filer["SN"])) {
      SNFd <- FinnDesignEtterFiltrering(FilDesL[[filer["SN"]]], PredFilter$Design, PredFilter$Pkols, FGP = FGPs[[filer["SN"]]], globs = globs)
      STNFd <- FinnFellesTab(STFd, SNFd, globs = globs)$FDes
    } else {
      STNFd <- STFd
    }
    # Finn FellesTab også med PN, denne gjelder som Til-design for PN
    STNPFd <- FinnFellesTab(STNFd, FilDesL[[filer["PN"]]], globs = globs)$FDes
    # Må filtrere STNPFd med Predfilter igjen for å finne ny STNFd som gir til-design for ST og SN
    # (merge med PN kan ha endra fra STNFd-versjonen over
    STNFd <- FinnDesignEtterFiltrering(STNPFd, PredFilter$Design, FGP = FGPs[[filer["ST"]]], globs = globs)
    cat("---Satt felles design ST,SN,PN\n")
    
    STN <- data.table::copy(LagTNtabell(filer, FilDesL, FGPs, STNPdscr, TT = "ST", NN = "SN", Design = STNFd, rapport = rapport, globs = globs)$TNF)
    
    # Fjern PredFilter$Pkols
    STN[, (PredFilter$Pkols) := NULL]
    
    # SETT RATE HER er mest effektivt!
    STN[!(NEVNER == 0 & NEVNER.f == 0), c("PREDRATE", "PREDRATE.f", "PREDRATE.a") := list(TELLER / NEVNER, pmax(TELLER.f, NEVNER.f), pmax(TELLER.a, NEVNER.a))]
    STN[NEVNER == 0 & NEVNER.f == 0, c("PREDRATE", "PREDRATE.f", "PREDRATE.a") := list(0, pmax(TELLER.f, 2), pmax(TELLER.a, NEVNER.a))]
    # MÅ JUKSE DET TIL LITT MED NEVNER 0. Bruken her er jo slik at dette er tomme celler, og ikke minst vil raten nesten garantert skulle ganges med et PREDTELLER=0
    # Tillater TELLER<=2 for å unngå evt numeriske problemer. Virker helt uskyldig gitt bruken
    STN[TELLER <= 2 & TELLER.f == 0 & NEVNER == 0 & NEVNER.f == 0, c("PREDRATE", "PREDRATE.f", "PREDRATE.a") := list(0, 0, pmax(TELLER.a, NEVNER.a))]
    
    ukurante <- nrow(subset(STN, is.na(TELLER) | is.na(NEVNER)))
    if (ukurante > 0) {
      cat("!!! NAs i ST og/eller SN (", ukurante, "), dette vil gi problemer i PREDTELLER\n")
    }
    soppelkols <- setdiff(names(STN), c(FinnTabKols(names(STN)), paste("PREDRATE", c("", ".f", ".a"), sep = "")))
    if (length(soppelkols) > 0) {
      STN[, (soppelkols) := NULL]
    }
    
    cat("------FERDIG med STN\n")
    cat("***Lager PN\n")
    PNrd <- FinnRedesign(FilDesL[[filer["PN"]]], STNPFd, globs = globs)
    PN <- OmkodFil(FinnFilT(filer["PN"]), PNrd, globs = globs)
    
    if (!(is.na(TNPdscr$PREDNEVNERFIL) | TNPdscr$PREDNEVNERFIL == "")) {
      PredNevnerKol <- gsub("^(.*):(.*)", "\\2", TNPdscr$PREDNEVNERFIL)
    } else {
      PredNevnerKol <- TNPdscr$NEVNERKOL
    }
    PNnames <- gsub(paste("^", PredNevnerKol, "(\\.f|\\.a|)$", sep = ""), "PREDNEVNER\\1", names(PN))
    data.table::setnames(PN, names(PN), PNnames)
    soppelkols <- setdiff(names(PN), c(FinnTabKols(names(PN)), paste("PREDNEVNER", c("", ".f", ".a"), sep = "")))
    if (length(soppelkols) > 0) {
      PN[, (soppelkols) := NULL]
    }
    cat("---Ferdig PN\n")
    # return(list(STN=STN,PN=PN))
    cat("******Lager STNP, dette kan bli en stor tabell før kollaps til PT\n")
    commonkols <- intersect(FinnTabKols(names(PN)), FinnTabKols(names(STN)))
    data.table::setkeyv(STN, commonkols)
    data.table::setkeyv(PN, commonkols)
    mismatch <- nrow(STN[!PN, allow.cartesian = TRUE])
    if (mismatch > 0) {
      cat("!!!!!ADVARSEL: Mismatch i STN[PN,] på ", mismatch, "kolonner\n")
    }
    
    # Finn omkoding til KUBEd, dvs design for TNF
    # NB: Her må det aggregeres opp for standardisering
    PNd <- FinnDesign(PN, FGP = FGPs[[filer["PN"]]], globs = globs)
    # Burde kanskje bruke STNFd i stedet, men da må den få på PredFilterDimensjonene. Må uansett sende til FinDesigmm
    RD <- FinnRedesign(PNd, list(Part = KUBEd$MAIN), SkalAggregeresOpp = globs$DefDesign$AggVedStand, globs = globs)
    cat("Før merge: dim(PN)", dim(PN), " og dim(STN)", dim(STN))
    STNP <- STN[PN, allow.cartesian = TRUE]
    STNP[, c("PREDTELLER", "PREDTELLER.f", "PREDTELLER.a") := list(PREDRATE * PREDNEVNER, pmax(PREDRATE.f, PREDNEVNER.f), pmax(PREDRATE.a, PREDNEVNER.a))]
    # Kast overflødige kolonner
    kastkols <- setdiff(names(STNP), c(FinnTabKols(names(STNP)), "PREDTELLER", "PREDTELLER.f", "PREDTELLER.a"))
    STNP[, (kastkols) := NULL]
    cat(" og etter mergre dim(STNP)", dim(STNP), "\n")
    PT <- OmkodFil(STNP, RD, globs = globs, echo = 1)
    cat("-----PREDTELLER (PT) ferdig med dim(PT)", dim(PT), "\n")
    cat("***Merger med TNF\n")
    orgdim <- dim(TNF)
    
    # Merge PT med TNF til ferdig kube
    tabkols <- FinnTabKols(names(TNF))
    data.table::setkeyv(TNF, tabkols)
    data.table::setkeyv(PT, tabkols)
    KUBE <- PT[TNF]
    KUBE <- SettMergeNAs(KUBE, FGPs[[filer[TT]]]$vals)
    cat("Før merge KUBE<-PT[TNF] er dim(TNF)", orgdim, " og etter merge dim(KUBE)", dim(KUBE), "\n")
    cat("------FERDIG MED PREDIKSJON\n")
    fullresult <- list(KUBE = KUBE, STN = STN, TNF = TNF, PN = PN, PT = PT, RD = RD, STNPFd = STNPFd)
  } else {
    KUBE <- data.table::copy(TNF)
    fullresult <- list(TNF = TNF)
  }
  # Fjern temporære BUFFER filer
  
  rydd <- setdiff(Finfo$tmpBUFFER, c("BEF_GKa", "BEF_GKu"))
  .GlobalEnv$BUFFER[rydd] <- NULL
  
  if ("raaKUBE0" %in% names(dumps)) {
    for (format in dumps[["raaKUBE0"]]) {
      DumpTabell(KUBE, paste(KUBEid, "raaKUBE0", sep = "_"), globs = globs, format = format)
    }
  }
  
  if (tmpbryt > 0) {
    raaKUBE0 <- data.table::copy(KUBE)
  }
  if (tmpbryt == 1) {
    return(fullresult)
  }
  if (bare_TN == 0) {
    if (D_develop_predtype == "DIR") {
      # Sett skala for teller (må gjøres før rate brukes i MEISskala)
      if (!(is.na(KUBEdscr$RATESKALA) | KUBEdscr$RATESKALA == "")) {
        KUBE[, RATE := RATE * as.numeric(KUBEdscr$RATESKALA)]
      }
      
      # FINN MEISskala Merk at dette gjelder både ved REFVERDI_VP=P og =V
      
      if (KUBEdscr$REFVERDI_VP == "P") {
        VF <- eval(parse(text = paste("subset(KUBE,", PredFilter$PfiltStr, ")", sep = "")))
        # Evt hvis en eller flere element i PredFilter ikke er med i Design for TNF og må lages
        if (nrow(VF) == 0) {
          cat("************************************\nNOE RART MED PredFilter, IKKE I KUBEDESIGN, MÅ UT PÅ NY OMKODING.\nER DETTE RETT?\n")
          VF <- OmkodFilFraPart(TNF, PredFilter$Design, FGP = FGPs[[filer["T"]]], globs = globs)
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
      
      print("D-develop")
      cat("Meisskala1:\n")
      print(unique(KUBE$MEISskala))
    }
    
    # Finn "snitt" for ma-år.
    # DVs, egentlig lages forløpig bare summer, snitt settes etter prikking under
    # Snitt tolerer missing av type .f=1 ("random"), men bare noen få anonyme .f>1, se KHaggreger
    # Rapporterer variabelspesifikk VAL.n som angir antall år brukt i summen når NA holdt utenom
    
    data.table::setkeyv(KUBE, c("AARl", "AARh"))
    aar <- unique(KUBE[, c("AARl", "AARh"), with = FALSE])
    int_lengde <- as.integer(unique(KUBE[, AARh - AARl + 1]))
    if (length(int_lengde) > 1) {
      KHerr(paste("!!!!!!HAR ULIKE LENGDER PÅ INTERVALLER!!"))
    }
    
    # Må "balansere" NA i teller og nevner slik sumrate og sumnevner balanserer  (Bedre/enklere å gjøre det her enn i KHaggreger)
    # Kunne med god grunn satt SPVFLAGG her og så bare operert med denne som en egenskap for hele linja i det som kommer
    # Men for å ha muligheten for å håndtere de forskjellige varibalene ulikt og i full detalj lar jeg det stå mer generelt
    # Slik at dataflyten støtter en slik endring
    
    tuppel <- intersect(c("TELLER", "NEVNER", "RATE"), names(KUBE))
    tuppel.f <- paste(tuppel, ".f", sep = "")
    fmax <- paste("pmax(", paste(tuppel.f, collapse = ","), ")", sep = "")
    if (length(tuppel) > 0) {
      KUBE[eval(parse(text = fmax)) > 0, (tuppel) := list(NA)]
      KUBE[eval(parse(text = fmax)) > 0, (tuppel.f) := eval(parse(text = fmax))]
      # Om enkeltobservasjoner ikke skal brukes, men samtidig tas ut av alle summeringer
      # kan man ha satt VAL=0,VAL.f=-1
      # Dette vil ikke ødelegge summer der tallet inngår. Tallet selv, eller sumemr av kun slike tall, settes nå til NA
      # Dette brukes f.eks når SVANGERROYK ekskluderer Oslo, Akershus. Dette er skjuling, så VAL.f=3
      KUBE[eval(parse(text = fmax)) == -1, (tuppel) := list(NA)]
      KUBE[eval(parse(text = fmax)) == -1, (tuppel) := list(3)]
    }
    
    ma_satt <- 0
    orgintMult <- 1
    if (KUBEdscr$MOVAV > 1) {
      if (any(aar$AARl != aar$AARh)) {
        KHerr(paste("Kan ikke sette snitt (ma=", ma, ") når det er intervaller i originaldata", sep = ""))
      } else {
        ma <- KUBEdscr$MOVAV
        
        # Finner evt hull i år for hele designet
        AntYMiss <- max(aar$AARl) - min(aar$AARl) + 1 - length(aar$AARl)
        if (AntYMiss > 0) {
          cat("Setter SumOverAar med AntYMiss=", AntYMiss, "\n")
        }
        maKUBE <- FinnSumOverAar(KUBE, per = ma, FyllMiss = TRUE, AntYMiss = AntYMiss, na.rm = TRUE, report_lpsvars = TRUE, globs = globs)
        
        # sett rate på nytt
        if (TNPdscr$NEVNERKOL != "-") {
          maKUBE <- LeggTilNyeVerdiKolonner(maKUBE, "RATE={TELLER/NEVNER}")
          if (D_develop_predtype == "DIR") {
            if (!(is.na(KUBEdscr$RATESKALA) | KUBEdscr$RATESKALA == "")) {
              maKUBE[, RATE := RATE * as.numeric(KUBEdscr$RATESKALA)]
            }
          }
        }
        ma_satt <- 1
        # maKUBE<-maKUBE[,names(maKUBE)[!grepl(".n$",names(maKUBE))],with=FALSE]  #Kast .n kolonner, ferdig med disse
        KUBE <- maKUBE
      }
    } else {
      # Må legge til VAL.n for regning under når orignale periodesummer, evt n=1 når originale snitt
      valkols <- FinnValKols(names(KUBE))
      orgint_n <- int_lengde[1]
      n <- orgint_n
      if (!is.na(FGPs[[filer["T"]]]$ValErAarsSnitt)) {
        n <- 1
        orgintMult <- orgint_n
      }
      lp <- paste("KUBE[,c(\"", paste(valkols, ".n", collapse = "\",\"", sep = ""), "\"):=list(", n, ")]", sep = "")
      KUBE[, eval(parse(text = lp))]
    }
    
    if (FGPs[[filer["T"]]][["B_STARTAAR"]] > 0) {
      valK <- FinnValKols(names(KUBE))
      KUBE[GEOniv == "B" & AARl < FGPs[[filer["T"]]][["B_STARTAAR"]], (valK) := NA]
      KUBE[GEOniv == "B" & AARl < FGPs[[filer["T"]]][["B_STARTAAR"]], (paste(valK, ".f", sep = "")) := 9]
    }
    
    ## Quick fix for special case of merged kommune in 2020 implementing the same principle as B_STARTAAR
    nameFGP <- filer["T"]
    selectedCol <- "DK2020_STARTAAR"
    if (FGPs[[nameFGP]][[selectedCol]] > 0) {
      mergedCounty <- as.character(c(5055, 5056, 5059, 1806, 1875))
      valK <- FinnValKols(names(KUBE))
      KUBE[GEOniv == "K" &
             GEO %chin% mergedCounty &
             AARl < FGPs[[nameFGP]][[selectedCol]], (valK) := NA]
      KUBE[GEOniv == "K" &
             GEO %chin% mergedCounty &
             AARl < FGPs[[nameFGP]][[selectedCol]], (paste0(valK, ".f")) := 9]
    }
    
    if ("maKUBE0" %in% names(dumps)) {
      for (format in dumps[["maKUBE0"]]) {
        DumpTabell(KUBE, paste(KUBEid, "maKUBE0", sep = "_"), globs = globs, format = format)
      }
    }
    
    # Anonymiser og skjul
    
    # Anonymiser, trinn 1 Filtrer snitt som ikke skal brukes pga for mye anonymt
    # Se KHaggreger!
    raaKUBE <- data.table::copy(KUBE)
    
    # Anonymiser, trinn 1   Filtrer snitt som ikke skal brukes pga for mye anonymt fra original
    if (ma_satt == 1) {
      valkols <- FinnValKols(names(KUBE))
      anon_tot_tol <- 0.2
      
      lp <- paste("KUBE[,':='(",
                  paste(valkols, "=ifelse(", valkols, ".n>0 & ", valkols, ".fn3/", valkols, ".n>=", anon_tot_tol, ",NA,", valkols, "),",
                        valkols, ".f=ifelse(", valkols, ".n>0 & ", valkols, ".fn3/", valkols, ".n>=", anon_tot_tol, ",3,", valkols, ".f)",
                        sep = "", collapse = ","
                  ),
                  ")]",
                  sep = ""
      )
      eval(parse(text = lp))
    }
    
    if ("anoKUBE1" %in% names(dumps)) {
      for (format in dumps[["anoKUBE1"]]) {
        DumpTabell(KUBE, paste(KUBEid, "anoKUBE1", sep = "_"), globs = globs, format = format)
      }
    }
    
    # Anonymiser, trinn 2 Ekte anonymisering basert på liten teller, liten nevner og liten N-T
    if (!(is.na(KUBEdscr$PRIKK_T) | KUBEdscr$PRIKK_T == "")) {
      # T<=PRIKK_T
      cat("T-PRIKKER", nrow(subset(KUBE, TELLER <= KUBEdscr$PRIKK_T)), "rader\n")
      KUBE[TELLER <= KUBEdscr$PRIKK_T & TELLER.f >= 0, c("TELLER", "TELLER.f", "RATE", "RATE.f") := list(NA, 3, NA, 3)]
      # N-T<=PRIKK_T
      cat("N-T-PRIKKER", nrow(subset(KUBE, NEVNER - TELLER <= KUBEdscr$PRIKK_T)), "rader\n")
      KUBE[NEVNER - TELLER <= KUBEdscr$PRIKK_T & TELLER.f >= 0 & NEVNER.f >= 0, c("TELLER", "TELLER.f", "RATE", "RATE.f") := list(NA, 3, NA, 3)]
    }
    
    if (!(is.na(KUBEdscr$PRIKK_N) | KUBEdscr$PRIKK_N == "")) {
      # N<PRIKK_N
      cat("N-PRIKKER", nrow(subset(KUBE, NEVNER <= KUBEdscr$PRIKK_N)), "rader\n")
      KUBE[NEVNER <= KUBEdscr$PRIKK_N & NEVNER.f >= 0, c("TELLER", "TELLER.f", "RATE", "RATE.f") := list(NA, 3, NA, 3)]
    }
    
    if ("anoKUBE2" %in% names(dumps)) {
      for (format in dumps[["anoKUBE2"]]) {
        DumpTabell(KUBE, paste(KUBEid, "anoKUBE2", sep = "_"), globs = globs, format = format)
      }
    }
    # Anonymiser trinn 3. Anonymiser naboer
    if (!(is.na(KUBEdscr$OVERKAT_ANO) | KUBEdscr$OVERKAT_ANO == "")) {
      # DEVELOP: BRuk .f=4 her slik at ikke slår ut i HULL under
      KUBE <- AnonymiserNaboer(KUBE, KUBEdscr$OVERKAT_ANO, FGP = FGPs[[filer[["T"]]]], D_develop_predtype, globs = globs)
    }
    if ("anoKUBE3" %in% names(dumps)) {
      for (format in dumps[["anoKUBE3"]]) {
        DumpTabell(KUBE, paste(KUBEid, "anoKUBE3", sep = "_"), globs = globs, format = format)
      }
    }
    
    raaKUBE2 <- data.table::copy(KUBE)
    # Anonymiser trinn 4. Skjule svake og skjeve tidsserrier
    SvakAndelAvSerieGrense <- 0.5
    HullAndelAvSerieGrense <- 0.2
    
    if (!(is.na(KUBEdscr$STATTOL_T) | KUBEdscr$STATTOL_T == "")) {
      tabkols <- setdiff(intersect(names(KUBE), globs$DefDesign$DesignKolsFA), c(globs$DefDesign$DelKols[["Y"]]))
      KUBE[TELLER.f < 9, AntAar := .N, by = tabkols]
      KUBE[TELLER.f < 9, SVAK := sum(is.na(TELLER) | TELLER <= KUBEdscr$STATTOL_T), by = tabkols]
      KUBE[TELLER.f < 9, HULL := sum(TELLER.f == 3), by = tabkols]
      KUBE[TELLER.f < 9, SKJUL := ifelse(SVAK / AntAar > SvakAndelAvSerieGrense | HULL / AntAar > HullAndelAvSerieGrense, 1, 0)]
      
      
      cat("Skjuler", nrow(subset(KUBE, SKJUL == 1)), "rader\n")
      KUBE[SKJUL == 1, c("TELLER", "TELLER.f") := list(NA, 3)]
      KUBE[SKJUL == 1, c("RATE", "RATE.f") := list(NA, 3)]
      KUBE[, c("SVAK", "HULL", "SKJUL", "AntAar") := NULL]
    }
    raaKUBE3 <- data.table::copy(KUBE)
    if ("anoKUBE4" %in% names(dumps)) {
      for (format in dumps[["anoKUBE4"]]) {
        DumpTabell(KUBE, paste(KUBEid, "anoKUBE4", sep = "_"), globs = globs, format = format)
      }
    }
    
    # LAYOUT
    
    if ("KUBE_SLUTTREDIGERpre" %in% names(dumps)) {
      for (format in dumps[["KUBE_SLUTTREDIGERpre"]]) {
        DumpTabell(KUBE, paste(KUBEid, "KUBE_SLUTTREDIGERpre", sep = "_"), globs = globs, format = format)
      }
    }
    
    # EVT SPESIALBEHANDLING
    
    if (!(is.na(KUBEdscr$SLUTTREDIGER) | KUBEdscr$SLUTTREDIGER == "")) {
      synt <- gsub("\\\r", "\\\n", KUBEdscr$SLUTTREDIGER)
      error <- ""
      ok <- 1
      if (grepl("<STATA>", synt)) {
        synt <- gsub("<STATA>[ \n]*(.*)", "\\1", synt)
        RES <- KjorStataSkript(KUBE, synt, tableTYP = "DT", batchdate = batchdate, globs = globs)
        if (RES$feil != "") {
          stop("Noe gikk galt i kjøring av STATA \n", RES$feil)
          ok <- 0
        } else {
          KUBE <- RES$TABLE
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
    
    if ("KUBE_SLUTTREDIGERpost" %in% names(dumps)) {
      for (format in dumps[["KUBE_SLUTTREDIGERpost"]]) {
        DumpTabell(KUBE, paste(KUBEid, "KUBE_SLUTTREDIGERpost", sep = "_"), globs = globs, format = format)
      }
    }
    
    # mapvalues(KUBE$SPVFLAGG,c(0,1,2,3),c(0,2,1,3),warn_missing = FALSE)     #BRUKER 1='.",2='.." i NESSTAR
    
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
      if (!(is.na(KUBEdscr$RATESKALA) | KUBEdscr$RATESKALA == "")) {
        KUBE[, RATE := RATE * as.numeric(KUBEdscr$RATESKALA)]
      }
    }
    
    # Legg til manglende kolonner for homogen behandling under
    missKol <- setdiff(unlist(lapply(c("TELLER", "NEVNER", "RATE", "PREDTELLER"), function(x) {
      paste(x, c("", ".f", ".a", ".n"), sep = "")
    })), names(KUBE))
    if (length(missKol) > 0) {
      KUBE[, (missKol) := NA]
    }
    
    # Behold sum, disse sendes til Friskvik
    KUBE[, sumTELLER := orgintMult * TELLER]
    KUBE[, sumNEVNER := orgintMult * NEVNER]
    KUBE[, sumPREDTELLER := orgintMult * PREDTELLER]
    
    # Ta snitt for alt annet enn RATE (der forholdstallet gjør snitt uønsket)
    # VAL:=VAL/VAL.n
    valkols <- setdiff(FinnValKols(names(KUBE)), c("RATE", "SMR"))
    if (length(valkols) > 0) {
      lp <- paste("KUBE[,c(\"", paste(valkols, collapse = "\",\""), "\"):=list(",
                  paste(valkols, "=", valkols, "/", valkols, ".n",
                        sep = "", collapse = ","
                  ),
                  ")]",
                  sep = ""
      )
      KUBE[, eval(parse(text = lp))]
    }
    
    if (!(is.na(TNPdscr$NYEKOL_RAD_postMA) | TNPdscr$NYEKOL_RAD_postMA == "")) {
      KUBE <- LeggTilNyeVerdiKolonner(KUBE, TNPdscr$NYEKOL_RAD_postMA, slettInf = TRUE, postMA = TRUE)
    }
    
    if (grepl("\\S", KUBEdscr$MTKOL)) {
      maltall <- KUBEdscr$MTKOL
      KUBE[, eval(parse(text = paste("MALTALL:=", KUBEdscr$MTKOL, sep = "")))]
    } else if (TNPdscr$NEVNERKOL == "-") {
      maltall <- "TELLER"
      KUBE[, MALTALL := TELLER]
    } else {
      maltall <- "RATE"
      KUBE[, MALTALL := RATE]
    }
    # maltallt<-intersect(paste(maltall,""))
    
    if (D_develop_predtype == "DIR") {
      print("D-develop")
      cat("Meisskala3:\n")
      print(unique(KUBE$MEISskala))
      print(KUBE)
      
      # SETT SMR og MEIS
      
      if (KUBEdscr$REFVERDI_VP == "P") {
        KUBE[, SMR := sumTELLER / sumPREDTELLER * 100]
        KUBE[, MEIS := (sumTELLER / sumPREDTELLER) * MEISskala]
      } else if (KUBEdscr$REFVERDI_VP == "V") {
        KUBE[, SMR := NA_real_]
        KUBE[, MEIS := MALTALL]
      } else {
        KUBE[, SMR := NA_real_]
        KUBE[, MEIS := MALTALL]
      }
    } else {
      # SETT SMRtmp. For å lage NORMSMR under må denne settes før NORM. Derfor kan jeg ikke sette SMR=MALTALL/NORM nå.
      # Men NORMSMR er selvsagt alltid 100 for REFVERDI_P="V"
      if (KUBEdscr$REFVERDI_VP == "P") {
        KUBE[, SMRtmp := sumTELLER / sumPREDTELLER * 100]
      } else if (KUBEdscr$REFVERDI_VP == "V") {
        KUBE[, SMRtmp := 100]
      } else {
        KUBE[, SMRtmp := NA]
      }
    }
    
    # FINN "LANDSNORMAL". Merk at dette gjelder både ved REFVERDI_VP=P og =V
    
    if (D_develop_predtype == "DIR") {
      # Midlertidig dirty løsning
      # KUBER:REFVERDI bør omdøpes til KUBER:PREDFILTER og det er denne som brukes i SettPredFilter
      # Det bør så lages en ny kolonne KUBER:REFGEOn som har GEOniv for referanseverdi. Denne brukes primært for å sette SMR i modus=V
      RefGEOn <- "L"
      RefGEOnFilt <- paste("GEOniv=='", RefGEOn, "'", sep = "")
      VF <- eval(parse(text = paste("subset(KUBE,", RefGEOnFilt, ")", sep = "")))
    } else {
      VF <- eval(parse(text = paste("subset(KUBE,", PredFilter$PfiltStr, ")", sep = "")))
    }
    
    # Evt hvis en eller flere element i PredFilter ikke er med i Design for TNF og må lages
    if (nrow(VF) == 0) {
      cat("************************************\nNOE RART MED LANDSNORM, IKKE I KUBEDESIGN, MÅ UT PÅ NY OMKODING.\nER DETTE RETT?\n")
      VF <- OmkodFilFraPart(TNF, PredFilter$Design, FGP = FGPs[[filer["T"]]], globs = globs)
    }
    
    if (D_develop_predtype == "IND") {
      VFtabkols <- setdiff(intersect(names(VF), globs$DefDesign$DesignKolsFA), PredFilter$Pkols)
      if (maltall %in% c("TELLER", "RATE")) {
        data.table::setnames(VF, c(paste(maltall, c("", ".f", ".a", ".n"), sep = ""), "SMRtmp"), c(paste("NORM", c("", ".f", ".a", ".n"), sep = ""), "NORMSMR"))
        VF <- VF[, c(VFtabkols, paste("NORM", c("", ".f", ".a", ".n"), sep = ""), "NORMSMR"), with = FALSE]
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
      # SMR>100 kan oppstå dersom det f.eks. er noen med ukjent alder/kjønn.
      # Ratene for ukjent alder/kjønn vil ikke matche nevner fra BEF, derfor vil det predikeres for får døde relativt til observert
      if (KUBEdscr$REFVERDI_VP == "P") {
        KUBE[, SMR := sumTELLER / sumPREDTELLER * 100]
      } else if (KUBEdscr$REFVERDI_VP == "V") {
        KUBE[, SMR := MALTALL / NORM * 100]
      } else {
        KUBE[, SMR := NA]
      }
      
      KUBE[, SMR := 100 * (SMR / NORMSMR)]
      
      KUBE[, MEIS := SMR * NORM / 100]
    } else {
      KUBE[, lopendeFORHOLDSVERDI := MEIS / lopendeMEISref * 100]
      
      # D-develop
      # Dirty tricks i denne midlertidige løsninga, bruker gamle utnavn
      KUBE[, SMR := lopendeFORHOLDSVERDI]
      KUBE[, NORM := lopendeMEISref]
    }
    
    # Bytt til eksterne TAB-navn for ekstradimensjoner
    FGP <- FGPs[[filer[["T"]]]]
    etabs <- character(0)
    for (etab in names(KUBE)[grepl("^TAB\\d+$", names(KUBE))]) {
      if (grepl("\\S", FGP[[etab]])) {
        data.table::setnames(KUBE, etab, FGP[[etab]])
        etabs <- c(etabs, FGP[[etab]])
      }
    }
    
    # SETT UTKOLONNER FOR ALLVISKUBE
    if (!(is.na(KUBEdscr$NESSTARTUPPEL) | KUBEdscr$NESSTARTUPPEL == "")) {
      NstarTup <- unlist(stringr::str_split(KUBEdscr$NESSTARTUPPEL, ","))
    } else if (KUBEdscr$REFVERDI_VP == "P") {
      NstarTup <- c("T", "RATE", "SMR", "MEIS")
    } else {
      NstarTup <- character(0)
    }
    OutVar <- globs$NesstarOutputDef[NstarTup]
    
    if (!(is.na(KUBEdscr$EKSTRAVARIABLE) | KUBEdscr$EKSTRAVARIABLE == "")) {
      hjelpeVar <- unlist(stringr::str_split(KUBEdscr$EKSTRAVARIABLE, ","))
      OutVar <- c(OutVar, hjelpeVar)
    }
    
    KHtabs <- c("GEO", "AAR", "KJONN", "ALDER", "UTDANN", "INNVKAT", "LANDBAK")
    tabs <- c(KHtabs, etabs)
    if (!(is.na(KUBEdscr$DIMDROPP) | KUBEdscr$DIMDROPP == "")) {
      dimdropp <- unlist(stringr::str_split(KUBEdscr$DIMDROPP, ","))
      tabs <- setdiff(tabs, dimdropp)
    }
    
    KUBE[, AAR := paste(AARl, "_", AARh, sep = "")]
    if (all(c("ALDERl", "ALDERh") %in% names(KUBE))) {
      KUBE[, ALDER := paste(ALDERl, "_", ALDERh, sep = "")]
    } else {
      tabs <- setdiff(tabs, "ALDER")
    }
    if (!"KJONN" %in% names(KUBE)) {
      tabs <- setdiff(tabs, "KJONN")
    }
    
    # EVT SPESIALBEHANDLING
    
    if ("STATAPRIKKpre" %in% names(dumps)) {
      for (format in dumps[["STATAPRIKKpre"]]) {
        DumpTabell(KUBE, paste(KUBEid, "STATAPRIKKpre", sep = "_"), globs = globs, format = format)
      }
    }
    
    # Lage stataspec og overskrive helseprofil/kubespec.csv inkludert DIMS 
    dims <- find_dims(dt = KUBE, spec = FGPs)
    stataspec <- kube_spec(spec = KUBEdscr, dims = dims)
    
    KUBE <- do_stata_prikk(dt = KUBE, spc = stataspec, batchdate = batchdate, globs = globs)
    
    if ("STATAPRIKKpost" %in% names(dumps)) {
      for (format in dumps[["STATAPRIKKpost"]]) {
        DumpTabell(KUBE, paste(KUBEid, "STATAPRIKKpost", sep = "_"), globs = globs, format = format)
      }
    }
    
    # Start RSYNT_postprosess
    if (!(is.na(KUBEdscr$RSYNT_POSTPROSESS) | KUBEdscr$RSYNT_POSTPROSESS == "")) {
      synt <- gsub("\\\r", "\\\n", KUBEdscr$RSYNT_POSTPROSESS)
      error <- ""
      ok <- 1
      if (grepl("<STATA>", synt)) {
        synt <- gsub("<STATA>[ \n]*(.*)", "\\1", synt)
        RES <- KjorStataSkript(KUBE, synt, tableTYP = "DT", batchdate = batchdate, globs = globs)
        if (RES$feil != "") {
          ok <- 0
          stop("Noe gikk galt i kjøring av STATA", RES$feil, sep = "\n")
        } else {
          KUBE <- RES$TABLE
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
    
    if ("RSYNT_POSTPROSESSpost" %in% names(dumps)) {
      for (format in dumps[["RSYNT_POSTPROSESSpost"]]) {
        DumpTabell(KUBE, paste(KUBEid, "RSYNT_POSTPROSESSpost", sep = "_"), globs = globs, format = format)
      }
    }
    
    # Filtrer bort GEO, ALDER og KJONN som ikke skal rapporteres
    KUBE <- KUBE[GEO %in% globs$UtGeoKoder]
    
    if ("ALDER" %in% names(KUBE)) {
      KUBE <- KUBE[!ALDER %in% c("999_999", "888_888"), ]
    }
    if ("KJONN" %in% names(KUBE)) {
      KUBE <- KUBE[!KJONN %in% c(8, 9), ]
    }
    
    # LAYOUT
    utkols <- c(tabs, OutVar)
    ALLVIS <- data.table::copy(KUBE)
    
    # SKJUL HELE TUPPELET
    # FLAGG PER VARIABEL KAN/BØR VURDERES!
    # Litt tricky å finne riktig ".f"-kolloner. Må ikke ta med mBEFc f.eks fra BEF fila dersom denne er irrelevant
    fvars <- intersect(names(ALLVIS), paste(union(globs$NesstarOutputDef, OutVar), ".f", sep = ""))
    # fvars<-intersect(names(NESSTAR),c(OrgKubeKolNames[grepl(".f$",OrgKubeKolNames)],"NORM.f","SMR.f"))
    ALLVIS[, SPVFLAGG := 0]
    if (length(fvars) > 0) {
      # Dette er unødvendig krongelete. Men dersom f.eks RATE.f=2 pga TELLER.f=1, ønskes SPVFLAGG=1
      ALLVIS[, tSPV1 := eval(parse(text = paste("pmax(", paste(lapply(fvars, function(x) {
        paste(x, "*(", x, "!=2)", sep = "")
      }), collapse = ","), ",na.rm = TRUE)", sep = "")))]
      ALLVIS[, tSPV2 := eval(parse(text = paste("pmax(", paste(fvars, collapse = ","), ",na.rm = TRUE)", sep = "")))]
      ALLVIS[, SPVFLAGG := ifelse(tSPV1 == 0, tSPV2, tSPV1)]
      ALLVIS[, c("tSPV1", "tSPV2") := NULL]
      ALLVIS[SPVFLAGG > 0, eval(parse(text = paste("c(\"", paste(OutVar, collapse = "\",\""), "\"):=list(NA)", sep = "")))]
    }
    ALLVIS[is.na(SPVFLAGG), SPVFLAGG := 0]
    ALLVIS[, SPVFLAGG := plyr::mapvalues(SPVFLAGG, c(-1, 9, 4), c(3, 1, 3), warn_missing = FALSE)]
    
    # Filtrer bort GEO som ikke skal rapporteres
    KUBE <- KUBE[GEO %in% globs$UtGeoKoder]
    ALLVIS <- ALLVIS[GEO %in% globs$UtGeoKoder]
    
    # If write = TRUE, Create FRISKVIK indicators, based on the censored ALLVIS kube
    if(isTRUE(write)){
      LagAlleFriskvikIndikatorerForKube(KUBEid = KUBEid, KUBE = ALLVIS, aargang = globs$KHaargang, modus = KUBEdscr$MODUS, FGP = FGPs[[filer["T"]]], versjonert = versjonert, batchdate = batchdate, globs = globs)
    }
    
    # Filter ALLVIS KUBE
    ALLVIS <- ALLVIS[, c(..utkols, "SPVFLAGG")]
    
    # Create QC KUBE based on the censored ALLVIS kube
    # Contain all the full ALLVIS kuve + uncensored TELLER/NEVNER/sumTELLER/sumNEVNER/RATE.n
    QC <- LagQCKube(allvis = ALLVIS,
                    allvistabs = tabs, 
                    kube = KUBE,
                    globs = globs)
    
    if (tmpbryt == 2) {
      print("TMPBRYT=2")
      return(list(raaKUBE0 = raaKUBE0, raaKUBE = raaKUBE, raaKUBE2 = raaKUBE2, raaKUBE3 = raaKUBE3, KUBE = KUBE, TNF = TNF, ALLVIS = ALLVIS))
    }
    
    cat("---------------------KUBE FERDIG\n\n")
    
    # Save RESULTAT to global env
    RESULTAT <<- list(KUBE = KUBE, ALLVIS = ALLVIS, QC = QC)
  }
  
  # If write = TRUE, save output files
  if(isTRUE(write)){
    cat("SAVING OUTPUT FILES:\n")
    ## Write .rds file to NYESTE/R
    utfiln <- paste(globs$path, "/", globs$KubeDirNy, "/", KUBEid, ".rds", sep = "")
    saveRDS(KUBE, file = utfiln)
    cat("\n", utfiln)
    
    ## If versjonert, Write .rds file to DATERT/R (copy from NYESTE)
    if (versjonert == TRUE) {
      utfilv <- paste(globs$path, "/", globs$KubeDirDat, "/R/", KUBEid, "_", batchdate, ".rds", sep = "")
      file.copy(utfiln, utfilv)
      cat("\n", utfilv)
    }
    
    ## If csvcopy, Write .csv file to DATERT/csv, and QC kube to QC
    if (csvcopy == TRUE) {
      utfild <- paste(globs$path, "/", globs$KubeDirDat, "/csv/", KUBEid, "_", batchdate, ".csv", sep = "")
      data.table::fwrite(ALLVIS, file = utfild, sep = ";")
      cat("\n", utfild)
      
      utfilq <- paste(globs$path, "/", globs$KubeDirQc, "/QC_", KUBEid, "_", batchdate, ".csv", sep = "")
      data.table::fwrite(QC, file = utfilq, sep = ";")
      cat("\n", utfilq)
    }
  }
  
  cat("-------------------------KUBE", KUBEid, "FERDIG--------------------------------------\n")
  cat("Se output med RESULTAT$KUBE (full), RESULTAT$ALLVIS (utfil) eller RESULTAT$QC (kvalkont)")
  return(RESULTAT)
}

#' LagFlereKuber
#' 
#' Wrapper aroung LagKUBE, allowing for more than one KUBE to be made simultaneously
#'
LagFlereKuber <- function(KUBEidA, versjonert = FALSE, csvcopy = FALSE, globs = FinnGlobs(), dumps = list(), ...) {
  is_kh_debug()
  
  batchdate <- SettKHBatchDate()
  loggfile <- paste(globs$path, "/", globs$KubeDir, "/LOGG/", batchdate, ".txt", sep = "")
  sink(loggfile, split = TRUE)
  cat("BATCH:", batchdate, "\n")
  for (KUBEid in KUBEidA) {
    KK <- LagKUBE(KUBEid, batchdate = batchdate, versjonert = versjonert, csvcopy = csvcopy, globs = globs, dumps = dumps, ...)
  }
  sink()
}

#' LagKubeDatertCsv
#' 
#' Wrapper around LagKUBE, with default options to save output files
#'
LagKubeDatertCsv <- function(KUBEID, dumps = list(), ...) {
  is_kh_debug()
  invisible(LagFlereKuber(KUBEID, versjonert = TRUE, csvcopy = TRUE, dumps = dumps, ...))
}