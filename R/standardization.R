#' @title add_predteller_to_tnf
#' @description
#' Adds predicted teller for age-standardization
#' 
#' PREDRATE
#' Maa JUKSE DET TIL LITT MED NEVNER 0. Bruken her er jo slik at dette er tomme celler, 
#' og ikke minst vil raten nesten garantert skulle ganges med et PREDTELLER=0
#' Tillater TELLER<=2 for aa unngaa evt numeriske problemer. Virker helt uskyldig gitt bruken
#'
#' @param TNF merged teller-nevner file
#' @param parameters cube parameters
#' @param globs global parameters
add_predteller <- function(dt, parameters, globs){
  if (parameters$CUBEinformation$REFVERDI_VP != "P") return(dt)
  designlist <- find_common_standard_teller_nevner_prednevner_design(parameters = parameters, globs = globs)
  
  predrate <- estimate_predrate(design = designlist$STNdesign, parameters = parameters, globs = globs)
  prednevner <- estimate_prednevner(design = designlist$STNPdesign, parameters = parameters, globs = globs)
  predteller <- estimate_predteller(predrate = predrate, prednevner = prednevner, parameters = parameters, globs = globs)
  
  cat("* Merger med KUBE\n")
  orgdim <- dim(dt)
  
  tabcols <- get_dimension_columns(names(dt))
  dt <- collapse::join(dt, predteller, how = "l", on = tabcols, overid = 2, verbose = 0)
  dt <- set_implicit_null_after_merge(dt, parameters$fileinformation[[parameters$files[["TELLER"]]]]$vals)
  setkeyv(dt, tabcols)
  cat("Foer merge PT[TNF] er dim", orgdim, " og etter merge dim", dim(dt), "\n")
  cat("------FERDIG MED PREDIKSJON\n")
  return(dt)
}

#' @title find_common_standard_teller_nevner_prednevner_design
#' @description
#' Finds the design of the standard
find_common_standard_teller_nevner_prednevner_design <- function(parameters, globs){
  cat("* Finner felles design for STANDARDTELLER, STANDARDNEVNER og PREDNEVNER\n")
  
  STdesign <- find_design_after_filter(file = "STANDARDTELLER", parameterlist = parameters, globs = globs)
  if ("STANDARDNEVNER" %in% names(parameters$files)) {
    SNdesign <- find_design_after_filter(file = "STANDARDNEVNER", parameterlist = parameters, globs = globs)
    STNdesign <- FinnFellesTab(DF1 = STdesign, DF2 = SNdesign, globs = globs)$FDes
  } else {
    STNdesign <- STdesign
  }
  PNdesign <- parameters$filedesign[[parameters$files$PREDNEVNER]]
  STNPdesign <- FinnFellesTab(DF1 = STNdesign, DF2 = PNdesign, globs = globs)$FDes
  
  # Maa filtrere standardtellerdesign med parameters$PredFilter igjen for aa finne fellesdesign som gir til-design for ST og SN
  STNdesign <- find_design_after_filter(file = "STANDARDTELLER", parameterlist = parameters, originaldesign = STNPdesign, outpredfilter = FALSE, globs = globs)
  
  return(list(STNdesign = STNdesign, STNPdesign = STNPdesign))
}

estimate_predrate <- function(design, parameters, globs){
  cat("* Merger standardteller og standardnevner og estimerer PREDRATE...\n")
  STNF <- merge_teller_nevner(parameterlist = parameters, standardfiles = TRUE, design = design, globs = globs)$TNF
  STNF[, (parameters$PredFilter$Pkols) := NULL]
  STNF[NEVNER != 0 & NEVNER.f == 0, let(PREDRATE = TELLER/NEVNER, PREDRATE.f = pmax(TELLER.f, NEVNER.f))]
  STNF[NEVNER == 0 & NEVNER.f == 0, let(PREDRATE = 0, PREDRATE.f = pmax(TELLER.f, 2))]
  STNF[TELLER <= 2 & TELLER.f == 0 & NEVNER == 0 & NEVNER.f == 0, let(PREDRATE = 0, PREDRATE.f = 0)]
  STNF[, let(PREDRATE.a = pmax(TELLER.a, NEVNER.a))]
  
  ukurante <- STNF[is.na(TELLER) | is.na(NEVNER), .N]
  if (ukurante > 0) cat("!!! Missing verdier i standardteller og/eller standardnevner (", ukurante, "), dette vil gi problemer i PREDTELLER\n")
  STNF <- STNF[, .SD, .SDcols = c(get_dimension_columns(names(STNF)), paste0("PREDRATE", c("", ".f", ".a")))]
  return(STNF)
}

estimate_prednevner <- function(design, parameters, globs){
  cat("* Estimerer PREDNEVNER...\n")
  PNrd <- FinnRedesign(fradesign = parameters$filedesign[[parameters$files$PREDNEVNER]], tildesign = design, globs = globs)
  PN <- OmkodFil(FinnFilT(parameters$files$PREDNEVNER), PNrd, globs = globs)
  
  PNfil <- parameters$TNPinformation$PREDNEVNERFIL
  is_PNfil <- !is.na(PNfil) && PNfil != ""
  PredNevnerKol <- ifelse(is_PNfil, gsub("^(.*):(.*)", "\\2", PNfil), parameters$TNPinformation$NEVNERKOL)
  PNnames <- gsub(paste0("^", PredNevnerKol, "(\\.f|\\.a|)$"), "PREDNEVNER\\1", names(PN))
  data.table::setnames(PN, names(PN), PNnames)
  PN <- PN[, .SD, .SDcols = c(get_dimension_columns(names(PN)), paste0("PREDNEVNER", c("", ".f", ".a")))]
  return(PN)
}

estimate_predteller <- function(predrate, prednevner, parameters, globs){
  cat("* Estimerer PREDTELLER...\n")
  # cat("Lager STNP, dette kan bli en stor tabell foer kollaps til PT\n")
  commondims <- intersect(get_dimension_columns(names(prednevner)), get_dimension_columns(names(predrate)))
  mismatch <- collapse::join(predrate, prednevner, how = "anti", multiple = T, on = commondims, overid = 2, verbose = 0)[, .N]
  if(mismatch > 0) cat("!!!!!ADVARSEL:", mismatch, "strata i predrate finnes ikke i prednevner!!!\n")
  
  cat("Foer merge: dim(prednevner)", dim(prednevner), " og dim(predrate)", dim(predrate))
  STNP <- collapse::join(prednevner, predrate, how = "l", on = commondims, multiple = T, overid = 2, verbose = 0)
  STNP[, let(PREDTELLER = PREDRATE * PREDNEVNER,
             PREDTELLER.f = pmax(PREDRATE.f, PREDNEVNER.f),
             PREDTELLER.a = pmax(PREDRATE.a, PREDNEVNER.a))]
  STNP <- STNP[, .SD, .SDcols = c(get_dimension_columns(names(STNP)), paste0("PREDTELLER", c("", ".f", ".a")))]
  cat(" og etter merge dim(STNP)", dim(STNP), "\n")
  
  # Finn omkoding til KUBEd, dvs design for TNF, NB: Her maa det aggregeres opp for standardisering
  prednevnerfil <- parameters$files$PREDNEVNER
  prednevnerparameters <- parameters$fileinformation[[prednevnerfil]]
  PNdesign <- find_filedesign(file = prednevner, fileparameters = prednevnerparameters, globs = globs)
  redesign <- FinnRedesign(PNdesign, list(Part = parameters$CUBEdesign$MAIN), SkalAggregeresOpp = globs$DefDesign$AggVedStand, globs = globs)
  PT <- OmkodFil(FIL = STNP, RD = redesign, globs = globs)
  cat("-----PREDTELLER ferdig med dim", dim(PT), "\n")
  return(PT)
}

#' @title find_design_after_filter
#'
#' @param file name of file to design
#' @param parameterlist parameters
#' @param originaldesign original design if existing. Default is design for file.
#' @param outpredfilter shuld columns in predfilter be removed, default = TRUE
#' @param globs global parameters, defaults to SettGlobs
find_design_after_filter <- function(file, parameterlist, originaldesign = NULL, outpredfilter = TRUE, globs = SettGlobs()) {
  filename <- parameterlist$files[[file]]
  fileinformation <- parameterlist$fileinformation[[filename]]
  designfilter <- parameterlist$PredFilter$Design
  if(is.null(originaldesign)) originaldesign <- parameterlist$filedesign[[filename]]
  filtercolumns <- character()
  if(outpredfilter) filtercolumns <- parameterlist$PredFilter$Pkols
  
  outdesign <- FinnRedesignForFilter(originaldesign, designfilter, globs = globs)$Dekk
  outcols <- setdiff(names(outdesign), filtercolumns)
  outdesign <- outdesign[, ..outcols]
  return(FinnDesign(outdesign, FGP = fileinformation, globs = globs))
}


#' @title add_meisskala
#' @description
#' 
#' @param dt 
#' @param parameters 
#' @param globs 
#'
#' @returns
#' @export
#'
#' @examples
add_meisskala <- function(dt, parameters, globs){
  # Kan MEISskala = NA_real_ settes her, sånn at den er med videre også når standardmethod != "DIR"
  if(parameters$standardmethod != "DIR") return(dt)

  # Rateskala settes også når standardmethod != DIR, bare senere. Kan det bare settes her uansett?
  is_rateskala <- !is.null(parameters$CUBEinformation$RATESKALA) && !is.na(parameters$CUBEinformation$RATESKALA) && parameters$CUBEinformation$RATESKALA != ""
  if (is_rateskala) dt[, RATE := RATE * as.numeric(parameters$CUBEinformation$RATESKALA)]
    
  if(parameters$CUBEinformation$REFVERDI_VP != "P"){
    dt[, MEISskala := NA_real_]
    return(dt)
  }

  VF <- dt[eval(rlang::parse_expr(parameters$PredFilter$PfiltStr))]
  # Evt hvis en eller flere element i parameters$PredFilter ikke er med i Design for TNF og maa lages
  # Omkoder fra dt, som = TNF + predteller
  # ER DETTE NØDVENDIG, KAN DET SKJE? Kan evt slettes eller trekkes ut til egen funksjon siden den brukes
  # senere i lagKUBE også
  if (nrow(VF) == 0) {
    cat("************************************\nNOE RART MED parameters$PredFilter, IKKE I KUBEDESIGN, MAA UT PAA NY OMKODING.\nER DETTE RETT?\n")
    VF <- OmkodFilFraPart(dt, parameters$PredFilter$Design, FGP = parameters$fileinformation[[parameters$files$TELLER]], globs = globs)
  }
  
  VF[, MEISskala := RATE]
  VFtabkols <- setdiff(intersect(names(VF), globs$DefDesign$DesignKolsFA), parameters$PredFilter$Pkols)
  dt <- collapse::join(dt, VF[, mget(c(VFtabkols, "MEISskala"))], how = "l", on = VFtabkols, overid = 2, verbose = 0)
  return(dt) 
}
