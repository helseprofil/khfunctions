#' @title add_predteller
#' @description
#' Adds predicted teller for age- and gender standardization
#' 
#' PREDRATE
#' Maa JUKSE DET TIL LITT MED NEVNER 0. Bruken her er jo slik at dette er tomme celler, 
#' og ikke minst vil raten nesten garantert skulle ganges med et PREDTELLER=0
#' Tillater TELLER<=2 for aa unngaa evt numeriske problemer. Virker helt uskyldig gitt bruken
#'
#' @param TNF merged teller-nevner file
#' @param parameters cube parameters
#' @keywords internal
#' @noRd
add_predteller <- function(dt, parameters){
  if(parameters$CUBEinformation$REFVERDI_VP != "P") return(dt)
  cat("\n* Skal estimere PREDTELLER for standardisering")
  designlist <- find_common_standard_teller_nevner_prednevner_design(parameters = parameters)
  predrate <- estimate_predrate(design = designlist$STNdesign, parameters = parameters)
  prednevner <- estimate_prednevner(design = designlist$STNPdesign, parameters = parameters)
  predteller <- estimate_predteller(predrate = predrate, prednevner = prednevner, parameters = parameters)
  
  cat("\n* Merger PREDTELLER med KUBE\n** Før merge dim:", dim(dt))
  tabcols <- get_dimension_columns(names(dt))
  dt <- collapse::join(dt, predteller, how = "l", on = tabcols, overid = 2, verbose = 0)
  dt[, PREDTELLER.n := TELLER.n]
  cat("\n** Etter merge dim:", dim(dt))
  
  dt <- set_implicit_null_after_merge(dt, parameters$fileinformation[[parameters$files[["TELLER"]]]]$vals)
  cat("\n\n*** FERDIG MED Å ESTIMERE PREDTELLER\n")
  return(dt)
}

#' @title find_common_standard_teller_nevner_prednevner_design
#' @description
#' Finds the design of the standard teller and nevner file
#' @keywords internal
#' @noRd
find_common_standard_teller_nevner_prednevner_design <- function(parameters){
  cat("\n** Finner felles design for STANDARDTELLER, STANDARDNEVNER og PREDNEVNER\n")
  
  STdesign <- find_design_after_filter(file = "STANDARDTELLER", parameters = parameters)
  if ("STANDARDNEVNER" %in% names(parameters$files)) {
    SNdesign <- find_design_after_filter(file = "STANDARDNEVNER", parameters = parameters)
    STNdesign <- FinnFellesTab(DF1 = STdesign, DF2 = SNdesign, parameters = parameters)$FDes
  } else {
    STNdesign <- STdesign
  }
  PNdesign <- parameters$filedesign[[parameters$files$PREDNEVNER]]
  STNPdesign <- FinnFellesTab(DF1 = STNdesign, DF2 = PNdesign, parameters = parameters)$FDes
  
  # Maa filtrere standardtellerdesign med parameters$PredFilter igjen for aa finne fellesdesign som gir til-design for ST og SN
  STNdesign <- find_design_after_filter(file = "STANDARDTELLER", parameters = parameters, originaldesign = STNPdesign, outpredfilter = FALSE)
  
  return(list(STNdesign = STNdesign, STNPdesign = STNPdesign))
}

#' @title find_design_after_filter
#'
#' @param file name of file to design
#' @param originaldesign original design if existing. Default is design for file.
#' @param outpredfilter shuld columns in predfilter be removed, default = TRUE
#' @param parameters parameters
#' @keywords internal
#' @noRd
find_design_after_filter <- function(file, parameters, originaldesign = NULL, outpredfilter = TRUE) {
  filename <- parameters$files[[file]]
  designfilter <- parameters$PredFilter$Design
  if(is.null(originaldesign)) originaldesign <- parameters$filedesign[[filename]]
  filtercolumns <- character()
  if(outpredfilter) filtercolumns <- parameters$PredFilter$Predfiltercolumns
  
  outdesign <- FinnRedesignForFilter(ORGd = originaldesign, Filter = designfilter, parameters = parameters)$Dekk
  outcols <- setdiff(names(outdesign), filtercolumns)
  outdesign <- outdesign[, ..outcols]
  return(find_filedesign(file = outdesign, filename = filename, parameters = parameters))
}

#' @title FinnRedesignForFilter (kb)
#' @keywords internal
#' @noRd
FinnRedesignForFilter <- function(ORGd, Filter, parameters) {
  MODd <- Filter
  for (del in setdiff(names(ORGd$Part), names(Filter))) {
    MODd[[del]] <- data.table::copy(ORGd$Part[[del]])
  }
  # return(FinnRedesign(fradesign = ORGd, tildesign = list(Part = MODd), parameters = parameters))
  
  return(find_redesign(orgdesign = ORGd, targetdesign = list(Part = MODd), parameters = parameters))
}

#' @title estimate_predrate
#' @description finds predrate to be used to calculate predteller
#' @keywords internal
#' @noRd
estimate_predrate <- function(design, parameters){
  cat("\n* Estimerer PREDRATE...")
  missyears <- parameters$MOVAVparameters$missyears
  predrate <- merge_teller_nevner(parameters = parameters, standardfiles = TRUE, design = design)$TNF
  if(missyears$n > 0 && any(missyears$years %in% unique(predrate$AARl))){
    problem <- intersect(missyears$years, unique(predrate$AARl))
    warning("\n--\n** OBS! Mangler tall for år som skal standardiseres mot: ", paste(problem, collapse = ", "), 
            "\n*** Dette vil påvirke landsraten i standardiseringsperioden!\n--\n", immediate. = TRUE)
    predrate <- predrate[!AARl %in% missyears$years]
  }
  predrate <- aggregate_to_periods(dt = predrate, parameters = parameters)
  predrate[, (parameters$PredFilter$Predfiltercolumns) := NULL]
  predrate[NEVNER != 0 & NEVNER.f == 0, let(PREDRATE = TELLER/NEVNER, PREDRATE.f = pmax(TELLER.f, NEVNER.f))]
  predrate[NEVNER == 0 & NEVNER.f == 0, let(PREDRATE = 0, PREDRATE.f = pmax(TELLER.f, 2))]
  predrate[TELLER <= 2 & TELLER.f == 0 & NEVNER == 0 & NEVNER.f == 0, let(PREDRATE = 0, PREDRATE.f = 0)]
  predrate[, let(PREDRATE.a = pmax(TELLER.a, NEVNER.a))]
  
  ukurante <- predrate[is.na(TELLER) | is.na(NEVNER), .N]
  if (ukurante > 0) cat("\n\n!!! Missing verdier i standardteller og/eller standardnevner (", ukurante, "), dette vil gi problemer i PREDTELLER\n")
  predrate <- predrate[, .SD, .SDcols = c(get_dimension_columns(names(predrate)), paste0("PREDRATE", c("", ".f", ".a")))]
  return(predrate)
}

#' @title estimate_prednevner
#' @description finds prednevner, to be used to calculate predteller
#' @keywords internal
#' @noRd
estimate_prednevner <- function(design, parameters){
  cat("\n* Estimerer PREDNEVNER...")
  missyears <- parameters$MOVAVparameters$missyears
  redesign <- find_redesign(orgdesign = parameters$filedesign[[parameters$files$PREDNEVNER]], targetdesign = design, parameters = parameters)
  prednevner <- fetch_filegroup_from_buffer(filegroup = parameters$files$PREDNEVNER, parameters = parameters)
  prednevner <- do_filter_and_recode_to_redesign(dt = prednevner, redesign = redesign, parameters = parameters)
  PredNevnerKol <- gsub("^(.*):(.*)", "\\2", parameters$TNPinformation$PREDNEVNERFIL)
  if(is_empty(PredNevnerKol)) PredNevnerKol <- parameters$TNPinformation$NEVNERKOL
  PNnames <- gsub(paste0("^", PredNevnerKol, "(\\.f|\\.a|)$"), "PREDNEVNER\\1", names(prednevner))
  data.table::setnames(prednevner, names(prednevner), PNnames)
  prednevner <- prednevner[, .SD, .SDcols = c(get_dimension_columns(names(prednevner)), grep("^PREDNEVNER", names(prednevner), value= T))]
  if(missyears$n > 0) prednevner <- prednevner[!AARl %in% missyears$years]
  prednevner <- aggregate_to_periods(dt = prednevner, parameters = parameters)
  return(prednevner)
}

#' @title estimate_predteller
#' @description estimates predteller, used for age standardization
#' @keywords internal
#' @noRd
estimate_predteller <- function(predrate, prednevner, parameters){
  cat("\n* Estimerer PREDTELLER...")
  commondims <- intersect(get_dimension_columns(names(prednevner)), get_dimension_columns(names(predrate)))
  mismatch <- collapse::join(predrate, prednevner, how = "anti", multiple = T, on = commondims, overid = 2, verbose = 0)[, .N]
  if(mismatch > 0) cat("!!!!!ADVARSEL:", mismatch, "strata i predrate finnes ikke i prednevner!!!\n")
  
  predteller <- collapse::join(prednevner, predrate, how = "l", on = commondims, multiple = T, overid = 2, verbose = 0)
  predteller[, let(PREDTELLER = PREDRATE * PREDNEVNER,
                   PREDTELLER.f = pmax(PREDRATE.f, PREDNEVNER.f),
                   PREDTELLER.a = pmax(PREDRATE.a, PREDNEVNER.a))]
  predteller <- predteller[, .SD, .SDcols = c(get_dimension_columns(names(predteller)), paste0("PREDTELLER", c("", ".f", ".a")))]
  
  filename <- parameters$files$PREDNEVNER
  prednevnerdesign <- find_filedesign(file = prednevner, filename = filename, parameters = parameters)
  cubedesign <- list(Part = parameters$CUBEdesign)
  redesign <- find_redesign(orgdesign = prednevnerdesign, targetdesign = cubedesign, aggregate = parameters$DefDesign$AggVedStand, parameters = parameters)
  predteller <- do_filter_and_recode_to_redesign(dt = predteller, redesign = redesign, parameters = parameters)
  # predteller[, PREDTELLER.n := TELLER.n]
  return(predteller)
}

#' @title add_meisskala
#' @description
#' Adds scale to standardize MEIS
#' @keywords internal
#' @noRd
add_meisskala <- function(dt, parameters){
  if(parameters$PredFilter$ref_year_type != "Specific") return(dt)
  cat("\n* Legger til MEISskala for standardisering\n")

  if(parameters$CUBEinformation$REFVERDI_VP != "P"){
    dt[, MEISskala := NA_real_]
    return(dt)
  }
  
  subset_meisskala <- dt[eval(rlang::parse_expr(parameters$PredFilter$meisskalafilter))]
  if (nrow(subset_meisskala) == 0) stop("Noe er feil i ACCESS::KUBER::REFVERDI, klarer ikke lage meisskala")
  subset_meisskala[, MEISskala := RATE]
  joincolumns <- setdiff(intersect(names(subset_meisskala), parameters$DefDesign$DesignKolsFA), parameters$PredFilter$Predfiltercolumns)
  dt <- collapse::join(dt, subset_meisskala[, mget(c(joincolumns, "MEISskala"))], how = "l", on = joincolumns, overid = 2, verbose = 0)
  return(dt) 
}
