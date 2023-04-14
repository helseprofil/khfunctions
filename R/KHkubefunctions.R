## Stata prikking do file
#' do_stata_prikk (ybk)
#' 
#' Function to censor the data using the STATA method (JRM)#'
do_stata_prikk <- function(dt, spc, batchdate, globs, test = FALSE){
  is_kh_debug()
  
  stataVar <- c("Stata_PRIKK_T", "Stata_PRIKK_N", "Stata_STATTOL_T")
  s_prikk <- sum(sapply(spc[, ..stataVar], get_col), na.rm = TRUE)
  
  RprikkVar <- c("PRIKK_T", "PRIKK_N", "STATTOL_T")
  r_prikk <- sum(sapply(spc[, ..RprikkVar], get_col), na.rm = TRUE)
  
  # Check that R prikk should be empty if Stata prikk should be used
  warn_prikk(r_prikk, s_prikk)
  RES <- NULL
  
  if (s_prikk > 0){
    ## synt <- 'include "F:\\Forskningsprosjekter\\PDB 2455 - Helseprofiler og til_\\PRODUKSJON\\BIN\\Z_Statasnutter\\Rsynt_Postprosess_naboprikking_del_1_LESEFERD_INNV.do'
    sfile <- paste(globs[["path"]], globs[["KubeStataPrikkFil"]], sep = "/")
    synt <- paste0('include "', sfile, '"')
    
    RES <- KjorStataSkript(dt, script = synt, tableTYP = "DT", batchdate = batchdate, globs = globs)
    dt <- RES$TABLE
  } else {
    RES[["feil"]] <- ""
  }
  
  if (RES$feil != "") {
    stop("Noe gikk galt i kjøring av STATA \n", RES$feil)
  }
  
  return(dt)
}

#' kube_spec (ybk)
#' 
#' Saves ACCESS specs + list of dimensions to be used in STATA censoring
kube_spec <- function(spec, dims){
  is_kh_debug()
  
  rootDir <- file.path(fs::path_home(), "helseprofil")
  if (!fs::dir_exists(rootDir))
    fs::dir_create(rootDir)
  
  specDF <- as.data.table(spec)
  varStata <- grep("^Stata", names(specDF), value = TRUE)
  varSpec <- c("KUBE_NAVN", varStata)
  varDF <- specDF[, .SD, .SDcols = varSpec]
  varDF[, DIMS := list(dims)]
  fileSpec <- file.path(rootDir, "kubespec.csv")
  data.table::fwrite(varDF, fileSpec, sep = ";", sep2 = c("", " ", ""))
  message("Create Stata spec in ", fileSpec)
  return(specDF)
}

#' warn_prikk
#' 
#' helper function in STATA censoring
warn_prikk <- function(r, s){
  is_kh_debug()
  
  if (r > 0 & s > 0){
    stop("You can't prikk for both R and Stata way. Choose either one!")
  }
  
  invisible()
}

#' get_col (ybk)
#' 
#' helper function in STATA censoring
#' Easier to check with sum by converting valid col value to 1
get_col <- function(var, num = TRUE){
  is_kh_debug()
  
  if (is.na(var) || var == ""){
    var <- NA
  }
  
  if (num){
    var <- var_num(var)
  }
  
  if (!is.na(var) && num){
    var <- 1
  }
  
  return(var)
}

#' var_num (ybk)
#' 
#' Helper function for STATA censoring
#' Avoid warning message "NAs introduced by coercion" when using as.numeric
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
var_num <- function(x){
  is_kh_debug()
  
  v <- is.numeric(x)
  if (!v){
    x <- NA
  }
  
  return(x)
}

#' find_dims (vl)
#' 
#' Helper function for kube_spec, finding dimensions in KUBE
find_dims <- function(dt, spec){
  is_kh_debug()
  # List standarddims
  standarddims <- c("GEO",
                    "AAR",
                    "ALDER",
                    "KJONN",
                    "UTDANN",
                    "INNVKAT",
                    "LANDBAK")
  
  # Extract everything written in TAB1, TAB2, and TAB3 in the files involved
  tabdims <- vector()
  for(i in 1:length(spec)){
    tabdims <- c(tabdims, 
                 unlist(spec[[i]][c("TAB1", "TAB2", "TAB3")], use.names = F))
  }
  
  # Remove NA from tabdims, combine with standarddims
  tabdims <- tabdims[!is.na(tabdims)]
  alldims <- c(standarddims, tabdims)
  # Extract column names from dt included in dimension list
  names(dt)[names(dt) %in% alldims]
}

#' LagQCkube (vl)
#' 
#' Saves QC kube containing standard columns defined in globs, 
#' and extra cols existing in the specific KUBE
LagQCKube <- function(KUBEid,
                      KUBE,
                      kubedims,
                      kubevals,
                      batchdate = batchdate,
                      globs = globs){
  QC <- copy(KUBE)
  qccols <- c(globs$QCTabs,setdiff(kubedims, globs$QCTabs),
              globs$QCVals,setdiff(kubevals, globs$QCVals),
              "SPVFLAGG")
  qcmisscols <- setdiff(qccols, names(QC))
  if (length(qcmisscols > 0)) {
    QC[, (qcmisscols) := NA]
  }
  QC <- QC[, ..qccols]
  
  return(QC)
}
