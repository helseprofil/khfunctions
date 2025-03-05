#' @title write_cube_output
#' @description
#' Writes KUBE, ALLVIS, and QC files from lagKUBE
#' 
#' REDUCE NUMBER OF ARGUMENTS TO A LIST OF INPUT-args, which can be added to parameters[["input_args"]] and passed to other functions
#' @param outputlist list of output to write
#' @param KUBEid KUBEid
#' @param batchdate batchdate
#' @param versjonert versjonert
#' @param csvcopy csvcopy
#' @param geonaboprikk geonaboprikk
write_cube_output <- function(outputlist, KUBEid, batchdate, versjonert, csvcopy, geonaboprikk){
  basepath <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kubedir"))
  cat("SAVING OUTPUT FILES:\n")
  
  if(!geonaboprikk) KUBEid <- paste0("ikkegeoprikket_", KUBEid)
  
  utfiln <- file.path(basepath, getOption("khfunctions.kube.ny"), paste0(KUBEid, ".rds"))
  saveRDS(outputlist$KUBE, file = utfiln)
  cat("\n", utfiln)
  
  if (versjonert == TRUE) {
    utfilv <- file.path(basepath, getOption("khfunctions.kube.dat"), "R", paste0(KUBEid, "_", batchdate, ".rds"))
    file.copy(utfiln, utfilv)
    cat("\n", utfilv)
  }
  
  if (csvcopy == TRUE) {
    utfild <- file.path(basepath, getOption("khfunctions.kube.dat"), "csv", paste0(KUBEid, "_", batchdate, ".csv"))
    data.table::fwrite(outputlistALLVIS, file = utfild, sep = ";")
    cat("\n", utfild)
    
    utfilq <- file.path(basepath, getOption("khfunctions.kube.qc"), paste0("QC_", KUBEid, "_", batchdate, ".csv"))
    data.table::fwrite(outputlistQC, file = utfilq, sep = ";")
    cat("\n", utfilq)
  }
}