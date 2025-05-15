#' @title write_cube_output
#' @description
#' Writes KUBE, ALLVIS, and QC files from lagKUBE
#' 
#' REDUCE NUMBER OF ARGUMENTS TO A LIST OF INPUT-args, which can be added to parameters-"input_args" and passed to other functions
#' @param outputlist list of output to write
#' @param name KUBEid
#' @param batchdate batchdate
#' @param versjonert versjonert
#' @param csvcopy csvcopy
#' @param geonaboprikk geonaboprikk
write_cube_output <- function(outputlist, name, batchdate, versjonert, csvcopy, geonaboprikk){
  basepath <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kubedir"))
  if(!geonaboprikk) name <- paste0("ikkegeoprikket_", name)
  nyeste <- file.path(basepath, getOption("khfunctions.kube.ny"), paste0(name, ".rds"))
  datert_R <- file.path(basepath, getOption("khfunctions.kube.dat"), "R", paste0(name, "_", batchdate, ".rds"))
  datert_csv <- file.path(basepath, getOption("khfunctions.kube.dat"), "csv", paste0(name, "_", batchdate, ".csv"))
  qc <- file.path(basepath, getOption("khfunctions.kube.qc"), paste0("QC_", name, "_", batchdate, ".csv"))
  
  cat("SAVING OUTPUT FILES:\n")
  saveRDS(outputlist$KUBE, file = nyeste)
  cat("\n", nyeste)
  
  if(versjonert){
    file.copy(nyeste, datert_R)
    cat("\n", datert_R)
  }
  
  if(csvcopy) {
    data.table::fwrite(outputlist$ALLVIS, file = datert_csv, sep = ";")
    cat("\n", datert_csv)
    data.table::fwrite(outputlist$QC, file = qc, sep = ";")
    cat("\n", qc)
  }
}

write_filegroup_output <- function(outfile, name, versjonert, batchdate){
  cat("SAVING OUTPUT FILES:\n")
  root <- getOption("khfunctions.root")
  nyeste <- file.path(root, getOption("khfunctions.filegroups.ny"), paste0(name, ".rds"))
  datert <- file.path(path, getOption("khfunctions.filegroups.dat"), paste0(name, "_", batchdate, ".rds"))
  saveRDS(outfile, file = nyeste)
  cat("\n", nyeste)
  if(versjonert){
    file.copy(nyeste, datert)
    cat("\n", datert)
  }
}

  