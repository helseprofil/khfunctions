#' @title write_cube_output
#' @description Writes KUBE, ALLVIS, and QC files from lagKUBE
#' @param outputlist list of output to write
#' @param parameters global parameters
#' @keywords internal
#' @noRd
write_cube_output <- function(outputlist, parameters){
  if(!parameters$write) return(invisible(NULL))
  basepath <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kubedir"))
  name <- ifelse(parameters$geonaboprikk, paste0("ikkegeoprikket_", parameters$cube_name), parameters$cube_name)
  datert_R <- file.path(basepath, getOption("khfunctions.kube.dat"), "R", paste0(name, "_", parameters$batchdate, ".rds"))
  datert_csv <- file.path(basepath, getOption("khfunctions.kube.dat"), "csv", paste0(name, "_", parameters$batchdate, ".csv"))
  qc <- file.path(basepath, getOption("khfunctions.kube.qc"), paste0("QC_", name, "_", parameters$batchdate, ".csv"))
  
  cat("SAVING OUTPUT FILES:\n")
  saveRDS(outputlist$KUBE, file = datert_R)
  cat("\n", datert_R)
  
  if(csvcopy) {
    data.table::fwrite(outputlist$ALLVIS, file = datert_csv, sep = ";")
    cat("\n", datert_csv)
    data.table::fwrite(outputlist$QC, file = qc, sep = ";")
    cat("\n", qc)
  }
}

#' @title write_access_specs
#' @param parameters global parameters
#' @keywords internal
#' @noRd
write_access_specs <- function(parameters){
  if(!parameters$write) return(invisible(NULL))
  specs <- data.table::rbindlist(list(melt_access_spec(parameters$CUBEinformation, name = "KUBER"),
                                      melt_access_spec(parameters$TNPinformation, name = "TNP_PROD")))
  if(parameters$CUBEinformation$REFVERDI_VP == "P") specs <- data.table::rbindlist(list(specs, melt_access_spec(parameters$STNPinformation, name = "STANDARD TNP")))
  
  for(i in names(parameters$fileinformation)){
    fgp <- parameters$fileinformation[[i]]
    end = which(names(fgp) == "vals")-1
    specs <- data.table::rbindlist(list(specs, melt_access_spec(fgp[1:end], name = paste0("FILGRUPPER: ", i))))
    if(i %in% parameters$FILFILTRE$FILVERSJON){
      specs <- data.table::rbindlist(list(specs, melt_access_spec(parameters$FILFILTRE[FILVERSJON == i], name = paste0("FILFILTRE: ", i))))
    }
  }
  
  if(length(parameters$friskvik$INDIKATOR) > 0){
    for(i in parameters$friskvik$ID){
      specs <- data.table::rbindlist(list(specs, melt_access_spec(parameters$friskvik[ID == i], name = paste0("FRISKVIK:ID-", i))))
    }
  }
  
  file <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kubedir"), getOption("khfunctions.kube.specs"), paste0("spec_", parameters$cube_name, "_", parameters$batchdate, ".csv"))
  data.table::fwrite(specs, file = file, sep = ";")
}

#' @title LagQCKube (vl)
#' @description
#' Adds uncensored columns sumTELLER/sumNEVNER/RATE.n to the ALLVISkube
#' @param allvis Censored ALLVIs kube
#' @param uprikk Uncensored KUBE
#' @param allvistabs Dimensions included in ALLVIS kube
#' @param allvisvals All columns 
#' @keywords internal
#' @noRd
LagQCKube <- function(allvis,
                      allvistabs,
                      kube){
  qcvals <- getOption("khfunctions.qcvals")
  QC <- data.table::copy(allvis)
  uprikk <- data.table::copy(kube)[, mget(c(allvistabs, qcvals))]
  data.table::setnames(uprikk, qcvals, paste0(qcvals, "_uprikk"))
  
  QC <- QC[uprikk, on = allvistabs]
  
  return(QC[])
}

#' @title melt_access_spec
#' @description
#' helper function for save_access_specs
#' @keywords internal
#' @noRd
melt_access_spec <- function(dscr, name = NULL){
  if(is.null(name)){
    name <- deparse(substitute(dscr))
  }
  d <- data.table::as.data.table(dscr)
  d[, names(d) := lapply(.SD, as.character)]
  d <- data.table::melt(d, measure.vars = names(d), variable.name = "Kolonne", value.name = "Innhold")
  d[, Tabell := name]
  data.table::setcolorder(d, "Tabell")
}

#' @title write_filegroup_output
#' @description Writes KUBE, ALLVIS, and QC files from lagKUBE
#' @param outfile filgruppe
#' @param parameters global parameters
#' @keywords internal
#' @noRd
write_filegroup_output <- function(outfile, parameters){
  if(!parameters$write) return(invisible(NULL))
  cat("SAVING OUTPUT FILES:\n")
  root <- getOption("khfunctions.root")
  nyeste <- file.path(root, getOption("khfunctions.filegroups.ny"), paste0(parameters$filegroup_name, ".rds"))
  datert <- file.path(path, getOption("khfunctions.filegroups.dat"), paste0(parameters$filegroup_name, "_", parameters$batchdate, ".rds"))
  saveRDS(outfile, file = nyeste)
  cat("\n", nyeste)
  file.copy(from = nyeste, to = datert)
  cat("\n", datert)
}

  