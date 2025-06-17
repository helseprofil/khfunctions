#' @title write_filegroup_output
#' @description Writes KUBE, ALLVIS, and QC files from lagKUBE
#' @param outfile filgruppe
#' @param parameters global parameters
#' @keywords internal
#' @noRd
write_filegroup_output <- function(dt, parameters){
  if(!parameters$write) return(invisible(NULL))
  cat("SAVING OUTPUT FILES:\n")
  root <- getOption("khfunctions.root")
  nyeste <- file.path(root, getOption("khfunctions.filegroups.ny"), paste0(parameters$name, ".rds"))
  datert <- file.path(root, getOption("khfunctions.filegroups.dat"), paste0(parameters$name, "_", parameters$batchdate, ".rds"))
  saveRDS(dt, file = nyeste)
  cat("\n", nyeste)
  file.copy(from = nyeste, to = datert)
  cat("\n", datert)
}

#' @title write_codebooklog
#' @keywords internal
#' @noRd
write_codebooklog <- function(log, parameters){
  if(!parameters$write) return(invisible(NULL))
  log <- log[, .SD, .SDcols = intersect(names(log), c("KOBLID", "DELID", "FELTTYPE", "ORG", "KBOMK", "FREQ"))]
  cat("\n* Skriver kodebok-logg til", getOption("khfunctions.filegroups.kblogg"))
  path <- file.path(getOption("khfunctions.root"), getOption("khfunctions.filegroups.kblogg"))
  name <- paste0("KBLOGG_", parameters$name, "_", parameters$batchdate, ".csv")
  move_old_files_to_archive(path = path, parameters = parameters)
  data.table::fwrite(log, file = file.path(path, name), sep = ";", bom = T)
}

#' @title write_codebooklog
#' @keywords internal
#' @noRd
write_cleanlog <- function(log, parameters){
  if(!parameters$write) return(invisible(NULL))
  cat("\n* Skriver filgruppesjekk til", getOption("khfunctions.filegroups.fgsjekk"))
  path <- file.path(getOption("khfunctions.root"), getOption("khfunctions.filegroups.fgsjekk"))
  name <- paste0("FGSJEKK_", parameters$name, "_", parameters$batchdate, ".csv")
  move_old_files_to_archive(path = path, parameters = parameters)
  data.table::fwrite(log, file = file.path(path, name), sep = ";", bom = T)
}

move_old_files_to_archive <- function(path, parameters){
  oldfiles <- list.files(path, pattern = paste0(".*_", parameters$name, "_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}.csv$"))
  if(length(oldfiles) == 0) return(invisible(NULL))
  cat("\n** Flytter gamle filer til arkiv-mappen")
  for(file in oldfiles){
    fs::file_move(path = file.path(path, file), new_path = file.path(path, "arkiv", file)) 
  }  
}

#' @title write_cube_output
#' @description Writes KUBE, ALLVIS, and QC files from lagKUBE
#' @param outputlist list of output to write
#' @param parameters global parameters
#' @keywords internal
#' @noRd
write_cube_output <- function(outputlist, parameters){
  if(!parameters$write) return(invisible(NULL))
  basepath <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kubedir"))
  name <- ifelse(parameters$geonaboprikk, paste0("ikkegeoprikket_", parameters$name), parameters$name)
  datert_R <- file.path(basepath, getOption("khfunctions.kube.dat"), "R", paste0(name, "_", parameters$batchdate, ".rds"))
  datert_csv <- file.path(basepath, getOption("khfunctions.kube.dat"), "csv", paste0(name, "_", parameters$batchdate, ".csv"))
  qc <- file.path(basepath, getOption("khfunctions.kube.qc"), paste0("QC_", name, "_", parameters$batchdate, ".csv"))
  
  cat("SAVING OUTPUT FILES:\n")
  saveRDS(outputlist$KUBE, file = datert_R)
  cat("\n", datert_R)
  data.table::fwrite(outputlist$ALLVIS, file = datert_csv, sep = ";")
  cat("\n", datert_csv)
  data.table::fwrite(outputlist$QC, file = qc, sep = ";")
  cat("\n", qc)
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
  
  file <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kubedir"), getOption("khfunctions.kube.specs"), paste0("spec_", parameters$name, "_", parameters$batchdate, ".csv"))
  data.table::fwrite(specs, file = file, sep = ";")
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

#' @title save_filedump_if_requested
#' @description
#' Saves a .csv, .rds, or .dta file at specific points during data processing
#' All RSYNT points have the possibility to save a filedump with the name rsyntname + pre/post, e.g. "RSYNT_POSTPROSESSpre" 
#' 
#' Filegroup processing: 
#' * RSYNT1pre/post (Rsynt when reading original file)
#' * RESHAPEpre/post (Before and after reshaping original file)
#' * RSYNT2pre/post (Rsynt during formatting of original file to table)
#' * KODEBOKpre/post (before and after recoding with codebook)
#' * RSYNT_PRE_FGLAGRINGpre/post (Before and after rsynt point prior to saving output)
#' 
#' Cube processing: 
#' * MOVAVpre/MOVAVpost (before and after aggregating to periods)
#' * SLUTTREDIGERpre/post (before and after RSYNT point SLUTTREDIGER, after aggregation and standardization)
#' * PRIKKpre/post (before and after censoring)
#' * RSYNT_POSTPROSESSpre/post (Before and after RSYNT_POSTPROSESS)
#' @param dumpname name of requested filedump
#' @param dt data to be stored
#' @param parameters global parameters
#' @param koblid used when requesting file dumps during processing of original files, default = NULL
#' @examples
#' # LagKUBE("KUBENAVN", dumps = list(PRIKKpre = "STATA", PRIKKpost = c("CSV", "STATA", "R"))
#' # LagFilgruppe("FILGRUPPENAVN", dumps = list(RSYNT1pre = "STATA", KODEBOKpost = c("CSV", "STATA", "R") )
save_filedump_if_requested <- function(dumpname = c("RSYNT1pre", "RSYNT1post", "RESHAPEpre", "RESHAPEpost", "RSYNT2pre", "RSYNT2post",
                                                    "KODEBOKpre", "KODEBOKpost", "RSYNT_PRE_FGLAGRINGpre", "RSYNT_PRE_FGLAGRINGpost",
                                                    "MOVAVpre", "MOVAVpost", "SLUTTREDIGERpre", "SLUTTREDIGERpost", 
                                                    "PRIKKpre", "PRIKKpost", "RSYNT_POSTPROSESSpre", "RSYNT_POSTPROSESSpost"), 
                                       dt, parameters, koblid = NULL){
  if(is.null(dumpname) || !dumpname %in% names(parameters$dumps)) return(invisible(NULL))
  format <- parameters$dumps[[dumpname]]
  dumpdir <- file.path(getOption("khfunctions.root"), getOption("khfunctions.dumpdir"))
  filename <- paste0(parameters$name, "_", dumpname)
  if(!is.null(koblid)) filename <- paste0(filename, "_", koblid)
    
  if("CSV" %in% format) data.table::fwrite(dt, file = file.path(dumpdir, paste0(filename, ".csv"), sep = ";"))
  if("R" %in% format){
    if(!exists("DUMPS", envir = .GlobalEnv)) .GlobalEnv$DUMPS <- list()
    .GlobalEnv$DUMPS[[filename]] <- data.table::copy(dt)
    # saveRDS(dt, file = file.path(dumpdir, paste0(filename, ".rds")))
  } 
  if("STATA" %in% format){
    dtout <- fix_column_names_pre_stata(dt)
    haven::write_dta(dtout, path = file.path(dumpdir, paste0(filename, ".dta")))
  }
}
  
