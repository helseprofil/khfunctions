#' @title write_filegroup_output
#' @param outfile filgruppe
#' @param parameters global parameters
#' @keywords internal
#' @noRd
write_filegroup_output <- function(dt, parameters){
  if(!parameters$write) return(invisible(NULL))
  cat("\n\n* SAVING OUTPUT FILES:\n")
  root <- file.path(getOption("khfunctions.root"), getOption("khfunctions.fgdir"))
  parquet <- file.path(root, getOption("khfunctions.fg.ny"), paste0(parameters$name, ".parquet"))
  datert <- file.path(root, getOption("khfunctions.fg.dat"), paste0(parameters$name, "_", parameters$batchdate, ".parquet"))
  cat("\n", parquet,"\n", datert)
  table <- convert_dt_to_arrow_table(dt)
  arrow::write_parquet(table, sink = parquet, compression = "snappy")
  file.copy(from = parquet, to = datert)
  if(parameters$name == "BEF_GKny") write_population_filegroup(table = table, root = root)
}

#' @title convert_dt_to_arrow_table
#' @description
#' Removes attributes except column names before converting to an arrow table (for writing)
#' @param dt data.table
#' @noRd
convert_dt_to_arrow_table <- function(dt){
  attremove <- grep("^(class|names)$", names(attributes(dt)), value = T, invert = T)
  for(att in attremove) data.table::setattr(dt, att, NULL)
  table <- arrow::as_arrow_table(dt)
  return(table)
}

#' @title do_write_parquet
#' @description
#' Wrapper around arrow::write_parquet, which first applies convert_dt_to_arrow_table() to generate a arrow table for writing.
#' @param dt data.table
#' @param filepath filepath to save file
#' @keywords internal
#' @noRd
do_write_parquet <- function(dt, filepath){
  table <- convert_dt_to_arrow_table(dt)
  arrow::write_parquet(table, sink = filepath, compression = "snappy")
}

#' @title write_population_filegroup
#' @description
#' Writes a partitioned dataset for BEF_GKny, for quicker read times when used as nevner file
#' Generates two helper columns to partition the data into age groups and with/without lks
#' @noRd
write_population_filegroup <- function(table, root){
  table <- add_partition_columns(table = table)
  cat("\n* Lagrer befolkningsfilgruppe splittet på AARl og GEOniv.....")
  do_write_parquet_dataset(table = table, 
                           path = file.path(root, getOption("khfunctions.fg.ny"), getOption("khfunctions.pop_aargeo")),
                           partitioncols = c("AARl", "lks"))
  cat("\n* Lagrer befolkningsfilgruppe splittet på ALDERl, AARl og GEOniv.....")
  do_write_parquet_dataset(table = table, 
                           path = file.path(root, getOption("khfunctions.fg.ny"), getOption("khfunctions.pop_alderaargeo")),
                           partitioncols = c("alder", "AARl", "lks"))
}

#' @keywords internal
#' @noRd
add_partition_columns <- function(table){
  table <- arrow::as_arrow_table(
    table |>
      dplyr::mutate(
        lks = dplyr::if_else(GEOniv == "V", 1L, 0L),
        alder = dplyr::case_when(
          ALDERh <= 17 ~ "0_17",
          ALDERh <= 29 ~ "18_29",
          ALDERh <= 44 ~ "30_44",
          ALDERh <= 67 ~ "45_67",
          ALDERh <= 79 ~ "68_79",
          .default = "80_120"
          )
        )
  )
  return(table)
}

#' @keywords internal
#' @noRd
do_write_parquet_dataset <- function(table, path, partitioncols){
  dataset <- table |> 
    dplyr::group_by(!!!rlang::syms(partitioncols)) |>
    dplyr::arrange(!!!rlang::syms(partitioncols))
  
  arrow::write_dataset(dataset = dataset, path = path, format = "parquet", partitioning = partitioncols, compression = "snappy")
  cat("Ferdig!")
}

#' @title write_codebooklog
#' @keywords internal
#' @noRd
write_codebooklog <- function(log, parameters){
  if(!parameters$write) return(invisible(NULL))
  log <- log[, .SD, .SDcols = intersect(names(log), c("KOBLID", "DELID", "FELTTYPE", "ORG", "KBOMK", "FREQ"))]
  cat("\n* Skriver kodebok-logg til", getOption("khfunctions.fgdir"), getOption("khfunctions.fg.kblogg"))
  path <- file.path(getOption("khfunctions.root"), getOption("khfunctions.fgdir"), getOption("khfunctions.fg.kblogg"))
  name <- paste0("KBLOGG_", parameters$name, "_", parameters$batchdate, ".csv")
  move_old_files_to_archive(path = path, parameters = parameters)
  data.table::fwrite(log, file = file.path(path, name), sep = ";", bom = T)
}

#' @title write_codebooklog
#' @keywords internal
#' @noRd
write_cleanlog <- function(log, parameters){
  if(!parameters$write) return(invisible(NULL))
  cat("\n* Skriver filgruppesjekk til", getOption("khfunctions.fgdir"), getOption("khfunctions.fg.sjekk"))
  path <- file.path(getOption("khfunctions.root"), getOption("khfunctions.fgdir"), getOption("khfunctions.fg.sjekk"))
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
  name <- ifelse(!parameters$geonaboprikk, paste0("ikkegeoprikket_", parameters$name), parameters$name)
  datert_parquet <- file.path(basepath, getOption("khfunctions.kube.dat"), "R", paste0(name, "_", parameters$batchdate, ".parquet"))
  datert_csv <- file.path(basepath, getOption("khfunctions.kube.dat"), "csv", paste0(name, "_", parameters$batchdate, ".csv"))
  qc_csv <- file.path(basepath, getOption("khfunctions.kube.qc"), paste0("QC_", name, "_", parameters$batchdate, ".csv"))
  qc_parquet <- file.path(basepath, getOption("khfunctions.kube.qc"), paste0("QC_", name, "_", parameters$batchdate, ".parquet"))
  
  cat("SAVING OUTPUT FILES:\n")
  data.table::fwrite(outputlist$ALLVIS, file = datert_csv, sep = ";") # Main output file for stat bank
  do_write_parquet(outputlist$KUBE, filepath = datert_parquet) # Full cube .parquet format
  cat("\n", datert_csv, "\n", datert_parquet)
  data.table::fwrite(outputlist$QC, file = qc_csv, sep = ";") # QC csv format (to be deprecated)
  do_write_parquet(dt = outputlist$QC, filepath = qc_parquet) # QC .parquet format
  cat("\n", qc_csv, "\n", qc_parquet)
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
  
