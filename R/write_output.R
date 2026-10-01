# Filgruppe ----

#' @title write_filegroup_output
#' @param outfile filgruppe
#' @param parameters global parameters
#' @keywords internal
#' @noRd
write_filegroup_output <- function(parameters){
  if(!parameters$write) return(invisible(NULL))
  khtools::msg("\n\n* SAVING OUTPUT FILES:\n")
  root <- file.path(getOption("khfunctions.root"), getOption("khfunctions.fgdir"))
  parquet <- file.path(root, getOption("khfunctions.fg.ny"), paste0(parameters$name, ".parquet"))
  datert <- file.path(root, getOption("khfunctions.fg.dat"), paste0(parameters$name, "_", parameters$batchdate, ".parquet"))
  con <- parameters$duck
  if(grepl("BEF_GKny", parameters$name, ignore.case = T)){
    add_lks_filter(con = con)
    sort_bef_gkny_duckdb(con = con)
  } 
  khtools::msg("\n Skriver:\n", parquet,"\n", datert)
  do_write_output_duckdb(con = con, source = "FILGRUPPE", filepath = parquet, format = "parquet")
  file.copy(from = parquet, to = datert)
}

#' @title write_codebooklog
#' @keywords internal
#' @noRd
write_codebooklog <- function(log, parameters){
  if(!parameters$write) return(invisible(NULL))
  log <- log[, .SD, .SDcols = intersect(names(log), c("KOBLID", "DELID", "FELTTYPE", "TYPE", "ORG", "OMK", "FREQ"))]
  khtools::msg("\n* Skriver kodebok-logg til", getOption("khfunctions.fgdir"), getOption("khfunctions.fg.kblogg"))
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
  khtools::msg("\n* Skriver filgruppesjekk til", getOption("khfunctions.fgdir"), getOption("khfunctions.fg.sjekk"))
  path <- file.path(getOption("khfunctions.root"), getOption("khfunctions.fgdir"), getOption("khfunctions.fg.sjekk"))
  name <- paste0("FGSJEKK_", parameters$name, "_", parameters$batchdate, ".csv")
  move_old_files_to_archive(path = path, parameters = parameters)
  data.table::fwrite(log, file = file.path(path, name), sep = ";", bom = T)
}

move_old_files_to_archive <- function(path, parameters){
  oldfiles <- list.files(path, pattern = paste0(".*_", parameters$name, "_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}.csv$"))
  if(length(oldfiles) == 0) return(invisible(NULL))
  khtools::msg("\n** Flytter gamle filer til arkiv-mappen")
  for(file in oldfiles){
    fs::file_move(path = file.path(path, file), new_path = file.path(path, "arkiv", file)) 
  }  
}

#' @title add_lks_filter
#' @description Legger til lks 0/1 for å kunne filtrere bort lks-data i innlesing
#' @param con kobling til duckdb
#' @noRd
add_lks_filter <- function(con) {
  khtools::msg("\n Legger til sorteringskolonner for befolkningsfilgruppe")
  invisible(DBI::dbExecute(con, "ALTER TABLE FILGRUPPE ADD COLUMN IF NOT EXISTS lks INTEGER;"))
  invisible(DBI::dbExecute(con, "UPDATE FILGRUPPE SET lks = CASE WHEN GEOniv = 'V' THEN 1 ELSE 0 END"))
  invisible(NULL)
}

#' @title sort_bef_gkny_duckdb
#' @description
#' Sorterer FILGRUPPE etter alle dimensjonskolonner.
#' Overskriver tabellen med sortert versjon.
#' @param con duckdb-connection
#' @noRd
sort_bef_gkny_duckdb <- function(con){
  khtools::msg("\n Sorterer befolkningsfilgruppe")
  
  dims <- khfunctions:::get_dimension_columns(khtools::duckdb_get_columns(con, "FILGRUPPE"))
  sort <- c("lks", "AARl", "ALDERl", "GEO", "KJONN", "UTDANN", "INNVKAT", "LANDBAK")
  sortdims <- union(sort, dims)
  sortdims_sql <- paste(sortdims, collapse = ", ")
  
  orgtabell <- "FILGRUPPE"
  
  sql <- sprintf("SELECT * FROM %s ORDER BY %s", 
                 khtools::sql_quote_I(con, orgtabell), sortdims_sql)
  khtools::duckdb_replace_existing_table(con, target = orgtabell, select_sql = sql)
}


# Kube ----

#' @title generate_allvis_base
#' @description
#' Lager grunnlaget for ALLVIS og QC-output
#' @noRd
generate_allvis_base <- function(parameters){
  con <- parameters$duck
  cols <- DBI::dbListFields(con, "KUBE")
  cols <- cols[!grepl("\\.(f|a|n|fn[0-9]+)$|^naboprikketIomg\\d+$|^spv_tmp$", cols) | cols == "RATE.n"]
  qcvals <- intersect(getOption("khfunctions.qcvals"), cols)
  
  qcols <- as.character(khtools::sql_quote_I(con, cols))
  select_expr <- c(qcols, "SPVFLAGG")
  
  censorvalues <- intersect(unique(c(parameters$outvalues, "MEIS")), cols)
  qcensor <- as.character(khtools::sql_quote_I(con, censorvalues))
  select_expr[match(censorvalues, cols)] <- sprintf(
    "CASE WHEN SPVFLAGG > 0 THEN NULL ELSE %s END AS %s",
    qcensor, qcensor
  )
  
  quprikk <- as.character(khtools::sql_quote_I(con, qcvals))
  quprikk_alias <- as.character(khtools::sql_quote_I(con, paste0(qcvals, "_uprikk")))
  select_expr <- c(select_expr, sprintf("%s AS %s", quprikk, quprikk_alias))
  
  khtools::duckdb_drop_tables(con, "ALLVIS_base")
 
  sql <- sprintf(
    "CREATE TABLE ALLVIS_base AS
    WITH base AS (
      SELECT *,
        CASE
            WHEN spv_tmp IS NULL THEN 0
            WHEN spv_tmp IN (-1, 4) THEN 3
            WHEN spv_tmp = 9 THEN 1
            ELSE spv_tmp
        END AS SPVFLAGG
      FROM KUBE
    )
    SELECT %s FROM base",
    paste(select_expr, collapse = ",\n        ")
  )
  
  invisible(DBI::dbExecute(con, sql))
  invisible(NULL)
}

#' @title generate_allvis_export_table
#' @description
#' Lager endelig ALLVIS-fil
#' @noRd
generate_allvis_export_table <- function(parameters){
  con <- parameters$duck
  khtools::duckdb_drop_tables(con, "ALLVIS")
  cols <- c(parameters$outdimensions, parameters$outvalues, "SPVFLAGG")
  cols <- as.character(khtools::sql_quote_I(con, cols))
  sql <- sprintf("CREATE TABLE ALLVIS AS SELECT %s FROM ALLVIS_base", 
                 paste(cols, collapse = ", "))
  invisible(DBI::dbExecute(parameters$duck, sql))
  invisible(NULL)
}

#' @title generate_qc_table
#' @description
#' Lager QC-fil med nødvendige hjelpekolonner
#' @noRd
generate_qc_table <- function(parameters){
  
  con <- parameters$duck
  qcvals <- intersect(getOption("khfunctions.qcvals"), DBI::dbListFields(con, "ALLVIS_base"))
  prikkvals <- intersect(getOption("khfunctions.prikkeinfo"), DBI::dbListFields(con, "ALLVIS_base"))
  
  cols <- unique(c(
    parameters$outdimensions,
    parameters$outvalues,
    "SPVFLAGG",
    paste0(qcvals, "_uprikk"),
    prikkvals
  ))
  cols <- as.character(khtools::sql_quote_I(con, cols))
  
  khtools::duckdb_drop_tables(con, "QC")
  sql <- sprintf("CREATE TABLE QC AS SELECT %s FROM ALLVIS_base", paste(cols, collapse = ", "))
  invisible(DBI::dbExecute(con, sql))
  invisible(NULL)
}

#' @title write_cube_output
#' @description
#' Skriver alle outputfiler fra LagKUBE.
#' @noRd
write_cube_output <- function(parameters){
  if(!parameters$write) return(invisible(NULL))
  basepath <- file.path(getOption("khfunctions.root"), getOption("khfunctions.kubedir"))
  name <- parameters$name
  datert_parquet_full <- file.path(basepath, getOption("khfunctions.kube.dat"), "R", paste0(name, "_", parameters$batchdate, ".parquet"))
  allvis_csv <- file.path(basepath, getOption("khfunctions.kube.dat"), "csv", paste0(name, "_", parameters$batchdate, ".csv"))
  allvis_parquet <- file.path(basepath, getOption("khfunctions.kube.dat"), "parquet", paste0(name, "_", parameters$batchdate, ".parquet"))
  qc_parquet <- file.path(basepath, getOption("khfunctions.kube.qc"), paste0("QC_", name, "_", parameters$batchdate, ".parquet"))
  qc_csv <- file.path(basepath, getOption("khfunctions.kube.qc"), paste0("QC_", name, "_", parameters$batchdate, ".csv"))
  
  con <- parameters$duck
  # Skriv KUBE
  khtools::msg("-", datert_parquet_full)
  do_write_output_duckdb(con, source = "KUBE", filepath = datert_parquet_full, format = "parquet")
  
  # Skriv ALLVIS
  khtools::msg("-", allvis_parquet)
  allvis_source <- generate_allvis_select(parameters = parameters)
  do_write_output_duckdb(con, source = allvis_source, filepath = allvis_parquet, format = "parquet")
  
  khtools::msg("-", allvis_csv)
  do_write_output_duckdb(con, source = "ALLVIS", filepath = allvis_csv, format = "csv")
  
  # Skriv QC
  khtools::msg("-", qc_parquet)
  do_write_output_duckdb(con, source = "QC", filepath = qc_parquet, format = "parquet")
  khtools::msg("-", qc_csv)
  do_write_output_duckdb(con, source = "QC", filepath = qc_csv, format = "csv")
}

#' @title generate_allvis_select
#' @description
#' Genererer et uttrekk til ALLVIS, som sørger for riktige kolonnetyper i parquetfilen
#' @noRd
generate_allvis_select <- function(parameters){
  con <- parameters$duck
  qdims <- as.character(khtools::sql_quote_I(con, parameters$outdimensions))
  qvals <- as.character(khtools::sql_quote_I(con, parameters$outvalues))
  dim_expr <- sprintf("CAST(%s AS VARCHAR) AS %s",qdims,qdims)
  val_expr <- sprintf("CAST(%s AS DOUBLE) AS %s",qvals,qvals)
  sprintf("(SELECT %s, %s, CAST(SPVFLAGG AS INTEGER) AS SPVFLAGG FROM ALLVIS)",
          paste(dim_expr, collapse = ",\n"),
          paste(val_expr, collapse = ",\n")
  )
}

# MISC ----

#' @title do_write_output_duckdb
#' @description
#' Skriver outputfiler fra duckdb som parquet eller CSV-format
#' @param con connection
#' @param source kildetabell eller selectuttrykk
#' @param filepath hvor skal filen lagres
#' @param format hvilket format, støtter parquet og csv, default er parquet
#' @noRd
do_write_output_duckdb <- function(con, source, filepath, format = c("parquet", "csv")){
  
  format <- match.arg(format)
  if((format == "parquet" && !grepl("\\.parquet$", filepath, ignore.case = TRUE)) ||
     (format == "csv" && !grepl("\\.csv$", filepath, ignore.case = TRUE))){
    stop("mismatch mellom format og filsti")
  }
  
  options <- switch(
    format,
    parquet = "
      FORMAT PARQUET,
      COMPRESSION ZSTD,
      ROW_GROUP_SIZE 1000000
    ",
    csv = "
      HEADER,
      DELIMITER ';'
    "
  )
  
  invisible(
    DBI::dbExecute(
      con,
      sprintf(
        "COPY %s TO %s (%s)",
        source,
        khtools::sql_quote_S(con, filepath),
        options
      )
    )
  )
  
  invisible(NULL)
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
#' @param dt data to be stored, can be NULL if data is in duckdb
#' @param parameters global parameters
#' @param koblid used when requesting file dumps during processing of original files, default = NULL
#' @param duck TRUE/FALSE, is data to be written located in duckdb
#' @param tablename name of table in duckdb to be stored
#' @examples
#' # LagKUBE("KUBENAVN", dumps = list(PRIKKpre = "STATA", PRIKKpost = c("CSV", "STATA", "R"))
#' # LagFilgruppe("FILGRUPPENAVN", dumps = list(RSYNT1pre = "STATA", KODEBOKpost = c("CSV", "STATA", "R") )
save_filedump_if_requested <- function(dumpname = c("RSYNT1pre", "RSYNT1post", "RESHAPEpre", "RESHAPEpost", "RSYNT2pre", "RSYNT2post",
                                                    "KODEBOKpre", "KODEBOKpost", "RSYNT_PRE_FGLAGRINGpre", "RSYNT_PRE_FGLAGRINGpost",
                                                    "MOVAVpre", "MOVAVpost", "SLUTTREDIGERpre", "SLUTTREDIGERpost", 
                                                    "PRIKKpre", "PRIKKpost", "RSYNT_POSTPROSESSpre", "RSYNT_POSTPROSESSpost"), 
                                       dt = NULL, parameters, koblid = NULL, duck = FALSE, tablename = NULL){
  if(is.null(dumpname) || !dumpname %in% names(parameters$dumps)) return(invisible(NULL))
  if(is.null(dt) && (isFALSE(duck) | is.null(tablename))) stop("Filedump requested, but data not provided or in duckdb")
  format <- parameters$dumps[[dumpname]]
  dumpdir <- file.path(getOption("khfunctions.root"), getOption("khfunctions.dumpdir"))
  filename <- paste0(parameters$name, "_", dumpname)
  if(!is.null(koblid)) filename <- paste0(filename, "_", koblid)
  
  if(is.null(dt) && isTRUE(duck)){
    dt <- khtools::duckdb_fetch_table(con = parameters$duck, tablename = tablename)
  }
    
  if("CSV" %in% format) data.table::fwrite(dt, file = file.path(dumpdir, paste0(filename, ".csv")), sep = ";")
  if("R" %in% format){
    if(!exists("DUMPS", envir = .GlobalEnv)) .GlobalEnv$DUMPS <- list()
    .GlobalEnv$DUMPS[[filename]] <- data.table::copy(dt)
    do_write_parquet(dt = dt, filepath = file.path(dumpdir, paste0(filename, ".parquet")))
  } 
  if("STATA" %in% format){
    dtout <- fix_column_names_pre_stata(dt)
    haven::write_dta(dtout, path = file.path(dumpdir, paste0(filename, ".dta")))
  }
  invisible(gc())
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

# Deprecated ----

