#' @title make_table_from_original_file
#' @description
#' Read and formats an original file before stacking
make_table_from_original_file <- function(file_number, codebooklog, parameters){
  dumps = parameters$dumps
  report_filegroup_progress(file_number = file_number, parameters = parameters)
  filedescription <- parameters$read_parameters[file_number]
  filecolumns <- identify_columns_in_file(filedescription = filedescription)
  DF <- read_original_file(filedescription = filedescription, parameters = parameters, dumps = dumps)
  
  give_columns_default_names(dt = DF, filedescription = filedescription, defcolumns = filecolumns$have)
  do_handle_fylltab(dt = DF, filedescription = filedescription)
  do_handle_kastkols(dt = DF, filedescription = filedescription)  
  do_reshape_var(dt = DF, filedescription = filedescription, dumps = dumps)
  do_split_multihead(dt = DF, filedescription = filedescription)
  if ("RSYNT2pre" %in% names(dumps)) DumpTabell(DF, paste(filedescription$FILGRUPPE, filedescription$KOBLID, "RSYNT2pre", sep = "_"), format = dumps[["RSYNT2pre"]])
  DF <- do_special_handling(dt = DF, code = filedescription$RSYNT2, batchdate = parameters$batchdate, stata_exe = parameters$StataExe, DTout = FALSE)
  if ("RSYNT2post" %in% names(dumps)) DumpTabell(DF, paste(filedescription$FILGRUPPE, filedescription$KOBLID, "RSYNT2post", sep = "_"), format = dumps[["RSYNT2post"]])
  give_columns_default_names(dt = DF, filedescription = filedescription, defcolumns = filecolumns$have)
  do_set_default_values(dt = DF, filedescription = filedescription, defaultcolumns = filecolumns$default)
  check_if_all_columns_exist(dt = DF, filecolumns = filecolumns)
  DF[, names(.SD) := NULL, .SDcols = names(DF)[!names(DF) %in% c(getOption("khfunctions.kolorgs"), "LEVEL")]]
  convert_all_columns_to_character(dt = DF)
  cat("\n* Innlesing OK!")
  do_aggregate_if_grunnkrets(dt = DF, filedescription = filedescription, parameters = parameters) # DEPRECATED?
  do_convert_na_to_empty(dt = DF)
  DF[, let(KOBLID = as.character(filedescription$KOBLID))]
  
  DF <- recode_columns_with_codebook(dt = DF, filedescription = filedescription, parameters = parameters, codebooklog = codebooklog, dumps = dumps)
  do_recode_tknr(dt = DF, tknr = filedescription$TKNR, parameters = parameters)
  do_recode_soner_4(dt = DF, filedescription = filedescription)
  return(DF)
}

#' @title report_filegroup_progress
#' @noRd
report_filegroup_progress <- function(file_number, parameters){
  n_files <- parameters$n_files
  filename <- parameters$read_parameters[file_number]$FILNAVN
  cat("\n", file_number, "/", n_files, ": ", filename, sep = "")
}

#' @title identify_columns_in_file
#' @description
#' Reads filedescription and indentify default columns existing in the file and columns that 
#' should be filled with default values. 
#' In INNLESING, a column name indicates that an existing column should be given a default name, and
#' values within <..> shuld be set as the default value for columns not existing in the file.
#' @param filedescription filedescription
#' @returns list of columns existing and columns to be given default values
#' @noRd
identify_columns_in_file <- function(filedescription){
  cols <- getOption("khfunctions.kolorgs")
  cols_in_file <- cols[grepl("^[^-<]", filedescription[, ..cols])]
  cols_with_default_value <- cols[grep("^<.*>", filedescription[, ..cols])]
  return(list(have = cols_in_file, default = cols_with_default_value))
}

#' @title give_columns_default_name
#' @description
#' Renames columns to GEO, AAR, KJONN, ALDER, UTDANN, INNVKAT, LANDBAK, TAB1:3, VAL1:3
#' Use information from INNLESING to check which columns should be renamed to standard names. 
#' Renaming happens by reference, no need to reassign data. 
#' @noRd
give_columns_default_names <- function(dt, filedescription, defcolumns){
  oldnames <- setNames(as.character(filedescription[, ..defcolumns]), defcolumns)
  renamecols <- oldnames[oldnames %in% names(dt)]
  data.table::setnames(dt, renamecols, names(renamecols))
}

#' @title do_handle_kastkols
#' @description
#' Remove columns as provided in INNLESING::KASTKOLS
#' @noRd
do_handle_kastkols <- function(dt, filedescription){
  if(is_empty(filedescription$KASTKOLS)) return(invisible(NULL))
  remove <- gsub("^c\\(|\\)$", "", filedescription$KASTKOLS)
  remove <- as.numeric(unlist(strsplit(remove, ",")))
  dt[, (remove) := NULL]
}

#' @title do_reshape_var
#' @description
#' Reshapes the data to collect columns representing the same variable into long format
#' @noRd
do_reshape_var <- function(dt, filedescription, dumps){
  if("RESHAPEpre" %in% names(dumps)) DumpTabell(dt, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "RESHAPEpre", sep = "_"), format = dumps[["RESHAPEpre"]])
  on.exit({
    if("RESHAPEpost" %in% names(dumps)) DumpTabell(dt, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "RESHAPEpost", sep = "_"), format = dumps[["RESHAPEpost"]])
  })
  if(is_empty(filedescription$RESHAPEvar)) return(invisible(NULL))
  
  cols <- get_reshape_parameters(filedescription = filedescription)
  if(!is.null(cols$id) && !all(cols$id %in% names(dt))) stop("Feil i RESHAPE: Kolonner angitt i RESHAPEid ikke funnet")
  if(!is.null(cols$measure) && !all(cols$measure %in% names(dt))) stop("Feil i RESHAPE: Kolonner angitt i RESHAPEmeas ikke funnet")
  if(!is.null(cols$id) && is.null(cols$measure)) stop("Feil i RESHAPE: Både RESHAPEid og RESHAPEmeas er tomme")
  reshape <- data.table::melt(dt, id.vars = cols$id, measure.vars = cols$measure, variable.name = cols$var, value.name = cols$val)
  dt[, names(dt) := NULL]
  dt[, (names(reshape)) := reshape]
  convert_all_columns_to_character(dt = dt)
}

#' @title get_reshape_parameters
#' @description
#' Identifies a list of columns needed for reshape
#' @noRd
get_reshape_parameters <- function(filedescription){
  id <- measure <- NULL
  if(is_not_empty(filedescription$RESHAPEid)) id <- gsub("\"", "", strsplit(filedescription$RESHAPEid, ",")[[1]])
  if(is_not_empty(filedescription$RESHAPEmeas)) measure <- gsub("\"", "", strsplit(filedescription$RESHAPEmeas, ",")[[1]])
  val <- ifelse(is_not_empty(filedescription$RESHAPEval), as.character(filedescription$RESHAPEval), "value")
  var <- data.table::fcoalesce(ifelse(is_not_empty(filedescription$MULTIHEAD), LesMultiHead(filedescription$MULTIHEAD)$varname, NA_character_),
                               ifelse(is_not_empty(filedescription$RESHAPEvar), as.character(filedescription$RESHAPEvar), NA_character_),
                               "variable")
  out <- list(id = id, measure = measure, var = var, val = val)
  out <- lapply(out, trimws)
  return(out)
}  

#' @title do_set_default_values
#' @description
#' Sets default values for columns where the default value are provided in ACCESS::INNLESING within <...>
#' @noRd
do_set_default_values <- function(dt, filedescription, defaultcolumns){
  default <- filedescription[, ..defaultcolumns]
  default[, names(.SD) := lapply(.SD, function(x) sub("^<(.*)>$", "\\1", x))]
  dt[, names(default) := default]
}

#' @title check_if_all_columns_exist
#' @description
#' Checks if obligatory columns GEO, AAR, Val1, and values defined in  exists
#' @param dt data
#' @param filecolumns list of columns present in file and columns to be given default value
#' @noRd
check_if_all_columns_exist <- function(dt, filecolumns){
  oblig <- c("GEO", "AAR", "VAL1")
  have <- filecolumns$have
  default <- filecolumns$default
  if(!all(oblig %in% names(dt))) stop("Feil i innlesing: Kolonnene <", oblig[!(oblig %in% names(dt))], "> finnes ikke\n")
  if(!all(have %in% names(dt))) stop("Feil i innlesing: Kolonnene <", have[!(have %in% names(dt))], "> finnes ikke\n")
  if(!all(default %in% names(dt))) stop("Feil i innlesing: Kolonnene <", default[!(default %in% names(dt))], "> skulle fått default verdi, men finnes ikke\n")
  return(invisible(NULL))
}

#' @title convert_all_columns_to_character
#' @description
#' Make sure all columns are of type character
#' @param dt data
#' @noRd
convert_all_columns_to_character <- function(dt){
  non_char_cols <- names(dt)[!sapply(dt, is.character)]
  if(length(non_char_cols) > 0) dt[, names(.SD) := lapply(.SD, as.character), .SDcols = non_char_cols]
}

do_convert_na_to_empty <- function(dt){
  dt[, names(.SD) := lapply(.SD, function(x) data.table::fifelse(is.na(x), "", x))]
}

#' @title initiate_codebook_log
#' @description initiates an empty codebooklog
#' @noRd
initiate_codebooklog <- function(nrow = 0){
  columns <- c("KOBLID", "DELID",  "FELTTYPE", "ORG", "KBOMK", "OMK", "FREQ")
  log <- data.table::setDT(as.list(setNames(rep(NA_character_, length(columns)), columns)))
  if(nrow == 0) return(log[0])
  return(log[1:nrow])
}

#' @title update_codebooklog
#' @description updates codebooklog by reference
#' @noRd
update_codebooklog <- function(codebooklog, recodelog){
  updated_codebooklog <- data.table::rbindlist(list(codebooklog, recodelog))
  codebooklog[, names(codebooklog) := NULL][, names(updated_codebooklog) := updated_codebooklog]
}

# TO BE DELETED ---- 

#' @description
#' Sannsynligvis aldri i bruk (bare i gamle STATBANK/INNVAND-filer), kan kanskje pensjoneres
#' @keywords deprecate
#' @noRd
merge_geo_d2 <- function(dt, filedescription){
  # Merge GEO delt i to
  if (filbesk$GEOd2 != "-" & !is.na(filbesk$GEOd2)) {
    DF[, filbesk$GEOd2] <- gsub("^(\\d|\\d{3})$", "0\\1", DF[, filbesk$GEOd2]) # setter på ledende 0
    DF$GEO <- paste(DF$GEO, DF[, filbesk$GEOd2], sep = "") # limer sammen GEO og GEOd2-kolonnen
  }
}

#' @description
#' Sannsynligvis aldri i bruk, kan kanskje pensjoneres
#' @keywords deprecate
#' @noRd
do_split_multihead <- function(dt, filedescription){
  if(is_empty(filedescription$MULTIHEAD)) return(invisible(NULL))
  mhl <- LesMultiHead(filedescription$MULTIHEAD)
  dt[, (mhl$colnames) := data.table::tstrsplit(mhl$varname, mhl$sep)]
}

#' @title do_handle_fylltab
#' @description
#' Fills columns according to information provided in INNLESING::FYLLTAB
#' @noRd
do_handle_fylltab <- function(dt, filedescription){
  if(is_empty(filedescription$FYLLTAB)) return(invisible(NULL))
  cols <- trimws(strsplit(filedescription$FYLLTAB, ",")[[1]])
  if(!all(cols %in% names(dt))) stop("Feil i FYLLTAB: ", paste0("Kolonner ", paste(cols[!cols %in% names(dt)], collapse = ","), " finnes ikke"))
  
  for(col in cols){
    dt[get(col) == "", col := NA]
    dt[, names(.SD) := zoo::na.locf(.SD, na.rm = FALSE), .SDcols = col]
  }
}

#' @title do_aggregate_if_grunnkrets
#' @description
#' If original data is provided on grunnkrets level, aggregate
#' Potentially deprecated, not needed if parameter INNLESING::GRUNNKRETS
#' is not active.
#' @noRd
do_aggregate_if_grunnkrets <- function(dt, filedescription, parameters){
  if(is_empty(filedescription$GRUNNKRETS) || filedescription$GRUNNKRETS != 1) return(invisible(NULL))
  cat("\n* Aggregerer fra grunnkrets...")
  colorder <- names(dt)
  aggregate <- collapse::join(dt, parameters$GkBHarm, how = "l", on = c("GEO" = "GK"), verbose = 0)
  aggregate[is.na(Bydel2004), Bydel2004 := paste(substr(GEO, 1, 4), "00", sep = "")]
  aggregate[, let(GEO = Bydel2004, Bydel2004 = NULL, FRA = NULL, TIL = NULL)]
  valcols <- names(aggregate)[grepl("^VAL\\d$", names(aggregate))]
  tabcols <- names(aggregate)[!names(dt) %in% valcols]
  aggregate[, names(.SD) := lapply(.SD, as.numeric), .SDcols = valcols]
  g = collapse::GRP(dt, tabcols)
  aggregate <- collapse::add_vars(g[["groups"]], collapse::fsum(collapse::get_vars(aggregate, valcols), g = g))
  dt[, names(dt):= NULL]
  dt[, names(aggregate) := aggregate]
  convert_all_columns_to_character()
  data.table::setcolorder(dt, colorder)
}
