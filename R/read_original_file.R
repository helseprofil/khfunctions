#' @title read_original_file
#' @description
#' Reads the original file, with specific methods to read XLS/XLSX/CSV/SPSS-files, 
#' all producing data.table for further processing. Excel/CSV-files are further formatted
#' according to parameters set inn INNLESARGS (or default values). 
#' - Sets manual header as specified in MANHEADER
#' - Applies special handling if code provided to RSYNT1
#' @param filedescription Row from parameters$read_parameters corresponding to the current file
#' @param parameters filegroup parameters, set by `get_filegroup_parameters()`
#' @param dumps any file dumps requested
#' @returns formatted original file, ready for further processing
read_original_file <- function(filedescription, parameters, dumps = list()){
  # initiate_read_log(filedescription = filedescription, parameters = parameters)
  cat("\n* Starter innlesing av fil")
  read_arg_list <- format_innlesarg_as_list(filedescription$INNLESARG)
  DF <- switch(filedescription$FORMAT,
               "XLS" = do_read_excel(filedescription = filedescription, read_arg_list = read_arg_list),
               "XLSX" = do_read_excel(filedescription = filedescription, read_arg_list = read_arg_list),
               "CSV" = do_read_csv(filedescription = filedescription, read_arg_list = read_arg_list),
               "SPSS" = do_read_spss(filedescription = filedescription))
  DF <- set_manheader(file = DF, manheader = filedescription$MANHEADER)
  names(DF) <- trimws(names(DF))
  names(DF)[names(DF) == ""] <- paste("C", which(names(DF) == ""), sep = "")
  
  if ("RSYNT1pre" %in% names(dumps)) DumpTabell(DF, paste(filedescription$FILGRUPPE, filedescription$KOBLID, "RSYNT1pre", sep = "_"), format = dumps[["RSYNT1pre"]])
  if(is_not_empty(filedescription$RSYNT1)){
    DF[, let(filgruppe = filedescription$FILGRUPPE, delid = filedescription$DELID, tab1_innles = filedescription$TAB1)]
    DF <- do_special_handling(dt = DF, code = filedescription$RSYNT1, batchdate = parameters$batchdate, stata_exe = parameters$StataExe, DTout = TRUE)
    extracols <- grep("^(filgruppe|delid|tab1_innles)$", names(DF), value = T)
    if(length(extracols) > 0) DF[, (extracols) := NULL]
  }
  if ("RSYNT1post" %in% names(dumps)) DumpTabell(DF, paste(filbesk$FILGRUPPE, filbesk$KOBLID, "RSYNT1post", sep = "_"), format = dumps[["RSYNT1post"]])
  return(DF)
}

#' @title format_innlesarg_as_list
#' @description
#' Splitter args ved komma, beholder uttrykk i hermetegn eller c()
#' @param args INNLESARG
#' @returns a list of argument name-value pairs, or empty list if no INNLESARG provided
format_innlesarg_as_list <- function(args){
  if(is_empty(args)) return(list())
  splitargs <- unlist(regmatches(args, gregexpr('(\\w+=(?:\"[^\"]*\"|c\\([^\\)]*\\)|[^,]+)|[^,]+)(,|$)', args, perl = TRUE)))
  splitargs <- gsub(",$", "", splitargs)
  valid_args <- grepl("^\\w+=.+", splitargs)
  if (!all(valid_args)) {
    invalid_args <- splitargs[!valid_args]
    stop(paste("Feil i INNLESARG: argumenter uten verdi funnet:", paste(invalid_args, collapse = ", ")))
  }
  arglist <- lapply(splitargs, function(x){
    x <- strsplit(x, "=")[[1]]
    name = trimws(x[1])
    value = trimws(x[2])
    if(grepl("^\"", value) && grepl("\"$", value)) value <- gsub("^\"|\"$", "", value)
    if(grepl("TRUE|FALSE", value)) value <- as.logical(value)
    if(!is.logical(value) && !is.na(suppressWarnings(as.numeric(value)))) value <- as.numeric(value)
    if (grepl("^c\\(", value) && grepl("\\)$", value)) {
      value <- gsub("^c\\(|\\)$", "", value)
      value <- as.numeric(strsplit(value, ",")[[1]])
    }
    return(setNames(list(value), name))
  })
  arglist <- unlist(arglist, recursive = F)
  return(arglist)
}

#' @noRd
do_read_spss <- function(filedescription){
  file <-try(foreign::read.spss(file = filedescription$filepath, use.value.labels = FALSE, max.value.labels = 0, as.data.frame = T), silent = T)
  if("try-error" %in% class(file)) stop("Error when reading file: ", filedescription$FILNAVN)
  data.table::setDT(file)
  return(file)
}

#' @noRd
do_read_csv <- function(filedescription, read_arg_list){
  # Change encoding = "latin1" (works for read.csv) to "Latin-1" (works for fread)
  if(is_not_empty(read_arg_list$encoding) && read_arg_list$encoding == "latin1") read_arg_list$encoding <- "Latin-1"
  sep <- ifelse("sep" %in% names(read_arg_list), read_arg_list$sep, ";")
  encoding <- ifelse("encoding" %in% names(read_arg_list), read_arg_list$encoding, "unknown")
  file <- try(data.table::fread(file = filedescription$filepath, header = FALSE, skip = 0, colClasses = "character", sep = sep, encoding = encoding))
  if("try-error" %in% class(file)) stop("Error when reading file: ", filedescription$FILNAVN)
  format_arg_list <- c(list(file = file, filedescription = filedescription), read_arg_list)
  file <- do.call(format_excel_and_csv_files, format_arg_list)
  return(file)
}

#' @noRd
do_read_excel <- function(filedescription, read_arg_list){
  sheets <- gsub("\'|\\$", "", readxl::excel_sheets(filedescription$filepath))
  sheet <- sheets[1]
  if(is_not_empty(read_arg_list$ark)){
    sheet <- find_correct_excel_sheet(sheet = read_arg_list$ark, sheets = sheets, filename = filedescription$FILNAVN)
  }
  
  file <- suppressMessages(try(readxl::read_excel(filedescription$filepath, sheet = sheet, col_names = FALSE, col_types = "text", skip = 0, na = "NA")))
  if("try-error" %in% class(file)) stop("Error when reading file: ", filedescription$FILNAVN)
  data.table::setDT(file)
  file <- do.call(format_excel_and_csv_files, c(list(file = file, filedescription = filedescription), read_arg_list))
  return(file)
}

#' @noRd
find_correct_excel_sheet <- function(sheet, sheets, filename){
  if(sheet %in% sheets) return(sheet)
  candidate <- sheets[grepl(sheet, sheets, ignore.case = TRUE)]
  if(length(candidate) == 1) return(candidate)
  if(length(candidate) > 1) stop("Feil i lesing av fil: ", filename, "\nArknavn '", sheet, "' passer med flere av (", paste(sheets, collapse = ","), ")", sep = "")
  if(length(candidate) == 0) stop("Feil i lesing av fil: ", filename, "\nArknavn '", sheet, "' finnes ikke!", sep = "")
}

#' @title format_excel_and_csv_files
#' @description
#' Formats excel and csv file further according to INNLESING::INNLESARG
#' @noRd
format_excel_and_csv_files <- function(file, filedescription, ...){
  read_arg_list <- list(...)
  default_args <- list(slettRader = numeric(),
                       skip = 0,
                       sisteRad = numeric(), 
                       header = TRUE, 
                       TomRadSlutt = FALSE, 
                       FjernTommeRader = FALSE, 
                       FjernTommeKol = TRUE)
  args <- modifyList(default_args, read_arg_list)
  
  data.table::setnames(file, names(file), excelcols()[1:ncol(file)])
  if(is_not_empty(filedescription$UNDERTABLOK)) file <- handle_undertablok(file = file, filedescription = filedescription)
  if(length(args$slettRader) > 0) file <- file[-args$slettRader]
  if(args$skip > 0) file <- file[-(1:args$skip)]
  if(length(args$sisteRad) > 0) file <- file[1:(args$sisteRad - args$skip - length(args$slettRader)), ]
  empty_rows <- file[rowSums(is.na(file[, .SD]) | file[, .SD] == "") == ncol(file), .I]
  if(args$TomRadSlutt && length(empty_rows) > 0) file <- file[1:empty_rows[1]-1]
  if(args$FjernTommeRader && length(empty_rows) > 0) file <- file[-empty_rows]
  empty_cols <- names(file)[sapply(file, function(col) all(is.na(col) | col == ""))]
  if(args$FjernTommeKol){
    file <- file[, .SD, .SDcols = setdiff(names(file), empty_cols)]
    data.table::setnames(file, names(file), excelcols()[1:ncol(file)])
  }
  
  if(is_not_empty(filedescription$MULTIHEAD)){
    file <- handle_multihead(file = file, args = args, filedescription = filedescription)
  } else if(args$header){
    file <- set_file_header_from_firstrow(file = file, filedescription = filedescription)
  }
  if(is_not_empty(filedescription$UNDERTABLOK)) {
    data.table::setnames(file, old = ncol(file), new = gsub("^(.*?):.*", "\\1", filedescription$UNDERTABLOK))
  }
  data.table::setnames(file, names(file), trimws(names(file)))
  return(file)
}

#' @noRd
set_file_header_from_firstrow <- function(file, filedescription){
  if(nrow(file) <= 1) stop("header = TRUE er default, men filen har bare en rad. Skal header = FALSE i INNLESARG eller er det noe annet galt med filen?\n",
                           filedescription$FILNAVN)
  firstrow <- as.character(file[1, .SD])
  nonempty <- which(firstrow != "")
  data.table::setnames(file, old = nonempty, new = firstrow[nonempty]) 
  file <- file[-1]
  return(file)
}

#' @title set_manheader
#' @description
#' Manually sets headers according to parameters set in INNLESING::MANHEADER
#' @noRd
set_manheader <- function(file, manheader){
  if(is_empty(manheader)) return(file)
  manheader_split <- trimws(unlist(strsplit(manheader, "=")))
  old <- format_colname_string_as_vector(string = manheader_split[1], old_new = "old", file = file)
  new <- format_colname_string_as_vector(string = manheader_split[2], old_new = "new", file = file)
  if(length(old) != length(new)) stop("Feil i MANHEADER: Ulikt antall kolonner angitt på hver side av '='")
  data.table::setnames(file, old = old, new = new)
  return(file)
}

#' @noRd
format_colname_string_as_vector <- function(string, old_new = c("old", "new"), file){
  if(grepl("^\\[", string)) string <- gsub("^\\[|\\]$", "", string)
  if(grepl("^c\\(", string)) string <- gsub("^c\\(|\\)$", "", string)
  if(grepl("\"", string)) string <- gsub("\"", "", string)
  colnames <- trimws(strsplit(string, ",")[[1]])
  if(old_new == "new") return(colnames)
  
  numeric <- suppressWarnings(as.numeric(colnames))
  if(all(!is.na(numeric))){
    if(min(numeric) <= 0 && max(numeric) > ncol(file)) stop("Feil i MANHEADER: Angitt kolonnenummer eksisterer ikke i filen")
    return(numeric)
  }
  if(all(is.na(numeric))){
    if(!all(colnames %in% names(file))) stop("Feil i MANHEADER: Minst ett angitt gammelt kolonnenavn [", paste(colnames, collapse = ", "), "] eksisterer ikke i filen")
    return(colnames)
  }
  stop("Feil i MANHEADER: både kolonnenavn og posisjon angitt som gamle kolonnenavn")
}

#' @noRd
#' @description
#' Empties previous record in log for the current file, and initiate the new record
initiate_read_log <- function(filedescription, parameters){
  RODBC::sqlQuery(parameters$log, paste0("DELETE * FROM INNLES_LOGG WHERE KOBLID=", filedescription$KOBLID, "AND SV='S'"))
  RODBC::sqlQuery(parameters$log, paste0("INSERT INTO INNLES_LOGG ( KOBLID,BATCH, SV, FILGRUPPE) SELECT =", filedescription$KOBLID, ",'", parameters$batchdate, "', 'S','", filedescription$FILGRUPPE, "'"))
  return(invisible(NULL))
}

#' @title excelcols
#' @return default excel headers
excelcols <- function(){
  single <- LETTERS
  double <- sapply(single, paste0, single)
  triple <- sapply(double, paste0, single)
  c(single, double, triple)
}


# TO BE DELETED ----
#' @noRd
#' @description
#' sannsynligvis overflødig
handle_multihead <- function(file, args, filedescription){
  # Bruk av flernivaa header. Ikke saerlig elegant syntaks, men prinsippet er rett fram
  # Disse pastes (evt) sammen til en header. Etter reshape splittes kolonneraden (som naa har blitt en kolonne)
  # i sine respektive kolonner. Kan ogsaa vaere pastet sammen originalt
  # Syntaks gir radnummer for de ulike leddene i multihead "c(TABNAVN1=rad1,TABNAVN2=rad2,...)
  # Prossesser parameterstreng for multihead, gir liste med relevante deler
  mhl <- LesMultiHead(filedescription$MULTIHEAD)
  # Juster radnummerering for skip
  mhl$rader <- mhl$rader - args$skip
  headers <- file[mhl$rader]
  headers[headers == ""] <- NA
  # Fyll inn ved "sparse" utfylling, slik som ved "innrykket" tabulering i kolonner
  headers <- zoo::na.locf(t(headers), na.rm = FALSE)
  headstr <- apply(headers, 1, paste, collapse = mhl$sep)
  # Sett nye kolonnenavn for de som dekkes av headstr, resten beholder sine standard ("excel") genererte navn
  nonempty <- as.vector(which(headstr != ""))
  names(file)[nonempty] <- headstr[nonempty]
  file <- file[-(1:length(mhl$rader))] # Dropp linjer brukt til header
  return(file)
}

#' LesMultiHead (kb)
#'
#' @param mhstr 
LesMultiHead <- function(mhstr) {
  # Leser parameterstreng for multihead og gjoer om til relevante variable
  # Velger aa kalle paa denne funksjonen ved behov for samme inputstreng heller enn aa porssessere strengen en gang og sende bitene rundt
  # Finn evt angitt separator (trengs bare settes dersom det er snakk om en originalt pastet rad med annen seaprator enn "|"
  # is_kh_debug()
  
  if (grepl("sep=\".\"", mhstr)) {
    sep <- sub(".*,sep=\"(.)\"", "\\1", mhstr)
    mhstr <- sub("(.*),sep=\".\"", "\\1", mhstr)
  } else {
    sep <- "&"
  }
  # Les inn rader som inneholder deler
  eval(parse(text = paste("mh<-c(", mhstr, ")")))
  colnames <- names(mh)
  # Sett paste av tabnavn. Denne blir senere splitta til kolonnenavn
  varname <- paste(names(mh), collapse = "_")
  # Fjern rader som er duplikater, dvs som allerede er pastet sammen originalt
  rader <- mh[!duplicated(mh)]
  return(list(rader = rader, sep = sep, colnames = colnames, varname = varname))
}

#' @noRd
handle_undertablok <- function(file, filedescription){
  # test med filgruppe UFORE, som er eneste som bruker denne
  utl <- unlist(stringr::str_split(filedescription$UNDERTABLOK, ":"))
  loks <- as.numeric(unlist(stringr::str_split(utl[3], ",")))
  offsets <- as.numeric(unlist(stringr::str_split(utl[4], ",")))
  nytab <- character(nrow(file))
  nytab[loks + offsets] <- file[loks, as.numeric(utl[2])]
  nytab[nytab == ""] <- NA
  nytab <- zoo::na.locf(nytab, na.rm = FALSE)
  file[, nytab := nytab]
  return(file)
}
