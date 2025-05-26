#' @title find_filedesign
#' @description
#' Finds design parameters for a file
#' 
#' @param file file. If NULL, gets the file from BUFFER[[filename]]. 
#' @param filename filegroup name
#' @param parameters global parameters
#'
#' @keywords internal
#' @noRd
find_filedesign <- function(file = NULL, filename = NULL, parameters){
  if(is.null(file) && is.null(filename)) stop("File or filename must be provided")
  fileparameters <- list(amin = getOption("khfunctions.amin"), amax = getOption("khfunctions.amax"))
  if(!is.null(filename)){
    fileparameters <- parameters$fileinformation[[filename]]
    if(is.null(file)) file <- .GlobalEnv$BUFFER[[filename]]
  }
  designs <- list()
  args <- get_filedesign_args(parameters = parameters, columns_in_file = names(file))
  designs[["observed"]] <- unique(file[, mget(args$design_columns)])
  output <- get_filedesign_initial_list(observeddesign = designs$observed, fileparameters = fileparameters, args = args)
  args[["unconditional"]] <- output$UBeting
  args[["conditional"]] <- c(output$BetingOmk, output$BetingF)
  designs[["full"]] <- do.call(expand.grid.dt, output$Part)[, let(HAR = 0)]
  output[["Design"]] <- designs$full[designs$observed, on = names(designs$observed), let(HAR = 1)]
  output[["SKombs"]] <- set_column_combinations(designs = designs, args = args)
  return(output)
}

#' @noRd
get_filedesign_args <- function(parameters, columns_in_file){
  args <- list()
  args[["part_columns"]] <- parameters$DefDesign$DelKols
  args[["dims_unconditional"]] <- parameters$DefDesign$UBeting
  args[["dims_conditional"]] <- parameters$DefDesign$BetingOmk
  args[["dims_tab"]] <- parameters$DefDesign$BetingF
  args[["design_columns"]] <- parameters$DefDesign$DesignKolsF[parameters$DefDesign$DesignKolsF %in% columns_in_file]
  args[["parts_in_file"]] <- get_file_parts(partcolumns = args$part_columns, designcolumns = args$design_columns)
  return(args)
}

#' @noRd
get_file_parts <- function(partcolumns, designcolumns){
  parts <- character()
  for(part in names(partcolumns)){
    if(all(partcolumns[[part]] %in% designcolumns)) parts <- c(parts, part)
  }
  return(parts)
}

#' @noRd
get_filedesign_initial_list <- function(observeddesign, fileparameters, args){
  designlist <- list()
  designlist[["UBeting"]] <- args$dims_unconditional[args$dims_unconditional %in% args$parts_in_file]
  designlist[["BetingOmk"]] <- args$dims_conditional[args$dims_conditional %in% args$parts_in_file]
  designlist[["BetingF"]] <- args$dims_tab[args$dims_tab %in% args$parts_in_file]
  designlist[["Part"]] <- get_parts_design(designdata = observeddesign, fileparameters = fileparameters, args = args)
  return(designlist)
}

#' @noRd
get_parts_design <- function(designdata, fileparameters, args){
  out <- list()
  for(part in args$parts_in_file){
    columns <- args$part_columns[[part]]
    part_data <- unique(designdata[, ..columns])[, paste0(part, "_HAR") := 1]
    if(part == "A") part_data <- fill_age_interval_gaps(agedata = part_data, agecolumns = columns, fileparameters = fileparameters)
    data.table::setkeyv(part_data, columns)
    out[[part]] <- part_data
  }
  return(out)
}

#' @noRd
fill_age_interval_gaps <- function(agedata, agecolumns, fileparameters){
  age_interval_total <- intervals::Intervals(c(fileparameters$amin, fileparameters$amax), type = "Z")
  age_interval_covered <- intervals::Intervals(agedata[, ..agecolumns], type = "Z")
  age_interval_missing <- intervals::interval_difference(age_interval_total, age_interval_covered)
  if(nrow(age_interval_missing) == 0) return(agedata)
  
  age_interval_missing <- setNames(data.table::as.data.table(age_interval_missing), agecolumns)
  age_interval_missing[, let(A_HAR = 0)]
  return(data.table::rbindlist(list(agedata, age_interval_missing)))
}

#' @noRd
set_column_combinations <- function(designs, args){
  combinations <- list()
  combinations[["bet"]] <- find_combination(part = character(), designs = designs, args = args)
  for(part in args$conditional){
    combinations[[paste0("bet", part)]] <- find_combination(part = part, designs = designs, args = args)
  }
  return(combinations)
}

#' @noRd
find_combination <- function(part, designs, args){
  part_combinations <- c(args$unconditional, part)
  columns <- character()
  if(length(part_combinations) == 0) return(invisible(NULL))
  for(column_part in part_combinations) columns <- c(columns, args$part_columns[[column_part]])
  
  combinations_observed <- unique(designs$observed[, ..columns])
  combinations_full <- unique(designs$full[, ..columns])[, let(HAR = 0)]
  combinations_full[combinations_observed, on = names(combinations_observed), let(HAR = 1)]
  data.table::setkeyv(combinations_full, columns)
  return(combinations_full)
}
