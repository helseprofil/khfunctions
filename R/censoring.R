#' @keywords internal
#' @noRd
do_censor_cube <- function(dt, parameters){
  save_filedump_if_requested(dumpname = "PRIKKpre", dt = dt, parameters = parameters)
  on.exit({save_filedump_if_requested(dumpname = "PRIKKpost", dt = dt, parameters = parameters)}, add = TRUE)
  if(is_empty(parameters$Censor_type)) return(dt)
  
  if(parameters$Censor_type == "R"){
    cat("\n* Prikker data (NY R-prikking)")
    do_censor_primary_secondary(dt = dt, parameters = parameters)
    # do_censor_kube_r(dt = dt, parameters = parameters)
  }
  if(parameters$Censor_type == "STATA"){
    cat("\n* Prikker data (Ny R-prikking som overtar for STATA-prikking)")
    # dims <- find_dims_for_stataprikk(dt = dt, etabs = parameters$etabs)
    # save_kubespec_csv(spec = parameters$CUBEinformation, dims = dims, geonaboprikk = parameters$geonaboprikk, geoprikktriangel = get_geonaboprikk_triangles())
    dt <- do_censor_primary_secondary(dt = dt, parameters = parameters)
    # if("spv_tmp" %in% names(dt)) dt[, spv_tmp := NULL] # must delete if old stata censoring is to be used
    # dt <- do_censor_kube_stata(dt = dt, parameters = parameters)
  }
  return(dt)
}

#' @title do_censor_primary_secondary
#' @description 
#' - 1. Primary censoring 
#' - 2. Serie censoring (weak or high proportion of primary censoring)
#' - 3. Secondary censoring
#' @param dt cube
#' @param parameters parameters
#' @keywords internal
#' @noRd
do_censor_primary_secondary <- function(dt, parameters){
  dt[, let(pvern = 0L, serieprikket = 0L)]
  
  limits <- get_censor_limits(spec = parameters$CUBEinformation)
  alltriangles <- get_censor_triangles(parameters = parameters)
  dims <- intersect(c(getOption("khfunctions.khtabs"), parameters$etabs$tabnames), names(dt))
  data.table::setkeyv(dt, c(dims))
  warn_if_special_triangles(alltriangles = alltriangles)
  
  cat("\n* Starter personvernhåndtering")
  do_censor_primary(dt = dt, limits = limits)
  do_censor_serie(dt = dt, limits = limits, dims = dims)
  
  cat("\n* NABOPRIKKING på:", names(alltriangles), "\n")
  do_naboprikk(dt = dt, alltriangles = alltriangles, limits = limits, dims = dims)
  valuesF <- paste0(get_value_columns(names(dt)), ".f")
  dt[spv_tmp %in% c(3,4), (valuesF) := 3] # Disse brukes Foreløpig til å sette spvflagg. Disse kan endres i postprosess.
}

#' @description Written with assistance from copilot
#' @keywords internal
#' @noRd
do_naboprikk <- function(dt, alltriangles, limits, dims){
  dt[, skal_naboprikkes := !(spv_tmp %in% c(2,9))]
  nyeprikker <- 1L
  iteration  <- 1L
  max_serie_rounds <- 3L
  force_runde <- TRUE
  dt[, rid__ := .I]
  if(is_empty(limits$TELLER)) limits[["TELLER"]] <- -1
  if(is_empty(limits$NEVNER)) limits[["NEVNER"]] <- -1
  
  # Forhåndsberegn strata per dimensjon (inkluderer AAR automatisk fordi dims inneholder AAR)
  strata_cols <- setNames(paste0("strata__", names(alltriangles)), names(alltriangles))
  for (censordim in names(alltriangles)){
    otherdims <- setdiff(dims, censordim)     # AAR er med her dersom det ligger i dims
    sc <- strata_cols[[censordim]]
    dt[skal_naboprikkes == TRUE, (sc) := .GRP, by = otherdims]
    data.table::setindexv(dt, c(sc, censordim))  # indeks, ikke key
  }
  
  while (nyeprikker > 0L || force_runde){
    itcol <- paste0("naboprikketIomg", iteration)
    dt[, (itcol) := 0L]
    
    for(censordim in names(alltriangles)){
      triangles <- alltriangles[[censordim]]
      sc <- strata_cols[[censordim]]
      cd <- censordim
      
      for(i in seq_along(triangles)){
        tri <- triangles[[i]]
        tri_levels_base <- if(is.atomic(tri)){
          tri
        } else {
          tri$base
        }
        
        if (length(tri_levels_base) == 0L) next
        
        in_triangle <- if(is.character(dt[[cd]])){
          dt[[cd]] %chin% tri_levels_base
        } else {
          dt[[cd]] %in% tri_levels_base
        } 
        
        # 1) Problemstrata:
        #    - antpvern og sumtellerprikk teller KUN pvern==1 (serieprikk teller ikke her)
        #    - minst to tilgjengelige kandidater (pvern==0) må finnes
        stats <- dt[skal_naboprikkes == TRUE & in_triangle, .(
          antpvern       = sum(pvern == 1L, na.rm = TRUE),
          sumtellerprikk = sum(sumTELLER[pvern == 1L], na.rm = TRUE),
          sumnevnerprikk = sum(sumNEVNER[pvern == 1L], na.rm = TRUE),
          ant_available  = sum(pvern == 0L, na.rm = TRUE)
        ), by = sc]
        
        prob <- stats[antpvern == 1L | (antpvern > 1L & (sumtellerprikk <= limits$TELLER | sumnevnerprikk <= limits$NEVNER) & ant_available >= 2L), get(sc)]
        if (length(prob) == 0L) next
        
        # 2) Prioritetskart for trekanten: NAVN-basert oppslag (kritisk når nivåene er numeric)
        #    Lavest prioritet = sist i tri_levels => høyest rank-verdi
        # rank_map <- setNames(seq_along(tri_levels), as.character(tri_levels))
        # pri_vec  <- unname(rank_map[as.character(dt[[cd]])])
        
        # 3) Serie-kandidat (pvern==0 & serieprikket==1): velg ÉN per strata, lavest prioritet
        ser <- dt[in_triangle & get(sc) %in% prob & pvern == 0L & serieprikket == 1L,
                  {
                    tri_levels_sc <- order_for_sc(tri, .SD)
                    rank_map_sc <- setNames(seq_along(tri_levels_sc), as.character(tri_levels_sc))
                    pri <- unname(rank_map_sc[as.character(get(cd))]) # ta ut prioritet for disse radene
                    if (all(is.na(pri))) {
                      .(rowid = integer())
                    } else {
                      k <- which.max(pri)
                      .(rowid = rid__[k])
                    }
                  },
                  by = sc
        ]
        
        serierowids <- if(nrow(ser) > 0L){
          unique(ser$rowid)
        } else {
          integer()
        }
        
        noncensoredrowids <- integer()
        # Strata som fortsatt trenger kandidat (ingen serie funnet, etter runder som bare får bruke serieprikker)
        if(iteration > max_serie_rounds){
          target <- if(nrow(ser) > 0L){
            setdiff(prob, ser[[sc]])
          } else {
            prob
          }
          
          if(length(target) > 0L){
            nonc_dt <- dt[in_triangle & get(sc) %in% target & pvern == 0L & (is.na(serieprikket) | serieprikket == 0L),
                          {
                            tri_levels_sc <- order_for_sc(tri, .SD)
                            rank_map_sc <- setNames(seq_along(tri_levels_sc), as.character(tri_levels_sc))
                            .(rowid = rid__, pri = unname(rank_map_sc[as.character(get(cd))]))
                          },
                          by = sc
            ][order(get(sc), -pri)]
            
            if (nrow(nonc_dt) > 0L) {
              noncensoredrowids <- nonc_dt[, .SD[1L], by = sc]$rowid
            }
          }
        }
        
        # 5) Oppdater i én batch for denne trekanten
        rows_to_censor <- unique(c(serierowids, noncensoredrowids))
        if (length(rows_to_censor) > 0L) {
          dt[rows_to_censor, let(pvern = 1L, spv_tmp = 3L)]
          dt[rows_to_censor, (itcol) := 1L]
          
          
          newly_resolved <- unique(dt[rows_to_censor, get(sc)])
          # resolved_sid <- union(resolved_sid, newly_resolved)
        }
      }
    }
    nyeprikker <- dt[, sum(get(itcol))]
    onlyserie <- ifelse(iteration <= max_serie_rounds, " (bare serieprikker brukt)", "")
    cat(paste0("\n** Antall nye prikker i runde ", iteration, ": ", nyeprikker, onlyserie, "\n"))
    force_runde <- iteration <= max_serie_rounds
    iteration <- iteration + 1L
  }
  dt[, (unname(strata_cols)) := NULL]
  dt[, let(rid__ = NULL, skal_naboprikkes = NULL)]
}

#' @title do_censor_serie
#' @description
#' Serieprikker på grensene som er satt i options anon_svakandel og anon_hullandel
#' Flagg 9 (finnes ikke eller kan ikke beregnes pga missing inn) holdes utenfor.
#' DEV:
#' Må håndtere evt spesialverdier som skal aksepteres, de kan ikke regnes som prikket her heller.
#' @keywords internal
#' @noRd
do_censor_serie <- function(dt, limits, dims){
  seriedims <- setdiff(dims, "AAR")
  primary_limit <- getOption("khfunctions.anon_hullandel")
  weak_limit <- getOption("khfunctions.anon_svakandel")
  helper_columns <- c("weak", "propweak", "propprimary")
  dt[, (helper_columns) := 0]
  
  dt[sumTELLER <= limits$STATTOL, let(weak = 1L)]
  dt[, let(propweak = mean(weak, na.rm = T), propprimary = sum(pvern, na.rm = T)/.N), by = seriedims]
  
  dt[spv_tmp == 0 & (propweak > weak_limit | propprimary > primary_limit), let(serieprikket = 1, spv_tmp = 4)]
  cat(paste0("\n** Serieprikker ", dt[serieprikket == 1, .N], " rader fordi tidsserien har en andel personvernprikker > ", primary_limit, 
             " eller at andelen sumTELLER <= ", limits$STATTOL, " > ", weak_limit))
  dt[, (helper_columns) := NULL]
}

#' @title do_censor_primary
#' @description
#' Initial censoring of numbers below the limit for censoring
#' TELLER, NEVNER-TELLER, NEVNER
#' DEV: 
#' For evt spesialverdier som skal aksepteres, f.eks. mobbing = 0, kan dette også legges i access 
#' og settes som unntak her. Kan settes som spesialgrense og inkluderes i limits.
#' Dersom høye tall skal aksepteres, bør også nevner-teller-prikking kunne slås av.
#' @keywords internal
#' @noRD
do_censor_primary <- function(dt, limits){
  if(is_not_empty(limits$TELLER)){
    cat("\n*** Prikker på liten teller og teller-nevner")
    dt[spv_tmp == 0 & sumTELLER <= limits$TELLER, let(pvern = 1, spv_tmp = 3)]
    dt[spv_tmp == 0 & sumNEVNER - sumTELLER <= limits$TELLER, let(pvern = 1, spv_tmp = 3)] 
  }
  if(is_not_empty(limits$NEVNER)){
    cat("\n*** Prikker på liten nevner")
    dt[spv_tmp == 0 & sumNEVNER <= limits$NEVNER, let(pvern = 1, spv_tmp = 3)]
  }
  cat("\n** Antall primærprikker i filen: ", dt[pvern == 1, .N])
}

#' @title get_censor_limits
#' @description Fetch censor limits from access
#' @keywords internal
#' @noRD
get_censor_limits <- function(spec){
  limits <- list()
  limits[["TELLER"]] <- ifelse(is_not_empty(spec$Stata_PRIKK_T), spec$Stata_PRIKK_T, spec$PRIKK_T)
  limits[["NEVNER"]] <- ifelse(is_not_empty(spec$Stata_PRIKK_N), spec$Stata_PRIKK_N, spec$PRIKK_N)
  limits[["STATTOL"]] <-ifelse(is_not_empty(spec$Stata_STATTOL_T), spec$Stata_STATTOL_T, spec$STATTOL_T)
  return(limits)
}

#' @title get_censor_triangles
#' @description Fetch censor triangles from access
#' @keywords internal
#' @noRD
get_censor_triangles <- function(parameters){
  spec <- parameters$CUBEinformation
  naboprikkdims <- grep("nabopr", names(spec), value = T)
  spc <- data.table::as.data.table(spec[naboprikkdims])
  for(dim in naboprikkdims){
    if(is_empty(spc[[dim]])){
      spc[, names(.SD) := NULL, .SDcols = dim]
      naboprikkdims <- setdiff(naboprikkdims, dim)
    }
  }
  dims <- setNames(naboprikkdims, toupper(gsub("Stata_nabopr([^_]+).*$", "\\1", naboprikkdims)))
  out <- list()
  for(dim in names(dims)){
    trianglecol <- dims[[dim]]
    out[[dim]] <- spec[[trianglecol]]
  }
  
  if("SPES" %in% names(out)){
    correctname <- sub("(.*)~.*", "\\1", spc[["Stata_naboprSpes"]])
    names(out)[names(out) == "SPES"] <- correctname
  }
  
  if(parameters$CUBEinformation$GEO_NABOPRIKK == "1"){
    out[["GEO"]] <- get_geonabotriangles(parameters = parameters)
  }
  
  out <- clean_triangles(list = out)
  return(out)
}

#' @title get_geonabotriangles
#' @description Fetch geo triangles from options and filters to geolevels in file
#' @keywords internal
#' @noRD
get_geonabotriangles <- function(parameters){
  geolevels <- strsplit(parameters$CUBEinformation$GEOniv, ",")[[1]]
  alltriangles <- getOption("khfunctions.geoprikk")
  out <- character()
  for(i in names(alltriangles)){
    levels <- strsplit(i, "")[[1]]
    if(i == "LKS") levels <- c("K", "B", "V")
    if(all(levels %in% geolevels)) out <- paste0(out, alltriangles[[i]])
  }
  return(out)
}

#' @title clean_triangles
#' @description Converts triangles to list of values in each triangle, handling special variants aswell
#' Written with assistance from copilot
#' @keywords internal
#' @noRD
clean_triangles <- function(list){
  if(length(list) == 0) return(invisible(NULL))
  for(dim in names(list)){
    content <- list[[dim]]
    tokens <- regmatches(content, gregexpr("\\{[^}]*\\}", content))[[1]]
    tokens <- gsub("^\\{|\\}$", "", tokens)
    
    out_dim <- vector("list", length(tokens))
    for (i in seq_along(tokens)) {
      x <- trimws(tokens[i])
      
      parens <- regmatches(x, gregexpr("\\([^)]*\\)", x))[[1]]
      if (length(parens) == 0L) {
        base_vec <- strsplit(x, ",")[[1]]
        out_dim[[i]] <- trimws(base_vec)
      } else {
        first_paren_pos <- regexpr("\\(", x)
        base_part <- substr(x, 1L, first_paren_pos - 1L)
        base_vec <- if(nchar(trimws(base_part))>0L) {
          trimws(strsplit(base_part, ",")[[1]])
        } else character(0)
        
        variants <- lapply(parens, function(p){
          s <- gsub("^\\(|\\)$", "", p)
          s <- sub("\\s+if\\s+", "_if_", s, ignore.case = TRUE)
          parts <- strsplit(s, "_if_")[[1]]
          
          ord_vec <- if(length(parts) >= 1L) trimws(strsplit(parts[1], ",")[[1]]) else character(0)
          cond_str <- if(length(parts) >= 2L) trimws(parts[2]) else stop("Variant mangler _if_-betingelse")
          cond_str <- normalize_cond(cond_str)
          
          list(
            order   = ord_vec,
            cond    = parse(text = cond_str)[[1]],  
            cond_raw = cond_str
          )
        })
        
        out_dim[[i]] <- list(base = base_vec, variants = variants)
      }
    }
    
    
    list[[dim]] <- out_dim
  }
  return(list)
}

#' @keywords internal
#' @noRd
normalize_cond <- function(s){
  s <- gsub("==\\s*([A-Za-zÆØÅæøå_][A-Za-z0-9_ÆØÅæøå]*)", '=="\\1"', s, perl = TRUE)
  s
}

#' @description Written with assistance from copilot
#' @keywords internal
#' @noRd
order_for_sc <- function(tri, subdt){
  if (is.atomic(tri)) return(as.character(tri))
  
  if (!is.null(tri$variants) && length(tri$variants)) {
    env <- as.list(subdt[1L])
    for (v in tri$variants){
      ok <- tryCatch(isTRUE(eval(v$cond, envir = env, enclos = parent.frame())),
                     error = function(e) FALSE)
      if (ok) return(as.character(v$order))
    }
  }
  
  return(as.character(tri$base))
}

#' @description Written with assistance from copilot
#' @keywords internal
#' @noRd
warn_if_special_triangles <- function(alltriangles) {
  special_dims <- names(alltriangles)[vapply(alltriangles, function(tris) {
    any(vapply(tris, function(tri) {
      is.list(tri) && !is.null(tri$variants) && length(tri$variants) > 0L
    }, logical(1L)))
  }, logical(1L))]
  
  if (length(special_dims) > 0L) {
    cat("\n**** Spesialstrata oppdaget for dimensjonene: ",
        paste(special_dims, collapse = ", "),
        "\n**** Dette kan øke kjøretiden for naboprikking pga betingede trekanter.\n")
  } 
}

#' @title do_remove_censored_observations
#' @description
#' Removes censored observations and set SPVFLAGG
#' 
#' Først beregnes max .f-variabel for rader der ingen av .f-variablene == 2. (tSPV_uten2) og for alle (tSPV_alle)
#' Dette er unoedvendig kronglete. Men dersom f.eks RATE.f=2 pga TELLER.f=1, oenskes SPVFLAGG=1. 
#' tSPV_uten2 prioriteres, og dersom denne == 0 vil tSPV_alle brukes for å sette SPVFLAGG.
#' @keywords internal
#' @noRd
do_remove_censored_observations <- function(dt, outvalues){
  valF <- paste0(union(getOption("khfunctions.valcols"), outvalues), ".f")
  valF <- intersect(names(dt), valF)
  if(length(valF) > 0){
    dt[, tSPV_alle := do.call(pmax, c(.SD, list(na.rm = T))), .SDcols = valF]
    dt[, tSPV_uten2 := do.call(pmax, c(.SD[, lapply(.SD, function(x) data.table::fifelse(x == 2, 0, x))], 
                                       list(na.rm = TRUE))), .SDcols = valF] 
    dt[, SPVFLAGG := data.table::fifelse(tSPV_uten2 == 0, tSPV_alle, tSPV_uten2)]
    dt[, c("tSPV_uten2", "tSPV_alle") := NULL]
    dt[SPVFLAGG > 0, (outvalues) := NA]
  }
  dt[is.na(SPVFLAGG), SPVFLAGG := 0]
  dt[SPVFLAGG %in% c(-1, 4), SPVFLAGG := 3]
  dt[SPVFLAGG == 9, SPVFLAGG := 1]
  return(dt)
}

#' @title do_censor_kube_stata
#' @description
#' Reads censoring information from ACCESS. 
#' If Stata_PRIKK parameters are set, the STATA censoring script (by Jørgen Meisfjord) 
#' is run using the do_stata_processing function
#' @param dt data file to be censored
#' @param parameters cube parameters
#' @keywords internal
#' @noRd
do_censor_kube_stata <- function(dt, parameters){
  sfile <- file.path(getOption("khfunctions.root"), getOption("khfunctions.stataprikkfile"))
  script <- paste0('include "', sfile, '"')
  dt <- do_stata_processing(dt = dt, script = script, parameters = parameters)
  return(dt)
}

#' @keywords internal
#' @noRd
get_geonaboprikk_triangles <- function(){
  data.table::data.table("Stata_naboprGeo_LF" = paste0("niva1", getOption("khfunctions.geoprikk")$LF),
                         "Stata_naboprGeo_FK" = paste0("niva2", getOption("khfunctions.geoprikk")$FK),
                         "Stata_naboprGeo_KB" = paste0("niva3", getOption("khfunctions.geoprikk")$KB),
                         "Stata_naboprGeo_KV" = paste0("niva4", getOption("khfunctions.geoprikk")$LKS))
}

#' @title save_kubespec_csv (ybk)
#' @description Saves ACCESS specs + list of dimensions to be used in STATA censoring
#' @keywords internal
#' @noRd
save_kubespec_csv <- function(spec, dims = NULL, geonaboprikk = NULL, geoprikktriangel = NULL){
  rootDir <- file.path(fs::path_home(), "helseprofil")
  if (!fs::dir_exists(rootDir))
    fs::dir_create(rootDir)
  
  specDF <- data.table::as.data.table(spec)
  varStata <- grep("^Stata", names(specDF), value = TRUE)
  varSpec <- c("KUBE_NAVN", varStata)
  varDF <- specDF[, .SD, .SDcols = varSpec]
  if(!is.null(dims)) varDF[, DIMS := list(dims)]
  if(!is.null(geonaboprikk)) varDF[, GEOnaboprikk := as.character(geonaboprikk)]
  if(!is.null(geoprikktriangel)) varDF[, names(geoprikktriangel) := geoprikktriangel]
  fileSpec <- file.path(rootDir, "kubespec.csv")
  data.table::fwrite(varDF, fileSpec, sep = ";", sep2 = c("", " ", ""))
  return(invisible(specDF))
}

#' find_dims (vl)
#' 
#' Helper function for kube_spec, finding dimensions in KUBE
#' @keywords internal
#' @noRd
find_dims_for_stataprikk <- function(dt, etabs){
  alldims <- c(getOption("khfunctions.khtabs"), etabs$tabnames)
  alldims[alldims %in% names(dt)]
}

