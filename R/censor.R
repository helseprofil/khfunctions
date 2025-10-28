# Oversetting av stataprikking til R
# - 1. Primærprikking på prikkegrenser
# - 2. Serieprikking av svake tidsserier
# - 3. Naboprikking

#' @title do_censor_cube_stata_r (temporary name)
#' @description
#' Replace STATA censor routines, and eventually take over all censoring.
#' @param dt cube
#' @param parameters parameters
do_censor_cube_stata_r <- function(dt, parameters){
  dt[, let(pvern = 0L, serieprikket = 0L, spv_tmp = 0L)]
  istellerf9 <- any(dt$TELLER.f == 9 | grepl("99$", dt$GEO))
  
  if(istellerf9){
    dt[TELLER.f == 9, let(spv_tmp = 9)]
    dt_spv9 <- dt[spv_tmp == 9 & grepl("99$", GEO)]
    dt <- dt[spv_tmp < 9 & !grepl("99$", GEO)]
  }
  limits <- get_censor_limits(spec = parameters$CUBEinformation)
  alltriangles <- get_censor_triangles(parameters = parameters)
  dims <- intersect(c(getOption("khfunctions.khtabs"), parameters$etabs$tabnames), names(dt))
  data.table::setkeyv(dt, c(dims))
  warn_if_special_triangles(alltriangles = alltriangles)
  
  cat("\n* Starter personvernhåndtering")
  do_censor_primary(dt = dt, limits = limits)
  do_censor_serie(dt = dt, limits = limits, dims = dims)
  
  cat("\n* NABOPRIKKING på:", names(alltriangles), "\n")
  do_naboprikk(dt = dt, alltriangles = alltriangles, limit = limits$TELLER, dims = dims)
  if(istellerf9) dt <- data.table::rbindlist(list(dt, dt_spv9), use.names = T, fill = T)
  return(dt)
}

do_naboprikk <- function(dt, alltriangles, limit, dims){
  nyeprikker <- 1L
  iteration  <- 1L
  max_serie_rounds <- 3L
  force_runde <- TRUE
  dt[, rid__ := .I]
  
  # Forhåndsberegn strata per dimensjon (inkluderer AAR automatisk fordi dims inneholder AAR)
  strata_cols <- setNames(paste0("strata__", names(alltriangles)), names(alltriangles))
  for (censordim in names(alltriangles)){
    otherdims <- setdiff(dims, censordim)     # AAR er med her dersom det ligger i dims
    sc <- strata_cols[[censordim]]
    dt[, (sc) := .GRP, by = otherdims]
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
        stats <- dt[in_triangle, .(
          antpvern       = sum(pvern == 1L, na.rm = TRUE),
          sumtellerprikk = sum(sumTELLER[pvern == 1L], na.rm = TRUE),
          ant_available  = sum(pvern == 0L, na.rm = TRUE)
        ), by = sc]
        
        prob <- stats[((antpvern == 1L) | (antpvern > 1L & sumtellerprikk <= limit)) & ant_available >= 2L, get(sc)]
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
  dt[, rid__ := NULL]
}

#' @title do_censor_serie
#' @description
#' Serieprikker på grensene som er satt i options anon_svakandel og anon_hullandel
#' Flagg 9 (finnes ikke eller kan ikke beregnes pga missing inn) holdes utenfor.
#' DEV:
#' Må håndtere evt spesialverdier som skal aksepteres, de kan ikke regnes som prikket her heller.
do_censor_serie <- function(dt, limits, dims){
  seriedims <- setdiff(dims, "AAR")
  n_year <- length(unique(dt$AAR))
  primary_limit <- getOption("khfunctions.anon_hullandel")
  weak_limit <- getOption("khfunctions.anon_svakandel")
  helper_columns <- c("weak", "propweak", "propprimary")
  dt[, (helper_columns) := NA_real_]
  
  dt[TELLER.f < 9, let(weak = ifelse(sumTELLER <= limits$STATTOL, 1, 0))]
  dt[, let(propweak = mean(weak, na.rm = T), propprimary = sum(pvern, na.rm = T)/n_year), by = seriedims]
  
  dt[pvern != 1 & (propweak > weak_limit | propprimary > primary_limit), let(serieprikket = 1, spv_tmp = 4)]
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
    dt[sumTELLER <= limits$TELLER, let(pvern = 1, spv_tmp = 3)]
    dt[sumNEVNER - sumTELLER <= limits$TELLER, let(pvern = 1, spv_tmp = 3)] 
  }
  if(is_not_empty(limits$NEVNER)){
    cat("\n*** Prikker på liten nevner")
    dt[sumNEVNER <= limits$NEVNER, let(pvern = 1, spv_tmp = 3)]
  }
  cat("\n** Antall primærprikker i filen: ", dt[pvern == 1, .N])
}

#' @title get_censor_limits
#' @description Fetch censor limits from access
#' @keywords internal
#' @noRD
get_censor_limits <- function(spec){
  limits <- list()
  limits[["TELLER"]] <- spec$Stata_PRIKK_T
  limits[["NEVNER"]] <- spec$Stata_PRIKK_N 
  limits[["STATTOL"]] <- spec$Stata_STATTOL_T  
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
  
  if(parameters$geonaboprikk){
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
#' @description Converts triangles to list of values in each triangle
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
      
      # Finn om vi har varianter i parenteser
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
          # Tillat både "_if_" og " if " i teksten
          s <- sub("\\s+if\\s+", "_if_", s, ignore.case = TRUE)
          parts <- strsplit(s, "_if_")[[1]]
          
          ord_vec <- if(length(parts) >= 1L) trimws(strsplit(parts[1], ",")[[1]]) else character(0)
          cond_str <- if(length(parts) >= 2L) trimws(parts[2]) else stop("Variant mangler _if_-betingelse")
          cond_str <- normalize_cond(cond_str)
          
          list(
            order   = ord_vec,
            cond    = parse(text = cond_str)[[1]],  # lagre som uttrykk (R AST)
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

normalize_cond <- function(s){
  # Eksempel: AARSAK==BRYSTKREFT  ->  AARSAK=="BRYSTKREFT"
  s <- gsub("==\\s*([A-Za-zÆØÅæøå_][A-Za-z0-9_ÆØÅæøå]*)", '=="\\1"', s, perl = TRUE)
  s
}


order_for_sc <- function(tri, subdt){
  # Bakoverkompatibelt: ren vektor => bruk direkte
  if (is.atomic(tri)) return(as.character(tri))
  
  # Hvis varianter er angitt, bruk første som treffer
  if (!is.null(tri$variants) && length(tri$variants)) {
    # Evaluer på første rad i strata (forutsatt at betingelsesvariabler er konstante i strata)
    env <- as.list(subdt[1L])
    for (v in tri$variants){
      ok <- tryCatch(isTRUE(eval(v$cond, envir = env, enclos = parent.frame())),
                     error = function(e) FALSE)
      if (ok) return(as.character(v$order))
    }
  }
  
  # Fallback: base
  return(as.character(tri$base))
}


warn_if_special_triangles <- function(alltriangles) {
  # Finn dimensjoner med minst én trekant som har variants
  special_dims <- names(alltriangles)[vapply(alltriangles, function(tris) {
    any(vapply(tris, function(tri) {
      is.list(tri) && !is.null(tri$variants) && length(tri$variants) > 0L
    }, logical(1L)))
  }, logical(1L))]
  
  if (length(special_dims) > 0L) {
    cat("\n**** Spesialstrata oppdaget for dimensjonene: ",
            paste(special_dims, collapse = ", "),
            "\n**** Dette kan øke kjøretiden for naboprikking pga betingede trekanter.\n")
  } else {
    cat("\n**** ingen spesialstrata test \n")
  }
}

# Opprinnelig STATA-script STATAprikking_master.do
# /* 
#   JM 24.6.2022: Videreutvikling basert på av ett av prikke-skriptene på 
# Rsynt_postprosess (KMI_LKU_L). 
# 
# KOMMENTARER
# # Skal ligge inne som fast snutt i R-løypa. 
# # Skriptet aktiveres hvis det i kubespec'en i KHELSA A) ikke er oppgitt prikke-
# parametre for vanlig prikking i R-løypa, og B) det ER oppgitt prikkeparametre 
# for Stataprikking, se KHELSA/KUBE/Stata_PRIKK_T, Stata_PRIKK_N, Stata_STATTOL_T
# # Skriptet starter med å hente parameterne om prikkegrenser og om hvilke dimen-
# sjoner og kategorier som skal naboprikkes.
# # Alle disse 6 parametrene har R dumpet til en csv-fil, kubespec.csv, lokalt på 
# maskinen (unngår konklikt med andre som kjører en annen kube samtidig).
# # Skriptet er tilpasset at prikkegrensene er "høyeste verdi som skal prikkes",
# altså; hvis grensen for teller er "9" så prikkes det når teller <= 9
# # Dette med GLOBALE makroer er noe som henger igjen fra tidligere versjon av  
# skriptet, men -macro drop _all- nederst sletter dem.
# # JM 8. sept. 2022 Denne versjonen flagger serieprikking med spv_tmp=4, og på denne måten 
# flagge datapunkter som naboprikket (og implisitt som personverntiltak) selv om
# de allerede er serieprikke-flagget. Hvis vellykket vil dette sørge for videre 
# nabo-prikking (noe de bare gjør dersom serieprikkene er flagget om fra serie-
#                  prikking (spv_tmp=4) til naboprikking (spv_tmp=3). Rutinen er samtidig blitt 
#                bedre på å utnytte serieprikking som naboprikker: 
#                  EKSEMPEL: 
#                  Rutinen leter etter serieprikker blant kvinner når den skal naboprikke kvinner  
#                på UTDANN. Hvis det ikke er noen serieprikker som kan brukes som naboprikk, vil
#                rutinen flagge naboprikking på vanlig måte, dvs. den av de uprikkede UTD-
#                  gruppene med lavest prioritet. 
#                UNNTAK:
#                  Hvis rutinen ikke finner en serieprikking den kan bruke som naboprikk blant 
#                kvinner, burde den ideelt sett sjekket om det er en serieprikk blant menn, og 
#                latt dette bestemme hvilken UTD-gruppe som skal naboprikkes blant kvinner. 
#                Dette er sikkert ganske komplisert (og potensielt feil-genererende) og ikke 
#                implementert. I en tilfeldig valgt årgang i GJENNOMFØRING ville dette spart én
#                prikking, se kommune 1535 i ferdigprikket dump:
#                  li GEO KJONN UTDANN MEIS sumt sumn Tu3 andelTu3 LitenNevner pvern spv_tmp 
#                serieprikket naboprikketIomg1 naboprikketIomg2 if AAR=="2012_2014" & sumSPV==5 & flagg2==1, sepby(GEO)
#                * JM 25.10.2022: Sette inn null i nevner dersom nevner er missing og missing er 
#                implisitt null.
#                * JM og VL 17.nov.2022: Vegard har fikset slik at 'kubespec.csv' inneholder info
#                om dimensjonsnavn i filen (se kolonnen DIMS), så vi kan ta vekk den shabby 
#                rutinen som ligger der fra før. (Rutinen til Vegard innebærer å lage en liste 
#                                                 over alle unike verdier av TAB1, TAB2 og TAB3 i KHELSA/FILGRUPPER, og krysser 
#                                                 denne med alle variabelnavn i filen.) 
#                * JM 9.jan.2023: Forrige versjon taklet ikke at det skal prikkes, samtidig som 
#                enkelte prikkegrenser er missing (som i GRUNNSKOLEPOENG; teller er kommunens 
#                                                  samlede antall grunnskolepoeng og skal ikke prikkes, mens nevner er antall 
#                                                  elever og skal prikkes).
#                * SB/JM 01.06.2023: Variabel AAR skal fjernes fra `prikkdimensjoner' (linje ca 27).
#   Men variabel AARSAK ble "ryddet" til "SAK" i stedet. 
#   Fikset ved å erstatte subinstr med subinword - den bytter ut hele ord.
# * VL/JM 25.01.2024: 2021-årgangen i KUHR kan ikke brukes. Dermed blir periodene 
# 	2019-2021
# 	2020-2022
# 	2021-2023
#   bare basert på to årganger hver, og da må prikkegrensen endres fra T=3x4 og N=3*9,
#   til T=2x4 og N=2*9 i denne tabellen disse periodene. 
# * JM 01-05.02.2024: Haram og Ålesund kan ikke skilles fra hverandre i årene 2020-
#   2023 dersom dataleveransen er på kommune. Det var først snakk om å legge inn 
#   sletting av Haram- og Ålesunddata her, men så landet vi på å legge det inn i 
#   løypen i stedet, der vi allerede har sletting av tilsvarende årganger for de 
#   såkalte delingskommunene i 2020 (Narvik, Heim osv.).
# * JM 29.08.2024: Litt rydding. Sparer kanskje litt tid. 
# * JM 30.08.2024: Lagt til Geo-naboprikking i parallellsesjoner. Kronologi:
#   1. Primærprikking og serieprikking
#   2. Naboprikking. La pakken -parallel- splitte opp denne jobben i N "Child 
#     processes" med et visst antall årganger i hver Child process. 
# 	# Antall prosesser kommer an på antall kjerner på den enkelte maskin, og 
# 	  totalt antall årganger i filen.  
#   filen (etter årganger), og kjøre hver . (Antall årganger i hver fil kan kanskje 
#      optimaliseres. Med f.eks. 30 årganger kan det i teorien gå fortere med 15
# 	 sesjoner som tar 2 årganger hver, enn 30 sesjoner som tar én årgang hver.)
# 	 Starter med det enkleste, splitte i enkeltårgangsfiler. Lagre på C:\. 
#   3. En serie Statasesjoner som tar seg av én årgangsfil hver, og naboprikker
#   4. Sy de ferdigprikkede filene sammen igjen. Ferdig.
# * JM 29.10.2024: StataPrikking_parallel_II_MASTER_medGEOnp. Tar ut variabler
#   som ikke trengs i naboprikkingen, og hekter dem på igjen etterpå. Forsøk på å 
#   speede opp skriptet.
# * JM 11.12.20001024: Setter av 2 prosessorer (kjerner) til andre kjøringer eller 
#   diverse prosesser på maskinen 
# * _parallel_III_MASTER: Denne versjonen skal fikse problemene med SPV=9 i sub-
#   skriptet (SPV=9 for bydelstall i perioden 1990-2002). Fiksen består i: 
# 	# Før -parallel-: Klippe av alle linjer med SPV=9
# 	# Etter -parallel-: Lime dem på igjen
# * Oppdatert til (...)_parallel_IV_MASTER: 
# 	* Linjer med TELLER_F=9 (tidlige bydelstall) blir ikke med i naboprikkingen,
# 	  men limes på igjen etterpå.
# 	* Noen endringer i variabelliste i Tidsforbruk.dta ("loggfilen" for tidsbruk).
# # ..._V_MASTER: Nytt opplegg der GEO-triangler som er laget her i MASTER (hvor 
# 	HELE kuben er tilgjengelig) også skal brukes i hver child-sesjon, slik at 
# 	alle child-sesjoner (og alle maskiner!) bruker samme GEO-triangler (i mot-
# 	setning til hvordan det var med det tidligere opplegget). 
# 	
# 	Med det tidligere opplegget kunne rangeringen av kommuner innen et triangel 
# 	variere mellom child-sesjoner (fordi beregningsgrunnlaget for fylkes-,  
# 	kommune-, bydels- og sonestørrelse var 
# 	forskjellig mellom child-sesjoner), og dermed avhengig av hvilken maskin man
# 	kjørte på (fordi store maskiner hadde få årganger og et annet beregnings-
# 	grunnlag per child-sesjon, sammenlignet med små maskiner).
# 
# 	GEO-trianglene som lages her i masterskriptet, må derfor lagres i en fil som 
# 	subskriptet kan aksessere. Det enkleste hadde vært å utvide kubespec.csv.
# 	Prøver det først. I følge co-pilot er det ingen begrensning på hvor mange 
# 	tegn det kan være i en "celle" i en csv-fil, og husk at en csv-fil ikke er 
# 	en form for excel-fil, men er mer som en txt-fil. Nå skal jo csv-filen 
# 	først og fremst åpnes av Stata. Tidvis også av Excel, og legg merke til at
# 	Excel har en begrensning på 32 767 tegn per celle (ref co-pilot), så det er
# 	ikke sikkert Excel klarer å vise hele innholdet (ikke show-stopper).
# # ..._VI_MASTER: Linjer med ukjent GEO blir heller ikke med i naboprikkingen, i 
# 	tillegg til med TELLER_f==9 (se versjon IV). Ville eliminere mismatcher som 
# 	bare skyldes ulik nabo-prikking av ukjent GEO. 
# # ..._VII_MASTER: Kutter ut dette med å filtrere ut ukjent GEO fordi det er 
# 	overflødig etter at Vegard har flyttet sletting av ukjent geo til *før* 
# 	prikkingen. Fortsetter å filtrere ut linjer med TELLER_f==9 siden disse 
# 	skapte en eller annen form for problemer (husker ikke akkurat hva).
# 	
# 	Det å filtrere ut linjer med TELLER_f==9 skapte et annet problem fordi det 
# 	kunne forekomme kombinasjoner av at geonaboprikkeparameteren inneholdt 
# 	bydelskoder mens noen av child_processene ikke gjorde det. Dette håndteres
# 	nå i subskriptet. 
# 	
# 	JM 12.2.2025: Fremdeles trøbbel når enkelt-kommuner har TELLER_f=9. Tar ut 
# 	hele greien fra dette skriptet (MASTER). Endrer ikke noe i subskriptet.
# # ..._VIII_MASTER, JM 13.-14.feb.2025: Det blir problemer uansett om man holder 
# 	linjer med TELLER_f=9 utenfor naboprikkingen, eller om man tar dem med: 
# 	A) Holder man dem utenfor, kan hele kommuner holdes utenfor (f.eks. kommunene
# 	1508, 1580, 1806, 1875, 5055, 5056 og 5059 i FORSVARET_TRENING_3). Og da får
# 	man feilmelding fra subskriptet om at det er verdier i GEO-triangelet som 
# 	ikke finnes i datasettet.
# 	B) Tar man dem med i naboprikkingen, naboprikkes Stavanger og de andre store
# 	byene i den perioden bydelene har missing (1990-2001), f.eks. i KREFT_1. 
# 	Dette siste burde kanskje vært fikset på men tør ikke det nå (februar 2025).
# 	
# 	Den tilsynelatende enkleste løsningen nå, er å (gå tilbake til å) holde 
# 	linjer med TELLER_f=9 utenfor naboprikkingen (MASTER), og (i sub-skriptet) 
# 	å akseptere at det er verdier i akkurat GEO-triangelet som ikke finnes i 
# 	datasettet. Husk at GEO-trianglene er laget maskinelt og ikke har skrivefeil
# 	(slik f.eks. alderstrianglene har hatt en del av). Åpner dette for at det 
# 	kan være f.eks. 2027-geo i filen, men 2026-geo i triangelet? I så fall bør 
# 	det f.eks. legges inn en test av at alle geo-verdier i filen også finnes i 
# 	(minst) ett geotriangel (selv om det motsatte altså ikke er påkrevd).
# 	
# 	En annen ting i dag: andelTu3>.2 og andelTu6>.5 endret til andelTu3>.20001 
# 	og andelTu6>.50001. Skal være ekvivalente i ca 100 000 år.
# 
# */
# 
# pause on
# set more off
# macro drop _all
# 
# if "`c(current_date)'"=="66 Oct 2025" {  
# keep if  ICD=="C15_26" & (GEO=="0" | substr(GEO,1,2)=="50") // årganger fjernes etter primærprikking
# * global child_processes=5 // samme som på VM-04
# *	keep if ICD=="C18" 
# }
#  
# 
# 
# local skript "StataPrikking_subskript_DEV.do"
# local skriptMappe "O:\Prosjekt\FHP\PRODUKSJON\BIN\Z_Statasnutter\"
# 
# * local skript "_DEVELOP_XIII_nivaadelt naboprParameter.do"
# * local skriptMappe "O:\Prosjekt\FHP\PRODUKSJON\DEVELOP\GEO_naboprikking\sandkasse\PARALLEL_test\"
# 
# 
# 
# global tid0= c(current_time)  // Bare for testformål (tidtagning)
# di "$tid0"
# * Optimalisering(?) av antall prosesser/kjerner/prosessorer til f.eks. de 25 
# * årgangene i DAAR, når vi har opptil 12 til disposisjon:
# 	*A. Det enkle ville vært å ta i bruk alle kjernene (12 i tilfellet BNT0), 
# 	  *men én av dem ville fått 3 årganger å jobbe med. 
# 	*B. Når først én kjerne har tre årganger å jobbe med, hadde det kanskje vært
# 	  *like godt å la flere kjerner få 3 årganger å jobbe med, og dermed frigjøre
# 	  *noen kjerner til bruk for annen aktivitet på maskinen? Og på denne måte 
# 	  *unngå de mulige konflikter som kanskje kan oppstå i alternativ A der man
# 	  *bruker ALLE kjernene til å kjøre Stata? Metode: Velge et så lavt antall 
# 	  *kjerner som mulig samtidig som ingen kjerner får mer enn 3 årganger å 
# 	  *jobbe med. I dette ville 9 kjerner vært løsningen.  
# qui tab AAR
# global antAarganger=`r(r)' // 25 i tilfelle DAAR (JM okt. 2024)
# qui describe
# global antVar=`r(k)'
# * Beregne max antall årganger per prosessor (gitt inneværende fil og maskin).
# local antProsessorer = c(processors_mach) // 12 i tilfelle BNT0 (JM okt. 2024)
# local maxAntAargPerProc=ceil($antAarganger/ `antProsessorer') // max 3 årg/proc
# * Beregne minimum antall prosessorer som skal til for at ingen prosesser får mer
# * enn 'maxAntAargPerProc' årganger å jobbe med. 
# global child_processes = ceil($antAarganger/ `maxAntAargPerProc')
# * Litteraturen (og erfaringen) tilsier at det ikke er lurt å bruke alle 
# * prosessorene. Vi frigjør to i første omgang.
# if $child_processes == `antProsessorer' global child_processes = $child_processes - 2
# if $child_processes == `antProsessorer'-1 global child_processes = $child_processes - 1
# 
# di "Optimalt antall child prosesser: $child_processes"
# 
# 
# 
# 
# 
# global merknad ""
# 
# preserve
# 
# * Del 1. Innhente parameterverdiene fra C:\Users\<brukernavn>\helseprofiler\kubespec.csv
# import delimited "C:\Users\\`c(username)'\helseprofil\kubespec.csv", delimiter(";") varnames(1) case(preserve) clear
# pause 1
# * capture drop Stata_naboprGeo
# * export delimited "C:\Users\\`c(username)'\helseprofil\kubespec.csv", delimiter(";") replace
# * import delimited "C:\Users\\`c(username)'\helseprofil\kubespec.csv", delimiter(";") varnames(1) case(preserve) clear
# local grense_sumN = Stata_PRIKK_N  // Prikker "dette tallet eller lavere", som i løypa.
# local grense_sumT_pv = Stata_PRIKK_T // (samme som ovenfor).
# *local spesgrense_sumT_pv=0 // Unntak fra regel om nedre grense. Brukkes vel bare i MOBBING (der teller=0 tillates)
# local grense_sumT_su = Stata_STATTOL_T // (samme som ovenfor). Grenseverdi for teller (mtp. statistisk utsagnskraft)
# local kube=KUBE_NAVN
# local GEO_naboprikking = GEOnaboprikk // (TRUE/FALSE) Om GEO-naboprikking er ønsket i denne kjøringen
# local prikkdimensjoner = DIMS
# local koblingsdimensjoner = DIMS
# di "`prikkdimensjoner'"
# local antDims = wordcount("`prikkdimensjoner'")
# * Fjerne AAR fra listen
# forvalues k=1/`antDims' {
#   if word("`prikkdimensjoner'", `k')=="AAR" { // AAR skal aldri være med på denne listen
# 		local prikkdimensjoner = subinword("`prikkdimensjoner'", word("`prikkdimensjoner'", `k'), "", 1)
# 	}
# }
# * Identifisere spesialdimensjon	
# local antDims = wordcount("`prikkdimensjoner'") // må telles opp på nytt nå
# local standarddimensjoner="GEO AAR ALDER KJONN UTDANN LANDBAK INNVKAT"
# forvalues k=1/`antDims' {
# 	if regexm("`standarddimensjoner'", word("`prikkdimensjoner'", `k'))==0 { 
# 	  local spesialdimensjon =  word("`prikkdimensjoner'", `k')
# 	}
# }
# 
# 	
# * Erstatte missing grenseverdi med "-1" (JM 9jan2023). Forklaring: Hvis grense-
#   *verdiene er missing (dvs. at det f.eks. ikke skal prikkes på små tellere, vil
#   *faktisk hele filen prikkes fordi grenseverdien er "." (altså
#   *uendelig). Iom. at alle tellerne er mindre enn "." blir alt prikket. 
# if `grense_sumN'==. local grense_sumN=-1
# if `grense_sumT_pv'==. local grense_sumT_pv=-1
# if `grense_sumT_su'==. local grense_sumT_su=-1
# di "`prikkdimensjoner'"
# di `grense_sumN'
# di `grense_sumT_pv'
# di `grense_sumT_su'
# 
# * Naboprikkeparametre
# ****************************************************************
# 
# * JM 30.1.2025; Dette med naboprikking har vel ikke noe her i MASTER-skript å gjøre .
# /**** Naboprikke-triangler for alle dimensjoner unntatt GEO *********************
# rename (*nabopr*), lower // for at feltnavnene i KHELSA skal være så lite case 
# 						// sensitive som mulig.
# foreach naboparam of varlist *nabopr* { // for hver naboprikkeparameter
# 	global `naboparam' = `naboparam' // gjøre om til makro
# 	global `naboparam' = subinstr("$`naboparam'"," ","",.) // fjerne evt. mellomrom	
# global `naboparam' = subinstr("$`naboparam'",",,",",",.) // fjerne evt. doble komma	
# 	global `naboparam' = subinstr("$`naboparam'",",}","}",.) 	
# global `naboparam' = subinstr("$`naboparam'","{,","{",.) 	
# }
# **** (end naboprikke-triangler for alle dimensjoner unntatt GEO) ***************
# */
# restore
# 
# 
# **** Lagre variablene som ikke trengs i prikkingen på C: (sparer dette tid?) ***
# preserve 
# * 1) Det varierer dessverre hvilke variabler som er i filen, avhengig av f.eks.
# *    standardisering eller ikke, og om BEF er nevner eller ikke. Prøver å lage 
# *    en indikator, vartest, på hva slags type fil vi har med å gjøre, og følgelig
# *    hvilket sett av variabler som er overflødig å ta med inn i naboprikkerutinen.
# capture describe *BEF*
# local bef=(_rc==0)
# capture describe MEISskala
# local meisskala=10*(_rc==0)
# capture describe NORM*
# local norm_=100*(_rc==0)
# local vartest=`meisskala'+`bef'+`norm_'
# 	//   0: Mangler MEISskala o.l., *BEF* og NORM*
# 	//   1: Mangler MEISskala o.l.        og NORM*
# 	//  10: Mangler                 *BEF* og NORM*
# 	//  11: Mangler                          NORM*
# 	// 100: Mangler MEISskala o.l., *BEF*
# 	// 101: Mangler MEISskala o.l.
# 	// 110: Mangler                 *BEF*
# 	// 111: Mangler           (mangler ingen)
# * Noen overflødige variabler finnes i alle filer (time will show, hvis dette ikke
# * stemmer vil det vel kræsje i -keep- kommandoen nedenfor).
# * local overfloedige1 " RATE_n TELLER_a  NEVNER_f NEVNER_a RATE_f RATE_a TELLER_n NEVNER_n  RATE_n " // TELLER_fn* 
# local overfloedige1 " RATE_n FYLKE TELLER_a  NEVNER_f NEVNER_a RATE_f RATE_a TELLER_n NEVNER_n  RATE_n " // TELLER_fn* 
# 	// og NEVNER_fn* finnes i DÅR-filen, men ikke i KREFT, så de utgår. JM 11.11.2024
# 	// FYLKE har vært med lenge, men finnes ikke i VEDVARENDE_LAVINNTEKT 7.10.2025
# * Hvilke andre overflødige variabler det er i filen, kommer an på. Indikatoren 
# * vartest skal vise vei. 
# if `vartest'==0 local overfloedige2 " "
# if `vartest'==1 local overfloedige2 "  *BEF*  "
# if `vartest'==10 local overfloedige2 "sumPREDTELLER lopendeFORHOLDSVERDI lopendeMEISref MEISskala PREDTELLER*  "
# if `vartest'==11 local overfloedige2 "sumPREDTELLER lopendeFORHOLDSVERDI lopendeMEISref MEISskala PREDTELLER* *BEF*  "
# if `vartest'==100 local overfloedige2 "  NORM*"
# if `vartest'==101 local overfloedige2 "  *BEF* NORM*"
# if `vartest'==110 local overfloedige2 "sumPREDTELLER lopendeFORHOLDSVERDI lopendeMEISref MEISskala PREDTELLER*       NORM*"
# if `vartest'==111 local overfloedige2 "sumPREDTELLER lopendeFORHOLDSVERDI lopendeMEISref MEISskala PREDTELLER* *BEF* NORM*"
# * Mellomlagre de overflødige variablene
# local OVERFLOEDIGE = " " + "`overfloedige1'"+" "+"`overfloedige2'"
# qui {
# 	n: di "`OVERFLOEDIGE'"
# 	n: di "Hvis det kommer feilmelding på neste linje, kan det skyldes at vi bruker"
# 	n: di "feil utgave av kubespec.csv. Sjekk i mappen \C:\Users\...\helseprofil"
# }
# keep `koblingsdimensjoner' `OVERFLOEDIGE'
# foreach var in KJONN UTDANN LANDBAK INNVKAT {
# 	capture tostring `var', replace
# }
# * Tar vare på disse kolonnene i en temp-fil (slik at ikke blir konflikt med 
# * eventuelle andre sesjoner).
# sort `koblingsdimensjoner'
# * save "C:\Users\\`c(username)'\helseprofil\overfloedige_variabler", replace
# tempfile overfloedige_variabler
# save "`overfloedige_variabler'"
# * Åpne full fil igjen og midlertidig fjerne de overflødige variablene
# restore
# drop `OVERFLOEDIGE'
# qui describe
# global antVar=`r(k)'
# ********************************************************************************
# 
# 
# 
# 	
# * DAAR: Telle opp antall triangler i hver dimensjon
# local antKJONNtriangler = length("$stata_naboprkjonn") - length(subinstr("$stata_naboprkjonn", "{", "", .)) 
# local antSPESIALDIMtriangler = length("$stata_naboprspes") - length(subinstr("$stata_naboprspes", "{", "", .)) 
# local antALDERtriangler = length("$stata_nabopralder") - length(subinstr("$stata_nabopralder", "{", "", .)) 
# local antGEOtriangler = length("$stata_naboprGeo") - length(subinstr("$stata_naboprGeo", "{", "", .)) 
# 
# 
# di "Alder: $stata_nabopralder , Antall triangler: `antALDERtriangler'"
# di "Kjønn: $stata_naboprkjonn , Antall triangler: `antKJONNtriangler'"
# di "Utd: $stata_naboprutdann , Antall triangler: `'"
# di "Innv: $stata_naboprinnvkat , Antall triangler: `'"
# di "Spesialdim: $stata_naboprspes , Antall triangler: `antSPESIALDIMtriangler'"
# 
# 
# 
# * pause triangler
# 
# 
# 
#  
# /* syntaks før 12.august.2022
# 				global nabopr_kjonn = subinstr(Stata_naboprKjonn," ","",.)	
# 				global nabopr_alder = subinstr(Stata_naboprAlder," ","",.)
# 				global nabopr_spesialdim_1=subinstr(Stata_naboprSpes," ","",.)	
# */
# * SYNTAKS for Stata_nabopr_*: Dimensjoner som trenger naboprikking (og hvordan) 
# **** MODELL:    global stata_nabopralder="ALDER~niva1{0_74,45_74,0_44}"	
# ****			global stata_nabopralder="ALDER~niva1{15_49,15_19,20_24,25_29,30_34,35_39,40_44,45_49}"
# ****			global stata_nabopralder="ALDER~niva1{16_79,16_44,45_79}niva2{16_44,16_24,25_44}{45_79,45_64,65_79}"	
# * NB: Alle naboprikkingsparametrene må være globals (på grunn av en extended 
# *  macro function lenger nede) og de må begynne med "Stata_nabopr"
# * Resten av makronavnet er ikke kritisk, scriptet looper over de navnene som finnes.
# 
# if "`c(current_date)'"=="21 Nov 2022" {
# *	save "prikketest", replace
# 	save "O:/Prosjekt/FHP\PRODUKSJON\TEST\dump_pre_prikking_`kube'", replace
# }
# 
# /* 
# Prikking og naboprikking, KUBESPESIFIKK
# 	# ALDER: Dersom det er nødvendig med vask av ALDER, se nederst i skriptet. De
# 	  linjene er kopiert fra en LKU-snutt
# 	
# */
# 
# 
# /* For develop-formål
# save "O:/Prosjekt/FHP\PRODUKSJON\TEST_DUMP\dump_pre_StataPrikking.dta", replace
# exit
# */
# 
# /* For develop-formål
# use "O:/Prosjekt/FHP\PRODUKSJON\TEST_DUMP\dump_pre_StataPrikking.dta", clear
# */
# 
# 
# 
# * Del 2. Identifisere "prikkedimensjoner"
# * Må vite hvilke dimensjoner som identifiserer unike tidsserier. Hvis det står f.eks.
#   *"GEO KJONN Landbakgrunn", så betyr det at det kun finnes én tidsserie for f.eks. 
#   *Bergen-gutter-innvandrere. Dersom man i tillegg hadde aldersgrupper, så skulle 
#   *ALDER også vært inkludert, slik: "GEO ALDER KJONN Landbakgrunn" (rekkefølgen
#   *er vilkårlig). Det er bedre  å ta med en dimensjon for mye enn en for lite. 
#   *Eksempel: Anta at UTDANN er en dimensjon som er med i filen, men kun med verdien
#   *null (altså utdanning samlet). Da er det hipp som happ oom man tar den med på
#   *listen eller ikke, med mindre det har stor betydning for tidsbruken. I så fall
#   *kunne man gå inn og identifisere hvilke av dimensjonene på listen som har 
#   *bare én verdi, og slette slike dimensjoner fra listen. (JM 29.juni2022)
#   
#   * AAR skal aldri på denne listen.
#   
#   * Bygger på lignende kode i boxplot-skriptet, og har ikke fjernet det som er 
#   *irrelevant (JM 29.juni2022)
#   
# if "`c(current_date)'"==" 6 Nov 2024" {  
# 	keep if length(GEO)<=2 
# }
#  
# 
# 
# 
# 
# * Finne var-navn for siste variabel, til den avsluttende oppryddingen
# describe, varlist
# local siste_var = word("`r(varlist)'", -1)
# local foerste_var = word("`r(varlist)'", 1)
# di "A0 siste og første variabel:  " "`foerste_var'" " " "`siste_var'"
# 
# 
# /* JM 30.1.2025 naboprikking skal ikke behandles her i MASTER
# * Lage en liste over naboprikking-makroene
# global naboprListe: all globals "stata_nabopr*"
# di "$naboprListe"
# */ 
# 
# *pause 1 i MASTER: Se den såkalte naboprListe (hentes opp i starten på child-skriptet)
# 
# * JM 25.10.2022: Sette inn null i nevner dersom nevner er missing og missing er 
#   *implisitt null.
# tempvar Nsnitt 
# 
# 
# egen `Nsnitt'=mean(sumNEVNER), by(GEO AAR) // Missing hvis f.eks. tidlige bydels-
#   // tall som faktisk skal være missing		
# replace sumNEVNER=0 if sumNEVNER==. & `Nsnitt'<. 
# replace NEVNER=0 if NEVNER==. & `Nsnitt'<. 
# replace sumTELLER=0 if sumNEVNER==0
# replace TELLER=0 if sumNEVNER==0
# replace TELLER_f=0 if sumNEVNER==0 // Rett her, før prikking
# 
# 
# * Til utviklingsformål: Skrive ut en fil med listen over "prikkedimensjoner" og
# *naboprikkeparametre.
# * Denne ligger lokalt på maskinen (C:\Users\jome\Documents), og "prikke-
#   *dimensjoner" skal 
# *A. Ikke ha annet enn dimensjoner (sjekk)
# *B. Ha alle dimensjonene untatt AAR. Sammenlign med dimensjonslisten i KHELSA!
#   * Naboprikkeparametre er inkludert selv om de er tomme (funker dette lengre 
#                                                           *nede i skriptet?)
# /* 
#   preserve
# clear
# set obs 1
# gen prikkedimDUMP="`prikkdimensjoner'"
# gen forklaring = "Dette er prikkedimensjonene prikkeskriptet kommer opp med"
# gen naboprGlobs="$naboprListe"
# gen prGrenser = "`grense_sumN' ; `grense_sumT_pv' ; `grense_sumT_su'"
# save "O:/Prosjekt/FHP\Forskningsfiler\JOME\Naboprikking_dev\Forslag_dims og_globs", replace
# restore
# */
#   
#   
#   
#   
#   
#   local _f_kode = 3 // TELLER_f gis denne verdien ved prikking. Mindre raffinert 
# // enn i R-løypen, men funker det (dvs. får vi alltid rett 
#                                    // SPV-flagg)?
#   
#   
#   
#   * ---------------------------------------------------------------------------
#   **Tilpasning til snutten:
#   capture drop GEOtmp
# capture rename GEO GEOtmp
# capture destring GEOtmp, gen(GEO)
# capture rename sumTELLER sumteller
# capture rename sumNEVNER sumnevner
# 
# * --- <NH_ALDER> og <NABOPR_ALDER>; kvalitetskontroll (påbegynt, Jørgen april 2017) --
#   *******************************************************************************	 
#   * 1. Sjekke at det ikke forekommer "implisitte undergrupper" i <ALDER_NH>.
# * 1.a. Lage tempfil der hver variabel representerer en av aldersgruppene i 
# *<ALDER_NH>. Aldersgruppene må analyseres i rett rekkefølge, først fra 
# *minste til største, og innimellom motsatt. Variablene får derfor navn som 
# *starter med hvor mange alderstrinn de hhv. dekker eller mangler. Variablene 
# *sorteres så med -order _all, sequential-, slik at aldersgruppene analyseres 
# *i rett rekkefølge når det loopes over dem.
# 
# 
# 
# ***************************************************(END QC ALDER_NH osv.)*******
#   
#   
#   
#   
#   di "grense: `grense_sumT_pv'"
# 
# 
# capture drop Tu3 
# capture drop LitenNevner 
# capture drop andelTu3 
# capture drop pvern
# 
# local antallNyeSPVflagg=0
# * 1. FLAGGE prikking (personvernhensyn). NB Analysen av skjuling (neste avsnitt)
# * er avhengig av at tall ikke slettes i dette avsnittet
# if `grense_sumT_pv'<0 { // Hvis det ikke skal prikkes på teller (f.eks. GRUNNSKOLEPOENGSKPOENG)
# 	gen Tu3 = (sumteller<=`grense_sumT_pv') 
# }
#   else { // Vanlig opplegg
#     gen Tu3 = (sumteller<=`grense_sumT_pv' | (sumnevner-sumteller)<=`grense_sumT_pv') 
# }
# capture replace Tu3=0 if sumteller==`spesgrense_sumT_pv' 
#   gen LitenNevner = (sumnevner<=`grense_sumN') // under 10 eller, LKU RFU, under 100 (mars 2014)
# 
# 
# 
# * KUHR 2021: Justering av flaggvariablene Tu3 og LitenNevner for KUHR i den   
# * perioden de treårige periodene bare har to årganger (som følge av at 2021-
# * årgangen ikke kan brukes).
# if "`kube'"=="KUHR" {
# 	* Først verifisere at kuben har treårige glidende gjennomsnitt 
# 	assert AARh-(AARl-1)==3
# 	* Så beregne nye grenseverdier og endre beregne flaggvariablene på nytt 
# 	local justertgrense_sumT_pv=round(2*`grense_sumT_pv'/3)
# 	local justertgrense_sumN=round(2*`grense_sumN'/3)
# 	replace Tu3=(sumteller<=`justertgrense_sumT_pv' ///
# 	| (sumnevner-sumteller)<=`justertgrense_sumT_pv') if AARl>=2019 & AARl<=2021
# 	replace LitenNevner= (sumnevner<=`justertgrense_sumN') if AARl>=2019 & AARl<=2021
# }
# 
# 
# di "HER; `prikkdimensjoner'"
# bysort `prikkdimensjoner' :  egen andelTu3=mean(Tu3) 
# gen pvern = (Tu3 | LitenNevner)
# 
# 
# 
# 
# * Tillate teller=0. Linjen nedenfor tillater tall selv om liten N, forutsatt at T=0
# * replace pvern=0 if sumteller==`spesgrense_sumT_pv' 
# * 2. FLAGGE skjuling   (av tall med lite statistisk utsagnskraft). Spesielt viktig 
# 		* så lenge vi ikke får vist konfidensintervall i Nesstar. Regel: Hvis 
# 		* over 50 % av tallene i en tidsserie er basert på færre enn 
# 		* 6 tilfeller, ryker hele tidsserien ut. 
# di "`prikkdimensjoner'"
# * b) flagge tellere som er mindre enn 6 (summert over perioden for indikatorer 
# 	  * der det er slått sammen over år
# gen Tu6=(sumteller<=`grense_sumT_su') // korrigert for gjennomsnitt over perioder		
# capture replace Tu6=0 if sumteller==`spesgrense_sumT_pv' // Ellers hadde vi antakelig 
# 	// serieprikket mange av de 0 % mobbing som vi ønsker at skal vises.
# bysort `prikkdimensjoner' : egen andelTu6=mean(Tu6)
# gen spv_tmp=.
# *order spv_tmp, before(skille)
# * capture replace spv_tmp=3 if sumteller==. // dette er nok feil. JM. 7/10/22 Droppes 
# 
# 
# 
# * JM sept. 2022: Prøvde med å flytte serieprikkingen til ETTER naboprikkingen 
# * (hva var bakgrunnen for det?), men det resulterte i overprikking. Serie-
# * prikkingen må gjøres før nabopr., fordi serieprikking kan erstatte noe av nabo-
# * prikkingen. Eksempel: Anta at man har 4 aldersgrupper i følgende prioriterte 
# * rekkefølge: A, B, C og D. Anta så at (bare) B må personvernprikkes. Normalt 
# * ville man da naboprikke D. Men, dersom C allerede var serieprikket ville det 
# * (vanligvis) være overflødig å naboprikke D. 
# 
# * I dette tilfellet ville sen naboprikking ført til prikking av først B, så 
# * D (nabopr.) og så C (seriepr.), mens tidlig serieprikking bare ville resultert i prikking av B og C. 
# 
# * Ergo må serieprikking gjøres før naboprikking.
# replace spv_tmp=4 if andelTu3>.20001 | andelTu6>.50001 // Skille mellom serie- og pv-
# 	// prikking. Nytt i dette skriptet, sept. 2022
# replace spv_tmp=3 if pvern==1 
# gen serieprikket=(andelTu3>.20001 | andelTu6>.50001)
# *replace spv_tmp=3 if pvern==1  // Ny versjon (okt. 2022). 
# 
# * Ytterligere filtrering for å speede opp skriptingen
# if "`c(current_date)'"==" 6 Jan 2025" {  
# keep if AAR=="1990_1990" | AAR=="2019_2019" // Må ha med en årgang der bydeler 
# // har nevner (mtp. rangering etter størrelse forut for GEO-triangler).
# * global child_processes=5 // samme som på VM-04
# *	keep if ICD=="C18" 
# }
# 
# count if spv_tmp==3 
# local antallNyeSPVflagg=`r(N)'-`antallNyeSPVflagg'
# 
# 
# * pause X2
# * 3. FLAGGE NABO-PRIKKING
# * På dette stadiet er en del tall flagget pga. statistisk utsagnskraft. Husk at
# * disse ikke skal resultere i naboprikking. 
# * Loope over naboprikkingsparametrene.
# * ------------------------------------------------
# * Først litt rydding (JM 29.8.2024) ************
# tostring `prikkdimensjoner', replace
# replace GEO="0"+GEO if length(GEO)==1 & real(GEO)>0 // 3 -> 03 men ikke 0 -> 00
# replace GEO="0"+GEO if length(GEO)==3 // 301 -> 0301
# replace GEO="0"+GEO if length(GEO)==5 // 30101 -> 030101
# replace GEO="0"+GEO if length(GEO)==7 // 3010101 -> 03010101
# global stata_nabopralder=subinstr("$stata_nabopralder","-","_",.) // X-Y  -->  X_Y Hvis stata_nabopralder ikke finnes, opprettes en tom global makro med samme navn. Ser ikke ut til å skape noen problemer.
# **********************************
#   
#   
#   * Klippe av linjer med TELLER_f=9 eller ukjent GEO. De blir limt på igjen 
# * straks etter at naboprikking er ferdig. 
# preserve
# *keep if TELLER_f==9 | substr(GEO,-2,2)=="99"
# keep if TELLER_f==9 
# tempfile TELLER_f_9
# save "`TELLER_f_9'", replace emptyok
# restore
# drop if TELLER_f==9 
# 
# 
# 
# /*
#   * for utvikling/ feilsøking i subskriptet:
#   include "`skriptMappe'\`skript'"
# global child_processes = .
# global merknad "med -include-"
# */
#   
#   
#   * pause primærprikking ferdig 
# 
# 
# * Parallell-sesjoner av Stata. Først installeres pakken parallel dersom den ikke 
# * allerede er installert. Usikker på det helst bør stå "http" eller "https".
# 
# * NB: HVIS DET OPPSTÅR FEIL PÅ DETTE PUNKTET, SÅ PRØV Å AVINSTALLERE parallel-
#   * PAKKEN. HVORDAN? GI KOMMANDOEN "help ado" I KOMMANDOVINDUET.
# net install st0572, from(https://www.stata-journal.com/software/sj19-3) 
# parallel initialize $child_processes
# * parallel initialize 
# sort AARl
# parallel do "`skriptMappe'\`skript'", by(AARl) timeout(60)
# 
# 
# *****************************************
#   local PLL_tmpfiler = `"`r(pll_dir)'"'
# local PLL_t_set = `"`r(pll_t_setu)'"'
# local PLL_t_exe = `"`r(pll_t_calc)'"'
# local PLL_t_app = `"`r(pll_t_fini)'"'
# local PLL_antCh = `"`r(pll_n)'"'
# *****************************************
#   
#   
#   * Lime linjene med SPV=9 (er vel egentlig TELLER_f=9) på igjen. 
# append using "`TELLER_f_9'"
# 
# 
# capture destring KJONN, force replace
# replace spv_tmp=0 if spv_tmp==.
# 
# 
# 
# * Gamle greier som ikke slår inn lenger? (Jørgen 7.mai.2021)
# * --- Ekstraordinær skjuling -------------(hvis det blir bedt om dette)---------
#   local eksekver "`eo_skjul'"
# `eksekver'
# // HUSK PÅ SPVFLAGGET I DISSE SITUASJONENE
#  // ----------------------------------end Ekstraordinær skjuling --------------
# 
# 
# * pause mellom flagging og prikking
#  
# * 3. SKJULING. PRIKKING
# ********************************************************************************
# * HAR FLYTTET HELE DETTE AVSNITTET, UNNTATT SPVFLAGGing, hit TIL SLUTTEN. MÅ
# * ENDRES EN DEL, F.EKS. 
# *gen spv_tmp=0
# *order spv_tmp, before(skille)
# *BÆRER PREG AV Å HA VÆRT PLASSERT ET ANNET STED TIDLIGERE 
# * Først flagge Serieprikking (flyttet ned hit av JM aug/sept. 2022. Søk på 
# * "JM aug/sept. 2022" for å se hvor det lå før). JM sept. 2022; Dette går ikke:
# * Serieprikkingen må gjøres før naboprikkingen, fordi serieprikking kan erstatte
# * noe av naboprikkingen.
# * replace spv_tmp=3 if andelTu3>.20001 | andelTu6>.50001 // Gammel versjon (før sept. 2022)
# 
# 
# local prikkvar
# foreach maaltall in antall Crude Adjusted SMR MEIS nevner_aarlig Left Right TELLER RATE MALTALL { // er det fler?
# 	capture count if `maaltall'==. //QC: antall prikker før prikking og skjuling
# if _rc==0 { 	// _rc=0 hvis "count if `maaltall'==." gikk bra, altså 
# // _rc=0 hvis i det hele tatt `maaltall' eksisterer. Inne-
# 					// bærer at hele den kommende bolken skippes for en variabel
# 					// som Crude, dersom denne ikke finnes i datasettet. Se f.x.
# 					// datasett som LKU som har indirekte satndardisering.
# 		local prikkvar  `prikkvar' `maaltall' // bygger seg opp til hele den
# 						// maaltall-listen som har inngått i prikkingen. Listen 
# 						// skal bare brukes i en QC-variabel senere(5.1.2015)
# 		local missingbefore = `r(N)'  // fra -count- like ovenfor
# replace `maaltall'=. if spv_tmp==3 | spv_tmp==4
# 		replace TELLER_f=`_f_kode' if spv_tmp==3 | spv_tmp==4
# capture count if `maaltall'==.
# 		local antprikk = `r(N)'-`missingbefore'	//brukes i en QC-variabel senere
# }
# } //-------------------------------------------------- end dette måltall--------
# ********************************************************************************
# 
#  
# pause Har fremdeles alle hjelpevariablene her.
# 
# *browse GEO AAR KJ Land sumt sumn MEIS Tu3 pvern spv_tmp if GEO==1119 & AAR=="2014_2016" 
# 
# * pause slutt naboprikking
# if "`c(current_date)'"=="21 Nov 2022" {
# 	save "O:\Prosjekt\FHP\PRODUKSJON\TEST\dump_post_prikking_`kube'", replace
# }
# 
# drop GEO
# rename GEOtmp GEO
# capture rename  sumteller sumTELLER
# capture rename  sumnevner sumNEVNER
# 
# /* Utgått kode. 
# * Slette hjelpevariabler
# capture gen DUMMY=.
# di "A siste variabel(?)  " "`siste_var'"
# order DUMMY, after(`siste_var')	 // Som regel ALDER. Settes aller øverst
# * Finne var-navn for det nå er siste variabel (ikke lenger ALDER)
# describe, varlist
# local siste_var = word("`r(varlist)'", -1)
# di "B  siste variabel(?)  " "`siste_var'"
# drop DUMMY-`siste_var'
# */
# 
# keep `foerste_var'-`siste_var' pvern serieprikket spv_tmp naboprikketIomg* 
# 
# 
# * li GEO AAR KJ ALDER ICD sumTELLER TELLER_f if (GEO=="55" | GEO=="56") & ICD=="HJERTE_OG_KARSYKDOMMER" & AAR=="2009_2018", sepby(GEO)
# 
# di "`tid0'"  // Bare for testformål (tidtagning)
# di c(current_time)  // Bare for testformål (tidtagning)
# 
# 
# 
# /*
# 
# */
# qui {
# 	n: di "start: $tid0"
# 	n: di "slutt: " c(current_time)
# }
# 
# * Registrere hvilken fil og hvilket skript som ble brukt
# local fil = subinstr(c(filename), "O:\Prosjekt\FHP\PRODUKSJON\RUNTIMEDUMP\", "", 1)
# local skript=subinstr("`skript'", "O:\Prosjekt\FHP\PRODUKSJON\DEVELOP\GEO_naboprikking\sandkasse\PARALLEL_test\", "", 1)
# 
# 
# 
# qui tab GEO
# global antGEOverdier=`r(r)'
# capture tab `spesialdimensjon'
# capture global antSpesdimverdier=`r(r)'
# 
# 
# 
# * Koble på igjen variablene som var overfødige for naboprikkingen
# foreach var in KJONN UTDANN LANDBAK INNVKAT {
# 	capture tostring `var', replace
# }
# capture drop _merge
# sort `koblingsdimensjoner'
# capture merge 1:1 `koblingsdimensjoner' using "`overfloedige_variabler'"
# di _rc
# if _rc!=0 {
# 	di as error "I forkant av naboprikkingen ble en del måltall o.l. tatt ut av filen"
# 	di "fordi de ikke trengtes i naboprikkingen og fordi naboprikkingen da "
# 	di "vil gå fortere. De ble da mellomlagret sammen med ALLE dimensjonene (GEO, AAR,"
# 	di "KJONN, osv.), slik at de etter naboprikkingen (her) kunne kobles tilbake"
# 	di "på kuben igjen. Når skriptet kræsjer her, er det fordi de inkluderte "
# 	di "dimensjonene 'do not uniquely identify observations in the master data',"
# 	di "altså fordi IKKE alle dimensjonene er kommet med i den lokale makroen "
# 	di "'koblingsdimensjoner'. `koblingsdimensjoner' henter sitt innehold fra "
# 	di "kolonnen DIMS i filen kubespec.csv som R nettopp har laget og lagt i"
# 	di "mappen C:\Brukere\<ditt brukernavn>\helseprofil\. Sjekk at kolonnen DIMS"
# 	di "eksisterer, og at listen over dimensjoner i kubefilen virker korrekt. "
# 	merge 1:1 `koblingsdimensjoner' using "`overfloedige_variabler'"
# }
# 
# 
# pause Dette er vel variablene som lastes opp til R-løypen igjen
# 
# preserve
# use "O:\Prosjekt\FHP\PRODUKSJON\DEVELOP\GEO_naboprikking\sandkasse\PARALLEL_test\Tidsforbruk.dta", clear
# insobs 1
# replace PC=c(hostname) in l // siste linje
# replace Dato=c(current_date) in l // in l = siste linje 
# replace PLL_tmpfiler="`PLL_tmpfiler'" in l
# replace PLL_time_setup="`PLL_t_set'" in l
# replace PLL_time_exec="`PLL_t_exe'" in l
# replace PLL_time_append="`PLL_t_app'" in l
# * replace KJONNtriangel="$stata_naboprkjonn"
# 
# replace start=clock( "$tid0", "hms") in l // double MÅ spesifiseres. Alltid med klokkeslett
# replace antÅr= $antAarganger in l
# replace antChild="`PLL_antCh'" in l
# 	capture replace antIterasjoner=$iterasjoner in l // $iterasjoner kan ikke 
# 	// sendes fra child-prosessene til parent-prosessen. Funker bare med 
# 	// -include- i stedet for -parallel-. Så da får vi klare oss uten.
# * replace maxPrikkenivåGEO = $høyestGEOnivånr in l
# replace antKJONNtriangler = `antKJONNtriangler' in l
# replace antAARSAKtriangler =`antSPESIALDIMtriangler' in l 
# replace antGEOtriangler  = `antGEOtriangler' in l
# replace antALDERtriangler =`antALDERtriangler' in l
# replace fil = "`fil'"  in l
# replace skript = "`skript'"  in l
# replace antGeo= $antGEOverdier  in l 
# capture replace antSpesdim= $antSpesdimverdier  in l
# replace antVar= $antVar in l
# replace slutt=clock( c(current_time), "hms") in l // double MÅ spesifiseres. Alltid med klokkeslett
# replace Tidsforbruk = slutt-start 
# format start slutt Tidsforbruk %tcHH:MM:SS // HH:MM må spesifiseres for å unngå sekund-angivelse
# replace Merknad="$merknad" in l
# compress _all
# save "O:\Prosjekt\FHP\PRODUKSJON\DEVELOP\GEO_naboprikking\sandkasse\PARALLEL_test\Tidsforbruk.dta", replace
# /*
# gen double start=clock( "$tid0", "hms") // double MÅ spesifiseres. Alltid med klokkeslett
# gen double slutt=clock( c(current_time), "hms") // double MÅ spesifiseres. Alltid med klokkeslett
# gen  Tidsforbruk = slutt-start 
# format start slutt Tidsforbruk %tcHH:MM:SS // HH:MM må spesifiseres for å unngå sekund-angivelse
# gen antallAAR=. // $antAarganger
# gen antProsesser=. // $child_processes
# gen antGEOverdier=. // $antGEOverdier 
# order Maskin antallAAR antProsesser start slutt Tidsforbruk antGEOverdier
# */
# restore
# capture drop _merge
# macro drop _all
# 
# * ---------------------------------------------------------------------------
# 
# 
# 
