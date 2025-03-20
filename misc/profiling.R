library(profvis)

path <- file.path(fs::path_home(), "helseprofil", "KHprofiler")
KUBENAVN <- "HKR"


for(b in c("master", "dev")){
  gc()
  branch <- b
  usebranch(branch)
  
  # endre for å indikere hvilken branch som brukes, evt juster filnavn ytterligere
  FILENAME <- paste0(KUBENAVN, "_", branch, "_", Sys.Date(), ".html")
  
  # Må sette til norsk for at profvis skal fungere
  Sys.setlocale(locale = "Norwegian")
  p <- profvis(LagKUBE(KUBENAVN, write = F))
  htmlwidgets::saveWidget(p, file.path(path, FILENAME))
  
  rm("RESULTAT")
  rm("BUFFER")
  rm(p)
  gc()
}


# To compare the total time spent, use e.g. system.time(LagKUBE(..)) 


time <- list()
for(b in c("master", "dev")){
  
  gc()
  branch <- b
  usebranch(branch)
  
  time[[branch]] <- system.time(LagKUBE(KUBENAVN, write = F))
  
  rm("RESULTAT")
  rm("BUFFER")
  gc()
  
}


library(microbenchmark)

microbenchmark(gammel = {FinnSumOverAar(KUBE = dt, per = period, FyllMiss = F, AntYMiss = n_missing_year, globs = globs)},
               ny = {calculate_period_sums(dt = dt, period = period, n_missing_year = n_missing_year, globs = globs)},
               times = 50)
