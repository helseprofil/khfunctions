library(profvis)

path <- file.path(fs::path_home(), "helseprofil", "KHprofiler")
KUBENAVN <- "ENPERSON"


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
