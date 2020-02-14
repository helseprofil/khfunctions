args <- commandArgs(trailingOnly = TRUE)
print(args)
source('F:/Prosjekter/Kommunehelsa/PRODUKSJON/BIN/KHfunctions.r')
KHglobs<-SettGlobs()
FG<-LagFilgruppe(args)

