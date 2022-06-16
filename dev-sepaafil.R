rm(list = ls())
source("KHfunctions.R")
source("KHfunctions.R", encoding = "UTF-8")

df <- LagKUBE("LESEFERD_INNVKAT")

k <- LagKUBE("RFU_NH_ROYK_5_fildumpdummy")





## stata kode for prikk
<STATA>
   include "F:\Forskningsprosjekter\PDB 2455 - Helseprofiler og til_\PRODUKSJON\BIN\Z_Statasnutter\Rsynt_Postprosess_naboprikking_del_1_LESEFERD_INNV.do"
include "F:\Forskningsprosjekter\PDB 2455 - Helseprofiler og til_\PRODUKSJON\BIN\Z_Statasnutter\Rsynt_Postprosess_naboprikking_del_2_FELLES.do"

* Ferdig
