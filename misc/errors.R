files <- list.files("O:/Prosjekt/FHP/PRODUKSJON/RUNTIMEDUMP/KUBELOGG", pattern = ".txt", full.names = T)

error <- numeric()
for(i in 1:length(files)){
  if(sum(grepl("FEIL", readLines(files[i]))) > 0) error <- c(error, i)
}

errorfiles <- files[error]

BEFPROG - "udekka i redesign"
DEPRESJON_UNGDATA
DODE1.1_FORVENTELEVEALDER - "udekka i redesign"
ENSOMHET_UNGDATA 
FORNOYD_HELSA_UNGDATA
FORTROLIGVENN_UNGDATA
FRAMTIDSOPT_UNGDATA
FRITIDSORG_10AAR_UNGDATA
FYSAK_UNGDATA
HASJBRUK_UNGDATA
LIVSKVALITET_UNGDATA
LOKALMIL_UNGDATA
LOKALTILBUD_UNGDATA
MEDIEBRUK_UNDERHOLD_UNGDATA
NEET_UTDANN - "udekka i redesign"
REGELBRUDD_UNGDATA
ROYK_RUS_1 - "udekka i redesign??"
SKJERMTID_UNGDATA
SMERTESTILLENDE_UNGDATA
SOSIALHJELP - "delstatus[[Y]] = B, men kanskje fikset"
SOVN_UNGDATA
SYSVAK_1 - "udekka i redesign"
SYSVAK - "udekka i redesign"
TRYGGHET_UNGDATA
VALGDELTAKELSE  - "udekka i redesign"
VENNER_INNEMEGDEM_UNGDATA
VENNER_UTE_UNGDATA