# KHfunction.R
Hoved hjernen til data prosessering for helseprofilene ie. FHP og OVP. Flere Stata do filer er også nødvending for at alle funkjsonene skal funke, men de er ikke inkludert her.

Restructurering og refactoring av funksjonen er under arbeid...

OBS!! :memo: Branch `hist` er en samlig av tilfeldige utvalgte historiske filer for **khfunction** før bruk av GitHub.

:warning: Dokumenter for khfunction er fortsatt under arbeid, men kan allerede leses her
https://folkehelseprofil.github.io/khfunction/ 

# Installasjon og bruk

For å sikre at alle pakker versjoner som brukes i *KHfunctions* er like for alle
maskiner, kjør denne kommandoen:

```r
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_restore("khfunctions")
```

Bruk *SePaaFil.R* fil som vises på nedre høyre vindu i RStudio for å bruke de
mest brukte kommandoene. Din aktive projektmappe blir i
**C:/Users/dittBrukerNavn/helseprofl/khfunctions**.
