# KHfunctions
Hoved hjernen til data prosessering for helseprofilene ie. FHP og OVP. Flere Stata do filer er også nødvending for at alle funkjsonene skal funke, men de er ikke inkludert her.

Restructurering og refactoring av funksjonen er under arbeid...

OBS!! :memo: Branch `hist` er en samlig av tilfeldige utvalgte historiske filer for **khfunctions** før bruk av GitHub.

:warning: Dokumenter for khfunction er fortsatt under arbeid, men kan allerede leses her
https://folkehelseprofil.github.io/khfunction/ 

# Bruk

For å sikre at den gjeldene *khfunctions.R* filen skal brukes, kjør kodene nedenfor: 

``` r
rm(list = ls())
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_source(repo = "khfunctions", branch = "master", file = "KHfunctions.R")
```

Betingelsen er at du har installert alle pakkene som trengs.


# Installasjon

For å sikre at alle pakker versjoner som brukes i *KHfunctions* er like for alle
maskiner, bruk denne kommandoen:

```r
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_restore("khfunctions")
```

Bruk *SePaaFil.R* fil som vises på nedre høyre vindu i RStudio for å bruke de
mest brukte kommandoene. Din aktive projektmappe blir i
**C:/Users/dittBrukerNavn/helseprofl/khfunctions**.

# Testing

Nye ide eller løsninger i Access kan testes ved bruk av filen *run-test.R*. Der
finner man veiledning om hvordan testing kan gjennomføres.

# Lokal fil 

Bruk av lokal filer kan være nytting i noen tilfeller bl.a dårlig
nettforbindelse eller på reise. Filen *run-local.R* kan brukes til dette. Her
gjelder det BARE for kjøring av `LagFilgruppe()` eller `LagKubeDatertCsv()` når
alle informasjonene er lagt inn i *skarpe* Access dvs. produksjon. 
