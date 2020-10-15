# KHfunction.R
Hoved hjernen til data prosessering for helseprofilene ie. FHP og OVP. Flere Stata do filer er ikke inkludert her.

Restructurering og refactoring av funksjonen er under arbeid...

OBS!! :memo: Branch `hist` er en samlig av tilfeldige utvalgte historiske filer for **khfunction** før bruk av GitHub.

:warning: Dokumenter for khfunction er fortsatt under arbeid, men kan allerede leses her
https://folkehelseprofil.github.io/khfunction/ 

# Bruk

- Alle funksjoner her anvender felles cache for R pakker som ligger i **PDB** med mappenavn **rsync**.
- For førstegangsbruk kjør `renv::restore()` i console.
- Filen *renv.lock* skal ikke endres manuelt.
