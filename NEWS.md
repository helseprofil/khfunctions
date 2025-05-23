# khfunctions (development version)

## Summary of changes overall

* Structured code as an R package
* Rewritten code to consistently use data.table instead of data.frame
* Cleaned up code, removed inactive parts, refactored into functions with more descriptive names
* Collecting all parameters used in `LagFilgruppe()` and `LagKUBE()` into one list, reducing the number of arguments in most other functions. Now most arguments are collected in `parameters`
* Adding `stop()` where the previous code just printed a message that something was wrong and continued

## Changes in LagFilgruppe

* Consistently read in files as data.table
* Collect information on recoding with kodebok and cleaning of columns into tables in R, instead of writing continuously to ACCESS
* Adding analysis of the column cleaning, which give a warning if something is not ok
* Output will always be both to NYESTE and to DATERT folder

## Changes in LagKube
* All filegroups are now consistently filtered to only use relevant TABX, AAR, and ALDER based on ACCESS input. This reduces data size, and avoids unnecessary memory use. 
** AAR prior to AAR_START is removed, ALDER < min and > max ALDER provided is removed, TABs is filtered to only keep what is provided in TABX(_0)
* Implemented standardization to multiyear-periods
* Filegroups are always read from NYESTE, simplifying the code. 
* When initially loading filegroups, the files will always be read from disk and never from BUFFER. 
* Moved R censoring to the same place as STATA censoring, 

