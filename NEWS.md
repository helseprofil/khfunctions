# khfunctions (development version)

## Summary of changes overall

* Structured code as an R package
* Adding documentation
* Rewritten code to consistently use data.table instead of data.frame
* To reduce memory usage, several functions now alter the data by reference instead of overwriting or copying objects.
* Cleaned up code
** removed inactive parts
** refactored into functions with more descriptive names
** moved relevant comments into roxygen where possible. 
* Collecting all parameters used in `LagFilgruppe()` and `LagKUBE()` into one list `parameters`, reducing the number of arguments passed to internal functions.`
* Adding `stop()` where the previous code just printed a message that something was wrong and continued. 
* Implementing saving of filedumps (pre/post) in `do_special_handling`, as well as in `aggregate_to_periods`, `do_censor_cube`, `do_reshape_var`, and `recode_columns_with_codebook`

## Changes in LagFilgruppe

* Consistently read in files as data.table
* Collect information on recoding with kodebok and cleaning of columns into tables in R, instead of writing continuously to ACCESS
* Adding analysis of the column cleaning, which give a warning if something is not ok
* Output will always be both to NYESTE and to DATERT folder, removing the need for argument `versjonert`
* KODEBOK_LOGG is now written as a date tagged CSV instead of writing to ACCESS
* FILGRUPPE_SJEKK is written as a date tagged CSV, and replaces INNLESLOGG previously written to ACCESS

## Changes in LagKube
* All filegroups are now consistently filtered to only use relevant TABX, AAR, and ALDER based on ACCESS input. This reduces data size, and avoids unnecessary memory use. 
** AAR prior to AAR_START is removed
** ALDER < min and > max ALDER provided is removed
** TABs are filtered to only keep what is provided in TABX(_0)
* Implemented standardization to multiyear-periods. In ACCESS, the last year of the period must be provided. It could be given as AARl=='XXXX', but it is more intuitive to use AARh =='XXXX'
* Filegroups are always read from NYESTE, simplifying the code. 
* When initially loading filegroups, the files will always be read from disk and never from BUFFER. 
* Moved R and STATA censoring into the same function `do_censor_cube`
** R censoring is no longer removing values, only setting flags
