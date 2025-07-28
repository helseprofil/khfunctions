# khfunctions 1.0.10
* Implemented `.parquet`-format. Filegroups are now saved as .parquet in addition to .rds. 
* `read_filegroup()` now prioritizes reading from PARQUET folder
* `read_original_file()` gains the ability to read .parquet files (FORMAT must be 'PARQUET'). When orgdata produces parquet files, this will be more efficient.
* Bugfix: fix_column_name_pre_stata is no longer failing due to encoding

# khfunctions 1.0.9
* Fix: cube files was saved with wrong name

# khfunctions 1.0.8

* Fixed do_reshape_var when measure.var is not provided and should be all non-id-columns
* Added Censor_type to parameters
* Started implementing qualcontrol in LagKUBE, set to inactive until done

# khfunctions 1.0.7
* Critical bugfix in set_recode_filter_filfiltre() when a cube use > 1 FILFILTRE
* Reactivated FF_RSYNT1 making it possible to add custom R code to a filefilter

# khfunctions 1.0.6
* Critical bugfix when fixing column names pre and post stata handling
* Added parameter `removebuffer` to LagKUBE, to disable removing of original files during processing. This is useful for dev work, and will be necessary when implementing KUBEFAMILIE which is using the same files.

# khfunctions 1.0.5
* Implemented function `fix_befgk_spelling()` to handle problem with inconsistent spelling of BEF_GK[..], which sometimes appears as BEF_Gk.
* Fixed bugs in LagFilgruppe where illegal values for INNVKAT was corrected, and illegal values for ALDER was mixed up with AAR. 
* `analyze_cleanlog()` now only gives a warning if illegal values are found. 
* Fixed bug when converting column names before STATA export

# khfunctions 1.0.4
* Fixed bugs in setting SPVFLAGG
* Fixed bugs in friskvik and write_cube_output
* Fixed bug in year-filter when loading filegroups 
* Ignore case in KUBER::REFVERDI, allowing both siste and SISTE

# khfunctions 1.0.1
* Fixed predictionfilter to make sure last period is always used for moving averages, and GEOniv=='L' is added if missing in REFVERDI
* Fix auto-update function

# khfunctions 1.0.0

## Summary of changes overall

* Structured code as an R package
* Adding documentation
* Rewritten code to consistently use data.table instead of data.frame, and implementing collapse functions where possible
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
* Rows deleted via codebook are deleted while stacking original files, limiting the data siz
* Adding analysis of the column cleaning, which forces a crash if something is not ok
* Output will always be both to NYESTE and to DATERT folder, removing the need for argument `versjonert`
* KODEBOK_LOGG is now written as a date tagged CSV instead of writing to ACCESS::KODEBOKLOGG
* FILGRUPPE_SJEKK is written as a date tagged CSV, and replaces ACCESS::INNLESLOGG
* Output are stored as a list "RESULTAT" in the global environment 

## Changes in LagKUBE
* Changed default for write to TRUE
* All filegroups are now consistently filtered to only use relevant TAB1/2/3, AAR, and ALDER based on ACCESS input. This reduces data size, and avoids unnecessary memory use. 
** AAR prior to AAR_START is removed
** ALDER < min and > max ALDER provided is removed
** TABs are filtered to only keep what is provided in TABX(_0)
* Implemented standardization to multiyear-periods. 
** In ACCESS, the last year of the period must be provided. It could be given as AARl=='XXXX', but it is more intuitive to use AARh =='XXXX'
* Filegroups are always read from NYESTE, simplifying the code. 
* When initially loading filegroups, the files will always be read from disk and never from BUFFER. 
* Moved R and STATA censoring into the same function `do_censor_cube`
** R censoring is no longer removing values, only setting flags
