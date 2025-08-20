# khfunctions 1.0.13
- `compute_new_value_from_row_sum()` replaces `LeggTilSumFraRader()`. Fixed bug when too many columns were added, causing problems when aggregating the file. 

## Bugfix
- NORM correctly renamed when MALTALL is not TELLER or RATE
- Quality control of CUBE failed when all sumTELLER or sumNEVNER was missing

# khfunctions 1.0.12 (2025-08-15)

## New features
* In `LagKUBE()`, moved Rsynt SLUTTREDIGER to after POSTPROSESS
* `add_crude_rate()` calculates crude RATE and helper columns, replacing compute_new_value_from_formula. 
* `control_meis_rate()` added to cube quality control, reports min/max of the MEIS/RATE ratio.

## Bugfix
* In `compute_new_value_from_formula()`, the .n column is now set to the maximum of the included .n-columns. Previously it was set to 1. 
* In `estimate_prednevner()`, missing years are filtered out to ensure match between observed and predicted teller. Otherwise, MEIS was not correct.

# Other changes
* Improved reporting on 99-codes in `LagFilgruppe`

# khfunctions 1.0.11 (2025-08-13)

## Bugfix
* Fixed problem in `do_clean_ALDER` where checks for ALDERl > ALDERh was performed on character columns, not numeric.

# khfunctions 1.0.10 (2025-08-12)

## New features
* Implemented `.parquet`-format. 
** Filegroups are now saved as `.parquet` instead of `.rds`.
** `read_filegroup()` now prioritizes reading `.parquet` files
** `read_original_file()` gains the ability to read `.parquet` files (FORMAT must be 'PARQUET'), in preparation for when orgdata produces `.parquet` files.
** QC-files and are saved as `.parquet` (in addition to `.csv`, which will be deprecated in the future)
** Full cube files are saved as `.parquet` instead of `.rds`
** `do_stata_processing()` now write and read `.parquet`, unless values contains norwegian characters (due to encoding).
** `generate_stata_do_file()` is adapted to use the correct files. This saves much time previously spent writing and reading large `.dta`-files
* `check_encoding()` scans character columns in filegroups looking for garbled characters indicating that files are read with wrong encoding.
* A written log of LagFilgruppe is saved, similar to what is saved when running LagKUBE. Both files are now only saved when write = TRUE. 
* `generate_specific_friskvik_indicators()` can be used to generate friskvik indicators without remaking the cube file. Useful if any friskvik spec is wrong or a new indicator must be set up. 
* `update_production_folder()` can be used to update the production folder, removing outdated and adding missing files from DATERT
* Qualcontrol of filegroup
** `analyze_cleanlog()` moved into qualcontrol
** `warn_geo_99` checks if any 99-codes is found, and tell the user to scroll up to find the original codes. 
* Qualcontrol of cube 
** `control_censoring` checks if any value are outside the limits
** `control_standardization` checks if standardization year is the most recent
** `control_aggregation`, `compare_geolevels`, and `compare_bydel_kommune` checks if aggregation between geographical levels is ok for sumteller and sumnevner (if in file)
** Print a summary message after all checks are performed

## Bugfixes
* `fix_column_name_pre_stata()` is no longer failing due to encoding
* `control_censoring()` handles cases where no values were outside the limits resulting in nrow = 0

## Other updates
* Remove all use of `with = FALSE` inside data.table
* In `LagFilgruppe`, codebooklog and cleanlog is only saved if write = TRUE

# khfunctions 1.0.9 (2025-07-02)
* Fix: cube files was saved with wrong name

# khfunctions 1.0.8 (2025-06-26)

* Fixed do_reshape_var when measure.var is not provided and should be all non-id-columns
* Added Censor_type to parameters
* Started implementing qualcontrol in LagKUBE, set to inactive until done

# khfunctions 1.0.7 (2025-06-24)
* Critical bugfix in set_recode_filter_filfiltre() when a cube use > 1 FILFILTRE
* Reactivated FF_RSYNT1 making it possible to add custom R code to a filefilter

# khfunctions 1.0.6 (2025-06-23)
* Critical bugfix when fixing column names pre and post stata handling
* Added parameter `removebuffer` to LagKUBE, to disable removing of original files during processing. This is useful for dev work, and will be necessary when implementing KUBEFAMILIE which is using the same files.

# khfunctions 1.0.5 (2025-06-20)
* Implemented function `fix_befgk_spelling()` to handle problem with inconsistent spelling of BEF_GK[..], which sometimes appears as BEF_Gk.
* Fixed bugs in LagFilgruppe where illegal values for INNVKAT was corrected, and illegal values for ALDER was mixed up with AAR. 
* `analyze_cleanlog()` now only gives a warning if illegal values are found. 
* Fixed bug when converting column names before STATA export

# khfunctions 1.0.4 (2025-06-19)
* Fixed bugs in setting SPVFLAGG
* Fixed bugs in friskvik and write_cube_output
* Fixed bug in year-filter when loading filegroups 
* Ignore case in KUBER::REFVERDI, allowing both siste and SISTE

# khfunctions 1.0.1 (2025-06-17)
* Fixed predictionfilter to make sure last period is always used for moving averages, and GEOniv=='L' is added if missing in REFVERDI
* Fix auto-update function

# khfunctions 1.0.0 (2025-06-16)

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
