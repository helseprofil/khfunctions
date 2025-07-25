% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/special_handling.R
\name{do_stata_processing}
\alias{do_stata_processing}
\title{do_stata_processing}
\usage{
do_stata_processing(dt, script, parameters)
}
\arguments{
\item{script}{stata script}

\item{TABLE}{data to be processed}

\item{batchdate}{used to generate file names}

\item{stata_exe}{path to STATA program}
}
\description{
Fix column names not allowed in STATA.
Save data to ..helseprofil/STATAtmp/
Generate .do-file based on STATA syntax passed to script
Start STATA and run .do-file with the .dta, overwrites .dta file.
Reads .log file to check for errors, function stops if any error occured.
Read, revert column name changes, and return processed file as data.table.
}
