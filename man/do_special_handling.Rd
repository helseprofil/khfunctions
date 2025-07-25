% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/special_handling.R
\name{do_special_handling}
\alias{do_special_handling}
\title{do_special_handling}
\usage{
do_special_handling(name, dt, code, parameters, koblid = NULL)
}
\arguments{
\item{name}{name of RSYNT point, refer to the column, to be used for filedump names}

\item{dt}{data to be processed}

\item{code}{code to be performed, either R or STATA}

\item{parameters}{global parameters}

\item{koblid}{for RSYNT points applied to individual original files, koblid is needed for filedump names}
}
\description{
Do special handling of data files, either in R or STATA.
code must be provided in specific files and specified in STATA to be used at certain points in data processing.
}
