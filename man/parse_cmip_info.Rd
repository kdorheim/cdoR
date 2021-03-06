% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/support_cdo_fxns.R
\name{parse_cmip_info}
\alias{parse_cmip_info}
\title{Parse out the CMIP information}
\usage{
parse_cmip_info(dt, not_required = NA)
}
\arguments{
\item{dt}{a tibble that contains columns of information in addition to other columns, no}

\item{not_required}{a vector of the CMIP infor that is not required}
}
\value{
A data.table of the relvant experiment / model / ect. information.
}
\description{
\code{parse_cmip_info} Extract the CMIP experiment / model / ensemble / ect. information from a
data frame of information to process. This function should work with both CMIP5 and CMIP6 data.
}
