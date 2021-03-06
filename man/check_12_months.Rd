% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal_fxn_checks.R
\name{check_12_months}
\alias{check_12_months}
\title{Check for 12 months of data.}
\usage{
check_12_months(dt)
}
\arguments{
\item{dt}{A tibble containg CMIP information for a time series of monthly data.}
}
\value{
A data frame of monthly data with the problem column indicating if there is an issue with the number of
months in the time series.
}
\description{
\code{check_12_months} Checks to see if the monthly data has 12 observations.
If not then return the time series with the relevant problem code.
}
