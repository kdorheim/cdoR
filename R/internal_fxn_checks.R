## internal_fnx_checks.R
## This script contains internal functions that are used to check intermediate outputs and ensure data quality.
## Because if there is one lesson I have learned from working with CMIP data is that never assume that something
## is what it is.

# TODO how do we want to keep track of the errors that are being triggered? Also do we want to convert
# this to data.table syntax?


#' Check CMIP info
#'
#' \code{check_cmip_info} Check to make sure that there is only data for a sinlge model / variable /
#' experiment and in a tibble of cmip data.
#'
#' @param dt A tibble object containg CMIP information.
#' @return A true or false if the conditions are met.
check_cmip_info <- function(dt){
  assertthat::assert_that(tibble::is_tibble(dt), msg = 'dt must be a tibble object.')
  dt %>%
    dplyr::select(variable, domain, model, experiment, ensemble, grid) %>%
    dplyr::distinct(.) ->
    info
  nrow(info) == 1
}


#' Check for 12 months of data.
#'
#' \code{check_12_months} Checks to see if the monthly data has 12 observations.
#' If not then return the time series with the relevant problem code.
#'
#' @param dt A tibble containg CMIP information for a time series of monthly data.
#' @return A data frame of monthly data with the problem column indicating if there is an issue with the number of
#' months in the time series.
check_12_months <- function(dt){

  # Check the data frame column names,
  required <- c('year', 'month', 'value', cdoR::cmip6_info)
  assertthat::assert_that(all(required %in% names(dt)), msg = 'Missing required column names.')
  assertthat::assert_that(check_cmip_info(dt), msg = 'Trying to process data from muliple MIP sources.')
  assertthat::assert_that(tibble::is_tibble(dt), msg = 'dt must be a tibble.')

  dt %>%
    dplyr::group_by(year, variable, domain, model, experiment, ensemble, grid) %>%
    dplyr::summarise(N = n()) %>%
    dplyr::ungroup() ->
    month_N

  if(all(month_N$N == 12)){

    dt$problem <- 0

  } else {

    dt$problem <- 1

  }

  dt


}


#' Check for annual data for duplicates.
#'
#' \code{check_annual_dupplicates} Checks to see if the annual data has duplicate years, this is espcially
#' relevant when modeling groups save output in mulitple netcdf files for the same experiment.
#' If not then return the time series with the relevant problem code.
#'
#' @param dt A tibble object containg CMIP information for a time series of annual data.
#' @return A data frame of monthly data with the problem column indicating if there is an issue with the number of
#' months in the time series.
check_annual_dupplicates <- function(dt){

  # Check the data frame column names,
  required <- c('year', 'value', cmip6_info)
  assertthat::assert_that(all(required %in% names(dt)), msg = 'Missing required column names.')
  assertthat::assert_that(check_cmip_info(dt), msg = 'Trying to process data from muliple MIP sources.')

  dt %>%
    dplyr::group_by(year, variable, domain, model, experiment, ensemble, grid) %>%
    dplyr::summarise(N = n()) %>%
    dplyr::ungroup() ->
    year_N

  if(all(year_N$N ==1 )){

    dt$problem <- 0

  } else {

    dt$problem <- 2

  }

  dt


}


#' Make sure that the annual data is continuous.
#'
#' \code{check_annual_continuous} Checks to make sure that annual data is continuous and that there are no gaps in the
#' annual data.
#'
#' @param dt A tibble object containg CMIP information for a time series of annual data.
#' @return A data frame of monthly data with the problem column indicating if there is an issue with the number of
#' months in the time series.
check_annual_continuous <- function(dt){

  required <- c('year', 'value', cmip6_info)
  assertthat::assert_that(all(required %in% names(dt)), msg = 'Missing required column names.')
  assertthat::assert_that(check_cmip_info(dt))

  yrs <- unique(sort(dt$year, FALSE))

  condition <- all(diff(yrs) == 1)

  if(condition){
    dt$problem <- 0
  } else {
    dt$problem <- 3
  }
dt
}




