## generate_pacakge_data.R
## This script generates the internal R package data.


#' Path to the CDO executable
#'
#' The path name to the CDO executable. This is currently set up to run on using the CDO installed on PNNL's HPC.
#' To used this pacakge on a different computer this will need to be updated.
#'
#' @format A vector with the length of 1.
#' \describe{A vector of the path to the cdo executable file.}
cdo_exe <- "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo"
usethis::use_data(cdo_exe, overwrite = TRUE)
"cdo_exe"

#' Names of the CMIP6 information columns
#'
#' The names of the different CMIP6 meta data infomration. CMIP6 has an extra information about the grid
#' that is not included in CMIP5.
#' TODO is there a way to make this so that it is easy to work with CMIP5 and CMIP6 data.
#'
#' @format A vector with the length of 8.
#' \describe{A vector of strings of the CMIP6 info names.}
cmip6_info <- c("variable",	"domain",	"model",	"experiment",	"ensemble",	"grid")
usethis::use_data(cmip6_info, overwrite = TRUE)
"cmip6_info"


#' Error code information
#'
#' Information about the different types of error messages returned by internal functions.
#' Based on the Anna Karinnia Coding Principal so problem = 0 means no errors were thrown
#' and any non 0 problem code means that some error was thrown.
#'
#' @format A dataframe of two columns.
#' \describe{\item{problem}{The code number returned by an error message}
#' \item{description}{A breif decription of what problem occured.}}
problem_codes <- data.table::data.table(problem = c(0, 1, 2, 3),
                            description = c('No errors',
                                            'Issue with number of months in monthly timeseries.',
                                            'Issue with the number of observations for each year.',
                                            'Annual data is not continuous'),
                            fxn = c(NA_character_, 'check_12_months', 'check_annual_dupplicates', 'check_annual_continuous'))
usethis::use_data(problem_codes, overwrite = TRUE)
"problem_codes"
