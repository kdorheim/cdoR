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
usethis::use_data(file.exists(cdo_exe), overwrite = TRUE)
"cdo_exe"


