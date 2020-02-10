# This script defines the functions that are useful to be paired with the
# cdo_fxns.

#' Extract mean results
#'
#' \code{extract_mean_results} Extract the results from a netcdf file
#' that does not have gridded data aka as been averaged or something.
#'
#' @param out_nc the output netcdf that has the data to extract.
#' @param info a data.table of the netcdf CMIP information.
#' @return A data.table of the processed netcdf results.
extract_mean_results <- function(out_nc, info){

  nc   <- ncdf4::nc_open(out_nc)
  time <- format_time(nc)

  # TODO this modify this section to make it easier to extract
  # data that has a depth to it (such as co2).
  cbind(time,
        value = ncvar_get(nc, info$variable),
        units = ncatt_get(nc, info$variable)$unit,
        info)

}


#' format time
#'
#' \code{format_time} Format the time information.
#'
#' @param nc an open netcdf file that contains the time information.
#' @return A data.table of the datetime, year, and month.
format_time <- function(nc){

  # Make sure that the object being read in is a netcdf file.
  assertthat::assert_that(class(nc) == "ncdf4")

  # Extract the time and time unit information.
  time_units <- ncatt_get(nc, 'time')$units
  print(time_units)
  time_units <- gsub(pattern = 'days since ', replacement = '', time_units)
  print(time_units)
  time <- lubridate::as_date(ncvar_get(nc, 'time'), origin = time_units)

  # Return the data frame.
  data.table(datetime = time,
             year = lubridate::year(time),
             month = lubridate::month(time))

}


#' Parse out the CMIP information
#'
#' \code{parse_cmip_info} Extract the CMIP experiment / model / ensemble / ect. information from a
#' data frame of information to process. This function should work with both CMIP5 and CMIP6 data.
#'
#' @param dt the input data.table object that contains cmip data and files to process.
#' @param not_required a vector of the CMIP infor that is not required
#' @return A data.table of the relvant experiment / model / ect. information.
parse_cmip_info <- function(dt, not_required = NA){

  # Make sure that all of the cmip information is being parsed out of the data frame
  # and returned as a data frame.
  required <- cdoR::cmip6_info[which(!cdoR::cmip6_info %in% not_required)]
  assertthat::assert_that(data.table::is.data.table(dt))
  assertthat::assert_that(all(required %in% names(dt)))

  # Select the columns that contain cmip information.
  cols <- which(names(dt) %in% cdoR::cmip6_info)
  out  <- unique.data.frame(dt[ , ..cols])

  # Make sure that the object that is going to be returned is a data frame
  # that only has one entry.
  assertthat::assert_that(nrow(out) == 1 & is.data.frame(out))

  # If there is no grid information it means we are processing cmip5 data
  # and drop the grid column, it will only be confuse the intermediate
  # output file nomencalture.
  if(all(is.na(out$grid))) {
    col <-  which(names(out)  == 'grid')
    out <-  out[ ,-..col]
  }

  out
}
