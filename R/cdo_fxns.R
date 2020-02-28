# This script defines the function that use the system2 calls to execute cdo
# opperators.

#' Concatenate netcdf files
#'
#' \code{cdo_concate} Sometimes modeling groups save results for a single experiment
#' in mutiple necdf files. Here we are going to concatenate the files together. This is some
#' thing good to do using cdo instead of R because sometimes the netcdf files will repeate
#' years / months in the different netcdf files and CDO can take care of that with the operator copy.
#'
#' @param  dt the input tibble object that contains cmip data and files to process
#' @return The path to the netcdf of the concatenated ncdf.
cdo_concate <- function(dt, intermed_dir){

  cmip_info <- parse_cmip_info(dt)
  name      <- paste(cmip_info, collapse = '_')
  nc_out    <- file.path(intermed_dir, paste0(name, '-Concat.nc'))
  if(file.exists(nc_out)) {file.remove(nc_out)}

  system2(cdoR::cdo_exe, args = c('cat', dt[['file']], nc_out), stdout = TRUE, stderr = TRUE)

  assertthat::assert_that(file.exists(nc_out))
  nc_out
}


#' Calculate the land area per grid cell
#'
#' \code{cdo_land_area} Calculate the land area per grid cell from the land fraction
#' and gird cell area files. The netcdf returned by this function can be used as a land
#' area weight when taking the weighted mean.
#'
#' @param  dt a wide tibble of area and land fraction data to process.
#' @return The path to the netcdf of the land area.
#' @export
cdo_land_area <- function(dt, intermed_dir){

  # Check the inputs
  assertthat::assert_that(all(c('areacella', 'sftlf') %in% names(dt)))
  info <- parse_cmip_info(dt, not_required = 'variable')
  basename <- paste(info, collapse = '_')
  PercentLand_nc <- file.path(intermed_dir, paste0(basename, '_PercentLand.nc'))
  LandArea_nc    <- file.path(intermed_dir, paste0(basename, '_LandArea.nc'))

  system2(cdoR::cdo_exe, args = c("-divc,100", dt[['sftlf']], PercentLand_nc), stdout = TRUE, stderr = TRUE)
  system2(cdoR::cdo_exe, args = c("-mul", dt[['areacella']], PercentLand_nc, LandArea_nc), stdout = TRUE, stderr = TRUE)

  assertthat::assert_that(file.exists(LandArea_nc))
  LandArea_nc

}


#' Calculate the ocean area per grid cell
#'
#' \code{cdo_ocean_area} Calculate the ocean area per grid cell from the land fraction
#' and gird cell area files. The netcdf returned by this function can be used as a
#' area weights to calcualte the mean weighted value over the oceans.
#'
#' @param  dt a wide tibble of area and land fraction data to process.
#' @return The path to the netcdf of the ocean area.
#' @export
cdo_ocean_area <- function(dt, intermed_dir){

  # Check the inputs
  assertthat::assert_that(all(c('areacella', 'sftlf') %in% names(dt)))
  info <- parse_cmip_info(dt, not_required = 'variable')
  basename <- paste(info, collapse = '_')
  PercentOcean_nc <- file.path(intermed_dir, paste0(basename, '_PercentOcean.nc'))
  OceanArea_nc    <- file.path(intermed_dir, paste0(basename, '_OceanArea.nc'))

  system2(cdoR::cdo_exe, args = c("-addc,1","-mulc,-0.01", dt[['sftlf']], PercentOcean_nc), stdout = TRUE, stderr = TRUE)
  system2(cdoR::cdo_exe, args = c("-mul", dt[['areacella']], PercentOcean_nc, OceanArea_nc), stdout = TRUE, stderr = TRUE)

  assertthat::assert_that(file.exists(OceanArea_nc))
 OceanArea_nc

}

#'
#' #' Calculate the area weighted field mean.
#' #'
#' #' \code{cdo_fldmean_area} Calculate the area weighted field mean for some varaibel.
#' #'
#' #' @param name A string for the base name of the intermediate netcdf files that will
#' #' be used to label the intermediate netcdf files.
#' #' @param in_nc the path for the input netcdf file that is going to be procssed.
#' #' @param area_nc the path for the area netcdf file that will be used as the area weights.
#' #' @param intermed_dir the path of the directory where all of the intermediate netcdf
#' #' files will be saved to.
#' #' @param showMessages default set to FALSE so hide messages, if set to TRUE will print messages
#' #' that may be helpful during the debugging process.
#' #' @return A data.table of the weighted fldmean result
#' cdo_fldmean_area <- function(name, in_nc, area_nc, intermed_dir, showMessages = FALSE){
#'   # TODO there is some problem wtih the cmip6 and cdo compatbility
#'   # the commands that worked on cmip5 netcdfs are no longer work. This
#'   # is puzzeling and I wonder if using the copy function could some how
#'   # overwrite whatever projections were there.
#'   stop()
#'   assertthat::assert_that(file.exists(area_nc))
#'   assertthat::assert_that(dir.exists(intermed_dir))
#'   areadata_nc <- file.path(intermed_dir, paste0(name, '-AreaData.nc'))
#'   out_nc      <- file.path(intermed_dir, paste0(name, '-fldmean.nc'))
#'   if(file.exists(areadata_nc)) file.remove(areadata_nc)
#'   if(file.exists(out_nc)) file.remove(out_nc)
#'
#'   system2(cdoR::cdo_exe, args = c(paste0("setgridarea,", area_nc), in_nc, areadata_nc), stdout = TRUE, stderr = TRUE)
#'   system2(cdoR::cdo_exe, args = c('fldmean,weights=TRUE', areadata_nc, out_nc), stdout = TRUE, stderr = TRUE)
#'
#'   assertthat::assert_that(file.exists(out_nc))
#'   out_nc
#' }

#' Calculate the annual average weighted by days in a month
#'
#' \code{cdo_yearmonmean} Calculate the annual average weighted by the number of
#' days in each month.
#'
#' @param  name a string of infomration that will be appended to the name of the netcdf.
#' @param intermed_dir the directory where the resulting netcdf will be stored.
#' @return The path to the netcdf of the annual average.
#' @export
cdo_yearmonmean <- function(name, in_nc, intermed_dir){

  assertthat::assert_that(file.exists(in_nc))
  assertthat::assert_that(dir.exists(intermed_dir))
  out_nc      <- file.path(intermed_dir, paste0(name, '-yearmonmean.nc'))
  if(file.exists(out_nc)) file.remove(out_nc)

  system2(cdoR::cdo_exe, args = c('yearmonmean', in_nc, out_nc), stdout = TRUE, stderr = TRUE)

  assertthat::assert_that(file.exists(out_nc))
  out_nc
}

#' Calculate the area weighted field mean without using cdo.
#'
#' \code{fldmean_area} Calculate the area weighted field mean for some varaible, without using
#' cdo operators. It looks like there might be some issues with how the cdo code is working
#' with the cmip6 netcdfs, untill those issuse are resolved the workaround will be to use
#' R to do the area weighting average.
#'
#' @param info The dataframe of CMIP information such as variable / ensemble / domain / ect.
#' @param in_nc the path for the input netcdf file that is going to be procssed.
#' @param area_nc the path for the area netcdf file that will be used as the area weights.
#' @param area_var the string name of the area variable, typically set to the default "areacella" but could also be "areacello"
#' @param showMessages default set to FALSE to hide messages
#' @return A tibble of the area weighted mean.
#' @export
fldmean_area <- function(info, in_nc, area_nc, area_var = 'areacella', showMessages = FALSE){

  assertthat::assert_that(file.exists(area_nc))
  assertthat::assert_that(file.exists(in_nc))

  if(showMessages) message('extracting time')
  nc <- ncdf4::nc_open(in_nc)
  time <- format_time(nc)
  data <- ncdf4::ncvar_get(nc, info$variable)

  area <- ncdf4::ncvar_get(ncdf4::nc_open(area_nc), area_var)

  mean <- apply(data, 3, weighted.mean, w = area, na.rm = TRUE)

  cbind(time,
        value = mean,
        units = ncdf4::ncatt_get(nc, info$variable)$unit,
        info)

}


#' Format the data frame of the lon/lat boundaries such that the cutoff values
#' are relvant to the netcdf file that is being porcessed. This is helpful because
#' not all of the models report output on the using 0 to 360 and -90 to 90 coords.
#'
#' \code{internal_format_coord_input} Format the lat and lon box boundries to reflect
#' the coordinate system used by the data netcdf file.
#'
#' @param file The netcdf file with the coordinate system the lat/lon coordinate data frame must match.
#' @param input A data frame of lat/lon coordinates;
#' @return A tibble of the appropriate lat/lon coordinates.
# TODO add unit test
internal_format_coord_input <- function(file, input){

  # Check the input data frame.
  assertthat::assert_that(is.data.frame(input), msg = 'Make sure that the input is a dataframe.')
  required_columns <- c('name', 'lat1', 'lat2', 'lon1', 'lon2')
  missing <- required_columns[which(!required_columns %in% names(input))]
  assertthat::assert_that(length(missing) == 0, msg = cat('Input missing', paste(required_columns, collapse = ', '), ' columns.'))

  assertthat::assert_that(all(input[['lon1']] < input[['lon2']]), msg = 'lon1 must be less than lon2')
  assertthat::assert_that(all(input[['lat1']] < input[['lat2']]), msg = 'lat1 must bes less than lat2')

  # This function assumes that the coordinates being read into the function
  # are on the scale of 0 to 360 on lon and -90 to 90 for lat.
  nc <- ncdf4::nc_open(file)
  min_lon <- min(ncdf4::ncvar_get(nc, 'lon'))
  max_lat <- max(ncdf4::ncvar_get(nc, 'lat'))
  ncdf4::nc_close(nc)

  if(min_lon < 0){
    input[['lon1']] <- input[['lon1']] + 180
    input[['lon2']] <- input[['lon2']] + 180}

  if(max_lat > 90){
    input[['lat1']] <- input[['lat1']] - 90
    input[['lat2']] <- input[['lat2']] - 90}

  input
}


#' Format sellonlat cdo argument
#'
#' \code{internal_format_sellonlat_cdo_argument} Format the lat and lon information into a
#' vector that matches the format expected by the cdo sellonlat operation.
#'
#' @param lon1 The western longitude boundary
#' @param lon2 The eastern longitude boundary
#' @param lat1 The northern latitude boundary
#' @param lat2 The southern latitude boundary
#' @return A vector that can be used as the argument to the cdo sellonlat operator.
# TODO add unit test
internal_format_sellonlat_cdo_argument <- function(lon1, lon2, lat1, lat2){
  # Based on the sellonlatbox documentation from https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf
  paste0("-sellonlatbox", ",", lon1,",", lon2,",", lat1,",", lat2, sep =",")
}

#' Select a lat/lon box for a single area in a single file.
#'
#' \code{internal_cdo_sellonlat} Select the lat/lon box fomr a single
#' netcdf file.
#'
#' @param name Some string that will be appended to the netcdf file.
#' @param box_name The name of the lat/lon box that is being selected from the netcdf file.
#' @param cdo_arg The sellonlat cdo argument created by \code{internal_format_sellonlat_cdo_argument}
#' @param nc_in The netcdf file to process
#' @param intermed_dir The name of the directory to store the intermediate
#' @return A vector that can be used as the argument to the cdo sellonlat operator.
# TODO add unit test
internal_cdo_sellonlat <- function(name, box_name, cdo_arg, nc_in, intermed_dir){

  # Check the inputs
  assertthat::assert_that(file.exists(nc_in))
  assertthat::assert_that(dir.exists(intermed_dir))
  nc_out    <- file.path(intermed_dir, paste0(name, '-', box_name, '.nc'))
  if(file.exists(nc_out)){file.remove(nc_out)}

  system2(cdoR::cdo_exe, args = c(cdo_arg, nc_in, nc_out), stdout = TRUE, stderr = TRUE)

  assertthat::assert_that(file.exists(nc_out))
  nc_out
}


#' Select a lat/lon box for a single area in a single file.
#'
#' \code{internal_cdo_sellonlat} Select the lat/lon box fomr a single
#' netcdf file.
#'
#' @param basename The string of CMIP information about the file that is being processed, this string
#' will be appended onto the intermediate netcdf files.
#' @param info A tibble of the CMIP information for the in_nc file that is going to be processed.
#' @param in_nc The netcdf file to be processed.
#' @param area_nc The netcdf file containing the area weights information
#' @param latlon_df The dataframe of the lat and lon boundaries, it must include the following columns,
#' name, lon1, lon2, lat1, and lat2.
#' @param intermed_dir The name of the directory to store the intermediate
#' @param cleanUP default set to TRUE to remove the intermediate netcdf files.
#' @return A dataframe of the area weighted field mean at each lat and lon box.
#' @importFrom foreach %do%
#' @export
cdo_sellonlat_fldmean <- function(basename, info, in_nc, area_nc, latlon_df, intermed_dir, cleanUP = TRUE){

  # Convert if needed from the 0W to 360E and -90S to 90N lat/lon boundries
  # to the -180W to 180E and 0S to 190N lat/lon boundries.
  relevant_latlon <- internal_format_coord_input(file = in_nc, input = latlon_df)

  # Format the cdo arguments for each lat/lon box to process.
  args <- mapply(FUN = internal_format_sellonlat_cdo_argument,
                 lon1 = relevant_latlon$lon1,
                 lon2 = relevant_latlon$lon2,
                 lat1 = relevant_latlon$lat1,
                 lat2 = relevant_latlon$lat2)
  names(args) <- relevant_latlon$name

  # For each of the lat/lon boxes select the data and calculate the area weighted average.
  # The result will be a concatnated data frame of the filed mean over all of the
  # lat/lon boxes.
  foreach::foreach(index = seq_along(args), .combine = 'rbind') %do% {

    # Select the cdo argument and box name.
    box_name <- names(args)[[index]]
    cdo_arg  <- args[[index]]

    # Select the relevant area and calcualte the total area.
    box_area <- internal_cdo_sellonlat(name = 'Area', box_name = box_name, cdo_arg = cdo_arg, nc_in = area_nc, intermed_dir = intermed_dir)
    box_area_nc <- ncdf4::nc_open(box_area)
    total_area  <- sum(ncdf4::ncvar_get(box_area_nc, 'areacella'))
    area_units  <- ncdf4::ncatt_get(box_area_nc, 'areacella')$units
    ncdf4::nc_close(box_area_nc)

    # Select the relevant data and calculate the weighted feild mean area.
    box_data <- internal_cdo_sellonlat(name = 'Data', box_name = box_name, cdo_arg = cdo_arg, nc_in = in_nc, intermed_dir = intermed_dir)
    data <- fldmean_area(info = info, in_nc = box_data, area_nc = box_area, showMessages = FALSE)

    # Clean up the intermediate nc files.
    if(cleanUP){file.remove(box_data, box_area)}

    # Add box information to the result data frame.
    data[['box']]        <- box_name
    data[['cdo_arg']]    <- cdo_arg
    data[['area']]       <- total_area
    data[['area_units']] <- area_units
    data
  }

}

