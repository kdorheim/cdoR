# This script defines the function that use the system2 calls to execute cdo
# opperators.

#' Concatenate netcdf files
#'
#' \code{cdo_concate} Sometimes modeling groups save results for a single experiment
#' in mutiple necdf files. Here we are going to concatenate the files together. This is some
#' thing good to do using cdo instead of R because sometimes the netcdf files will repeate
#' years / months in the different netcdf files and CDO can take care of that with the operator copy.
#'
#' @param  dt the input data.table object that contains cmip data and files to process
#' @return The path to the netcdf of the concatenated ncdf.
cdo_concate <- function(dt, intermed_dir){

  cmip_info <- parse_cmip_info(dt)
  name      <- paste(cmip_info, collapse = '_')
  nc_out    <- file.path(intermed_dir, paste0(name, '-Concat.nc'))
  if(file.exists(nc_out)) {file.remove(nc_out)}

  system2(cdoR::cdo_exe, args = c('cat', dt[['file']], nc_out), stdout = TRUE, stderr = TRUE)

  assertthat::assert_that(file.exists(nc_out))
  as.string(nc_out)
}


#' Calculate the land area per grid cell
#'
#' \code{cdo_land_area} Calculate the land area per grid cell from the land fraction
#' and gird cell area files. The netcdf returned by this function can be used as a land
#' area weight when taking the weighted mean.
#'
#' @param  dt a wide data.table of area and land fraction data to process.
#' @return The path to the netcdf of the land area.
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
  as.character(LandArea_nc)

}


#' Calculate the ocean area per grid cell
#'
#' \code{cdo_ocean_area} Calculate the ocean area per grid cell from the land fraction
#' and gird cell area files. The netcdf returned by this function can be used as a
#' area weights to calcualte the mean weighted value over the oceans.
#'
#' @param  dt a wide data.table of area and land fraction data to process.
#' @return The path to the netcdf of the ocean area.
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
  as.character(OceanArea_nc)

}


#' Calculate the area weighted field mean.
#'
#' \code{cdo_fldmean_area} Calculate the area weighted field mean for some varaibel.
#'
#' @param name A string for the base name of the intermediate netcdf files that will
#' be used to label the intermediate netcdf files.
#' @param in_nc the path for the input netcdf file that is going to be procssed.
#' @param area_nc the path for the area netcdf file that will be used as the area weights.
#' @param intermed_dir the path of the directory where all of the intermediate netcdf
#' files will be saved to.
#' @return A data.table of the weighted fldmean result
cdo_fldmean_area <- function(name, in_nc, area_nc, intermed_dir){

  assertthat::assert_that(file.exists(area_nc))
  assertthat::assert_that(dir.exists(intermed_dir))
  areadata_nc <- file.path(intermed_dir, paste0(name, '-AreaData.nc'))
  out_nc      <- file.path(intermed_dir, paste0(name, '-fldmean.nc'))
  if(file.exists(areadata_nc)) file.remove(areadata_nc)
  if(file.exists(out_nc)) file.remove(out_nc)

  system2(cdoR::cdo_exe, args = c(paste0("setgridarea,", area_nc), in_nc, areadata_nc), stdout = TRUE, stderr = TRUE)
  system2(cdoR::cdo_exe, args = c('fldmean,weights=TRUE', paste0("-setgridarea,", area_nc), in_nc, out_nc), stdout = TRUE, stderr = TRUE)

  assertthat::assert_that(file.exists(out_nc))
  out_nc
}
