context('Test cdo functions.')

# Define the directory to write out all of the intermediate netcdf files to.
test_dir <- getwd()

# Create a fake cmip data frame for the test data.
dt <- data.table::data.table(file = c(file.path(test_dir, "data/areacella_fx_CNRM-CM6-1_historical_r10i1p1f2_gr.nc"),
                                      file.path(test_dir, "data/sftlf_fx_CNRM-CM6-1_historical_r10i1p1f2_gr.nc"),
                                      file.path(test_dir, "data/tas_Amon_CNRM-CM6-1_historical_r10i1p1f2_gr_185001-201412.nc")),
                             variable = c('areacella', 'sftlf', 'tas'),
                             domain = c('domain', 'domain', 'domain'),
                             model = c('model', 'model', 'model'),
                             experiment = c('experiment', 'experiment', 'experiment'),
                             ensemble = c('ensemble', 'ensemble', 'ensemble'),
                             grid = c('grid', 'grid', 'grid'))


# Format the input data for the cdo land and ocean area weights.
area_in <- dcast(dt, domain + model + experiment + ensemble + grid ~ variable, value.var = 'file')
tas_nc <- dt[variable == 'tas']$file
dt_tas <- dt[variable == 'tas']

land_area <- cdo_land_area(dt = area_in, intermed_dir = test_dir)
ocean_area <- cdo_ocean_area(area_in, test_dir)

testthat::test_that('test environement set up correctly', {

testthat::expect_true(all(file.exists(dt$file)))

})

testthat::test_that('generate area weights works', {

  # Extract the total area, ocean, and land areas should equal the total area.
  # Since the oceans cover 70% of the globe the ratio of the ocean to land area
  # should reflect this.
  land_area_rslt  <- sum(ncdf4::ncvar_get(ncdf4::nc_open(land_area), 'areacella'))
  ocean_area_rslt <- sum(ncdf4::ncvar_get(ncdf4::nc_open(ocean_area), 'areacella'))
  globe_area_rslt <- sum(ncdf4::ncvar_get(ncdf4::nc_open(area_in[['areacella']]), 'areacella'))

  testthat::expect_equal(land_area_rslt + ocean_area_rslt, globe_area_rslt, tolerance = 0.1)
  testthat::expect_equal(ocean_area_rslt / land_area_rslt, 70 / 30,  tolerance = 0.1)

})

cat_tas <- cdo_concate(dt = dt_tas, intermed_dir = test_dir)

testthat::test_that('cdo_concate runs',{
  testthat::expect_true(file.exists(cat_tas))
})


testthat::test_that('fldmean works', {

  # Calculate the weighted means with the different area weights, then compare temperature.
  ## TODO we are currently using an R based function which is going to be slow, but we should
  ## be able to
  land_mean   <- cdo_fldmean_area(name = 'land', in_nc = tas_nc, area_nc = land_area, intermed_dir = test_dir)
  ocean_mean  <- cdo_fldmean_area(name = 'ocean', in_nc = tas_nc, area_nc = ocean_area, intermed_dir = test_dir)
  global_mean <- cdo_fldmean_area(name = 'globe', in_nc = tas_nc, area_nc = area_in[['areacella']], intermed_dir = test_dir)

  land_tas    <- ncdf4::ncvar_get(ncdf4::nc_open(land_mean), 'tas')
  ocean_tas   <- ncdf4::ncvar_get(ncdf4::nc_open(ocean_mean), 'tas')
  glaobal_tas <- ncdf4::ncvar_get(ncdf4::nc_open(global_mean), 'tas')

})

file.remove(list.files(test_dir, pattern = '.nc'))
