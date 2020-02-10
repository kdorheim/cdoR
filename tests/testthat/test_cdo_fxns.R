context('Test cdo functions.')

# Define the directory to write out all of the intermediate netcdf files to.
test_dir <- getwd()

# Create a fake cmip data frame for the test data.
dt <- data.table::data.table(file = c("./data/areacella_fx_model_experiment_ensemble_grid.nc",
                                      "./data/sftlf_fx_model_experiment_ensemble_grid.nc",
                                      "./data/tas_domain_model_experiment_ensemble_grid_time.nc"),
                             variable = c('areacella', 'sftlf', 'tas'),
                             domain = c('domain', 'domain', 'domain'),
                             model = c('model', 'model', 'model'),
                             experiment = c('experiment', 'experiment', 'experiment'),
                             ensemble = c('ensemble', 'ensemble', 'ensemble'),
                             grid = c('grid', 'grid', 'grid'))


# Format the input data for the cdo land and ocean area weights.
area_in <- dcast(dt, domain + model + experiment + ensemble + grid ~ variable, value.var = 'file')

land_area <- cdo_land_area(area_in, test_dir)
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

  testthat::expect_equal(land_area_rslt + ocean_area_rslt, globe_area_rslt)
  testthat::expect_equal(ocean_area_rslt / land_area_rslt, 70 / 30,  tolerance = 0.1)

})


file.remove(land_area, ocean_area)
