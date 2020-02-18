context('Test cdo functions.')

# Only test the cdo functions if the correct environment is set up for it.
if(file.exists(cdoR::cdo_exe)){

  # Define the directory to write out all of the intermediate netcdf files to.
  test_dir <- getwd()

  # Create a fake cmip data frame for the test data.
  # TODO figure out if the testing netcdfs should be commited to git, if they are too pic should the comparison
  # data be pulled from the cmip6 archive??
  dt <- tibble::tibble(file = c(file.path(test_dir, "data/areacella_fx_CNRM-CM6-1_historical_r10i1p1f2_gr.nc"),
                                file.path(test_dir, "data/sftlf_fx_CNRM-CM6-1_historical_r10i1p1f2_gr.nc"),
                                file.path(test_dir, "data/tas_Amon_CNRM-CM6-1_historical_r10i1p1f2_gr_185001-201412.nc")),
                       variable = c('areacella', 'sftlf', 'tas'),
                       domain = c('domain', 'domain', 'domain'),
                       model = c('model', 'model', 'model'),
                       experiment = c('experiment', 'experiment', 'experiment'),
                       ensemble = c('ensemble', 'ensemble', 'ensemble'),
                       grid = c('grid', 'grid', 'grid'))


  # Format the input data for the cdo land and ocean area weights.
  #area_in <- dcast(dt, domain + model + experiment + ensemble + grid ~ variable, value.var = 'file')
  area_in <- tidyr::spread(dt, variable, file)
  tas_nc <- dt[dt$variable == 'tas', ][['file']]
  dt_tas <- dt[dt$variable == 'tas', ]
  info   <- parse_cmip_info(dt_tas)
  name   <- paste(info, collapse = '_')


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

  annual_nc <- cdo_yearmonmean(name = name, in_nc = tas_nc, intermed_dir = test_dir)

  testthat::test_that('cdo_yearmonmean runs',{
    monthly <- ncdf4::ncvar_get( ncdf4::nc_open(tas_nc), 'tas')
    annual <- ncdf4::ncvar_get( ncdf4::nc_open(annual_nc), 'tas')

    monthly_temporal_dim <- dim(monthly)[3]
    annual_temporal_dim  <- dim(annual)[3]

    testthat::expect_equal(monthly_temporal_dim/12, annual_temporal_dim)

  })


  testthat::test_that('fldmean works', {

    # Calculate the weighted means with the different area weights, then compare temperature.
    ## TODO we are currently using an R based function which is going to be slow, but we should
    ## be able to make it work.
    land_mean   <- fldmean_area(info = info, in_nc = annual_nc, area_nc = land_area)
    ocean_mean  <- fldmean_area(info = info, in_nc = annual_nc, area_nc = ocean_area)
    global_mean <- fldmean_area(info = info, in_nc = annual_nc, area_nc = area_in[['areacella']])

    # The ocean, land, and global mean temperature should be different from one another.
    testthat::expect_true(mean((land_mean$value - ocean_mean$value)^2) > 0)
    testthat::expect_true(mean((land_mean$value - global_mean$value)^2) > 0)

  })

  file.remove(list.files(test_dir, pattern = '.nc'))


} else {

  testthat::test_that('Skipping CDO Related Functions', {


  })


}


