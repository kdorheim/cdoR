context('Test cdo functions.')

dt <- data.table::data.table(file = c("./data/areacella_fx_model_experiment_ensemble_grid.nc",
                                      "./data/sftlf_fx_model_experiment_ensemble_grid.nc",
                                      "./data/tas_domain_model_experiment_ensemble_grid_time.nc"),
                             variable = c('areacella', 'sftlf', 'tas'),
                             domain = c('domain', 'domain', 'domain'),
                             model = c('model', 'model', 'model'),
                             experiment = c('experiment', 'experiment', 'experiment'),
                             ensemble = c('ensemble', 'ensemble', 'ensemble'),
                             grid = c('grid', 'grid', 'grid'))

testthat::test_that('test environement set up correctly', {

testthat::expect_true(all(file.exists(dt$file)))

})
