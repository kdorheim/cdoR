context('Test internal package data.')
testthat::test_that('cdo_exe', {

  testthat::expect_true(!is.null(cdoR::cdo_exe))
  testthat::expect_true(dir.exists(cdoR::cdo_exe))


})
