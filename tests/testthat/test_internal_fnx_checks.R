context('Test internal fucntion checks.')
## There are a number of problems or data quirks that occur in the MIP data, one of the adventages of
## aopting the R cdo code in a package is that we can use intermediate functions to robustly test
## and check for a number of data quaility issues. This is the unit testing script for these
## internal funcitons.

testthat::test_that('check_cmip_info', {

  data <- tibble::tibble(year = rep(1990:1994),
                                         value = NA,
                                         variable = 'tas',
                                         domain = 'Amon',
                                         model = 'pacakge',
                                         experiment = 'pacakge',
                                         ensemble = 'package',
                                         grid = 'gn')

  # Check to see if it throws an error.
  testthat::expect_error(check_cmip_info(as.data.frame(data)), 'dt must be a tibble object.')


  # When only one kind of CMIP data is read the function
  # should return true but if multiple types of data are read in then
  # return FALSE.
  testthat::expect_true(check_cmip_info(data))

  data$variable <- c('tas', 'tas', 'pr', 'pr', 'pr')
  testthat::expect_false(check_cmip_info(data))

})

testthat::test_that('check_12_months', {

  # Quickly make some example monthly data.
  monthly_data <- tibble::tibble(year = rep(1990:1994, each = 12),
                                         month = rep(1:12, length(1990:1994)),
                                         value = NA,
                                         variable = 'tas',
                                         domain = 'Amon',
                                         model = 'pacakge',
                                         experiment = 'pacakge',
                                         ensemble = 'package',
                                         grid = 'gn')


  # Make sure error messages are thrown.
  testthat::expect_error(check_12_months(monthly_data[, 1:5]), 'Missing required column names.')

  # Make sure that it reurns expected problem codes.
  out <- check_12_months(dt = monthly_data)
  testthat::expect_equal(unique(out$problem), 0)

  # What problem code should be returned if there is an issue with the number of months in a year?
  code <- problem_codes[fxn == 'check_12_months']$problem

  # There are twice as many months as expected.
  duplicate_months <- rbind(monthly_data, monthly_data)
  out              <- check_12_months(duplicate_months)
  testthat::expect_equal(unique(out$problem), code)

  # If a month is missing.
  missing_month <- monthly_data[monthly_data$month != 12 & monthly_data$year == 1994, ]
  out           <- check_12_months(missing_month)
  testthat::expect_equal(unique(out$problem), code)

})

# Quickly make some example annual data, this data will be used
# by multiple tests.
annual_data <- tibble::tibble(year = 1990:1994,
                                      value = NA,
                                      variable = 'tas',
                                      domain = 'Amon',
                                      model = 'pacakge',
                                      experiment = 'pacakge',
                                      ensemble = 'package',
                                      grid = 'gn')

testthat::test_that('check_annual_dupplicates', {

  # Make sure that the function returns expected output.
  out <- check_annual_dupplicates(annual_data)
  testthat::expect_equal(unique(out$problem), 0)

  duplicate_years <- rbind(annual_data, annual_data)
  out             <- check_annual_dupplicates(duplicate_years)
  code            <- problem_codes[problem_codes$fxn == 'check_annual_dupplicates', ]$problem
  testthat::expect_equal(unique(out$problem), code)

  # Make sure that the function throws error messages.
  testthat::expect_error(check_annual_dupplicates(annual_data[, 1:3]), 'Missing required column names')
  annual_data$model <- c(rep('package', 4), 'bad')
  testthat::expect_error(check_annual_dupplicates(annual_data), 'Trying to process data from muliple MIP sources.')

})

testthat::test_that('check_annual_continuous', {

  out <- check_annual_continuous(dt = annual_data)
  testthat::expect_true(unique(out$problem) == 0)

 missing_data <- annual_data[annual_data$year != 1993, ]
 out          <- check_annual_continuous(missing_data)
 epected_code <- problem_codes[problem_codes$fxn == 'check_annual_continuous', ]$problem
 testthat::expect_true(unique(out$problem) == epected_code)

})
