# This script defines the functions that are going to be used to process netcdfs in large batches.

#' Calculate the weighted means
#'
#' \code{calculate_weighted_mean} Use gridded netcdfs to calculate the weighted area for some
#' fld mean.
#'
#' @param dt a data.table of CMIP information with the area file to process.
#' @param intermed_dir the directory path indicating where to save intermeidate output files to.
#' @param output_dir the directory location indicating where to save the final csv output to.
#' @return A data.table of the datetime, year, and month.
#' @export
calculate_weighted_mean <- function(dt, intermed_dir, output_dir){

  req_cols <- c('file', cdoR::cmip6_info, 'area')
  assertthat::assert_that(all(req_cols %in% names(dt)))

  to_process <- split(dt, interaction(dt$variable, dt$domain, dt$model, dt$experiment, dt$ensemble, dt$grid, drop = TRUE))

  lapply(to_process, function(input){

    info <- parse_cmip_info(input)
    basename <- paste(info, collapse = '_')
    out_csv  <- file.path(output_dir, paste0(basename, '_WeightedMean.csv'))

    area_nc  <- unique(input$area)

    cat_nc  <- cdo_concate(input, intermed_dir)
    mean_nc <- cdo_fldmean_area(basename, cat_nc, area_nc, intermed_dir)

    rslt <- extract_mean_results(mean_nc, info)
    write.csv(rslt, file = out_csv, row.names = FALSE)

    out_csv
  }) %>%
    unlist

}
