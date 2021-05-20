
context('Shiny-application')

#..............
# required data
#..............

file_heart = system.file('tests_vignette_rds', 'heart_dat.RDS', package = 'fitbitViz')
dat_heart = readRDS(file_heart)

file_sleep = system.file('tests_vignette_rds', 'sleep_ts.RDS', package = 'fitbitViz')
dat_sleep = readRDS(file_sleep)

file_tcx = system.file('tests_vignette_rds', 'res_tcx.RDS', package = 'fitbitViz')
dat_tcx = readRDS(file_tcx)

file_rst = system.file('tests_vignette_rds', 'raysh_rst.tif', package = 'fitbitViz')
dat_rst = raster::raster(file_rst)


#....................................................................
# tests only for the functions that do not require a user-id or token         [ load the data from the .RDS files ]
#....................................................................


testthat::test_that("the function 'ggplot_each_date' returns a ggplot", {

  plt = fitbitViz:::ggplot_each_date(date_intraday = dat_heart$heart_rate_intraday[[1]], date = '2021-03-08')

  testthat::expect_true( inherits(plt, c("gg", "ggplot")) )
})


testthat::test_that("the function 'heart_rate_heatmap' returns a ggplot", {

  plt = fitbitViz::heart_rate_heatmap(heart_rate_intraday_data = dat_heart$heart_rate_intraday, angle_x_axis = 0)

  testthat::expect_true( inherits(plt, c("gg", "ggplot")) )
})


testthat::test_that("the function 'heart_rate_variability_sleep_time' returns a ggplot & a data.table", {

  dat_out = fitbitViz::heart_rate_variability_sleep_time(heart_rate_data = dat_heart,
                                                         sleep_begin = "00H 40M 0S",
                                                         sleep_end = "08H 00M 0S",
                                                         ggplot_hr_var = TRUE,
                                                         angle_x_axis = 25)

  testthat::expect_true( inherits(dat_out$hr_var_plot, c("gg", "ggplot")) & inherits(dat_out$hr_var_data, c('data.frame', 'data.table')) )
})


testthat::test_that("the function 'sleep_heatmap' returns a ggplot & a data.table", {

  sleep_ts = suppressWarnings(fitbitViz:::sleep_heatmap(level_data = dat_sleep$heatmap_data, angle_x_axis = 0))

  testthat::expect_true( inherits(sleep_ts, c("gg", "ggplot")) )
})


testthat::test_that("the function 'leafGL_point_coords' returns a leaflet", {

  tcx_dat = fitbitViz::leafGL_point_coords(dat_gps_tcx = dat_tcx,
                                           color_points_column = 'AltitudeMeters',
                                           provider = leaflet::providers$Esri.WorldImagery,
                                           option_viewer = rstudioapi::viewer,
                                           CRS = 4326)

  testthat::expect_true( inherits(tcx_dat, c("leaflet", "htmlwidget")) )
})


testthat::test_that("the function 'extend_AOI_buffer' returns multiple objects", {

  sf_rst_ext = fitbitViz::extend_AOI_buffer(dat_gps_tcx = dat_tcx,
                                            buffer_in_meters = 1000,
                                            CRS = 4326,
                                            verbose = FALSE)

  testthat::expect_true( inherits(sf_rst_ext, "list") & all(names(sf_rst_ext) %in% c('buffer_bbox', 'sfc_obj', 'raster_obj_extent', 'centroid_buffer')) )
})


testthat::test_that("the function 'gps_lat_lon_to_LINESTRING' returns multiple objects", {

  linestring_dat_lubr = fitbitViz::gps_lat_lon_to_LINESTRING(dat_gps_tcx = dat_tcx,
                                                             CRS = 4326,
                                                             time_split_asc_desc = lubridate::hms('17:05:00'),
                                                             verbose = FALSE)

  testthat::expect_true( inherits(linestring_dat_lubr, "list") & all(names(linestring_dat_lubr) %in% c('line_ASC', 'line_DESC')) & all(unlist(lapply(linestring_dat_lubr, function(x) inherits(x, c("sfc_LINESTRING", "sfc"))))) )
})


testthat::test_that("the function 'gps_lat_lon_to_LINESTRING' returns multiple objects", {

  idx_3m = c(which.min(dat_tcx$AltitudeMeters),
             as.integer(length(dat_tcx$AltitudeMeters) / 2),
             which.max(dat_tcx$AltitudeMeters))

  cols_3m = c('latitude', 'longitude', 'AltitudeMeters')
  dat_3m = dat_tcx[idx_3m, ..cols_3m]

  sf_rst_ext = fitbitViz::extend_AOI_buffer(dat_gps_tcx = dat_tcx,
                                            buffer_in_meters = 1000,
                                            CRS = 4326,
                                            verbose = FALSE)

  res_grid = lapply(1:nrow(dat_3m), function(idx) {

    suppressMessages(fitbitViz:::meshgrids_XY_LatLon(longitude = dat_3m$longitude[idx],
                                                     latitude = dat_3m$latitude[idx],
                                                     buffer_raster = dat_rst,
                                                     buffer_bbox = sf_rst_ext$buffer_bbox,
                                                     distance_metric = "vincenty",
                                                     digits = 8))
  })

  testthat::expect_true( inherits(res_grid, "list") & all(unlist(lapply(res_grid, function(x) all(names(x) %in% c('coords_row', 'dist_meters'))))))
})

