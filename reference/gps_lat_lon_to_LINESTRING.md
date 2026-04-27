# Convert the GPS, TCX data to a LINESTRING

Convert the GPS, TCX data to a LINESTRING

## Usage

``` r
gps_lat_lon_to_LINESTRING(
  dat_gps_tcx,
  CRS = 4326,
  verbose = FALSE,
  time_split_asc_desc = NULL
)
```

## Arguments

- dat_gps_tcx:

  this parameter corresponds to the output data.table of the
  'GPS_TCX_data()' function

- CRS:

  an integer specifying the Coordinates Reference System. The
  recommended value for this data is 4326 (which is also the default
  value)

- verbose:

  a boolean. If TRUE then information will be printed out in the console

- time_split_asc_desc:

  if NULL then the maximum altitude coordinates point will be used as a
  split point of the route, otherwise the user can give a lubridate
  'hours-minutes-seconds' object such as: lubridate::hms('17:05:00')

## Value

an object of class list

## Details

Separate the Ascending and Descending coordinate points into 2 groups
and give a different color to the Ascending and Descending routes

## Examples

``` r
if (FALSE) { # \dontrun{

require(fitbitViz)

# export the TCX file from the Fitbit website or app for the desired activity

res_tcx <- GPS_TCX_data(
  tcx_file = "/path/to/activity.tcx",
  time_zone = "Europe/Athens",
  verbose = TRUE
)
str(res_tcx)


# ..................................................................
# By using using the maximum altitude as a split point of the route
# ..................................................................

linestring_dat_init <- gps_lat_lon_to_LINESTRING(
  dat_gps_tcx = res_tcx,
  CRS = 4326,
  time_split_asc_desc = NULL,
  verbose = TRUE
)

# .................................................................
# By using a customized split of the route (ascending, descending)
# .................................................................

linestring_dat_lubr <- gps_lat_lon_to_LINESTRING(
  dat_gps_tcx = res_tcx,
  CRS = 4326,
  time_split_asc_desc = lubridate::hms("17:05:00"),
  verbose = TRUE
)
} # }
```
