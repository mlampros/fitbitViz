# Extract the sf-object and raster extent based on a buffer (in meters)

Extract the sf-object and raster extent based on a buffer (in meters)

## Usage

``` r
extend_AOI_buffer(
  dat_gps_tcx,
  buffer_in_meters = 1000,
  CRS = 4326,
  verbose = FALSE
)
```

## Arguments

- dat_gps_tcx:

  this parameter corresponds to the output data.table of the
  'GPS_TCX_data()' function

- buffer_in_meters:

  an integer value specifying the buffer in meters. The bounding box of
  the input coordinates (longitudes, latitudes) will be extended by that
  many meters. The default value is 1000 meters.

- CRS:

  an integer specifying the Coordinates Reference System. The
  recommended value for this data is 4326 (which is also the default
  value)

- verbose:

  a boolean. If TRUE then information will be printed out in the console

## Value

an object of class list

## Details

To create the buffer in meters using the 'sf' package I had to transform
to another projection - by default I've used 7801 - as suggested in the
following stackoverflow thread,
https://stackoverflow.com/a/54754935/8302386

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

# ....................................................
# then compute the sf-object buffer and raster-extend
# ....................................................

sf_rst_ext <- extend_AOI_buffer(
  dat_gps_tcx = res_tcx,
  buffer_in_meters = 1000,
  CRS = 4326,
  verbose = TRUE
)
sf_rst_ext
} # }
```
