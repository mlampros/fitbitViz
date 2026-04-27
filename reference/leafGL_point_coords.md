# Create a Leafet map (including information pop-ups)

Create a Leafet map (including information pop-ups)

## Usage

``` r
leafGL_point_coords(
  dat_gps_tcx,
  color_points_column = "AltitudeMeters",
  provider = leaflet::providers$Esri.WorldImagery,
  option_viewer = rstudioapi::viewer,
  CRS = 4326
)
```

## Arguments

- dat_gps_tcx:

  this parameter corresponds to the output data.table of the
  'GPS_TCX_data()' function

- color_points_column:

  a character string specifying the column of the output data.table
  ('GPS_TCX_data()' function) that is used in the map-markers. The
  default value is 'AltitudeMeters' but it can be any column of type
  numeric

- provider:

  either a character string specifying a leaflet provider (such as
  'Esri.WorldImagery') or a direct call to the leaflet provider list
  (such as leaflet::providers\$Esri.WorldImagery). The default value is
  leaflet::providers\$Esri.WorldImagery

- option_viewer:

  either NULL or rstudioapi::viewer. If NULL then the output map will be
  shown in the web browser

- CRS:

  an integer specifying the Coordinates Reference System. The
  recommended value for this data is 4326 (which is also the default
  value)

## Value

a leaflet map of class 'leaflet'

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


# ........................
# then visualize the data
# ........................

res_lft <- leafGL_point_coords(
  dat_gps_tcx = res_tcx,
  color_points_column = "AltitudeMeters",
  provider = leaflet::providers$Esri.WorldImagery,
  option_viewer = rstudioapi::viewer,
  CRS = 4326
)
res_lft
} # }
```
