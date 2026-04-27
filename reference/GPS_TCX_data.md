# The GPS-TCX data as a formated data.table

The GPS-TCX data as a formated data.table

## Usage

``` r
GPS_TCX_data(tcx_file, time_zone = "Europe/Athens", verbose = FALSE)
```

## Arguments

- tcx_file:

  a character string specifying the path to a TCX file exported from the
  Fitbit app or website. TCX (Training Center XML) files can be exported
  from the Fitbit website under account settings or from the Fitbit app
  by sharing a specific activity.

- time_zone:

  a character string specifying the time zone parameter ('tz') as is
  defined in the 'lubridate::ymd_hms()' function

- verbose:

  a boolean. If TRUE then information will be printed out in the console

## Value

either NULL or an object of class data.table

## Examples

``` r
if (FALSE) { # \dontrun{

require(fitbitViz)

# export the TCX file from the Fitbit website or app for the desired activity
# then pass the file path to GPS_TCX_data()

res_tcx <- GPS_TCX_data(
  tcx_file = "/path/to/activity.tcx",
  time_zone = "Europe/Athens",
  verbose = TRUE
)
str(res_tcx)
} # }
```
