# heart rate activity time series

heart rate activity time series

## Usage

``` r
heart_rate_time_series(
  heart_rate_intraday_list,
  heart_rate = NULL,
  detail_level = "1min",
  ggplot_intraday = FALSE,
  ggplot_ncol = NULL,
  ggplot_nrow = NULL,
  verbose = FALSE
)
```

## Arguments

- heart_rate_intraday_list:

  a named list of data.tables with the intraday heart rate data. Each
  element is a data.table with columns `time` (character, format
  "YYYY-MM-DD HH:MM:SS") and `value` (numeric heart rate). Names must be
  dates in "YYYY-MM-DD" format. This is the pre-downloaded intraday
  heart rate data.

- heart_rate:

  either NULL or a data.frame with the daily heart rate summary data

- detail_level:

  a character string specifying the detail level of the heart rate time
  series. It can be either '1min' or '1sec', for 1-minute and 1-second
  intervals

- ggplot_intraday:

  a boolean. If TRUE then the ggplot of the heart rate time series will
  be returned too

- ggplot_ncol:

  either NULL or an integer specifying the number of columns of the
  output ggplot

- ggplot_nrow:

  either NULL or an integer specifying the number of rows of the output
  ggplot

- verbose:

  a boolean. If TRUE then information will be printed out in the console

## Value

an object of class list

## Examples

``` r
if (FALSE) { # \dontrun{

require(fitbitViz)

# load pre-downloaded intraday heart rate data (one data.table per day, named by date)
# heart_rate_intraday_list is a named list where each element has columns 'time' and 'value'

heart_dat <- heart_rate_time_series(
  heart_rate_intraday_list = heart_rate_intraday_list,
  heart_rate = heart_rate_daily,
  detail_level = "1min",
  ggplot_intraday = TRUE,
  verbose = TRUE
)
heart_dat$plt
heart_dat$heart_rate
heart_dat$heart_rate_intraday
} # }
```
