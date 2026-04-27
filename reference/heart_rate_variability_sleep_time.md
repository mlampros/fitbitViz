# Heart Rate Variability during Sleep Time (the root mean square of successive differences)

\`r lifecycle::badge("deprecated")\`

This function was deprecated, so please use the
'fitbit_data_type_by_date()' function instead with the 'type' parameter
set to 'hrv' (Heart Rate Variability). See the documentation and the
example section of the 'fitbit_data_type_by_date()' function for more
details.

## Usage

``` r
heart_rate_variability_sleep_time(
  heart_rate_data,
  sleep_begin = "00H 40M 0S",
  sleep_end = "08H 00M 0S",
  ggplot_hr_var = TRUE,
  angle_x_axis = 45
)
```

## Arguments

- heart_rate_data:

  a list object. This is the output of the 'heart_rate_time_series()'
  function

- sleep_begin:

  a character string specifying the begin of the sleep time. For
  instance, the time "00H 40M 0S" where the input order is
  'hours-minutes-seconds' and the format corresponds to the
  'lubridate::hms()' function

- sleep_end:

  a character string specifying the end of the sleep time. For instance,
  the time "08H 00M 0S" where the input order is 'hours-minutes-seconds'
  and the format corresponds to the 'lubridate::hms()' function

- ggplot_hr_var:

  a boolean. If TRUE then the ggplot of the heart rate variability will
  be returned

- angle_x_axis:

  an integer specifying the angle of the x-axis labels. The default
  values is 45 (it can take for instance values such as 0, 90 etc.)

## Value

an object of class list

## Details

I use the '1min' rather than the '1sec' interval because it is
consistent (it shows the 1-minute differences), whereas in case of
'1sec' the difference between observations varies between 1 second and
less than 60 seconds

This function calculates the root mean square of successive differences
(RMSSD) and a higher heart rate variability is linked with better health

Based on the Fitbit application information weblink and the Wikipedia
article (https://en.wikipedia.org/wiki/Heart_rate_variability) the heart
rate variability is computed normally in ms (milliseconds)

## Examples

``` r
if (FALSE) { # \dontrun{

require(fitbitViz)

# ...........................................
# first compute the heart rate intraday data
# ...........................................

USER_ID <- "99xxxx"
token <- "my_long_web_api_token"

heart_dat <- heart_rate_time_series(
  user_id = USER_ID,
  token = token,
  date_start = "2021-03-09",
  date_end = "2021-03-16",
  time_start = "00:00",
  time_end = "23:59",
  detail_level = "1min",
  ggplot_intraday = TRUE,
  verbose = TRUE,
  show_nchar_case_error = 135
)

# .......................
# heart rate variability
# .......................

hrt_rt_var <- heart_rate_variability_sleep_time(
  heart_rate_data = heart_dat,
  sleep_begin = "00H 40M 0S",
  sleep_end = "08H 00M 0S",
  ggplot_hr_var = TRUE,
  angle_x_axis = 25
)

hrt_rt_var
} # }
```
