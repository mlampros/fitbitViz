# Sleep Data of single day

Sleep Data of single day

## Usage

``` r
sleep_single_day(
  sleep_data,
  date = "2021-03-09",
  ggplot_color_palette = "ggsci::blue_material",
  verbose = FALSE
)
```

## Arguments

- sleep_data:

  a list object containing the pre-downloaded sleep data for the
  specified date. This is the parsed JSON response from the Fitbit sleep
  endpoint (e.g.
  [`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
  on the API response for
  `/1.2/user/[user-id]/sleep/date/[date]/[date].json`). The list must
  have a `$sleep` element.

- date:

  a character string specifying the Date for which the sleep data should
  be returned. For instance, the date '2021-12-31' where the input order
  is 'year-month-day'

- ggplot_color_palette:

  a character string specifying the color palette to be used. For a full
  list of palettes used in the ggplot see:
  https://pmassicotte.github.io/paletteer_gallery/ The following
  color-palettes were tested and work well: "rcartocolor::Purp",
  "rcartocolor::Teal"

- verbose:

  a boolean. If TRUE then information will be printed out in the console

## Value

an object of class list

## Examples

``` r
if (FALSE) { # \dontrun{

require(fitbitViz)

# 'sleep_data_day' is the parsed JSON list for a single date from the Fitbit sleep endpoint

lst_out <- sleep_single_day(
  sleep_data = sleep_data_day,
  date = "2021-03-09",
  ggplot_color_palette = "ggsci::blue_material",
  verbose = TRUE
)
str(lst_out)
} # }
```
