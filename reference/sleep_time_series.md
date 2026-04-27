# Sleep Data Time Series

Sleep Data Time Series

## Usage

``` r
sleep_time_series(
  sleep_data_list,
  ggplot_color_palette = "ggsci::blue_material",
  ggplot_ncol = NULL,
  ggplot_nrow = NULL,
  verbose = FALSE
)
```

## Arguments

- sleep_data_list:

  a named list of pre-downloaded sleep data, one element per day. Names
  must be dates in "YYYY-MM-DD" format. Each element is the parsed JSON
  response from the Fitbit sleep endpoint for that date (the same
  structure as the `sleep_data` parameter of
  [`sleep_single_day()`](https://mlampros.github.io/fitbitViz/reference/sleep_single_day.md)).

- ggplot_color_palette:

  a character string specifying the color palette to be used. For a full
  list of palettes used in the ggplot see:
  https://pmassicotte.github.io/paletteer_gallery/ The following
  color-palettes were tested and work well: "rcartocolor::Purp",
  "rcartocolor::Teal"

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

# 'sleep_data_list' is a named list (names = dates "YYYY-MM-DD") 
# of parsed Fitbit sleep endpoint responses

sleep_ts <- sleep_time_series(
  sleep_data_list = sleep_data_list,
  ggplot_color_palette = "ggsci::blue_material",
  verbose = TRUE
)

sleep_ts$plt_lev_segments
sleep_ts$plt_lev_heatmap
sleep_ts$heatmap_data


# ...........................................
# (option to) save the ggplot to a .png file
# ...........................................

png_file <- tempfile(fileext = ".png")

ggplot2::ggsave(
  filename = png_file,
  plot = sleep_ts$plt_lev_segments,
  device = "png",
  scale = 1,
  width = 35,
  height = 25,
  limitsize = TRUE
)
} # }
```
