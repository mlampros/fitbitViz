
utils::globalVariables(c('time',
                         'value',
                         'level_data',
                         '..cols_breaks',
                         '.',
                         'weekday',
                         'Date',
                         'level',
                         'heart_rate',
                         'hr_variability',
                         'level_start',
                         'percent',
                         'dateTime_end',
                         'level_end',
                         'dateTime_start',
                         'GROUP',
                         '..cols_accum',
                         '.SD'))


#' inner function of 'compute_elapsed_time'
#'
#' @param secs a numeric value specifying the seconds
#' @param estimated a boolean. If TRUE then the output label becomes the 'Estimated time'
#' @return a character string showing the estimated or elapsed time
#'
#' @keywords internal

inner_elapsed_time = function(secs, estimated = FALSE) {
  tmp_hours = as.integer((secs / 60) / 60)
  tmp_hours_minutes = (secs / 60) %% 60
  tmp_seconds = secs %% 60
  est_verb = ifelse(estimated, "Estimated time: ", "Elapsed time: ")
  res_out = paste(c(est_verb, tmp_hours, " hours and ", as.integer(tmp_hours_minutes), " minutes and ", as.integer(tmp_seconds), " seconds."), collapse = "")
  return(res_out)
}


#' elapsed time in hours & minutes & seconds
#'
#' @param time_start a numeric value specifying the start time
#' @return It does not return a value but only prints the time in form of a character string in the R session
#'
#' @keywords internal

compute_elapsed_time = function(time_start) {
  t_end = proc.time()
  time_total = as.numeric((t_end - time_start)['elapsed'])
  time_ = inner_elapsed_time(time_total)
  cat(time_, "\n")
}


#' function for the weeks (including the date-from and date-to)
#'
#' @param year an integer value specifying the year
#' @return a sequence of Dates of class 'Date'
#'
#' @keywords internal
#'
#' @importFrom glue glue
#'
#' @references
#'
#' https://statistics.berkeley.edu/computing/faqs/dates-and-times-r

split_year_in_weeks = function(year) {

  weeks_data = seq(from = as.Date(glue::glue('{year}-01-01')), to = as.Date(glue::glue('{year}-12-31')), by = '1 week')
  return(weeks_data)
}


#' base function to return the data for the specified activity based on the url
#'
#' @param url a character string specifying the input url
#' @param oauth_token a character string specifying the authentication token
#' @param show_nchar_case_error an integer value specifying the number of characters to show in case of an error
#' @return an object of class list
#'
#' @importFrom httr GET add_headers content
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#'
#' @keywords internal

base_url_request = function(url,
                            oauth_token,
                            show_nchar_case_error = 135) {

  auth_code = paste("Bearer", oauth_token)
  query_response = httr::GET(url = url, httr::add_headers(Authorization = auth_code))

  if (query_response$status_code != 200) {                                              # in case of an error use "httr::content()" to get more details
    content_list_obj = httr::content(query_response, "parsed")
    stop(glue::glue("The request gave an error code of '{query_response$status_code}' with the following first '{show_nchar_case_error}' characters as error message: '{substr(content_list_obj, 1, show_nchar_case_error)}'"), call. = F)
  }
  else {
    content_list_obj = httr::content(query_response, as = "text")                       # see: https://cran.r-project.org/web/packages/jsonlite/vignettes/json-apis.html
    content_list_obj = jsonlite::fromJSON(content_list_obj, simplifyVector = TRUE)
  }

  return(content_list_obj)
}


#' plot function for a single day (heart rate)
#'
#' @param date_intraday a data.table of Intraday Dates of heart rate measurements
#' @param date a character string specifying a Date
#' @return a plot object of class ggplot2
#'
#' @import ggplot2
#' @importFrom lubridate wday
#'
#' @keywords internal

ggplot_each_date = function(date_intraday, date) {

  date_intraday$time = as.POSIXct(date_intraday$time, tz = 'UTC')
  MAX_heart_rate = max(date_intraday$value)
  idx_max = which(date_intraday$value == MAX_heart_rate)
  max_time = date_intraday$time[idx_max]

  plt = ggplot2::ggplot(date_intraday, ggplot2::aes(x = time, y = value, group = 1)) +
    ggplot2::geom_line(color = "green") +
    ggplot2::ggtitle(glue::glue("Heart-Rate value at {date} ( '{as.character(lubridate::wday(date, label = TRUE))}' )")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")) +
    ggplot2::geom_hline(yintercept = MAX_heart_rate, linetype = "dotted", colour = 'blue') +
    ggplot2::scale_y_continuous(breaks = sort(c(seq(min(date_intraday$value), MAX_heart_rate, length.out = 5), MAX_heart_rate))) +
    ggplot2::scale_x_datetime(date_breaks = "3 hours", date_labels = "%H:%M") +
    ggplot2::geom_vline(xintercept = max_time, linetype = "dotted", colour = 'blue')

  return(plt)
}


#' heart rate activity time series
#'
#' @param user_id a character string specifying the encoded ID of the user. For instance '99xxxx' of the following URL 'https://www.fitbit.com/user/99xxxx' of the user's account corresponds to the 'user_id'
#' @param token a character string specifying the secret token that a user receives when registers a new application in https://dev.fitbit.com/apps
#' @param date_start a character string specifying a start Date. For instance, the date '2021-12-31' where the input order is 'year-month-day'
#' @param date_end a character string specifying a end Date. For instance, the date '2021-12-31' where the input order is 'year-month-day'
#' @param time_start a character string specifying the start time. For instance, the time '00:00' where the input order is 'hours-minutes'
#' @param time_end a character string specifying the end time. For instance, the time '23:59' where the input order is 'hours-minutes'
#' @param detail_level a character string specifying the detail level of the heart rate time series. It can be either '1min' or '1sec', for 1-minute and 1-second intervals
#' @param ggplot_intraday a boolean. If TRUE then the ggplot of the heart rate time series will be returned too
#' @param ggplot_ncol either NULL or an integer specifying the number of columns of the output ggplot
#' @param ggplot_nrow either NULL or an integer specifying the number of rows of the output ggplot
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @param show_nchar_case_error an integer that specifies the number of characters that will be returned in case on an error. The default value is 135 characters.
#' @return an object of class list
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom patchwork wrap_plots
#' @importFrom data.table data.table
#'
#' @examples
#'
#' \dontrun{
#'
#' require(fitbitViz)
#'
#' USER_ID = '99xxxx'
#' token = 'my_long_web_api_token'
#'
#' heart_dat = heart_rate_time_series(user_id = USER_ID,
#'                                    token = token,
#'                                    date_start = '2021-03-09',
#'                                    date_end = '2021-03-16',
#'                                    time_start = '00:00',
#'                                    time_end = '23:59',
#'                                    detail_level = '1min',
#'                                    ggplot_intraday = TRUE,
#'                                    verbose = TRUE,
#'                                    show_nchar_case_error = 135)
#' heart_dat$plt
#' heart_dat$heart_rate
#' heart_dat$heart_rate_intraday
#'
#' }

heart_rate_time_series = function(user_id,
                                  token,
                                  date_start,
                                  date_end,
                                  time_start = '00:00',
                                  time_end = '23:59',
                                  detail_level = '1min',
                                  ggplot_intraday = FALSE,
                                  ggplot_ncol = NULL,
                                  ggplot_nrow = NULL,
                                  verbose = FALSE,
                                  show_nchar_case_error = 135) {

  if (verbose) t_start = proc.time()
  seq_dates = as.character(seq(from = as.Date(date_start), to = as.Date(date_end), by = 1))

  heart_rate = heart_rate_intraday = list()

  for (idx in 1:length(seq_dates)) {

    if (verbose) cat(glue::glue("Day:  '{seq_dates[idx]}'  will be processed ..."), '\n')

    URL = glue::glue('https://api.fitbit.com/1/user/{user_id}/activities/heart/date/{seq_dates[idx]}/{seq_dates[idx]}/{detail_level}/time/{time_start}/{time_end}.json')

    process_data = base_url_request(url = URL,
                                    oauth_token = token,
                                    show_nchar_case_error = show_nchar_case_error)

    heart_rate[[idx]] = process_data$`activities-heart`

    tmp_intraday = process_data$`activities-heart-intraday`$dataset

    if (!inherits(tmp_intraday, 'list') & length(tmp_intraday) > 0) {
      tmp_intraday$time = as.character(glue::glue("{seq_dates[idx]} {tmp_intraday$time}"))
      heart_rate_intraday[[seq_dates[idx]]] = data.table::data.table(tmp_intraday)
    }
    else {
      if (verbose) message(glue::glue("Data for Day:  '{seq_dates[idx]}'  does not exist!"))
    }
  }

  if (length(heart_rate) > 0) {
    heart_rate = do.call(rbind, heart_rate)
  }

  all_dat = list(heart_rate = heart_rate, heart_rate_intraday = heart_rate_intraday)

  if (ggplot_intraday) {

    plt = NULL

    if (length(heart_rate_intraday) > 0) {

      NAMS_dates = names(heart_rate_intraday)
      lst_plt = list()

      for (idx_plt in 1:length(heart_rate_intraday)) {
        dat_iter = heart_rate_intraday[[NAMS_dates[idx_plt]]]
        plt_iter = ggplot_each_date(date_intraday = dat_iter, date = NAMS_dates[idx_plt])
        lst_plt[[idx_plt]] = plt_iter
      }

      plt_all = patchwork::wrap_plots(lst_plt,
                                      ncol = ggplot_ncol,
                                      nrow = ggplot_nrow)
      all_dat[['plt']] = plt_all
    }
  }

  all_dat[['detail_level']] = detail_level

  if (verbose) compute_elapsed_time(t_start)

  return(all_dat)
}



#' Heart Rate Intraday Heatmap (by extracting the 'min.', 'median' and 'max.' values of the day)
#'
#' @param heart_rate_intraday_data a list object specifying the intraday heart rate data (this is one of the sublists returned from the 'heart_rate_time_series' function)
#' @param angle_x_axis an integer specifying the angle of the x-axis labels. The default values is 0 (it can take for instance values such as 45, 90 etc.)
#' @return a plot object of class ggplot2
#'
#' @export
#'
#' @importFrom lubridate wday
#' @importFrom data.table setDT rbindlist
#' @importFrom stats median
#' @importFrom viridis scale_fill_viridis
#' @importFrom scales date_format
#' @importFrom ggthemes theme_tufte
#' @import ggplot2
#'
#' @examples
#'
#' \dontrun{
#'
#' require(fitbitViz)
#'
#' #...........................................
#' # first compute the heart rate intraday data
#' #...........................................
#'
#' USER_ID = '99xxxx'
#' token = 'my_long_web_api_token'
#'
#' heart_dat = heart_rate_time_series(user_id = USER_ID,
#'                                    token = token,
#'                                    date_start = '2021-03-09',
#'                                    date_end = '2021-03-16',
#'                                    time_start = '00:00',
#'                                    time_end = '23:59',
#'                                    detail_level = '1min',
#'                                    ggplot_intraday = TRUE,
#'                                    verbose = TRUE,
#'                                    show_nchar_case_error = 135)
#'
#' #..........................................
#' # use the heart-rate-intraday data as input
#' # to the 'heart_rate_heatmap' function
#' #..........................................
#'
#' hrt_heat = heart_rate_heatmap(heart_rate_intraday_data =  heart_dat$heart_rate_intraday,
#'                               angle_x_axis = 0)
#' hrt_heat
#'
#' }


heart_rate_heatmap = function(heart_rate_intraday_data,
                              angle_x_axis = 0) {

  data_heart = lapply(1:length(heart_rate_intraday_data), function(x) {

    iter_date = names(heart_rate_intraday_data)[x]
    wday = as.character(lubridate::wday(iter_date, label = TRUE))
    min_med_max = c('min', 'median', 'max')

    iter = data.table::setDT(list(level = factor(min_med_max, levels = rev(sort(min_med_max))),       # reverse the order of the levels so that max. appears at the top of the heatmap, see: https://stackoverflow.com/a/30293853/8302386
                                  heart_rate = c(min(heart_rate_intraday_data[[x]]$value),
                                                 stats::median(heart_rate_intraday_data[[x]]$value),
                                                 max(heart_rate_intraday_data[[x]]$value)),
                                  Date = as.Date(rep(iter_date, 3)),
                                  weekday = rep(wday, 3)))
    iter
  })

  data_heart = data.table::rbindlist(data_heart)

  cols_breaks = c('Date', 'weekday')
  dat_lev_breaks = data_heart[, ..cols_breaks]
  dat_lev_breaks = dat_lev_breaks[, .(weekday = unique(weekday)), by = 'Date']
  dat_lev_breaks = dat_lev_breaks[order(dat_lev_breaks$Date, decreasing = F), ]

  plt = ggplot2::ggplot(data = data_heart, ggplot2::aes(x = Date, y = level)) +
    ggplot2::geom_tile(ggplot2::aes(fill = heart_rate)) +
    ggplot2::coord_equal(ratio = 1) +
    viridis::scale_fill_viridis(option = "magma", limits = c(40, 220), breaks = round(seq(40, 220, by = 20))) +
    ggthemes::theme_tufte(base_family = "Helvetica") +
    ggplot2::geom_text(ggplot2::aes(label = heart_rate, fontface = 2), color = "yellow", size = 4) +                   # fontface:  2 (bold), 3 (italic), 4 (bold.italic)
    ggplot2::scale_x_date(breaks = dat_lev_breaks$Date, labels = scales::date_format("%Y-%m-%d"), sec.axis = ggplot2::dup_axis(labels = dat_lev_breaks$weekday)) +
    ggplot2::ylab("Level") +
    ggplot2::ggtitle("Heart Rate Level Heatmap") +
    ggplot2::theme(strip.placement = 'outside',
                   plot.title = ggplot2::element_text(size = "16", hjust = 0.5, face = "bold", colour = "blue"),
                   axis.title.x = ggplot2::element_text(size = 12, face = "bold", colour = "blue"),
                   axis.title.y = ggplot2::element_text(size = 12, face = "bold", colour = "blue"),
                   axis.text.x = ggplot2::element_text(size = 12, face = "bold", colour = "black", angle = angle_x_axis, vjust = 1.0, hjust = 1.0),
                   axis.text.y = ggplot2::element_text(size = 12, face = "bold", colour = "black"))
  return(plt)
}



#' Heart Rate Variability during Sleep Time (the root mean square of successive differences)
#'
#' @param heart_rate_data a list object. This is the output of the 'heart_rate_time_series()' function
#' @param sleep_begin a character string specifying the begin of the sleep time. For instance, the time "00H 40M 0S" where the input order is 'hours-minutes-seconds' and the format corresponds to the 'lubridate::hms()' function
#' @param sleep_end a character string specifying the end of the sleep time. For instance, the time "08H 00M 0S" where the input order is 'hours-minutes-seconds' and the format corresponds to the 'lubridate::hms()' function
#' @param ggplot_hr_var a boolean. If TRUE then the ggplot of the heart rate variability will be returned
#' @param angle_x_axis an integer specifying the angle of the x-axis labels. The default values is 45 (it can take for instance values such as 0, 90 etc.)
#' @return an object of class list
#'
#' @export
#'
#' @details
#'
#' I use the '1min' rather than the '1sec' interval because it is consistent (it shows the 1-minute differences), whereas in case of '1sec' the difference between observations varies between 1 second and less than 60 seconds
#'
#' This function calculates the root mean square of successive differences (RMSSD) and a higher heart rate variability is linked with better health
#'
#' Based on the Fitbit application information weblink and the Wikipedia article (https://en.wikipedia.org/wiki/Heart_rate_variability) the heart rate variability is computed normally in ms (milliseconds)
#'
#' @importFrom lubridate hms
#' @importFrom varian rmssd
#' @importFrom data.table setDT
#' @import ggplot2
#'
#' @examples
#'
#' \dontrun{
#'
#' require(fitbitViz)
#'
#' #...........................................
#' # first compute the heart rate intraday data
#' #...........................................
#'
#' USER_ID = '99xxxx'
#' token = 'my_long_web_api_token'
#'
#' heart_dat = heart_rate_time_series(user_id = USER_ID,
#'                                    token = token,
#'                                    date_start = '2021-03-09',
#'                                    date_end = '2021-03-16',
#'                                    time_start = '00:00',
#'                                    time_end = '23:59',
#'                                    detail_level = '1min',
#'                                    ggplot_intraday = TRUE,
#'                                    verbose = TRUE,
#'                                    show_nchar_case_error = 135)
#'
#' #.......................
#' # heart rate variability
#' #.......................
#'
#' hrt_rt_var = heart_rate_variability_sleep_time(heart_rate_data = heart_dat,
#'                                                sleep_begin = "00H 40M 0S",
#'                                                sleep_end = "08H 00M 0S",
#'                                                ggplot_hr_var = TRUE,
#'                                                angle_x_axis = 25)
#'
#' hrt_rt_var
#'
#' }


heart_rate_variability_sleep_time = function(heart_rate_data,
                                             sleep_begin = "00H 40M 0S",
                                             sleep_end = "08H 00M 0S",
                                             ggplot_hr_var = TRUE,
                                             angle_x_axis = 45) {

  if (heart_rate_data$detail_level != '1min') stop("You have to run the 'heart_rate_time_series' function first with 'detail_level' set to '1min'!", call. = F)

  heart_rate_intraday = heart_rate_data$heart_rate_intraday
  LEN = length(heart_rate_intraday)
  nams = names(heart_rate_intraday)
  hr_var = rep(NA_real_, LEN)

  for (i in 1:LEN) {
    iter_dat = heart_rate_intraday[[i]]
    iter_time = as.vector(unlist(lapply(strsplit(iter_dat$time, ' '), function(x) x[2])))
    iter_time = lubridate::hms(iter_time)
    idx_night = which(iter_time >= lubridate::hms(sleep_begin) & iter_time < lubridate::hms(sleep_end))
    hr_var[i] = varian::rmssd(x = iter_dat$value[idx_night])
  }

  lst_out = list()
  hr_var = data.table::setDT(list(Date = as.Date(nams), hr_variability = hr_var))
  lst_out[['hr_var_data']] = hr_var

  if (ggplot_hr_var) {

    plt = ggplot2::ggplot(hr_var, ggplot2::aes(x = Date, y = hr_variability)) +
      ggplot2::geom_line(linetype = 'dashed', size = 1, color = 'purple') +
      ggplot2::coord_cartesian(ylim = c(min(hr_var$hr_variability) - 1, max(hr_var$hr_variability) + 1.0)) +
      ggplot2::scale_x_date(date_breaks = "1 day") +
      ggplot2::ylab('heart rate var.') +
      ggplot2::xlab("Date") +
      ggplot2::ggtitle("Heart Rate Variability (root mean square of successive differences) per minute") +
      ggplot2::labs(color='Heart Rate Variability (during sleep') +
      ggplot2::geom_point(color = 'green', size = 3) +
      ggplot2::geom_text(ggplot2::aes(label = round(hr_var$hr_variability, 3), fontface = 2), color = "maroon", size = 4, vjust = -2) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = "16", hjust = 0.5, face = "bold", colour = "blue"),
                     axis.title.x = ggplot2::element_text(size = 12, face = "bold", colour = "blue"),
                     axis.title.y = ggplot2::element_text(size = 12, face = "bold", colour = "blue"),
                     axis.text.x = ggplot2::element_text(size = 12, face = "bold", colour = "black", angle = angle_x_axis, vjust = 1, hjust=1),
                     axis.text.y = ggplot2::element_text(size = 12, face = "bold", colour = "black"))

    lst_out[['hr_var_plot']] = plt
  }

  return(lst_out)
}



#' Sleep Data of single day
#'
#' @param user_id a character string specifying the encoded ID of the user. For instance '99xxxx' of the following URL 'https://www.fitbit.com/user/99xxxx' of the user's account corresponds to the 'user_id'
#' @param token a character string specifying the secret token that a user receives when registers a new application in https://dev.fitbit.com/apps
#' @param date a character string specifying the Date for which the sleep data should be returned. For instance, the date '2021-12-31' where the input order is 'year-month-day'
#' @param ggplot_color_palette a character string specifying the color palette to be used. For a full list of palettes used in the ggplot see:  https://pmassicotte.github.io/paletteer_gallery/ The following color-palettes were tested and work well: "rcartocolor::Purp", "rcartocolor::Teal"
#' @param show_nchar_case_error an integer that specifies the number of characters that will be returned in case on an error. The default value is 135 characters.
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @return an object of class list
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom httr GET add_headers content
#' @importFrom lubridate ymd_hms seconds wday
#' @importFrom data.table setDT rbindlist
#' @importFrom paletteer scale_color_paletteer_d
#' @import ggplot2
#'
#' @examples
#'
#' \dontrun{
#'
#' require(fitbitViz)
#'
#' USER_ID = '99xxxx'
#' token = 'my_long_web_api_token'
#'
#' lst_out = sleep_single_day(user_id = USER_ID,
#'                            token = token,
#'                            date = '2021-03-09',
#'                            ggplot_color_palette = 'ggsci::blue_material',
#'                            show_nchar_case_error = 135,
#'                            verbose = TRUE)
#' str(lst_out)
#'
#' }


sleep_single_day = function(user_id,
                            token,
                            date = '2021-03-09',
                            ggplot_color_palette = 'ggsci::blue_material',
                            show_nchar_case_error = 135,
                            verbose = FALSE) {
  if (verbose) {
    t_start = proc.time()
    cat("Authentication will be performed ...\n")
  }

  URL = glue::glue('https://api.fitbit.com/1.2/user/{user_id}/sleep/date/{date}/{date}.json')
  auth_code = paste("Bearer", token)
  query_response = httr::GET(url = URL, httr::add_headers(Authorization = auth_code))
  if (query_response$status_code != 200) {                                              # in case of an error use "httr::content()" to get more details
    content_list_obj = httr::content(query_response, "parsed")
    stop(glue::glue("The request gave an error code of '{query_response$status_code}' with the following first '{show_nchar_case_error}' characters as error message: '{substr(content_list_obj, 1, show_nchar_case_error)}'"), call. = F)
  }

  if (verbose) cat("The sleep content will be red ...\n")
  content_list_obj = httr::content(query_response, "parsed")
  sleep = content_list_obj$sleep[[1]]$levels$data
  sleep = lapply(sleep, unlist)
  sleep = data.frame(do.call(rbind, sleep), stringsAsFactors = F)
  sleep$dateTime = lubridate::ymd_hms(sleep$dateTime)
  sleep$seconds = as.numeric(sleep$seconds)

  if (verbose) cat("The sleep-time will be transformed ...\n")
  lst_dtbl = list()

  for (ROW in 1:nrow(sleep)) {

    iter_row = sleep[ROW, , drop = F]
    iter_row_begin = iter_row$dateTime + lubridate::seconds(iter_row$seconds)                # Initially I subtracted 1 second in this line so that it can serve as the end of the duration BUT then the levels are not connected
    iter_row_end = iter_row$dateTime + lubridate::seconds(iter_row$seconds)

    iter_dat = data.table::setDT(list(dateTime_start = iter_row$dateTime,
                                      level_start = iter_row$level,
                                      dateTime_end = iter_row_begin,
                                      level_end = iter_row$level,
                                      seconds = lubridate::seconds(iter_row$seconds)))
    lst_dtbl[[ROW]] = iter_dat
  }

  lst_dtbl = data.table::rbindlist(lst_dtbl)

  if (verbose) cat("Groups for the sleep-time will be created ...\n")
  lst_dtbl_group = list()

  for (ROW in 1:(nrow(sleep)-1)) {

    iter_row = sleep[ROW, , drop = F]
    iter_row$dateTime_end = iter_row$dateTime + lubridate::seconds(iter_row$seconds)
    iter_row$level_end = iter_row$level
    COLNAMS = colnames(iter_row)

    iter_row_next = sleep[ROW + 1, , drop = F]
    iter_row_next$dateTime_end = iter_row_next$dateTime + lubridate::seconds(iter_row_next$seconds)
    iter_row_next$level_end = iter_row_next$level
    colnames(iter_row_next) = c("dateTime_end", "level", "seconds", "dateTime", "level_end")
    iter_row_next = iter_row_next[, COLNAMS]

    iter_row$seconds = NULL
    iter_row$GROUP = as.character(ROW)

    iter_row_next$seconds = NULL
    iter_row_next$GROUP = as.character(ROW)

    dat_iter = data.table::rbindlist(list(iter_row, iter_row_next))
    colnames(dat_iter) = c('dateTime_start', 'level_start', 'dateTime_end', 'level_end', 'GROUP')

    lst_dtbl_group[[ROW]] = dat_iter
  }

  lst_dtbl_group = data.table::rbindlist(lst_dtbl_group)

  if (verbose) cat(glue::glue("The 'ggplot' for day '{date}' will be created ..."), '\n')

  plt = ggplot2::ggplot(lst_dtbl, ggplot2::aes(x = dateTime_end,
                                               y = level_end,
                                               xend = dateTime_start,
                                               yend = level_start,
                                               group = level_start,
                                               colour = level_start)) +
    ggplot2::geom_segment(size = 2, ggplot2::aes(colour = level_end)) +
    ggplot2::geom_line(data = lst_dtbl_group, ggplot2::aes(group = GROUP), size = 2) +
    paletteer::scale_color_paletteer_d(ggplot_color_palette, direction = -1) +
    ggplot2::ggtitle(glue::glue("Sleep value at {date} ( '{as.character(lubridate::wday(date, label = TRUE))}' )")) +
    ggplot2::scale_x_datetime(date_breaks = "30 min", date_labels = "%H:%M") +
    ggplot2::ylab(glue::glue("Sleep Level")) +
    ggplot2::xlab("Time") +
    ggplot2::labs(color='Sleep Level') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", colour = "dark green"),
                   axis.title.x = ggplot2::element_text(size = 10, face = "bold", colour = "dark green"),
                   axis.title.y = ggplot2::element_text(size = 10, face = "bold", colour = "dark green"),
                   axis.text.x = ggplot2::element_text(size = 10, face = "bold", colour = "black", angle = 30, vjust = 1.0, hjust = 1.0),
                   axis.text.y = ggplot2::element_text(size = 10, face = "bold", colour = "black"))

  cols_accum = c('level_start', 'seconds')
  level_accum = lst_dtbl[, ..cols_accum]
  level_accum = level_accum[, lapply(.SD, sum), by = 'level_start']
  level_accum$percent = round((level_accum$seconds / sum(level_accum$seconds)) * 100.0, 2)
  level_accum$Date = as.Date(rep(date, nrow(level_accum)))
  level_accum$weekday = lubridate::wday(level_accum$Date, label = T, abbr = T)

  lst_dat = list(init_data = lst_dtbl,
                 grouped_data = lst_dtbl_group,
                 level_accum = level_accum,
                 plt = plt)

  if (verbose) compute_elapsed_time(t_start)

  return(lst_dat)
}



#' sleep data heatmap
#'
#' @param level_data a data.table specifying the input level data
#' @param angle_x_axis a float number specifying the angle of the x-axis text of the output ggplot
#' @return a plot object of class ggplot2
#'
#' @importFrom viridis scale_fill_viridis
#' @importFrom ggthemes theme_tufte
#' @import ggplot2
#'
#' @keywords internal

sleep_heatmap = function(level_data,
                         angle_x_axis = 0) {

  cols_breaks = c('Date', 'weekday')
  dat_lev_breaks = level_data[, ..cols_breaks]
  dat_lev_breaks = dat_lev_breaks[, .(weekday = unique(weekday)), by = 'Date']
  dat_lev_breaks = dat_lev_breaks[order(dat_lev_breaks$Date, decreasing = F), ]

  plt = ggplot2::ggplot(data = level_data, ggplot2::aes(x = Date, y = level_start)) +
    ggplot2::geom_tile(ggplot2::aes(fill = percent)) +
    ggplot2::coord_equal(ratio = 1) +
    viridis::scale_fill_viridis(option = "magma", limits = c(0.0, 100.0), breaks = round(seq(0.0, 100.0, by = 10))) +
    ggthemes::theme_tufte(base_family = "Helvetica") +
    ggplot2::geom_text(ggplot2::aes(label = glue::glue("min: {round(as.integer(level_data$seconds) / 60.0, 1)} \n ({round(as.numeric(level_data$percent), 2)}%)"), fontface = 2), color = "yellow", size = 4) +                   # fontface:  2 (bold), 3 (italic), 4 (bold.italic)
    ggplot2::scale_x_date(breaks = dat_lev_breaks$Date, labels = scales::date_format("%Y-%m-%d"), sec.axis = ggplot2::dup_axis(labels = dat_lev_breaks$weekday)) +
    ggplot2::ylab("Level") +
    ggplot2::ggtitle("Sleep Level Heatmap (Minutes & Percentage of sleep)") +
    ggplot2::theme(strip.placement = 'outside',
                   plot.title = ggplot2::element_text(size = "16", hjust = 0.5, face = "bold", colour = "blue"),
                   axis.title.x = ggplot2::element_text(size = 12, face = "bold", colour = "blue"),
                   axis.title.y = ggplot2::element_text(size = 12, face = "bold", colour = "blue"),
                   axis.text.x = ggplot2::element_text(size = 12, face = "bold", colour = "black", angle = angle_x_axis, vjust = 1.0, hjust = 1.0),
                   axis.text.y = ggplot2::element_text(size = 12, face = "bold", colour = level_data$colour_y_axis))
  return(plt)
}


#' Sleep Data Time Series
#'
#' @param user_id a character string specifying the encoded ID of the user. For instance '99xxxx' of the following URL 'https://www.fitbit.com/user/99xxxx' of the user's account corresponds to the 'user_id'
#' @param token a character string specifying the secret token that a user receives when registers a new application in https://dev.fitbit.com/apps
#' @param date_start a character string specifying the start Date for which the sleep data should be returned. For instance, the date '2021-12-31' where the input order is 'year-month-day'
#' @param date_end a character string specifying the end Date for which the sleep data should be returned. For instance, the date '2021-12-31' where the input order is 'year-month-day'
#' @param ggplot_color_palette a character string specifying the color palette to be used. For a full list of palettes used in the ggplot see:  https://pmassicotte.github.io/paletteer_gallery/ The following color-palettes were tested and work well: "rcartocolor::Purp", "rcartocolor::Teal"
#' @param ggplot_ncol either NULL or an integer specifying the number of columns of the output ggplot
#' @param ggplot_nrow either NULL or an integer specifying the number of rows of the output ggplot
#' @param show_nchar_case_error an integer that specifies the number of characters that will be returned in case on an error. The default value is 135 characters.
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @return an object of class list
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom data.table rbindlist
#' @importFrom patchwork wrap_plots
#' @importFrom paletteer paletteer_c
#'
#' @examples
#'
#' \dontrun{
#'
#' require(fitbitViz)
#'
#' #.........................................
#' # first compute the sleep time time series
#' #.........................................
#'
#' USER_ID = '99xxxx'
#' token = 'my_long_web_api_token'
#'
#' sleep_ts = sleep_time_series(user_id = USER_ID,
#'                              token = token,
#'                              date_start = '2021-03-09',
#'                              date_end = '2021-03-16',
#'                              ggplot_color_palette = 'ggsci::blue_material',
#'                              show_nchar_case_error = 135,
#'                              verbose = TRUE)
#'
#' sleep_ts$plt_lev_segments
#' sleep_ts$plt_lev_heatmap
#' sleep_ts$heatmap_data
#'
#'
#' #...........................................
#' # (option to) save the ggplot to a .png file
#' #...........................................
#'
#' png_file = tempfile(fileext = '.png')
#'
#' ggplot2::ggsave(filename = png_file,
#'                 plot = sleep_ts$plt_lev_segments,
#'                 device = 'png',
#'                 scale = 1,
#'                 width = 35,
#'                 height = 25,
#'                 limitsize = TRUE)
#' }


sleep_time_series  = function(user_id,
                              token,
                              date_start,
                              date_end,
                              ggplot_color_palette = 'ggsci::blue_material',
                              ggplot_ncol = NULL,
                              ggplot_nrow = NULL,
                              show_nchar_case_error = 135,
                              verbose = FALSE) {

  if (verbose) t_start = proc.time()
  seq_dates = as.character(seq(from = as.Date(date_start), to = as.Date(date_end), by = 1))

  sleep_intraday_plt = level_accum_lst = list()

  for (idx in 1:length(seq_dates)) {

    if (verbose) {
      cat('-----------------------------------------\n')
      cat(glue::glue("Day:  '{seq_dates[idx]}'  will be processed ..."), '\n')
    }

    sleep_ggplt = sleep_single_day(user_id = user_id,
                                   token = token,
                                   date = seq_dates[idx],
                                   ggplot_color_palette = ggplot_color_palette,
                                   show_nchar_case_error = show_nchar_case_error,
                                   verbose = FALSE)                                  # disable verbose

    sleep_intraday_plt[[idx]] = sleep_ggplt$plt
    level_accum_lst[[idx]] = sleep_ggplt$level_accum
  }

  if (verbose) cat("The sleep data heatmap will be added ...\n")
  level_accum_lst = data.table::rbindlist(level_accum_lst)

  if (verbose) cat("Assign color to the y-axis labels ..\n")
  unq_labs = sort(unique(level_accum_lst$level_start))
  pal_y_axis = paletteer::paletteer_c(palette = "grDevices::Blues 3", n = length(unq_labs) + 2, direction = 1)
  pal_y_axis = as.character(pal_y_axis)[3:length(pal_y_axis)]
  level_accum_lst$colour_y_axis = as.factor(pal_y_axis[ match(level_accum_lst$level_start, unq_labs) ])

  if (verbose) cat("Create the sleep heatmap ..\n")
  heat_map = sleep_heatmap(level_data = level_accum_lst, angle_x_axis = 0)

  if (verbose) cat("Wrap all plots into a single one multiplot ...\n")
  plt_all = patchwork::wrap_plots(sleep_intraday_plt,
                                  ncol = ggplot_ncol,
                                  nrow = ggplot_nrow) # / heat_map          # although this is feasible it somehow shrinks the maps, therefore return the 'intraday' plots and the 'heatmap' separately

  if (verbose) compute_elapsed_time(t_start)

  return(list(plt_lev_segments = plt_all,
              plt_lev_heatmap = heat_map,
              heatmap_data = level_accum_lst))
}



#' Extract the log-id (it's possible that I receive more than one id)
#'
#' @param user_id a character string specifying the encoded ID of the user. For instance '99xxxx' of the following URL 'https://www.fitbit.com/user/99xxxx' of the user's account corresponds to the 'user_id'
#' @param token a character string specifying the secret token that a user receives when registers a new application in https://dev.fitbit.com/apps
#' @param after_Date a character string specifying the Date after which the log-ids will be returned. For instance, the date '2021-12-31' where the input order is 'year-month-day'
#' @param limit an integer specifying the total of log-id's to return. The default value is 10
#' @param sort a character string specifying the order ('asc', 'desc') based on which the output log-id's should be sorted
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @return an integer specifying the log ID
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom httr GET add_headers content
#'
#' @examples
#'
#' \dontrun{
#'
#' require(fitbitViz)
#'
#' USER_ID = '99xxxx'
#' token = 'my_long_web_api_token'
#'
#' log_id = extract_LOG_ID(user_id = USER_ID,
#'                         token = token,
#'                         after_Date = '2021-03-13',
#'                         limit = 10,
#'                         sort = 'asc',
#'                         verbose = TRUE)
#' log_id
#'
#' }


extract_LOG_ID = function(user_id,
                          token,
                          after_Date = '2021-03-13',
                          limit = 10,
                          sort = 'asc',
                          verbose = FALSE) {

  if (verbose) t_start = proc.time()
  URL = glue::glue('https://api.fitbit.com/1/user/{user_id}/activities/list.json?afterDate={after_Date}&offset=0&limit={limit}&sort={sort}')

  auth_code = paste("Bearer", token)
  query_response = httr::GET(url = URL, httr::add_headers(Authorization = auth_code))
  content_list_obj = httr::content(query_response, "parsed")
  # str(content_list_obj)

  res_activities = content_list_obj$activities
  if (length(res_activities) == 0) stop(glue::glue("There are no activities after (and including) Date  '{after_Date}'!"), call. = F)

  LOG_ID = res_activities[[1]]$logId

  if (verbose) {
    cat(glue::glue("The activity after (and including) Date  '{after_Date}'  has 'id'  {LOG_ID}"), '\n')
    compute_elapsed_time(t_start)
  }

  return(LOG_ID)
}



#' The GPS-TCX data as a formated data.table
#'
#' @param log_id the returned log-id of the 'extract_LOG_ID()' function
#' @param user_id a character string specifying the encoded ID of the user. For instance '99xxxx' of the following URL 'https://www.fitbit.com/user/99xxxx' of the user's account corresponds to the 'user_id'
#' @param token a character string specifying the secret token that a user receives when registers a new application in https://dev.fitbit.com/apps
#' @param time_zone a character string specifying the time zone parameter ('tz') as is defined in the 'lubridate::ymd_hms()' function
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @return either NULL or an object of class data.table
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom httr GET add_headers
#' @importFrom XML xmlParse xmlToList
#' @importFrom data.table data.table
#' @importFrom lubridate ymd_hms
#' @importFrom hms as_hms
#'
#' @examples
#'
#' \dontrun{
#'
#' require(fitbitViz)
#'
#' #............................
#' # first extract the log-id(s)
#' #............................
#'
#' USER_ID = '99xxxx'
#' token = 'my_long_web_api_token'
#'
#' log_id = extract_LOG_ID(user_id = USER_ID,
#'                         token = token,
#'                         after_Date = '2021-03-13',
#'                         limit = 10,
#'                         sort = 'asc',
#'                         verbose = TRUE)
#' str(log_id)
#'
#' #...................................
#' # then return the gps-ctx data.table
#' #...................................
#'
#' res_tcx = GPS_TCX_data(log_id = log_id,
#'                        user_id = USER_ID,
#'                        token = token,
#'                        time_zone = 'Europe/Athens',
#'                        verbose = TRUE)
#' str(res_tcx)
#'
#' }

GPS_TCX_data = function(log_id,
                        user_id,
                        token,
                        time_zone = 'Europe/Athens',
                        verbose = FALSE) {
  if (verbose) {
    t_start = proc.time()
    cat("Extract the activity data ...\n")
  }

  URL = glue::glue('https://api.fitbit.com/1/user/{user_id}/activities/{log_id}.tcx')
  auth_code = paste("Bearer", token)
  query_response = httr::GET(url = URL, httr::add_headers(Authorization = auth_code))

  if (verbose) cat("Use XML to traverse the data ...\n")
  tcx_dat = XML::xmlParse(query_response)
  tcx_lst = XML::xmlToList(tcx_dat)
  if (length(tcx_lst) == 0) stop("The 'XML::xmlToList()' function returned an emtpy list!", call. = F)

  dat_tcx = tcx_lst$Activities$Activity$Lap$Track
  if (is.null(dat_tcx)) {
    return(NULL)
  }
  dat_tcx_df = lapply(dat_tcx, unlist)
  dat_tcx_df = data.table::data.table(do.call(rbind, dat_tcx_df), stringsAsFactors = F)
  if (verbose) cat(glue::glue("The created data.table includes {nrow(dat_tcx_df)} rows and {ncol(dat_tcx_df)} columns"), '\n')

  if (verbose) cat("The data will be formated and the columns will be renamed ...\n")
  dat_tcx_df$Position.LongitudeDegrees = as.numeric(dat_tcx_df$Position.LongitudeDegrees)
  dat_tcx_df$Position.LatitudeDegrees = as.numeric(dat_tcx_df$Position.LatitudeDegrees)
  dat_tcx_df$AltitudeMeters = as.numeric(dat_tcx_df$AltitudeMeters)
  dat_tcx_df$DistanceMeters = as.numeric(dat_tcx_df$DistanceMeters)
  dat_tcx_df$HeartRateBpm.Value = as.numeric(dat_tcx_df$HeartRateBpm.Value)
  colnames(dat_tcx_df) = c('Time', 'latitude', 'longitude', 'AltitudeMeters', 'DistanceMeters', 'HeartRate_Bpm')

  date_time = suppressMessages(lubridate::ymd_hms(dat_tcx_df$Time, tz = time_zone))                        # specify region to get the correct time once converted to 'ymd_hms()' using lubridate  [ see: https://stackoverflow.com/a/59900748/8302386 ]
  vec_date = as.Date(date_time)
  vec_time = hms::as_hms(date_time)
  dat_tcx_df$Time = NULL
  dat_tcx_df$Date = vec_date
  dat_tcx_df$Time = vec_time

  if (verbose) compute_elapsed_time(t_start)

  return(dat_tcx_df)
}



#' Create a Leafet map (including information pop-ups)
#'
#' @param dat_gps_tcx this parameter corresponds to the output data.table of the 'GPS_TCX_data()' function
#' @param color_points_column a character string specifying the column of the output data.table ('GPS_TCX_data()' function) that is used in the map-markers. The default value is 'AltitudeMeters' but it can be any column of type numeric
#' @param provider either a character string specifying a leaflet provider (such as 'Esri.WorldImagery') or a direct call to the leaflet provider list (such as leaflet::providers$Esri.WorldImagery). The default value is leaflet::providers$Esri.WorldImagery
#' @param option_viewer either NULL or rstudioapi::viewer. If NULL then the output map will be shown in the web browser
#' @param CRS an integer specifying the Coordinates Reference System. The recommended value for this data is 4326 (which is also the default value)
#' @return a leaflet map of class 'leaflet'
#'
#' @export
#'
#' @importFrom leaflet providers leaflet addProviderTiles colorNumeric fitBounds addLegend
#' @importFrom sf st_as_sf st_set_crs st_bbox
#' @importFrom rstudioapi viewer
#' @importFrom grDevices heat.colors
#' @importFrom glue glue
#' @importFrom leafgl addGlPoints
#'
#' @examples
#'
#' \dontrun{
#'
#' require(fitbitViz)
#'
#' #............................
#' # first extract the log-id(s)
#' #............................
#'
#' USER_ID = '99xxxx'
#' token = 'my_long_web_api_token'
#'
#' log_id = extract_LOG_ID(user_id = USER_ID,
#'                         token = token,
#'                         after_Date = '2021-03-13',
#'                         limit = 10,
#'                         sort = 'asc',
#'                         verbose = TRUE)
#' str(log_id)
#'
#' #...................................
#' # then return the gps-ctx data.table
#' #...................................
#'
#' res_tcx = GPS_TCX_data(log_id = log_id,
#'                        user_id = USER_ID,
#'                        token = token,
#'                        time_zone = 'Europe/Athens',
#'                        verbose = TRUE)
#' str(res_tcx)
#'
#'
#' #........................
#' # then visualize the data
#' #........................
#'
#' res_lft = leafGL_point_coords(dat_gps_tcx = res_tcx,
#'                               color_points_column = 'AltitudeMeters',
#'                               provider = leaflet::providers$Esri.WorldImagery,
#'                               option_viewer = rstudioapi::viewer,
#'                               CRS = 4326)
#' res_lft
#'
#' }


leafGL_point_coords = function(dat_gps_tcx,
                               color_points_column = 'AltitudeMeters',
                               provider = leaflet::providers$Esri.WorldImagery,
                               option_viewer = rstudioapi::viewer,
                               CRS = 4326) {

  #.............................  reset the options on.exit()
  init_options <- options()
  on.exit(options(init_options))
  #.............................

  options(viewer = option_viewer)

  dat_gps_tcx = sf::st_as_sf(dat_gps_tcx, coords = c("longitude", "latitude"))          # create a simple feature from lat, lon
  dat_gps_tcx = sf::st_set_crs(dat_gps_tcx, CRS)

  bbox_vec = sf::st_bbox(dat_gps_tcx)
  bbox_vec = as.vector(bbox_vec)

  lft = leaflet::leaflet()
  lft = leaflet::addProviderTiles(map = lft, provider = provider)

  heat_pal = leaflet::colorNumeric(palette = as.character(grDevices::heat.colors(n = 9, alpha = 1, rev = TRUE)), domain = dat_gps_tcx[[color_points_column]])
  COLOR = heat_pal(dat_gps_tcx[[color_points_column]])

  popup_info = sprintf(glue::glue("<b>Time: </b>%s<br><b> Altitude: </b>%g<br><b> Distance: </b>%g<br><b> HeartRate_Bpm: </b>%g"),
                       dat_gps_tcx$Time,
                       round(dat_gps_tcx$AltitudeMeters, 2),
                       round(dat_gps_tcx$DistanceMeters, 2),
                       dat_gps_tcx$HeartRate_Bpm)

  lft = leafgl::addGlPoints(map = lft,
                            data = dat_gps_tcx,
                            opacity = 1.0,
                            fillColor = COLOR,
                            popup = popup_info)

  def_lft = leaflet::fitBounds(map = lft,
                               lng1 = bbox_vec[1],
                               lat1 = bbox_vec[2],
                               lng2 = bbox_vec[3],
                               lat2 = bbox_vec[4])

  def_lft = leaflet::addLegend(map = def_lft,
                               pal = heat_pal,
                               values = dat_gps_tcx[[color_points_column]],
                               opacity = 0.7,
                               position = "bottomright",
                               title = color_points_column)
  return(def_lft)
}



#' Extract the sf-object and raster extent based on a buffer (in meters)
#'
#' @param dat_gps_tcx this parameter corresponds to the output data.table of the 'GPS_TCX_data()' function
#' @param buffer_in_meters an integer value specifying the buffer in meters. The bounding box of the input coordinates (longitudes, latitudes) will be extended by that many meters. The default value is 1000 meters.
#' @param CRS an integer specifying the Coordinates Reference System. The recommended value for this data is 4326 (which is also the default value)
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @return an object of class list
#'
#' @details
#'
#' To create the buffer in meters using the 'sf' package I had to transform to another projection - by default I've used 7801 - as suggested in the following stackoverflow thread, https://stackoverflow.com/a/54754935/8302386
#'
#' @export
#'
#' @importFrom sf st_as_sf st_bbox st_as_sfc st_transform st_buffer st_centroid st_coordinates
#' @importFrom glue glue
#' @importFrom raster extent
#' @importFrom data.table data.table
#'
#' @examples
#'
#' \dontrun{
#'
#' require(fitbitViz)
#'
#' #............................
#' # first extract the log-id(s)
#' #............................
#'
#' USER_ID = '99xxxx'
#' token = 'my_long_web_api_token'
#'
#' log_id = extract_LOG_ID(user_id = USER_ID,
#'                         token = token,
#'                         after_Date = '2021-03-13',
#'                         limit = 10,
#'                         sort = 'asc',
#'                         verbose = TRUE)
#' str(log_id)
#'
#' #...................................
#' # then return the gps-ctx data.table
#' #...................................
#'
#' res_tcx = GPS_TCX_data(log_id = log_id,
#'                        user_id = USER_ID,
#'                        token = token,
#'                        time_zone = 'Europe/Athens',
#'                        verbose = TRUE)
#' str(res_tcx)
#'
#' #....................................................
#' # then compute the sf-object buffer and raster-extend
#' #....................................................
#'
#' sf_rst_ext = extend_AOI_buffer(dat_gps_tcx = res_tcx,
#'                                buffer_in_meters = 1000,
#'                                CRS = 4326,
#'                                verbose = TRUE)
#' sf_rst_ext
#'
#' }


extend_AOI_buffer = function(dat_gps_tcx,
                             buffer_in_meters = 1000,
                             CRS = 4326,
                             verbose = FALSE) {
  if (verbose) {
    t_start = proc.time()
    cat("Convert the data.table to an 'sf' object ...\n")
  }

  dat_gps_tcx = sf::st_as_sf(dat_gps_tcx, coords = c("longitude", "latitude"), crs = CRS)
  dat_gps_tcx = sf::st_bbox(dat_gps_tcx)
  dat_gps_tcx = sf::st_as_sfc(dat_gps_tcx)

  if (verbose) cat(glue::glue("Transform the projection of the 'sf' object from  {CRS}  to 7801 ..."), '\n')
  dat_gps_tcx = sf::st_transform(dat_gps_tcx, crs = 7801)             # transform to another projection (by default 7801) so that I can use meters for the 'dist' parameter as suggested in: https://stackoverflow.com/a/54754935/8302386

  if (verbose) cat(glue::glue("Create a buffer of  {buffer_in_meters}  meters using as input the initial sf object ..."), '\n')
  buffer = sf::st_buffer(dat_gps_tcx,
                         dist = buffer_in_meters,
                         endCapStyle = "SQUARE")

  if (verbose) cat("Back-tranformation of the projection and computation of the bounding box ...\n")
  buffer_trans = sf::st_transform(buffer, crs = CRS)
  buf_bbx = sf::st_bbox(buffer_trans)
  dat_buf_bbx = sf::st_as_sfc(buf_bbx, crs = CRS)
  # cat(sf::st_as_text(dat_buf_bbx))

  if (verbose) cat("Use the bounding box to extract the raster extent ...\n")
  rst_ext = raster::extent(x = buf_bbx)

  if (verbose) cat("Compute the centroid of the sf-buffer object ...\n")
  buf_centr = suppressWarnings(sf::st_centroid(dat_buf_bbx))
  buf_centr = sf::st_coordinates(buf_centr)
  buf_centr = data.table::data.table(buf_centr)
  colnames(buf_centr) = c('longitude', 'latitude')

  lst_out = list(buffer_bbox = buf_bbx,
                 sfc_obj = dat_buf_bbx,
                 raster_obj_extent = rst_ext,
                 centroid_buffer = buf_centr)

  if (verbose) compute_elapsed_time(t_start)

  return(lst_out)
}



#' Function to crop the AOI from the downloaded DEM .tif file
#'
#' @param tif_or_vrt_dem_file a valid path to the elevation .tif or .vrt file
#' @param sf_buffer_obj a simple features ('sf') object that will be used to crop the input elevation raster file ('tif_or_vrt_dem_file' parameter)
#' @param CRS an integer specifying the Coordinates Reference System. The recommended value for this data is 4326 (which is also the default value)
#' @param digits the number of digits to use. It defaults to 6
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @return an object of class raster
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom raster raster rasterFromXYZ res
#' @importFrom exactextractr exact_extract
#' @importFrom sp CRS
#' @importFrom data.table setDT
#'
#' @examples
#'
#' \dontrun{
#'
#' require(fitbitViz)
#'
#' #............................
#' # first extract the log-id(s)
#' #............................
#'
#' USER_ID = '99xxxx'
#' token = 'my_long_web_api_token'
#'
#' log_id = extract_LOG_ID(user_id = USER_ID,
#'                         token = token,
#'                         after_Date = '2021-03-13',
#'                         limit = 10,
#'                         sort = 'asc',
#'                         verbose = TRUE)
#' str(log_id)
#'
#' #...................................
#' # then return the gps-ctx data.table
#' #...................................
#'
#' res_tcx = GPS_TCX_data(log_id = log_id,
#'                        user_id = USER_ID,
#'                        token = token,
#'                        time_zone = 'Europe/Athens',
#'                        verbose = TRUE)
#' str(res_tcx)
#'
#' #....................................................
#' # then compute the sf-object buffer and raster-extend
#' #....................................................
#'
#' sf_rst_ext = extend_AOI_buffer(dat_gps_tcx = res_tcx,
#'                                buffer_in_meters = 1000,
#'                                CRS = 4326,
#'                                verbose = TRUE)
#' sf_rst_ext
#'
#' #...............................................................
#' # Download the Copernicus DEM 30m elevation data because it has
#' # a better resolution, it takes a bit longer to download because
#' # the .tif file size is bigger
#' #...............................................................
#'
#' dem_dir = tempdir()
#' # dem_dir
#'
#' dem30 = CopernicusDEM::aoi_geom_save_tif_matches(sf_or_file = sf_rst_ext$sfc_obj,
#'                                                  dir_save_tifs = dem_dir,
#'                                                  resolution = 30,
#'                                                  crs_value = 4326,
#'                                                  threads = parallel::detectCores(),
#'                                                  verbose = TRUE)
#'
#' TIF = list.files(dem_dir, pattern = '.tif', full.names = T)
#' # TIF
#'
#' if (length(TIF) > 1) {
#'
#'   #....................................................
#'   # create a .VRT file if I have more than 1 .tif files
#'   #....................................................
#'
#'   file_out = file.path(dem_dir, 'VRT_mosaic_FILE.vrt')
#'
#'   vrt_dem30 = create_VRT_from_dir(dir_tifs = dem_dir,
#'                                   output_path_VRT = file_out,
#'                                   verbose = TRUE)
#' }
#'
#' if (length(TIF) == 1) {
#'
#'   #..................................................
#'   # if I have a single .tif file keep the first index
#'   #..................................................
#'
#'   file_out = TIF[1]
#' }
#'
#' raysh_rst = crop_DEM(tif_or_vrt_dem_file = file_out,
#'                      sf_buffer_obj = sf_rst_ext$sfc_obj,
#'                      CRS = 4326,
#'                      digits = 6,
#'                      verbose = TRUE)
#'
#' sp::plot(raysh_rst)
#'
#' }


crop_DEM = function(tif_or_vrt_dem_file,
                    sf_buffer_obj,
                    CRS = 4326,
                    digits = 6,
                    verbose = FALSE) {

  if (!file.exists(tif_or_vrt_dem_file)) stop(glue::glue("The  '{tif_or_vrt_dem_file}'  file does not exist or is not a valid one!"), call. = F)

  if (verbose) cat("The raster will be red ...\n")
  rst_elev = raster::raster(tif_or_vrt_dem_file)

  if (verbose) cat("The AOI will be extracted from the raster DEM ...\n")
  extr_buf = tryCatch(exactextractr::exact_extract(x = rst_elev,
                                                   y = sf_buffer_obj,
                                                   fun = function(value, cov_frac) value,
                                                   progress = F,
                                                   include_xy = TRUE),
                      error = function(e) e)

  crs = sp::CRS(glue::glue("+init=epsg:{CRS}"))

  if (verbose) cat("A data.table will be created from the x,y,z vectors ...\n")
  mt_upd = data.table::setDT(list(x = extr_buf$x,
                                  y = extr_buf$y,
                                  z = extr_buf$value))
  mt_upd = as.matrix(mt_upd)
  dimnames(mt_upd) = NULL

  if (verbose) cat("A raster will be created from the x,y,z data.table ...\n")
  rst_upd = raster::rasterFromXYZ(xyz = mt_upd, res = raster::res(rst_elev), crs = crs, digits = digits)
  return(rst_upd)
}


#' Convert the GPS, TCX data to a LINESTRING
#'
#' @param dat_gps_tcx this parameter corresponds to the output data.table of the 'GPS_TCX_data()' function
#' @param CRS an integer specifying the Coordinates Reference System. The recommended value for this data is 4326 (which is also the default value)
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @param time_split_asc_desc if NULL then the maximum altitude coordinates point will be used as a split point of the route, otherwise the user can give a lubridate 'hours-minutes-seconds' object such as: lubridate::hms('17:05:00')
#' @return an object of class list
#'
#' @import magrittr
#' @importFrom sf st_as_sf st_combine st_cast
#'
#' @export
#'
#' @details
#'
#' Separate the Ascending and Descending coordinate points into 2 groups and give a different color to the Ascending and Descending routes
#'
#' @examples
#'
#' \dontrun{
#'
#' require(fitbitViz)
#'
#' #............................
#' # first extract the log-id(s)
#' #............................
#'
#' USER_ID = '99xxxx'
#' token = 'my_long_web_api_token'
#'
#' log_id = extract_LOG_ID(user_id = USER_ID,
#'                         token = token,
#'                         after_Date = '2021-03-13',
#'                         limit = 10,
#'                         sort = 'asc',
#'                         verbose = TRUE)
#' str(log_id)
#'
#' #...................................
#' # then return the gps-ctx data.table
#' #...................................
#'
#' res_tcx = GPS_TCX_data(log_id = log_id,
#'                        user_id = USER_ID,
#'                        token = token,
#'                        time_zone = 'Europe/Athens',
#'                        verbose = TRUE)
#' str(res_tcx)
#'
#'
#' #..................................................................
#' # By using using the maximum altitude as a split point of the route
#' #..................................................................
#'
#' linestring_dat_init = gps_lat_lon_to_LINESTRING(dat_gps_tcx = res_tcx,
#'                                                 CRS = 4326,
#'                                                 time_split_asc_desc = NULL,
#'                                                 verbose = TRUE)
#'
#' #.................................................................
#' # By using a customized split of the route (ascending, descending)
#' #.................................................................
#'
#' linestring_dat_lubr = gps_lat_lon_to_LINESTRING(dat_gps_tcx = res_tcx,
#'                                                 CRS = 4326,
#'                                                 time_split_asc_desc = lubridate::hms('17:05:00'),
#'                                                 verbose = TRUE)
#' }

gps_lat_lon_to_LINESTRING = function(dat_gps_tcx,
                                     CRS = 4326,
                                     verbose = FALSE,
                                     time_split_asc_desc = NULL) {

  dat_gps_tcx = dat_gps_tcx[order(dat_gps_tcx$Time, decreasing = F), ]            #  sort by time in case that rows are not already sorted

  if (is.null(time_split_asc_desc)) {
    idx_max = which.max(dat_gps_tcx$AltitudeMeters)
  }
  else {
    idx_max = which(dat_gps_tcx$Time >= time_split_asc_desc)                      # find the observations that have a time greater or equal to the input parameter 'time_split_asc_desc'
    idx_max = idx_max[1]                                                          # and then set as the split index the first observation of the extracted indices
  }

  if (verbose) {
    min_t = as.character(dat_gps_tcx$Time[which.min(dat_gps_tcx$Time)])
    max_t = as.character(dat_gps_tcx$Time[which.max(dat_gps_tcx$Time)])
    spl_t = as.character(dat_gps_tcx$Time[idx_max])

    cat(glue::glue("The time  '{spl_t}'  was picked as a split point [ with minimum  '{min_t}'  and maximum  '{max_t}'  time ]"), '\n')
  }

  color_line = rep(NA_character_, nrow(dat_gps_tcx))
  color_line[1:idx_max] = 'blue'
  color_line[(idx_max+1):length(color_line)] = 'red'

  dat_gps_tcx$color_line = color_line
  dat_gps_tcx = split(dat_gps_tcx, by = 'color_line')

  dat_line_str_ASC = sf::st_as_sf(dat_gps_tcx$blue, coords = c("longitude", "latitude"), crs = CRS) %>% sf::st_combine() %>% sf::st_cast("LINESTRING")          # see: https://github.com/r-spatial/sf/issues/321#issuecomment-489045859
  # sp::plot(dat_line_str_ASC)
  dat_line_str_DESC = sf::st_as_sf(dat_gps_tcx$red, coords = c("longitude", "latitude"), crs = CRS) %>% sf::st_combine() %>% sf::st_cast("LINESTRING")

  return(list(line_ASC = dat_line_str_ASC,
              line_DESC = dat_line_str_DESC))
}



#' Find the image (x,y)-coordinates using meshgrids of (lat,lon)-coordinates
#'
#' @param longitude a numeric value specifying the longitude
#' @param latitude a numeric value specifying the latitude
#' @param buffer_raster an object of class raster
#' @param buffer_bbox a numeric vector specifying the bounding box
#' @param distance_metric a character string specifying the distance metric, one of "haversine" "vincenty", "geodesic" or "cheap" (see the 'geodist::geodist()' function of the 'geodist' package)
#' @param digits an integer value specifying the number of digits
#' @return an object of class list
#'
#' @keywords internal
#'
#' @import OpenImageR
#' @importFrom data.table setDT
#' @importFrom geodist geodist
#' @importFrom utils getFromNamespace
#'
#' @details
#'
#' I've used this workaround to label the rayshader 3d-plot because the 'lat', 'lon' parameters did not work properly

meshgrids_XY_LatLon = function(longitude,
                               latitude,
                               buffer_raster,
                               buffer_bbox,
                               distance_metric = "vincenty",
                               digits = 8) {

  #.............................  reset the options on.exit()
  init_options <- options()
  on.exit(options(init_options))
  #.............................

  options(digits = digits)

  DIMS = dim(buffer_raster)[1:2]

  meshgrid_x_openim = utils::getFromNamespace(x = "meshgrid_x", ns = "OpenImageR")
  meshgrid_y_openim = utils::getFromNamespace(x = "meshgrid_y", ns = "OpenImageR")

  x = meshgrid_x_openim(rows = DIMS[1], cols = DIMS[2]) + 1
  y = meshgrid_y_openim(rows = DIMS[1], cols = DIMS[2]) + 1

  # grid_lat_lon = HiClimR::grid2D(lon = c(buffer_bbox['xmin'], buffer_bbox['xmax']),
  #                                lat = c(buffer_bbox['ymin'], buffer_bbox['ymax']))               # it is not required, keep it as a reference

  lon_seq = seq(from = buffer_bbox['xmin'], to = buffer_bbox['xmax'], length.out = DIMS[2])         # computation of longitudes column-wise (you can verify the direction on the web browser consulting a map)
  lon_seq = lapply(1:DIMS[1], function(x) {
    lon_seq
  })
  lon_seq = do.call(rbind, lon_seq)

  lat_seq = seq(from = buffer_bbox['ymin'], to = buffer_bbox['ymax'], length.out = DIMS[1])         # computation of latitudes row-wise (you can verify the direction on the web browser consulting a map)
  lat_seq = lapply(1:DIMS[2], function(x) {
    lat_seq
  })
  lat_seq = do.call(cbind, lat_seq)

  seq_mt_coords = data.table::setDT(list(x = as.vector(x),
                                         y = as.vector(y),
                                         longitude = as.vector(lon_seq),
                                         latitude = as.vector(lat_seq)))

  mt_input_coords = matrix(c(longitude, latitude), nrow = 1, ncol = 2)

  dist_coords = suppressMessages(geodist::geodist(x = seq_mt_coords[, 3:4],
                                                  y = mt_input_coords,
                                                  paired = FALSE,
                                                  sequential = FALSE,
                                                  pad = TRUE,
                                                  measure = distance_metric))

  idx_min = which.min(dist_coords[, 1])[1]                        # keep the 1st. index in case that more than 1 coordinate pairs are close to the input (lat, lon) pair
  return(list(coords_row = seq_mt_coords[idx_min, ],
              dist_meters = dist_coords[idx_min, 1]))
}



#' Rayshader 3-dimensional using the Copernicus DEM elevation data
#'
#' @param rst_buf this parameter corresponds to the 'sfc_obj' object of the 'extend_AOI_buffer()' function
#' @param rst_ext this parameter corresponds to the 'raster_obj_extent' object of the 'extend_AOI_buffer()' function
#' @param rst_bbx this parameter corresponds to the 'buffer_bbox' object of the 'extend_AOI_buffer()' function
#' @param linestring_ASC_DESC If NULL then this parameter will be ignored. Otherwise, it can be an 'sf' object or a named list of length 2 (that corresponds to the output of the 'gps_lat_lon_to_LINESTRING()' function)
#' @param elevation_sample_points if NULL then this parameter will be ignored. Otherwise, it corresponds to a data.table with column names 'latitude', 'longitude' and 'AltitudeMeters'. For instance, it can consist of 3 or 4 rows that will be displayed as vertical lines in the 3-dimensionsal map to visualize sample locations of the route (the latitudes and longitudes must exist in the output data.table of the 'GPS_TCX_data()' function)
#' @param zoom a float number. Lower values increase the 3-dimensional DEM output. The default value is 0.5
#' @param windowsize a numeric vector specifying the window dimensions (x,y) of the output 3-dimensional map. The default vector is c(1600, 1000)
#' @param add_shadow_rescale_original a boolean. If TRUE, then 'hillshade' will be scaled to match the dimensions of 'shadowmap'. See also the 'rayshader::add_shadow()' function for more information.
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @return it doesn't return an object but it displays a 3-dimensional 'rayshader' object
#'
#' @export
#'
#' @references
#'
#' https://www.tylermw.com/a-step-by-step-guide-to-making-3d-maps-with-satellite-imagery-in-r/
#'
#' @importFrom sf st_bbox
#' @importFrom glue glue
#' @importFrom rayshader raster_to_matrix sphere_shade add_water add_shadow detect_water ray_shade add_overlay generate_line_overlay plot_3d render_label
#'
#' @examples
#'
#' \dontrun{
#'
#' require(fitbitViz)
#'
#' #............................
#' # first extract the log-id(s)
#' #............................
#'
#' USER_ID = '99xxxx'
#' token = 'my_long_web_api_token'
#'
#' log_id = extract_LOG_ID(user_id = USER_ID,
#'                         token = token,
#'                         after_Date = '2021-03-13',
#'                         limit = 10,
#'                         sort = 'asc',
#'                         verbose = TRUE)
#' str(log_id)
#'
#' #...................................
#' # then return the gps-ctx data.table
#' #...................................
#'
#' res_tcx = GPS_TCX_data(log_id = log_id,
#'                        user_id = USER_ID,
#'                        token = token,
#'                        time_zone = 'Europe/Athens',
#'                        verbose = TRUE)
#' str(res_tcx)
#'
#' #....................................................
#' # then compute the sf-object buffer and raster-extend
#' #....................................................
#'
#' sf_rst_ext = extend_AOI_buffer(dat_gps_tcx = res_tcx,
#'                                buffer_in_meters = 1000,
#'                                CRS = 4326,
#'                                verbose = TRUE)
#' sf_rst_ext
#'
#' #...............................................................
#' # Download the Copernicus DEM 30m elevation data because it has
#' # a better resolution, it takes a bit longer to download because
#' # the .tif file size is bigger
#' #...............................................................
#'
#' dem_dir = tempdir()
#' # dem_dir
#'
#' dem30 = CopernicusDEM::aoi_geom_save_tif_matches(sf_or_file = sf_rst_ext$sfc_obj,
#'                                                  dir_save_tifs = dem_dir,
#'                                                  resolution = 30,
#'                                                  crs_value = 4326,
#'                                                  threads = parallel::detectCores(),
#'                                                  verbose = TRUE)
#'
#' TIF = list.files(dem_dir, pattern = '.tif', full.names = T)
#' # TIF
#'
#' if (length(TIF) > 1) {
#'
#'   #....................................................
#'   # create a .VRT file if I have more than 1 .tif files
#'   #....................................................
#'
#'   file_out = file.path(dem_dir, 'VRT_mosaic_FILE.vrt')
#'
#'   vrt_dem30 = create_VRT_from_dir(dir_tifs = dem_dir,
#'                                   output_path_VRT = file_out,
#'                                   verbose = TRUE)
#' }
#'
#' if (length(TIF) == 1) {
#'
#'   #..................................................
#'   # if I have a single .tif file keep the first index
#'   #..................................................
#'
#'   file_out = TIF[1]
#' }
#'
#' raysh_rst = crop_DEM(tif_or_vrt_dem_file = file_out,
#'                      sf_buffer_obj = sf_rst_ext$sfc_obj,
#'                      CRS = 4326,
#'                      digits = 6,
#'                      verbose = TRUE)
#'
#' # sp::plot(raysh_rst)
#'
#'
#' #................................................................
#' # create the 'elevation_sample_points' data.table parameter based
#' # on the min., middle  and max. altitude of the 'res_tcx' data
#' #................................................................
#'
#' idx_3m = c(which.min(res_tcx$AltitudeMeters),
#'            as.integer(length(res_tcx$AltitudeMeters) / 2),
#'            which.max(res_tcx$AltitudeMeters))
#'
#' cols_3m = c('latitude', 'longitude', 'AltitudeMeters')
#' dat_3m = res_tcx[idx_3m, ..cols_3m]
#' # dat_3m
#'
#' #...............................................................
#' # Split the route in 2 parts based on the maximum altitude value
#' #...............................................................
#'
#' linestring_dat = gps_lat_lon_to_LINESTRING(dat_gps_tcx = res_tcx,
#'                                            CRS = 4326,
#'                                            time_split_asc_desc = NULL,
#'                                            verbose = TRUE)
#' #.....................................
#' # open the 3-dimensional rayshader map
#' #.....................................
#'
#' ray_out = rayshader_3d_DEM(rst_buf = raysh_rst,
#'                            rst_ext = sf_rst_ext$raster_obj_extent,
#'                            rst_bbx = sf_rst_ext$buffer_bbox,
#'                            linestring_ASC_DESC = linestring_dat,
#'                            # elevation_sample_points = dat_3m,
#'                            zoom = 0.5,
#'                            windowsize = c(1600, 1000),
#'                            add_shadow_rescale_original = FALSE,
#'                            verbose = TRUE)
#' }


rayshader_3d_DEM = function(rst_buf,
                            rst_ext,
                            rst_bbx,
                            linestring_ASC_DESC = NULL,
                            elevation_sample_points = NULL,         # it works but it's possible that the a few vertical lines are not displayed in the right position
                            zoom = 0.5,
                            windowsize = c(1600, 1000),
                            add_shadow_rescale_original = FALSE,
                            verbose = FALSE) {

  elevation_aoi = rayshader::raster_to_matrix(raster = rst_buf, verbose = verbose)
  rayshade_3d = rayshader::sphere_shade(heightmap = elevation_aoi, zscale = 0.95, texture = "desert", progbar = verbose)
  rayshade_3d = rayshader::add_water(hillshade = rayshade_3d, watermap = rayshader::detect_water(elevation_aoi), color = "desert")
  rayshade_3d = rayshader::add_shadow(hillshade = rayshade_3d, shadowmap = rayshader::ray_shade(elevation_aoi, zscale = 3, maxsearch = 65), max_darken = 0.5, rescale_original = add_shadow_rescale_original)

  if (!is.null(linestring_ASC_DESC)) {
    if (inherits(linestring_ASC_DESC, 'list')) {
      if (!all(names(linestring_ASC_DESC) %in% c("line_ASC", "line_DESC"))) stop("The named list must include the 'line_ASC', 'line_DESC' sublists!", call. = F)

      rayshade_3d = rayshader::add_overlay(hillshade = rayshade_3d, overlay = rayshader::generate_line_overlay(linestring_ASC_DESC$line_ASC,
                                                                                                               linewidth=3,
                                                                                                               color="blue",
                                                                                                               extent = rst_ext,
                                                                                                               heightmap = elevation_aoi),
                                           alphalayer=0.8)

      rayshade_3d = rayshader::add_overlay(hillshade = rayshade_3d, overlay = rayshader::generate_line_overlay(linestring_ASC_DESC$line_DESC,
                                                                                                               linewidth=3,
                                                                                                               color="red",
                                                                                                               extent = rst_ext,
                                                                                                               heightmap = elevation_aoi),
                                           alphalayer=0.8)
    }
    else if (inherits(linestring_ASC_DESC, c('sfc', 'sf'))) {

      rayshade_3d = rayshader::add_overlay(hillshade = rayshade_3d, overlay = rayshader::generate_line_overlay(linestring_ASC_DESC,
                                                                                                               linewidth=3,
                                                                                                               color="red",
                                                                                                               extent = rst_ext,
                                                                                                               heightmap = elevation_aoi),
                                           alphalayer=0.8)
    }
    else {
      stop("The 'linestring_ASC_DESC' parameter can be either a named list ('line_ASC', 'line_DESC') or an object of 'sf' or 'sfc'!", call. = F)
    }
  }

  rayshade_3d = tryCatch(rayshader::plot_3d(heightmap = elevation_aoi,
                                            hillshade = rayshade_3d,
                                            zoom = 0.5,
                                            zscale = 10,
                                            # fov = 0,
                                            # theta = 0,
                                            # phi = 1,
                                            windowsize = windowsize,
                                            water = TRUE,
                                            waterdepth = 0,
                                            wateralpha = 0.5,
                                            watercolor = "dodgerblue",
                                            waterlinecolor = "white",
                                            waterlinealpha = 0.3,
                                            verbose = verbose), error = function(e) e)

  if (!is.null(elevation_sample_points)) {

    for (ROW in 1:nrow(elevation_sample_points)) {

      label = meshgrids_XY_LatLon(longitude = elevation_sample_points$longitude[ROW],
                                  latitude = elevation_sample_points$latitude[ROW],
                                  buffer_raster = rst_buf,
                                  buffer_bbox = rst_bbx,
                                  distance_metric = "vincenty",
                                  digits = 8)

      rayshader::render_label(heightmap = elevation_aoi,
                              # long = elevation_sample_points$longitude[ROW],
                              # lat = elevation_sample_points$latitude[ROW],          # using 'lat', 'long' does not return any label data
                              x = label$coords_row$x,
                              y = label$coords_row$y,
                              z = as.integer(elevation_sample_points$AltitudeMeters[ROW]),
                              # z = max_elev_area + (increment_elev_to_distinguish_locations * ROW),
                              # zscale = 50,
                              zscale = 15,
                              text = as.character(glue::glue("Elevation: {round(elevation_sample_points$AltitudeMeters[ROW], 2)}")),
                              textcolor = "darkred",
                              linecolor = "darkred",
                              textsize = 1.3,
                              linewidth = 5)
      Sys.sleep(0.5)
    }
  }
}

