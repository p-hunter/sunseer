#' @title PV Data Functions

hh_period <- function(dt) {

  1 + 2 * clock::get_hour(dt) + as.numeric(clock::get_minute(dt) > 21)

}



pv_download <- function(.date, save_dir = "pv/", return_df = FALSE) {

  .date2 <- .date + 1

  from_date <- format.POSIXct(as.POSIXct(paste0(.date, " 00:00:00"), tz = "GMT"), format = "%Y-%m-%dT%H:%M:%S")

  to_date <- format.POSIXct(as.POSIXct(paste0(.date2, " 00:00:00"), tz = "GMT"), format = "%Y-%m-%dT%H:%M:%S")

  URL_Base <- "https://api0.solar.sheffield.ac.uk/pvlive/api/v4/gsp/0?start="

  URL <- paste0(URL_Base, from_date, "&end=", to_date, "&data_format=csv")

  fn <- paste0(save_dir, .date, ".csv")

  download.file(URL, destfile = fn)

  if(return_df) {

    return(readr::read_csv(fn))

  } else {

    return(invisible(NULL))

  }


}
