#' @title Miscellaneous

hh_period <- function(dt) {

  1 + 2 * clock::get_hour(dt) + as.numeric(clock::get_minute(dt) > 21)

}
