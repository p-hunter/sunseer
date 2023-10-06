#' @title sentinel functions


download_cloud_cube_csv <- function(.dir, .from, .to, .return_data = FALSE) {

  require(sits)
  require(magrittr)
  require(dplyr)
  require(lubridate)


  if(!lubridate::is.Date(.from) | !lubridate::is.Date(.to)) {

    .from <- as.Date(.from)

    .to <- as.Date(.to)

  }

  if(is.na(.from) | is.na(.to) | is.null(.from) | is.null(.to)) {

    stop("Cannot decipher to- and from dates. Please provide dates or character strings in yyyy-mm-dd format.")

  }

  message("Downloading data from ", .from, " to ", .to)

  cloud_cube <- sits::sits_cube(
    source = "MPC",
    collection = "SENTINEL-2-L2A",
    roi = c(
      "lat_min" = 50.148746,
      "lon_min" = -11.074219,
      "lat_max" = 60.823494,
      "lon_max" = 2.5488281
    ),
    bands = c("CLOUD"),
    start_date = as.Date(.from),
    end_date = as.Date(.to),
    progress = FALSE
  )


  cloud_cube_df <- cloud_cube$file_info %>%
    dplyr::bind_rows() %>%
    dplyr::rename(Date = `date`) %>%
    dplyr::mutate(
      DateTime = as.POSIXct(paste0(Date, substr(fid, 21, 26)), format = "%Y-%m-%d%H%M%S"),
      .before = band
    ) %>%
    dplyr::mutate(Key = substr(fid, 28,38), .before = band)

  save_as <- paste0(.dir, "/cloud_cube_df_From_", .from, "_To_", .to, ".csv")

  write.csv(cloud_cube_df, save_as)

  message("Data sucessfully saved as CSV: ", save_as)

  if(!.return_data) {

  return(invisible(NULL))

    } else {

      message("\nReturning dataframe...")

      return(cloud_cube_df)

      }

  invisible(NULL)

}
