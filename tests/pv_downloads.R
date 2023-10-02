# pv_downloads


library(tidyverse)
library(magrittr)
library(clock)


test_url <- "https://api0.solar.sheffield.ac.uk/pvlive/api/v4/gsp/0?start=2022-11-01T00:00:00&end=2022-11-01T10:00:00&data_format=csv"





test_df <- readr::read_csv(test_url)




URL_Base <- "https://api0.solar.sheffield.ac.uk/pvlive/api/v4/gsp/0?start="

Dates <- seq.POSIXt(as.POSIXct("2016-01-01 00:00:00", tz = "GMT"), Sys.time(), by = "1 days") %>%
  format.POSIXct( format = "%Y-%m-%dT%H:%M:%S")


urls <- paste0(URL_Base, Dates, "&end=", dplyr::lead(Dates, default = format.POSIXct(Sys.time(), format = "%Y-%m-%dT%H:%M:%S")), "&data_format=csv")



p <- progress::progress_bar$new(
  format = "Getting Datasets - [:bar] | Completion: :percent (:current/:total) | ETA: :eta | Time: :elapsedfull",
  clear = FALSE,
  total = length(urls)

)


for(i in 1:length(urls)) {

  p$tick()

  fndate <-  as.Date(Dates[i], format = "%Y-%m-%dT%H:%M:%S")

  fn <- paste0("pv/", fndate, ".csv")

  if(!file.exists(fn)) {


  readr::read_csv(urls[i], show_col_types = FALSE) %>%
    write.csv(file = fn)

  st <- (pmax(5, rpois(1, 2)+1) + runif(1, 1, runif(1, 2, 5)))


  Sys.sleep(st)

  } else {


  }

  closeAllConnections()

}



Files <- dir("pv/", full.names = TRUE, pattern = "-")


p <- progress::progress_bar$new(
  clear = FALSE,
  format = "Reading PV Files [:bar] | Completion: :percent (:current/:total) | ETA: :eta | Time: :elapsedfull",
  total = length(Files)
)

test_df_2_list <- purrr::map(
  .x = Files,
  .f = ~ {

    p$tick()

    suppressMessages(
      readr::read_csv(
        file = .x,
        show_col_types = FALSE,
        col_select = c("gsp_id", "datetime_gmt", "generation_mw"),
        progress = FALSE
      )
    )

    }
  )


test_df_2 <- dplyr::bind_rows(test_df_2_list)

write.csv(test_df_2, "pv/pv_live.csv")









