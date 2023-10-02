library(tidyverse)
library(magrittr)



eso_data_desc <- jsonlite::fromJSON("https://data.nationalgrideso.com/demand/historic-demand-data/datapackage.json")


eso_data_urls <- eso_data_desc$displayResources$resource$path %>%
  data.frame(URL=.) %>%
  dplyr::filter(!grepl("download/faq|embedded-forecast", URL)) %>%
  dplyr::mutate(FileName = stringr::str_remove_all(URL, ".*/download/"))


for(i in 1:nrow(eso_data_urls)) {

  if(eso_data_urls$FileName[i] !="demanddata.csv" & !file.exists(paste0("demand_data/", eso_data_urls$FileName[i]))) {

    curl::curl_download(eso_data_urls$URL[i], paste0("demand_data/", eso_data_urls$FileName[i]))

  }

}


test_df <- purrr::map_df(
  .x = dir("demand_data/", full.names = T),
  .f = ~ {
    print(.x)
    if(grepl("_", .x)) {
    readr::read_csv(.x, show_col_types = FALSE, progress = FALSE) %>%
        dplyr::mutate(SETTLEMENT_DATE = as.Date(SETTLEMENT_DATE, format = "%d-%b-%Y"))
    } else {
      readr::read_csv(.x, show_col_types = FALSE, progress = FALSE)

    }
  }
)















