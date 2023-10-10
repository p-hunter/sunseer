# model tests again




library(tidymodels)
library(tidyverse)
library(poissonreg)
library(pscl)
library(finetune)
library(magrittr)

source("R/pv.R")


pv_df <- readr::read_csv("pv/pv_live.csv", col_select = c("gsp_id", "datetime_gmt", "generation_mw")) %>%
  dplyr::mutate(date_gmt = as.Date(datetime_gmt))

cloud_cube_data <- readr::read_csv("cloud_cube/cloud_cube_data.csv")

cloud_cube_data_agg <- cloud_cube_data %>%
  dplyr::filter(cloud_cover>0) %>%
  dplyr::group_by(date_gmt=Date) %>%
  dplyr::summarise(mean_cloud_cover=mean(cloud_cover, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::mutate(lag_mean_cloud_cover=dplyr::lag(mean_cloud_cover)) %>%
  dplyr::select(date_gmt, lag_mean_cloud_cover) %>%
  na.omit() %>%
  dplyr::distinct()

pv <- pv_df %>%
  dplyr::left_join(cloud_cube_data_agg) %>%
  na.omit()%>%
  dplyr::arrange(datetime_gmt) %>%
  dplyr::mutate(
    generation_mw = round(generation_mw),
    Period = hh_period(datetime_gmt),
    doy = lubridate::yday(date_gmt),
    lag_1_generation_mw = dplyr::lag(generation_mw, 1),
    lag_2_generation_mw = dplyr::lag(generation_mw, 2),
    znz = ifelse(generation_mw==0, "Zero", "NonZero")
  ) %>%
  dplyr::distinct() %>%
  dplyr::filter(date_gmt >= as.Date("2019-01-01"))  %>%
  na.omit()

pv_fitted <- readRDS("models/pv_glm_model")

future_pv <- data.frame(
  gsp_id=0L,
  datetime_gmt = seq.POSIXt(pv$datetime_gmt[nrow(pv)], by = "30 min", length.out = 49),
  lag_mean_cloud_cover = 100L# pv$lag_mean_cloud_cover[nrow(pv)]

) %>%
  dplyr::mutate(date_gmt = as.Date(datetime_gmt), .before=lag_mean_cloud_cover) %>%
  dplyr::mutate(Period=hh_period(datetime_gmt),
                doy=lubridate::yday(date_gmt),
                lag_1_generation_mw=pv$generation_mw[nrow(pv)],
                lag_2_generation_mw=pv$generation_mw[nrow(pv)-1],
                znz = "Unknown")

future_pv$generation_mw <- 0

future_pv_nrow <- nrow(future_pv)

stime <- Sys.time()

for(i in 1:(future_pv_nrow)) {

  future_pv$generation_mw[i] <- unlist(predict(pv_fitted, future_pv[i,] ))
  future_pv$generation_mw[i] <- pmax(0, future_pv$generation_mw[i])


  if(i <= future_pv_nrow - 2) {

    future_pv$lag_1_generation_mw[i+1] <- future_pv$generation_mw[i]
    future_pv$lag_2_generation_mw[i+2] <- future_pv$generation_mw[i]

  }

}


etime <- Sys.time()

etime-stime





ggplot(subset(pv, date_gmt == Sys.Date()-6), aes(x=Period, y= generation_mw)) +
  geom_line()

ggplot(future_pv, aes(x=datetime_gmt, y=generation_mw)) +
  geom_line()

library(DALEX)
library(DALEXtra)

me <- DALEXtra::explain_tidymodels(pv_fitted, pv)
DALEX::variable_effect_partial_dependency(me, variables = "lag_mean_cloud_cover") %>%
  tibble::as.tibble() %>%
  ggplot(aes(`_x_`, `_yhat_`)) +
  geom_point() +
  geom_line() +
  labs(title = "Partial Dependence - Mean Lagged Cloud Cover %") +
  theme_bw() +
  theme(plot.title = element_text(size = 20))

DALEX::variable_effect_partial_dependency(me, variables = "Period") %>%
  tibble::as.tibble() %>%
  ggplot(aes(`_x_`, `_yhat_`)) +
  geom_point() +
  geom_line() +
  labs(title = "Partial Dependence - Mean Lagged Cloud Cover %") +
  theme_bw() +
  theme(plot.title = element_text(size = 20))

DALEX::variable_effect_partial_dependency(me, variables = "doy") %>%
  tibble::as.tibble() %>%
  ggplot(aes(`_x_`, `_yhat_`)) +
  geom_point() +
  geom_line() +
  labs(title = "Partial Dependence - Mean Lagged Cloud Cover %") +
  theme_bw() +
  theme(plot.title = element_text(size = 20))
