


library(tidymodels)
library(tidyverse)
library(magrittr)

source("R/misc.R")


pv_df <- readr::read_csv("pv/pv_live.csv", col_select = c("gsp_id", "datetime_gmt", "generation_mw")) %>%
  dplyr::mutate(date_gmt = as.Date(datetime_gmt))


pv_df %>%
  dplyr::count(date_gmt) %>% View()






pv_df %>%
  dplyr::filter(date_gmt > Sys.Date()-15, date_gmt < Sys.Date()-3) %>%
  ggplot(aes(x=datetime_gmt, y=generation_mw)) +
  geom_line()


cloud_cube_data <- purrr::map_df(.x=dir("cloud_cube/", full.names = T), ~{suppressMessages(readr::read_csv(.x, show_col_types = FALSE))}, .progress=T)


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







cloud_cube_data_agg %>%
  dplyr::right_join(pv_df %>%
                      dplyr::group_by(date_gmt) %>%
                      dplyr::summarise(max_pv=median(generation_mw, na.rm=T)) %>%
                      dplyr::ungroup()) %>%
  ggplot(aes(
    #dplyr::lag(
    lag_mean_cloud_cover
    # )
    ,  max_pv)) +
  geom_point()


cloud_cube_data_agg %>%
  dplyr::right_join(pv_df) %>%
  ggplot(aes(
    dplyr::lag(
      generation_mw,48
    )
    ,  generation_mw)) +
  geom_point()




pv <- pv_df %>%
  dplyr::left_join(cloud_cube_data_agg) %>%
  na.omit()%>%
  dplyr::arrange(datetime_gmt) %>%
  dplyr::mutate(
    generation_mw = round(generation_mw),
    Period = hh_period(datetime_gmt),
    doy = lubridate::yday(date_gmt),
    lag_1_generation_mw = dplyr::lag(generation_mw, 1)
    ) %>%
  dplyr::distinct() %>%
  dplyr::filter(date_gmt >= as.Date("2019-01-01"))  %>%
  na.omit()



pv_split <- rsample::initial_time_split(pv)


pv_train <- rsample::training(pv_split)


pv_folds <- rsample::vfold_cv(pv_train)

library(poissonreg)
library(pscl)
library(finetune)

pv_preprocessor <- pv_train %>%
  recipes::recipe(generation_mw ~ .) %>%
  recipes::update_role(gsp_id, datetime_gmt, date_gmt, new_role = "passive") %>%
  recipes::step_bs(Period, deg_free = tune(id = "Period_Spline")) %>%
  recipes::step_bs(doy, deg_free = tune(id = "DOY_Spline"))

pv_model <- parsnip::poisson_reg(mode = "regression", engine = "hurdle")

pv_wf <- workflow(pv_preprocessor, spec =  pv_model)

pv_tuned <- finetune::tune_race_anova(
  pv_wf,
  resamples = pv_folds,
  grid = 20,
  control=finetune::control_race(T, T, F)
)



pv_best <- tune::select_best(pv_tuned)

pv_final <- tune::finalize_workflow(pv_wf, pv_best)

pv_fitted <- fit(pv_final, pv_train)

pv_preds <- predict(pv_fitted, pv)

pv <- dplyr::bind_cols(pv, pv_preds)


pv$.pred <- round(pv$.pred)




pv %>%
  tidyr::pivot_longer(cols = c(".pred", "generation_mw")) %>%
  dplyr::filter(date_gmt >= as.Date("2023-09-18")) %>%
  ggplot(aes(x=datetime_gmt, y=value, colour=name)) +
  geom_line(linewidth=1, alpha=.5) +
  theme_bw() +
  labs(title = "Predictions vs Actual", y="Megawatts", x="Date-Time")



saveRDS(pv_fitted, "models/pv_hurdle_model")


future_pv <- data.frame(
  gsp_id=0L,
  datetime_gmt = seq.POSIXt(pv$datetime_gmt[nrow(pv)], by = "30 min", length.out = 49),
  lag_mean_cloud_cover = pv$lag_mean_cloud_cover[nrow(pv)]

) %>%
  dplyr::mutate(date_gmt = as.Date(datetime_gmt), .before=lag_mean_cloud_cover) %>%
  dplyr::mutate(Period=hh_period(datetime_gmt),
                doy=lubridate::yday(date_gmt),
                lag_1_generation_mw=pv$generation_mw[nrow(pv)])

future_pv$generation_mw <- 0

for(i in 1:(-1+nrow(future_pv))) {

  print(future_pv[i,])

future_pv$generation_mw[i] <- unlist(predict(pv_fitted, future_pv[i,] ))
future_pv$lag_1_generation_mw[i+1] <- future_pv$generation_mw[i]


}


ggplot(subset(pv, date_gmt == Sys.Date()-3), aes(x=Period, y= generation_mw)) +
  geom_line()

ggplot(future_pv, aes(x=Period, y=generation_mw)) +
  geom_line()
