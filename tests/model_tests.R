


library(tidymodels)
library(tidyverse)
library(magrittr)

source("R/pv.R")


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




cloud_cube_data_wide <- dplyr::select(cloud_cube_data, Date, Key, cloud_cover) %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(id_cols = "Date", names_from = "Key", values_from = "cloud_cover", values_fn = median) %>%
  tidyr::fill(dplyr::contains("_"), .direction = "downup")



cloud_cube_data_agg %>%
  dplyr::right_join(pv_df %>%
                      dplyr::group_by(date_gmt) %>%
                      dplyr::summarise(max_pv=mean(generation_mw, na.rm=T)) %>%
                      dplyr::ungroup()) %>%
  ggplot(aes(
    #dplyr::lag(
    lag_mean_cloud_cover
    # )
    ,  max_pv)) +
  geom_point() +
  geom_smooth(method="lm")

cloud_cube_data_agg %>%
  dplyr::right_join(pv_df %>%
                      dplyr::group_by(date_gmt) %>%
                      dplyr::summarise(max_pv=mean(generation_mw, na.rm=T)) %>%
                      dplyr::ungroup()) %>%
  ggplot(aes(
    #dplyr::lag(
    lag_mean_cloud_cover
    # )
    ,  lead(lag_mean_cloud_cover))) +
  geom_point() +
  geom_smooth(method="lm")


cloud_cube_data_agg %>%
  dplyr::right_join(pv_df) %>%
  ggplot(aes(
    dplyr::lag(
      generation_mw,1
    )
    ,  generation_mw)) +
  geom_point() +
  theme_bw()






pv <- pv_df %>%
#  dplyr::left_join(cloud_cube_data_wide %>%
#                     dplyr::rename(date_gmt=Date)) %>%
  dplyr::left_join(cloud_cube_data_agg) %>%
  na.omit()%>%
  dplyr::arrange(datetime_gmt) %>%
  dplyr::mutate(
    generation_mw = round(generation_mw),
    Period = hh_period(datetime_gmt),
    doy = lubridate::yday(date_gmt),
    lag_1_generation_mw = dplyr::lag(generation_mw, 1),
    lag_2_generation_mw = dplyr::lag(generation_mw, 2),
    znz = ifelse(generation_mw>0, "NonZero", "Zero")

    ) %>%
  dplyr::distinct() %>%
  dplyr::filter(date_gmt >= as.Date("2019-01-01"))  %>%
  na.omit()

p <- pacf(pv$generation_mw, plot=F)

pacf(pv$generation_mw[pv$generation_mw>0], plot=F)


data.frame( Lag = c(0, p$lag), Partial_ACF  = c(1, p$acf)) %>%
  ggplot(aes(x = Lag, y = Partial_ACF)) +
  geom_col() +
  theme_bw() +
  scale_y_continuous(labels=scales::percent, breaks=-10:10/10) +
  scale_x_continuous(breaks = seq(0, 50, by=1)) +
  labs(title = "Partial Autocorrelation", y = "Partial ACF") +
  theme(title = element_text(size=20))

pv %>%
  dplyr::group_by(Period) %>%
  dplyr::summarise(Average_Generation_MW=mean(generation_mw, na.rm=T)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x=Period, y = Average_Generation_MW)) +
  geom_line() +
  labs(title = "Average generation per half-hour period", y="Average Generation (MW)") +
  scale_x_continuous(breaks=1:48)+
  scale_y_continuous(breaks=seq(0, 5000, by=200))+
  theme_bw()+
  theme(title = element_text(size=20))

pv %>%
  dplyr::mutate(zero = as.numeric(generation_mw == 0),
                non_zero = as.numeric(generation_mw >0)) %>%
  dplyr::group_by(Period) %>%
  dplyr::summarise(`Count of Zeroes` = sum(zero),
                   Total = n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`Proportion of Zeroes` = `Count of Zeroes` / Total) %>%
  ggplot(aes(x=Period, y=`Proportion of Zeroes`)) +
  geom_line() +
  theme_bw() +
  labs(title = "Proportion of Zero Values Per HH Period") +
  scale_x_continuous(breaks=1:48) +
  scale_y_continuous(labels=scales::percent, breaks=0:10/10)+
  theme(title = element_text(size=20))

pv %>%
  ggplot(aes(x=generation_mw/1000))  +
  geom_histogram(fill=rainbow(30), alpha=.5) +
  theme_bw()

pv %>%
  subset(generation_mw>0) %>%
  ggplot(aes(x=generation_mw/1000))  +
  geom_density(fill="gold", alpha=.5) +
  theme_bw() +
  geom_segment(aes(x=0,y=0, xend=0, yend=.245))+
  geom_segment(aes(x=0,y=0, xend=11,yend=0)) +
  labs(title = "Distribution of Non-Zero PV Values", x="Gigawatts", y="Density")+
  scale_x_continuous(breaks=0:11) +
  theme(title = element_text(size=20))


mean(pv$generation_mw/1000)
var(pv$generation_mw/1000)



pv %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise(Average_Generation_MW=mean(generation_mw, na.rm=T)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x=doy, y = Average_Generation_MW)) +
  geom_line() +
  labs(title = "Average generation per Day of Year", y="Average Generation (MW)", x = "Day of the Year") +
  scale_x_continuous(breaks=seq(1, 365, by=30))+
  scale_y_continuous(breaks=seq(0, 15000, by=200))+
  theme_bw()+
  theme(title = element_text(size=20))


pv %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise(Zeroes=sum(generation_mw ==0, na.rm=T), Total=dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`Proportion of Zeroes`=Zeroes/Total) %>%
  ggplot(aes(x=doy, y=`Proportion of Zeroes`)) +
  geom_line() +
  theme_bw() +
  labs(title = "Proportion of Zero Values Per Day of Year", x="Day of the Year") +
  scale_x_continuous(breaks=seq(1, 365, by=30)) +
  scale_y_continuous(labels=scales::percent, breaks=0:10/10)+
  theme(title = element_text(size=20))



cloud_cube_data_agg %>%
  dplyr::right_join(pv_df) %>%
  ggplot(aes(
    dplyr::lag(
      generation_mw,  2
    )
    ,
    dplyr::lag(
      generation_mw,  1
    )
  )
  ) +
  geom_point() +
  theme_bw()



library(poissonreg)
library(pscl)
library(finetune)
library(embed)
library(doParallel)

pv_split <- rsample::initial_time_split(pv)

pv_train <- rsample::training(pv_split)

pv_folds <- rsample::vfold_cv(pv_train)


pv_preprocessor <- pv_train %>%
  recipes::recipe(generation_mw ~ .) %>%
  recipes::update_role(gsp_id, datetime_gmt, date_gmt, znz, new_role = "passive") %>%
  recipes::step_bs(Period, deg_free = tune(id = "Period_Spline")) %>%
  recipes::step_bs(doy, deg_free = tune(id = "DOY_Spline")) #%>%
#recipes::step_pca(dplyr::starts_with("R"), num_comp = tune())

pv_model <- parsnip::poisson_reg(mode = "regression", engine = "zeroinfl")

pv_wf <- workflows::workflow(pv_preprocessor, spec =  pv_model)

all_cores <- parallel::detectCores(logical = FALSE)

cl <- parallel::makePSOCKcluster(all_cores)

doParallel::registerDoParallel(cl)

pv_tuned <- finetune::tune_race_anova(pv_wf,  resamples = pv_folds,  grid = 28,
  control = finetune::control_race(T, T, T, parallel_over = "everything")
)

pv_best <- tune::select_best(pv_tuned)

pv_final <- tune::finalize_workflow(pv_wf, pv_best)

pv_fitted <- fit(pv_final, pv_train)

doParallel::stopImplicitCluster()
closeAllConnections()





pv_preprocessor <- pv_train %>%
  recipes::recipe(generation_mw ~ .) %>%
  recipes::update_role(gsp_id, datetime_gmt, date_gmt, znz, new_role = "passive") %>%
  recipes::step_bs(Period, deg_free = tune(id = "Period_Spline")) %>%
  recipes::step_bs(doy, deg_free = tune(id = "DOY_Spline")) #%>%
  #recipes::step_pca(dplyr::starts_with("R"), num_comp = tune())

# pv_model <- parsnip::poisson_reg(mode = "regression") %>%
#   set_engine(engine = "zeroinfl", dist="geometric")

pv_model2 <- parsnip::linear_reg(mode = "regression") %>%
  set_engine(engine = "glm")

pv_wf2 <- workflow(pv_preprocessor, spec =  pv_model2)


all_cores <- parallel::detectCores(logical = FALSE)

library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

pv_tuned2 <- finetune::tune_race_anova(
  pv_wf2,
  resamples = pv_folds,
  grid = 28,
  control=finetune::control_race(T, T, T, parallel_over = "everything")
)



pv_best2 <- tune::select_best(pv_tuned2)

pv_final2 <- tune::finalize_workflow(pv_wf2, pv_best2)

pv_fitted2 <- fit(pv_final2, pv_train)

stopImplicitCluster()
closeAllConnections()







pv_preprocessor3 <- pv_train %>%
  recipes::recipe(znz ~ .) %>%
  recipes::update_role(gsp_id, datetime_gmt, date_gmt,generation_mw, dplyr::contains(c("cloud")), new_role = "passive") %>%
  recipes::step_bs(Period, deg_free = tune(id = "Period_Spline")) %>%
  recipes::step_bs(doy, deg_free = tune(id = "DOY_Spline")) #%>%
#recipes::step_pca(dplyr::starts_with("R"), num_comp = tune())

pv_model3 <- parsnip::logistic_reg(mode = "classification") %>%
  set_engine(engine = "glm")

pv_wf3 <- workflow(pv_preprocessor3, spec =  pv_model3)


all_cores <- parallel::detectCores(logical = FALSE)

library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

pv_tuned3 <- finetune::tune_race_anova(
  pv_wf3,
  resamples = pv_folds,
  grid = 28,
  control=finetune::control_race(T, T, T, parallel_over = "everything")
)



pv_best3 <- tune::select_best(pv_tuned3)

pv_final3 <- tune::finalize_workflow(pv_wf3, pv_best3)

pv_fitted3 <- fit(pv_final3, pv_train)

stopImplicitCluster()
closeAllConnections()








pv_preds <- predict(pv_fitted, pv)

pv <- dplyr::bind_cols(pv, pv_preds)


pv$.pred <-  round(pv$.pred)
(mean((pv$generation_mw - pv$.pred)))
sqrt(mean((pv$generation_mw - pv$.pred)^2))

max(pv$generation_mw)
pv %>%
  dplyr::mutate(Predicted = pmax(0,.pred)) %>%
  tidyr::pivot_longer(cols = c("Predicted", "generation_mw")) %>%
  dplyr::filter(date_gmt >= as.Date("2023-09-18"), date_gmt <= as.Date("2023-09-22")) %>%
  ggplot(aes(x=datetime_gmt, y=value, colour=name)) +
  geom_line(linewidth=0.5, alpha=1) +
  theme_bw() +
  labs(title = "Predictions vs Actual - Part of Test Set Data", subtitle = "Zero-Inflated Poisson", y="Megawatts", x="Date-Time") +
  theme(plot.title = element_text(size=20))

pv %>%
  dplyr::filter(date_gmt >= as.Date("2023-09-18")) %>%
  dplyr::mutate(Predicted = pmax(0,.pred)) %>%
  ggplot(aes(x=datetime_gmt)) +
  geom_col(aes(y=generation_mw), fill="gold") +
  geom_line(aes(y=Predicted), colour="green") +
  #geom_point(linewidth=1, al, pha=.5) +
  theme_bw() +
  labs(title = "Predictions vs Actual", y="Megawatts", x="Date-Time")



saveRDS(pv_fitted, "models/pv_zip_model")
saveRDS(pv_fitted2, "models/pv_glm_model")
saveRDS(pv_fitted3, "models/pv_logit_model")



pv_fitted <- readRDS("models/pv_zip_model")

future_pv <- data.frame(
  gsp_id=0L,
  datetime_gmt = seq.POSIXt(pv$datetime_gmt[nrow(pv)], by = "30 min", length.out = 49),
  lag_mean_cloud_cover = pv$lag_mean_cloud_cover[nrow(pv)]

) %>%
  dplyr::mutate(date_gmt = as.Date(datetime_gmt), .before=lag_mean_cloud_cover) %>%
  dplyr::mutate(Period=hh_period(datetime_gmt),
                doy=lubridate::yday(date_gmt),
                lag_1_generation_mw=pv$generation_mw[nrow(pv)],
                lag_2_generation_mw=pv$generation_mw[nrow(pv)-1],
                znz = "Not Known"

                )

future_pv$generation_mw <- 0

future_pv_nrow <- nrow(future_pv)

for(i in 1:(future_pv_nrow)) {

future_pv$generation_mw[i] <- unlist(predict(pv_fitted, future_pv[i,] ))
future_pv$generation_mw[i] <- pmax(0, future_pv$generation_mw[i])


if(i <= future_pv_nrow - 2) {

  future_pv$lag_1_generation_mw[i+1] <- future_pv$generation_mw[i]
  future_pv$lag_2_generation_mw[i+2] <- future_pv$generation_mw[i]

  }

}


ggplot(subset(pv, date_gmt == Sys.Date()-3), aes(x=Period, y= generation_mw)) +
  geom_line()

ggplot(future_pv, aes(x=datetime_gmt, y=generation_mw)) +
  geom_line()






pacf(pv$generation_mw, plot = F)

acf(pv$generation_mw, plot = F)

