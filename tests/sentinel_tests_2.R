# sentinel_test_2


source("R/sentinels.R")


Dates_From <- seq.Date(
  as.Date("2017-05-06"),
  Sys.Date() - 2,
  by = "20 days"
)

Dates_To <- Dates_From + 20

Dates_Len <- length(Dates_From)

Dates_Len_Consider <- Dates_Len - 2

for(i in 1:Dates_Len) {

  message(i," of ", Dates_Len, ":\n")

  fp <-  paste0(getwd(), "/cloud_cube/cloud_cube_df_From_", Dates_From[i], "_To_", Dates_To[i], ".csv")

  Cond1 <- !grepl(Dates_From[i],fp)

  Cond2 <- i >= Dates_Len_Consider

  if((!grepl(Dates_From[i],fp) & i >= Dates_Len_Consider) |  i >= Dates_Len_Consider) {

    download_cloud_cube_csv(
      paste0(getwd(), "/cloud_cube"),
      .from = Dates_From[i],
      .to = Dates_To[i]
    )

    st <- pmax(2*runif(1, 4.4, 60), 0.33*rweibull(1, shape = 2, scale = 20))

    message("sleeping for ", st, "seconds....")

    Sys.sleep(st)

  } else {

    message("File ", fp, " already exists")

  }


}
