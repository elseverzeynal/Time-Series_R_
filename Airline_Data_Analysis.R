library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(inspectdf)
library(data.table)
library(dplyr)
library(h2o)
library(rsample)
library(forecast)
library(highcharter)
library(earth)

setwd("C:/Users/99470/Downloads")
df <- fread("AirPassengers (3).csv")

colnames(df) <- c('Dates','Count')

df %>% glimpse()

df %>% inspect_na()

#changing data types
df$Dates <- df$Dates %>% data.frame()

df$Dates <- as.Date(paste(df$Dates,1,sep="-"),"%Y-%m-%d")
#visualize
df %>% plot_time_series(Dates, Count,
                        # .color_var=lubridate::week(Date),
                        # .color_lab="Month",
                        .interactive = T,
                        .plotly_slider = T,
                        .smooth=F)

#visualize seasonality
df %>%  plot_seasonal_diagnostics(Dates, Count, .interactive = T)


#date-i hisselere ayririq, lazimsizlari cixiriq
df <- df %>% tk_augment_timeseries_signature(Dates) %>% select(Dates,everything())
df <- df %>%
  select(-contains("hour"),
         -contains("day"),
         -contains("week"),
         -minute,-second,-am.pm) %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)


#Time Series Models__________________________________

# Split Data 
splits <- initial_time_split(df, prop = 0.8)

#modelde bir dene sorussa da men oyrenmek meqsedli bir necesini qurdum

# Model 1: arima_boost ----
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(Count ~ Dates,
      data = training(splits))


# Model 2: ets ----
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(Count ~ Dates, data = training(splits))


# Model 3: prophet ----
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(Count ~ Dates, data = training(splits))



#Add fitted models to a Model Table.----

models_tbl <- modeltime_table(
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet
)

models_tbl




#Calibrate the model to a testing set.----
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl

#Testing Set Forecast & Accuracy Evaluation_________

#Visualizing the Forecast Test
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = df
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25,
    .interactive      = T
  )


#Accuracy Metrics----
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = T
  )
#we choose ARİMA_BOOSTED as it has lowest RMSE

#Refit to Full Dataset & Forecast Forward----

#note:normalda bu asagida bir model secmek lazim deyil, HW ucun etdim
calibration_tbl <- model_fit_arima_boosted %>%
  modeltime_calibrate(new_data = testing(splits))


calibration_tbl %>%
  modeltime_forecast(h = "1 years", actual_data = df) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25,
    .interactive      = T
  )
