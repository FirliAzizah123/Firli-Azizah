library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)
library(astsa)
library(MLmetrics)

#Input Data
library(readxl)
Forecast_QTY <- read_excel("C:/Users/Firli Azizah/Documents/Forecasting/Full data Agustus Forecast.xlsx")
View(Forecast_QTY)
penjualan<-(Forecast_QTY$`Glasskin Drink`)
penjualan
glimpse(penjualan)

#Seasonal Naive
#seasonal.naive <- snaive(, h=length(test))

#Data Input
#dat <- read_csv("timeseriesdata.csv")
#glimpse(dat)

dat_ts <- ts(penjualan, start = c(2021, 1), end = c(2022, 9), frequency = 12)

#lines 2 to 4
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}
#Naive Forecasting
naive_mod <- naive(dat_ts, h = 3)
summary(naive_mod)

#dat_test$naive = 10376
#mape(dat_test$unemploy, dat_test$naive)  ## 8.5%

# Smoothing
se_model <- ses(dat_ts, h = 3)
summary(se_model)

#Holt Winter
holt_model <- holt(dat_ts, h = 3)
summary(holt_model)

#TBATS
model_tbats <- tbats(dat_ts)
summary(model_tbats)
for_tbats <- forecast::forecast(model_tbats, h = 3)
df_tbats = as.data.frame(for_tbats)
df_tbats

#Seasonal Naive
seasonal.naive <- snaive(dat_ts, h=3)
summary(seasonal.naive)

#Double Seasonal Holth Winter
dshw <- dshw(dat_ts, period1=2, period2 = 4, h=3)
summary(dshw)

