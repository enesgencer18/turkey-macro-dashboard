library(tidyverse)
library(zoo)
library(xts)
library(PerformanceAnalytics)
library(forecast)
library(lubridate)
library(modeltime)
library(timetk)
library(skimr)
library(tidymodels)
library(ggthemes)

load("./df.Rda")

df <- na.locf(df)

df <- df %>% 
  mutate(CPI = CPI/lag(CPI,12) - 1) %>% 
  mutate(Domestic_PPI = Domestic_PPI/lag(Domestic_PPI,12) -1) %>% 
  mutate(Production_Volume = Production_Volume/lag(Production_Volume, 12) -1) %>% 
  mutate(Export_Orders = Export_Orders/lag(Export_Orders, 12) -1) %>% 
  mutate(Average_Import_Price = Average_Import_Price * Usd_Try) %>% 
  mutate(Import_Annual_Ret = Average_Import_Price/lag(Average_Import_Price, 12) -1) %>%
  mutate(Import_Monthly_Ret = Average_Import_Price/lag(Average_Import_Price, 1) -1) %>%
  mutate(UsdTry_Annual_Ret = Usd_Try/lag(Usd_Try, 12) -1) %>%
  mutate(UsdTry_Monthly_Ret = Usd_Try/lag(Usd_Try, 1) -1) %>%
  select(-Average_Import_Price, -Usd_Try) %>% 
  filter(Date >= "2014-01-01")

forecast_df <- df %>%
  future_frame(
    .date_var = Date,
    .length_out = "12 months",
    .bind_data = TRUE)


lag_variables <- list()

# Desired Lag Length
n = 6

# Construct A List Consisting of Lag Variables
lag_variables <- map(.x = 1:n, .f = function(i){
  x <- forecast_df %>% 
    mutate_if(is.numeric, lag, i)
  colnames(x) <- c("Date",paste0(colnames(df[,-1]), "_Lag", i))
  x
})

lag_df <- reduce(lag_variables, 
                 left_join, 
                 by = "Date")

forecast_df <- forecast_df %>% 
  select(Date, CPI) %>% 
  left_join(lag_df, 
            by = "Date")

forecast_df <- forecast_df %>% 
  filter(Date > "2015-01-31")

forecast_df <- forecast_df %>% 
  mutate(Month = as.factor(month(Date)))

save(forecast_df, file = "./processed_data/processed_data.Rdata")
