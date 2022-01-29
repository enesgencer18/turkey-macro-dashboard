library(tidyverse)
library(zoo)
library(xts)
library(PerformanceAnalytics)

load("~/turkey-macro-dashboard/raw_data/df.Rda")
source("~/turkey-macro-dashboard/functions/Best_Lambda_Y.R")
source("~/turkey-macro-dashboard/functions/HP_Filter.R")


df <- df %>% 
  mutate(Inflation_Annual = round((CPI/lag(CPI,12) - 1),2)) %>%
  mutate(Inflation_Monthly = round((CPI/lag(CPI,1) - 1),2))

inf_annual <- zoo(x = df$Inflation_Annual, order.by = df$Date) %>% na.omit()
inf_monthly <- zoo(x = df$Inflation_Monthly, order.by = df$Date) %>% na.omit()


inf_annual_filtered <- Best.Lambdas.Y(Y = inf_annual,
               metric = "corr", 
               hp_lambda_1 = 5 ^ c(1:5),
               hp_lambda_2 = 5 ^ c(1:5),
               ifPlot = T)
autoplot(inf_annual_filtered$final_dat)


inf_monthly_filtered <- Best.Lambdas.Y(Y = inf_monthly,
                                      metric = "corr", 
                                      hp_lambda_1 = 10 ^ c(1:3),
                                      hp_lambda_2 = 10 ^ c(1:3),
                                      ifPlot = T)
autoplot(inf_monthly_filtered$final_dat)
