library(tidyverse)
library(tidymodels)
library(modeltime)
library(lubridate)
library(forecast)

load("~/turkey-macro-dashboard/raw_data/df.Rda")

end_month <- month(df$Date[nrow(df)])
end_year <- year(df$Date[nrow(df)])

start_month <- month(df$Date[1])
start_year <- year(df$Date[1])

# define CPI as ts object
CPI <- ts(
    df$CPI,
    start = c(start_year, start_month),
    end = c(end_year, end_month),
    frequency = 12
  )

# define CLI as ts object
CLI <- ts(
    df$CLI,
    start = c(start_year, start_month),
    end = c(end_year, end_month),
    frequency = 12
  )

# define Domestic PPI as ts object
Domestic_PPI <- ts(
    df$Domestic_PPI,
    start = c(start_year, start_month),
    end = c(end_year, end_month),
    frequency = 12
  )

# define Inflation Expectation as ts object
Inf_Exp <- ts(
  df$Inflation_Expectation,
  start = c(start_year, start_month),
  end = c(end_year, end_month),
  frequency = 12
)

# define Financial Sector Confidience Index as ts object
FS_Conf <- ts(
  df$FS_Confidience,
  start = c(start_year, start_month),
  end = c(end_year, end_month),
  frequency = 12
)

# define Real Sector Confidience Index as ts object
RS_Conf <- ts(
  df$RS_Confidience,
  start = c(start_year, start_month),
  end = c(end_year, end_month),
  frequency = 12
)

# define Production Volume as ts object
Prod_Vol <- ts(
  df$Production_Volume,
  start = c(start_year, start_month),
  end = c(end_year, end_month),
  frequency = 12
)

# define Export Orders as ts object
Exp_Ord <- ts(
  df$Export_Orders,
  start = c(start_year, start_month),
  end = c(end_year, end_month),
  frequency = 12
)
