library(XML)
library(methods)
library(httr)
library(xts)
library(lubridate)
library(quantmod)
library(tidyverse)

#source("~/turkey-macro-dashboard/functions/Get_Cb_Data.R")

Get_Cb_Data <- function(Serie = "TP.FG.J0",
                        Start_Date = "01-01-2013",
                        End_Date = lubridate::today(),
                        API = "4VwjGbTONS"){
  URL <- paste0(
      "https://evds2.tcmb.gov.tr/service/evds/series=",
      paste0(Serie, collapse = "-"),
      "&startDate=",
      Start_Date,
      "&endDate=",
      format(End_Date, "%d-%m-%Y"),
      "&type=xml&key=",
      API
    )
  doc <- xmlTreeParse(GET(URL))
  root <- xmlRoot(doc)
  my_data <- xmlSApply(root, function(x)
    xmlSApply(x, xmlValue))
  my_data <- data.frame(my_data, row.names = NULL)
  my_data <- t(my_data)
  colnames(my_data) <- unique(names(root[2]$items))
  my_data <- my_data[-1, c(1:(ncol(my_data) - 1))]
  rownames(my_data) <- NULL
  my_data <- as_tibble(my_data)
  my_data
}

cb_series <-c(
    "TP.FG.J0",
    "TP.CLI2.A02",
    "TP.TUFE1YI.T1",
    "TP.BEK.S01.E.U",
    "TP.FHGE.S01",
    "TP.GY1.N2.MA",
    "TP.GY4.N2.MA",
    "TP.GY7.N2.MA",
    "TP.ODEAYRSUNUM6.Q1",
    "TP.KKO.MA",
    "TP.TG2.Y03",
    "TP.DT.IT.FIY.D01.2010"
  )
col_names <- c(
    "Date",
    "CPI",
    "CLI",
    "Domestic_PPI",
    "Inflation_Expectation",
    "FS_Confidience",
    "RS_Confidience",
    "Production_Volume",
    "Export_Orders",
    "BoP",
    "Utilization_Rate",
    "Consumer_Confidience",
    "Average_Import_Price"
  )

df_list <- map(1:length(cb_series), 
    .f = ~Get_Cb_Data(Serie = cb_series[.x]))

df <- reduce(df_list, left_join, by = "Tarih")
colnames(df) <- col_names

usdtry <- getSymbols("USDTRY=X", 
                     src ="yahoo", 
                     auto.assign = FALSE) %>% 
  fortify() %>% 
  as_tibble()

usdtry <- usdtry %>% 
  rename(Date = Index,
         Usd_Try = "USDTRY=X.Adjusted") %>% 
  select(Date, Usd_Try) %>% 
  filter(Date >= "2013-01-01") %>% 
  mutate(year = year(Date)) %>% 
  mutate(month = month(Date)) %>% 
  mutate(day = day(Date)) %>% 
  group_by(year, month) %>% 
  filter(day == max(day)) %>% 
  mutate(Date = paste0(year(Date), "-", month(Date))) %>% 
  ungroup() %>% 
  select(Date, Usd_Try)

df <- df %>% 
  left_join(usdtry, by = "Date")

df <- df %>%
  mutate(Date = as.Date(as.yearmon(Date, format = "%Y-%m")) + months(1) - days(1)) %>% 
  mutate_if(.predicate = is.character, .funs = as.numeric)

save(df, file = "./raw_data/df.Rda")



