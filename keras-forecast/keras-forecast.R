### LSTM Model

rm(list = ls())

library(tidyverse)
library(lubridate)
library(caret)
library(zoo)
library(keras)
library(reticulate)


#install.packages("remotes")
#remotes::install_github(sprintf("rstudio/%s", c("reticulate", "tensorflow", "keras")))
#reticulate::miniconda_uninstall() # start with a blank slate
#reticulate::install_miniconda()
#keras::install_keras()

tensorflow::as_tensor("Hello World")


load("./processed_data/processed_data.Rdata")

forecast_date = as.Date(as.yearmon(today())) + months(1) - days(1)

train_data <- forecast_df %>% 
  dplyr::filter(Date < forecast_date)

test_data <- forecast_df %>% 
  filter(Date  == forecast_date)

train_data <- train_data %>% 
  select(-CPI_Forecast) %>% 
  drop_na()

y <- train_data %>% 
  select(CPI) %>% 
  as.matrix()

range01 <- function(x){
  m <- mean(x)
  s <- sd(x)
  x <- (x-mean(x))/sd(x)
  x
}

X <- train_data %>% 
  select(-Date, -starts_with("CPI"), -Month) %>% 
  mutate_all(.funs = range01) %>% 
  as.matrix()

step = ncol(X)  
X <- array(X, dim = c(nrow(X), ncol(X), 1))


model = keras_model_sequential() %>%   
  layer_lstm(units=128, input_shape=c(step, 1), activation="relu") %>%  
  layer_dense(units=64, activation = "relu") %>%  
  layer_dense(units=32) %>%  
  layer_dense(units=1, activation = "relu")

model %>% 
  compile(loss = 'mse',
          optimizer = 'rmsprop')


model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE)

model
