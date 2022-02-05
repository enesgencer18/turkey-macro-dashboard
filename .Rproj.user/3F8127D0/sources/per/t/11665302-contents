rm(list = ls())

library(tidyverse)
library(lubridate)
library(caret)
library(zoo)

load("./processed_data/processed_data.Rdata")

forecast_date = as.Date(as.yearmon(today())) + months(1) - days(1)

train_data <- forecast_df %>% 
  filter(Date < forecast_date)

test_data <- forecast_df %>% 
  filter(Date  == forecast_date)

# Construct time slices for time-series cross validation
time_slices <- trainControl(
  method = "timeslice",
  initialWindow = 36,
  fixedWindow = TRUE,
  horizon = 3,
  savePredictions = TRUE,
  verboseIter = TRUE
)

# Fit Regularized Regression Model
lasso_fit <- train(
  CPI ~ .,
  data = train_data,
  na.action = "na.pass",
  method = "glmnet",
  preProcess = c("center", "scale"),
  trControl = time_slices
)


# Choose the best regularized lasso parameters
best_alpha = lasso_fit$bestTune[1] %>% pull
best_lambda = lasso_fit$bestTune[2] %>% pull

lasso_preds <- lasso_fit$pred %>% 
  filter(alpha == best_alpha & lambda == best_lambda) 

get_predictions <- function(x){
  x1 <- x %>% 
    filter(!Resample == unique(Resample)[length(unique(Resample))]) %>%  
    group_by(Resample) %>% 
    filter(rowIndex == min(rowIndex)) %>%
    ungroup() %>% 
    select(rowIndex,pred, obs) %>% 
    rename(CPI = obs) %>% 
    left_join(forecast_df %>% select(Date, CPI), by = "CPI") %>% 
    select(Date, CPI, pred) 
  
  x2 <- x %>% 
    filter(Resample == unique(Resample)[length(unique(Resample))]) %>% 
    select(rowIndex,pred, obs) %>% 
    rename(CPI = obs) %>% 
    left_join(forecast_df %>% select(Date, CPI), by = "CPI") %>% 
    select(Date, CPI, pred) 
  
  res <- bind_rows(x1, x2)
  res
}

lasso_preds = get_predictions(lasso_preds)
mape_vec(lasso_preds$CPI, lasso_preds$pred)
lasso_test <- tibble(Date = test_data$Date,
                     pred = predict(lasso_fit, test_data))
lasso_preds <- bind_rows(lasso_preds, lasso_test)

save(list = c("lasso_fit",
              "lasso_preds",
              "best_alpha",
              "best_lambda"),file = "./analysis/ml_data.Rdata")


