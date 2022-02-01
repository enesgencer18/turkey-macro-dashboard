rm(list = ls())

library(tidyverse)
library(lubridate)
library(caret)

load("~/turkey-macro-dashboard/processed_data/processed_data.Rdata")

forecast_date = as.Date(as.yearmon(today())) + months(1) - days(1)

train_data <- forecast_df %>% 
  filter(Date < forecast_date)

test_data <- forecast_df %>% 
  filter(Date  == forecast_date)

# Construct time slices for time-series cross validation
time_slices <- trainControl(
  method = "timeslice",
  initialWindow = 48,
  fixedWindow = FALSE,
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

# Fit xgbDART
rf_fit <- train(CPI ~ .,
                data = train_data,
                method = "xgbDART",
                trControl = time_slices)


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

### Choose the best xgbDART parameters
best_nrounds = rf_fit$bestTune$nrounds
best_max_depth = rf_fit$bestTune$max_depth
best_eta = rf_fit$bestTune$eta
best_gamma = rf_fit$bestTune$gamma
best_subsample = rf_fit$bestTune$subsample
best_colsample = rf_fit$bestTune$colsample_bytree
best_rate_drop = rf_fit$bestTune$rate_drop
best_skip_drop = rf_fit$bestTune$skip_drop
best_min_child = rf_fit$bestTune$min_child_weight

rf_preds <- rf_fit$pred %>% 
  filter(nrounds == best_nrounds, 
           max_depth == best_max_depth,
           eta == best_eta,
           gamma == best_gamma, 
           subsample == best_subsample,
           colsample_bytree == best_colsample,
           rate_drop == best_rate_drop,
           skip_drop == best_skip_drop, 
           min_child_weight == best_min_child) 


rf_preds <- get_predictions(rf_preds)


ensemble_preds <- rf_preds %>% 
  left_join(lasso_preds %>% select(Date, pred), by = "Date") %>% 
  mutate(average_pred = (pred.x + pred.y)/2) %>% 
  select(Date, CPI, average_pred)

test_preds <- tibble(Date = test_data$Date,
                     CPI = test_data$CPI,
                     average_pred = (predict(rf_fit, test_data) + predict(lasso_fit, test_data))/2)

lasso_test <- tibble(Date = test_data$Date,
                     pred = predict(lasso_fit, test_data))
rf_test <- tibble(Date = test_data$Date,
                     pred = predict(rf_fit, test_data))

lasso_preds <- bind_rows(lasso_preds, lasso_test)
rf_preds <- bind_rows(rf_preds, rf_test)
ensemble_preds <- bind_rows(ensemble_preds, test_preds)

save(list = c("lasso_fit",
              "rf_fit",
              "lasso_preds",
              "rf_preds",
              "ensemble_preds"),file = "~/turkey-macro-dashboard/analysis/ml_data.Rdata")


