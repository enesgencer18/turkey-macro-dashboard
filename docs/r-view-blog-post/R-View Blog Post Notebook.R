### Session Info

#R version 4.1.0 (2021-05-18)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 22000)

#Matrix products: default

#locale:
#[1] LC_COLLATE=English_Netherlands.1252  LC_CTYPE=English_Netherlands.1252   
#[3] LC_MONETARY=English_Netherlands.1252 LC_NUMERIC=C                        
#[5] LC_TIME=English_Netherlands.1252    

#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#[1] ggthemes_4.2.4  zoo_1.8-9       caret_6.0-90    lattice_0.20-44
#[5] lubridate_1.8.0 forcats_0.5.1   stringr_1.4.0   dplyr_1.0.7    
#[9] purrr_0.3.4     readr_2.1.2     tidyr_1.2.0     tibble_3.1.6   
#[13] ggplot2_3.3.5   tidyverse_1.3.1


# Also, you need to load processed_data.Rdata

rm(list = ls())

library(tidyverse)
library(lubridate) 
library(caret)
library(zoo)
library(ggthemes)

load("~/turkey-macro-dashboard/processed_data/processed_data.Rdata")


forecast_date = "2022-03-31"

train_data <- forecast_df %>% 
  filter(Date < forecast_date)

test_data <- forecast_df %>% 
  filter(Date  == forecast_date)

tail(train_data,3)
head(test_data)

# Create time-slices for time-series cross-validation

time_slices <- trainControl(
  method = "timeslice",
  initialWindow = 36,
  fixedWindow = TRUE,
  horizon = 3,
  savePredictions = TRUE,
  verboseIter = FALSE
)

# Train the model

elastic_net_fit <- train(
  CPI ~ .,
  data = train_data[,-1],
  na.action = "na.pass",
  method = "glmnet",
  preProcess = c("center", "scale"),
  trControl = time_slices
)



# Choose the best regularization parameters

best_alpha = elastic_net_fit$bestTune[1] %>% pull
best_alpha

best_lambda = elastic_net_fit$bestTune[2] %>% pull
best_lambda

lasso_preds <- elastic_net_fit$pred %>% 
  filter(alpha == best_alpha,
         lambda == best_lambda) 

# Collect predictions

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
lasso_test <- tibble(Date = test_data$Date,
                     pred = predict(elastic_net_fit,
                                    test_data))
lasso_preds <- bind_rows(lasso_preds, lasso_test)

### Visualize results

# First plot

lasso_preds %>% 
  rename(Actual = CPI,
         Forecasted = pred) %>% 
  gather(key = "Variable", value = "Value", Actual:Forecasted) %>% 
  ggplot(aes(x = Date, y = Value, color = Variable))+
  geom_line(size = 0.75, alpha = 0.5)+
  geom_point(size = 0.75, alpha = 0.75)+
  scale_color_manual(values = c("steelblue4", "darkred"))+
  theme_bw()+
  scale_y_continuous(labels = scales::percent)+
  scale_x_date(breaks = seq(min(lasso_preds$Date), 
                            max(lasso_preds$Date), 
                            by="4 months"),
               date_labels = '%m-%y')+
  labs(x = "",
       y = "",
       subtitle = "Based on YoY CPI changes",
       title = "One-month ahead inflation forecast",
       color = "")+
  expand_limits(y = 0)+
  theme(plot.subtitle = element_text(face = "italic"))

# Second Plot

lasso_preds %>% 
  ggplot(aes(x = pred, y = CPI))+
  geom_point(size = 1.5, alpha = 0.8, color = "darkred")+
  geom_abline(intercept = 0, 
              slope = 1, 
              size = 0.3, 
              linetype = 3, 
              alpha = 0.8, 
              color = "steelblue4")+
  theme_bw()+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = scales::percent)+
  expand_limits(y = 0, x = 0)+
  labs(title = "Model Prediction & True Value",
       x = "Model Prediction",
       y = "True Value")

# Third plot

lasso_preds %>% 
  mutate(Residual = CPI - pred) %>% 
  ggplot(aes(x = Date, y = Residual))+
  geom_point(size = 1.5, alpha = 0.8, color = "steelblue4")+
  geom_ribbon(stat='smooth', se=TRUE, alpha=0.05) +
  geom_line(stat='smooth', alpha=1, color = "darkred")+
  theme_bw() +
  labs(y = "Residuals",
       x = "",
       title = "Elastic Net Model Residuals",
       subtitle = "Over time")+
  scale_y_continuous(labels = scales::percent)

# Fourth Plot

var_names <- coef(elastic_net_fit$finalModel, 
                  elastic_net_fit$finalModel$lambdaOpt) %>% 
  rownames()

var_values <- coef(elastic_net_fit$finalModel, 
                   elastic_net_fit$finalModel$lambdaOpt) %>% 
  as.numeric()

model_result <- tibble(Variable_Names = var_names,
                       Coefficients = var_values)

model_result %>% 
  mutate(Variable = gsub('[[:digit:]]+', '', 
                         str_remove(model_result$Variable_Names, 
                                    "_Lag"))) %>% 
  group_by(Variable) %>% 
  summarise(Overall_Effect = sum(Coefficients)) %>% 
  filter(!Overall_Effect == 0) %>%
  filter(!Variable == "(Intercept)") %>% 
  mutate(If_Positive = ifelse(Overall_Effect >0, "Positive", "Negative")) %>% 
  mutate(Sum = sum(abs(Overall_Effect))) %>% 
  mutate(Percent_Effect = Overall_Effect / Sum) %>% 
  ggplot(aes(x = reorder(Variable, abs(Overall_Effect)), y = abs(Percent_Effect), fill = If_Positive))+
  geom_col(alpha = 3)+
  scale_y_continuous(labels = scales::percent)+
  expand_limits(y = c(0,0.45))+
  coord_flip() +
  labs(y = "",
       x = "",
       fill = "",
       title = "The weighted effects macroeconomic variables on Turkey's Inflation",
       subtitle = "Shows the overall effect through different lags", 
       caption = "Source: CBRT")+
  theme_bw()+
  scale_fill_wsj()
