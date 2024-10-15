# Libraries
library(tidymodels)
library(tidyverse)
library(dplyr)
library(vip)
library(earth)
library(caret)

#Data transformation
options(scipen = 999)
sp500_data <- read.csv("sp500_companies_ml.csv", sep = ",", header = TRUE, fileEncoding = "UTF-8")

sp500_change <- sp500_data %>%
  mutate(Exceptional_growth = ifelse(Revenuegrowth > 0.10, TRUE, FALSE)) %>%
  select(-Industry, -Currentprice, -Weight, -Revenuegrowth, -State, -Exchange, -Symbol, -Country, -Longbusinesssummary, -Longname, -Shortname, -City) %>%
  na.omit()

sp500_final <- mutate(Sector = factor(Sector), sp500_change, Exceptional_growth = factor(Exceptional_growth))

view(sp500_final)


# Logistic Regression Model
set.seed(123)
sp_split <- initial_split(sp500_final, prop = 0.7, strata = "Exceptional_growth")  
sp_train <- training(sp_split)
sp_test <- testing(sp_split)

set.seed(123)
kfold <- vfold_cv(sp_train, v = 5)

results <- logistic_reg() %>%
  fit_resamples(Exceptional_growth ~ ., kfold)

final_fit <- logistic_reg() %>%
  fit(Exceptional_growth ~ ., data = sp_train)
tidy(final_fit)


final_fit %>%
  predict(sp_test) %>%
  bind_cols(sp_test %>% select(Exceptional_growth)) %>%
  conf_mat(truth = Exceptional_growth, estimate = .pred_class)

vip(final_fit$fit, num_features =  3)

