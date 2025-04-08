

# Ridge: form of regularized linear regression model
# Penalizes the coefficients with high value and forces them to be lower

# Lasso: form of regularized linear regression model
# Penalizes having too many coefficients by setting unimportant ones to zero
# Lasso very important for feature importance

library(readr)
library(rsample)
library(glmnet)
library(stringr)
library(tidymodels)
library(caret)

setwd("C:/Users/u2970/Downloads")
housing_cleaned <- read_csv('housing_cleaned.csv', quote = ',')

# # Changing names of columns at index 1 and 8
# names(housing)[c(1,8)] <- c('sf','elevation')
# 
# # Removing strings '\' from the housing$sf column
# housing$sf <- str_remove(housing$sf,'\"')
# 
# # Removing strings '\' from the housing$elevation column
# housing$elevation <- str_remove(housing$elevation,'\"')
# 
# # Changing data type of beds to factor
# housing$beds <- factor(housing$beds, ordered = TRUE)
# 
# # Changing data type of elevation to numeric
# housing$elevation <- as.numeric(housing$elevation)
# 
# housing <- housing[c('sf','beds','bath','year_built','sqft','price_per_sqft','elevation')]
# 
# housing_cleaned <- housing[c('sf','beds','bath','year_built','sqft','price_per_sqft','elevation')]


## Splitting and working with data using the rsample library

split1 <- initial_split(housing_cleaned, prop = 0.8, strata = 'price_per_sqft')

train <- training(split1)
test <- testing(split1)

beds <- train["beds"]
dummyBeds <- dummyVars("~.",data=beds)


train <- train %>% mutate(across(c(3),factor))
dummy::dummy(train[c(3)])

str(train)

train$beds <- dummy::dummy(train[c(2)])

train$bath <- dummy::dummy(train[c(3)])

housing_training <- model.matrix(price_per_sqft~.,train)




