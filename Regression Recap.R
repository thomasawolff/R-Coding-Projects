
library(readr)
library(caret)
library(MASS)
library(stringr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(ggfortify)
library(gridExtra)

setwd("C:/R Coursework")
housing <- read_csv('housing.csv', quote = ',')

# Changing names of columns at index 1 and 8
names(housing)[c(1,8)] <- c('sf','elevation')

# Removing strings '\' from the housing$sf column
housing$sf <- str_remove(housing$sf,'\"')

# Removing strings '\' from the housing$elevation column
housing$elevation <- str_remove(housing$elevation,'\"')

# Changing data type of beds to factor
housing$beds <- factor(housing$beds, ordered = TRUE)

# Changing data type of elevation to numeric
housing$elevation <- as.numeric(housing$elevation)

housing <- housing[c('sf','beds','bath','price','year_built','sqft','price_per_sqft','elevation')]


# Plotting the sqft to price_per_sqft with a linear model for all data
all <- housing %>% ggplot(aes(x = sqft, y = price_per_sqft)) + geom_point() +
  geom_smooth(method = 'lm') + ggtitle("All data")

# Plotting the sqft to price_per_sqft with a linear model for sf data
sf <- housing %>% filter(sf == 1) %>%  ggplot(aes(x = sqft, y = price_per_sqft)) + geom_point() + 
  geom_smooth(method = 'lm') + ggtitle("SF data")

# Plotting the sqft to price_per_sqft with a linear model for ny data
ny <- housing %>% filter(sf == 0) %>%  ggplot(aes(x = sqft, y = price_per_sqft)) + geom_point() + 
  geom_smooth(method = 'lm') + ggtitle("NY data")


# Plotting all models and plots at once
grid.arrange(all,sf,ny)



boxplot(price_per_sqft ~ sf, data= housing)

# Creatign a linear model (callable) for data
model_sqft <- lm(price_per_sqft ~ sqft, data = housing)

# Summary of the model
summary(model_sqft)

# y lntercept of model
intercept <- coefficients(model_sqft)[[1]]

# Slope of the model
slope <- coefficients(model_sqft)[[2]]

# Calling the first index values for sqft and price_per_sqft
housing[1,c('sqft','price_per_sqft')]

# Making a prediction of what the value for sqft should be
first_fit_sqft <- intercept + housing[1,'sqft']*slope

# Fitting of every observation in housing data
fit_sqft <- predict(model_sqft,housing)

# Mean squared error for all data
mse <- mean((housing$price_per_sqft - fit_sqft)^2)





# Model for data including city data in sf field
model_sqft_city <- lm(price_per_sqft ~ sqft + sf, data = housing)

# Model summary
summary(model_sqft_city)

# Fitting of every observation in housing data with sf field
fit_sqft_city <- predict(model_sqft_city,housing)

# Mean squared error for all data with sf field for city
mseSf <- mean((housing$price_per_sqft - fit_sqft_city)^2)

# ANOVA on model for interactions
anova(model_sqft_city)





# Including model interactions with the model
model_interaction <- lm(price_per_sqft ~ (sqft*sf)+beds+bath+year_built,elevation, data = housing)

# Model summary
summary(model_interaction)

# Getting coefficients from model_interaction model
coefficients(model_interaction)

# Getting predictions from model_interaction model
prediction_interaction <- data.frame(fitting = predict(model_interaction, housing))

# Creating the model from scratch from what was fitted
prediction_interaction$manual <- coefficients(model_interaction)[[1]] + 
  coefficients(model_interaction)[[2]] * housing$sqft +
  coefficients(model_interaction)[[3]] * as.numeric(housing$sf) + 
  housing$sqft * as.numeric(housing$sf) * coefficients(model_interaction)[[4]]


# ANOVA testing to compare models
anova(model_sqft_city,model_interaction)

housing_cleaned$bath <- as.ordered(housing_cleaned$bath)
housing_cleaned$beds <- as.ordered(housing_cleaned$beds)






# Taking only rows of data with bedrooms between 1 and 6 and baths between 1 and 7
housing_cleaned <- housing %>% dplyr::filter(beds %in% c(0,1,2,3,4,5,6)) %>% dplyr::filter(bath %in% c(1,2,3,4,5,6,7))


housing_cleaned$price <- NULL
#housing_cleaned$bath <- as.ordered(housing_cleaned$bath)
#housing_cleaned$beds <- as.ordered(housing_cleaned$beds)

model_additive <- lm(price_per_sqft~.,data = housing_cleaned)
summary(model_additive)

fitted_additive <- stats::fitted(model_additive)

mse <- mean((housing_cleaned$price_per_sqft - fitted_additive)^2)


model_multiplicative <- lm(price_per_sqft~(sf*sqft)+beds+bath+year_built+elevation,data = housing_cleaned)
summary(model_multiplicative)


str(housing_cleaned)

anova(model_additive,model_multiplicative)

fitted_multiplicative <- stats::fitted(model_multiplicative)

mse <- mean((housing_cleaned$price_per_sqft - fitted_multiplicative)^2)



feature_importance <- varImp(model_multiplicative)
feature_importance %>% arrange(desc(Overall))

length(coefficients(model_multiplicative))


model_multiplicative2 <- lm(price_per_sqft~sf*sqft*beds*bath*year_built*elevation,data = housing_cleaned)
step_process <- stepAIC(model_multiplicative2, direction = 'both')


proposed_model <- lm(price_per_sqft ~ sf + sqft + beds + bath + year_built + elevation + 
                       sf:sqft + sf:beds + sqft:beds + sf:bath + sqft:bath + beds:bath + 
                       sf:year_built + sqft:year_built + beds:year_built + bath:year_built + 
                       sf:elevation + sqft:elevation + beds:elevation + bath:elevation + 
                       year_built:elevation + sf:sqft:beds + sf:sqft:bath + sf:beds:bath + 
                       sqft:beds:bath + sf:sqft:year_built + sf:beds:year_built + 
                       sqft:beds:year_built + sf:bath:year_built + sqft:bath:year_built + 
                       beds:bath:year_built + sf:sqft:elevation + sf:beds:elevation + 
                       sqft:beds:elevation + sf:bath:elevation + sqft:bath:elevation + 
                       sf:year_built:elevation + sqft:year_built:elevation + beds:year_built:elevation + 
                       bath:year_built:elevation + sf:sqft:beds:bath + sf:sqft:beds:year_built + 
                       sf:sqft:bath:year_built + sf:beds:bath:year_built + sf:sqft:beds:elevation + 
                       sf:sqft:bath:elevation + sf:sqft:year_built:elevation + sf:beds:year_built:elevation + 
                       sqft:beds:year_built:elevation + sf:bath:year_built:elevation + 
                       sqft:bath:year_built:elevation + sf:sqft:beds:year_built:elevation + 
                       sf:sqft:bath:year_built:elevation, data = housing_cleaned)


summary(proposed_model)

anova(model_multiplicative,proposed_model)

fitted_multiplicative <- stats::fitted(proposed_model)

mse <- mean((housing_cleaned$price_per_sqft - fitted_multiplicative)^2)




lmPerformance <- function(model1,model2,targetField) {
  #coefficients(model1)
  #coefficients(model2)
  print(summary(model2))
  print(anova(model_multiplicative,model2))
  fitted_multiplicative <- stats::fitted(model2)
  mse <- mean((targetField - fitted_multiplicative)^2)
  print(mse)
  
}

lmPerformance(model_multiplicative,proposed_model,housing_cleaned$price_per_sqft)


lmResidualsPlot <- function(residuals,fitModel) {
  plot(density(residuals))
  autoplot(proposed_model)
}

lmResidualsPlot(residuals(proposed_model),predict(proposed_model))



#> qqnorm(residuals)
#> qqline(residuals)
#> plot(density(residuals))
#> plot(predict(proposed_model),residuals)

