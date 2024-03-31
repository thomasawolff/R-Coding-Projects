
library(readr)
library(caret)
library(MASS)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(ggfortify)
library(gridExtra)
library(moments)


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

#housing <- housing[c('sf','beds','bath','price','year_built','sqft','price_per_sqft','elevation')]
housing <- housing[c('beds','bath','price','year_built','sqft','price_per_sqft','elevation')]
housing$beds <- as.numeric(housing$beds)


model <- lm(price_per_sqft~sqft+beds+bath+year_built+elevation,data = housing)
summary(model)


# Website for ggplot2 & Tidyverse: https://ggplot2.tidyverse.org/index.html



normalizeData <- function(dataField) {
  MinimaxNorm <- (dataField - min(dataField))/(max(dataField) - min(dataField))
  #zscaleNorm <-(dataField - mean(dataField))/sd(dataField)
  return(MinimaxNorm)
}



skew <- function(x) {
  print(skewness(x)) 
  print(kurtosis(x)) 
  jarque.test((x)) # Performing Jarque-Bera test. P val < 0.05 shows 
  # data does not match normal distribution. Null hypothesis
  # is that data has normal normal distribution
}




ExploratoryAnalysis <- function(data,dataField) {
  pairs(data)
  M <- cor(data)
  corrplot(M, method = 'number')
}





scatterplot <- function(data,x,y) {
  ggplot(data, aes(x=x, y=y)) + geom_point(size = 2, color = 'orange') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE),limits=c(0, 30000000))
}





histogram <- function(data,x,y,bins) {
  ggplot(data, aes(x=x)) +
    geom_histogram(aes(y=..density..),color='lightblue',bins = bins) +
    geom_density(color = 'lightgreen',size = 2) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
}





lmResidualsPlot <- function(model,residuals,fitModel) {
  plot(density(residuals))
  #qqnorm(residuals)
  #qqline(residuals)
  #autoplot(fitModel)
  par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
  plot(model)  # Plot the model information
}





plotPointsResiduals <- function(data,x,y,residuals,fitModel) {
  ggplot(data, aes(x = x, y = y)) +
    geom_segment(aes(xend = x, yend = fitModel), alpha = .2) +  # Lines to connect points
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
    geom_point() +  # Points of actual values
    geom_point(aes(y = fitModel), shape = 1) +  # Points of predicted values
    theme_bw()
}

# sqft+beds+bath+year_built+elevation


modelInteractions <- function(model) {
  anova(model)
}




lmPerformance <- function(model1,model2,targetField) {
  #coefficients(model1)
  #coefficients(model2)
  print(summary(model2))
  print(anova(model_multiplicative,model2))
  fitted_multiplicative <- stats::fitted(model2)
  mse <- mean((targetField - fitted_multiplicative)^2)
  print(mse)
}





