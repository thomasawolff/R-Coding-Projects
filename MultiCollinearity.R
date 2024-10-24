
library(readr)
library(ggplot2)

setwd("C:/Users/u2970/Downloads")

housing <- read_csv("housing_cleaned.csv")

ggplot(data = housing, aes(x = price_per_sqft, y = sqft)) + geom_point()

ggplot(data = housing, aes(x = price, y = sqft)) + geom_point()