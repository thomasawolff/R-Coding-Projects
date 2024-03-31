

library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)

setwd('C:/Users/moose_m7y2ik3/Downloads')
housingPre <- read_csv('housing.csv', quote = ',')

names(housingPre)[c(1,8)]<-c('sf','elevation')

housingPre$sf<- str_remove(housing$sf,'\"')

housing <- housingPre[,c('sf','beds','bath','price','year_built','sqft','price_per_sqft','elevation')]

housing$elevation <- str_remove(housing$elevation,'\"')

housing$beds <- factor(housing$beds, ordered=TRUE)

housing$bath <- factor(round(housing$bath,digits= 0), ordered=TRUE)

housing$elevation <- as.numeric(housing$elevation)

summary(housing)

all <- housing %>% ggplot(aes(x = sqft,y = price_per_sqft))+
  geom_point()+geom_smooth(method='lm')+ggtitle("All data")

sf <- housing %>% filter(sf == 1) %>% ggplot(aes(x = sqft,y = price_per_sqft))+
  geom_point()+geom_smooth(method='lm')+ggtitle("In SF")

ny <- housing %>% filter(sf == 0) %>% ggplot(aes(x = sqft,y = price_per_sqft))+
  geom_point()+geom_smooth(method='lm')+ggtitle("In NY")

grid.arrange(all,sf,ny)

boxplot(price_per_sqft ~ sf, data = housing)
