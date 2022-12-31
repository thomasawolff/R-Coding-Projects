

## Distributions

library(gamlss)
library(readr)
library(gamlss.dist)
library(gamlss.add)

sku_distributions <- read_csv('sku_distributions.csv')

head(sku_distributions)

summary(sku_distributions)

# turning data into vector
apple <- as.vector(sku_distributions[['apple_juice']])

grape <- as.vector(sku_distributions[['grape_juice']])

cantalop <- as.vector(sku_distributions[['cantalop_juice']])

# Chi Squared test

table(sku_distributions$cantalop_juice)

apple_dist <- fitDist(apple, k=3.84, try.gamlss = TRUE, type = "realAll", trace=FALSE)
grape_dist <- fitDist(grape, k=3.84, try.gamlss = TRUE, type = "counts", trace=FALSE)
cantalop_dist <- fitDist(cantalop, k=3.84, try.gamlss = TRUE, type = "counts", trace=FALSE)

# normally distributed apple juice
average <- mean(apple)
sd <- sd(apple)
cover_90 <- qnorm(0.9, mean = average, sd = sd)

# Poisson distributed grape juice
cover_90_grape_poisson <- qpois(0.9,mean(grape))


setwd('C:/Users/moose_m7y2ik3/Google Drive/R-Course-HTML-Notes')

pin <- read_csv('Pinapple_juice.csv')

pinapple <- as.vector(pin[["Pinapple juice"]])

pin_dist <- fitDist(pinapple, k = 3.84, try.gamlss = TRUE, type = "realAll", trace=FALSE)

x <- pin$Price
y <- pin$`Pinapple juice`

pinLm <- lm(y~x)
pinLm$coefficients

print(summary(pinLm))


