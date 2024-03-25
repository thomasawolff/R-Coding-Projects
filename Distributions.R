

## Distributions

library(gamlss)
library(readr)
library(gamlss.dist)
library(gamlss.add)
library(vcdExtra)

setwd('C:/Users/CBA463/Downloads')

sku_distributions <- read_csv('sku_distributions.csv')

head(sku_distributions)

summary(sku_distributions)

# turning data into vectorS
apple <- as.vector(sku_distributions[['apple_juice']])

grape <- as.vector(sku_distributions[['grape_juice']])

cantalop <- as.vector(sku_distributions[['cantalop_juice']])

# Chi Squared test: 
  # if p <= 0.05, then dist not normal: Alternate hypothesis
  # if p > 0.05, then dist is normal: NULL hypothesis
  # Degrees if freedom: k - p - 1, p is number of parameters estimated from sample data used to generate dist

table(sku_distributions$apple_juice)

par(mfrow=c(2,2))
hist(apple)
hist(grape)
hist(cantalop)

# This distribution may be continuous. The apple data fits the normal distribution
apple_dist <- fitDist(apple, k=3.84, try.gamlss = TRUE, type = "realAll", trace=FALSE)

# This data is discrete, so type = "counts". "Logarithmic" distribution
grape_dist <- fitDist(grape, k=3.84, try.gamlss = TRUE, type = "counts", trace=FALSE)

# This data is discrete, so type = "counts". "Double Poisson" distribution
cantalop_dist <- fitDist(cantalop, k=3.84, try.gamlss = TRUE, type = "counts", trace=FALSE)

# normally distributed apple juice
average <- mean(apple)
sd <- sd(apple)
# to be certain that you will cover 90% of demand for juices
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


################################################################################

par(mfrow=c(2,2))
set.seed(333)
breaks <- 20


normal.example = rnorm(n = 1000, mean = 16, sd = 1)
hist(normal.example, breaks = breaks)

# Beta distributions model the probability of a probability

beta.example = rbeta(n = 1000, shape1 = 2, shape2 = 8)
hist(beta.example, breaks = breaks)

gamma.example = rgamma(n = 1000, shape = 1, rate = 2)
hist(beta.example, breaks = breaks)

weibull.example = rweibull(n = 1000, shape = 1, scale = 2)
hist(weibull.example, breaks = breaks)


## fitdistrplus

x = normal.example

library(fitdistrplus)
library(logspline)

# Cullen and Frey graph
descdist(data = x, discrete =  FALSE, boot=1000)
