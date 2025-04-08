
###############################################
###   MONTE CARLO METHODS IN INFERENCE      ###
###############################################

# Monte Carlo Methods may refer to any method in
# statistical inference or numerical analysis 
# where simulation is used.

# Here we just look at inference.
# Can use MC to estimate parameters of sampling
# distribution of a statistic, mean squared 
# error (MSE), percentiles, or other measures 
# of interest.

# In statistical inference there is uncertainty
# in an estimate. The methods we are going to 
# look at now use repeated sampling from a 
# given probability model, known as parametric 
# bootstrap, to investigate.

# We simulate the stochastic process that 
# generated the data, repeatedly drawing 
# samples under identical conditions.

# Other MC methods known as nonparametric 
# use repeated sampling from an observed sample.

### MONTE CARLO METHODS FOR ESTIMATION

## Let's begin with simply estimating a 
## probability. Sometimes this is referred 
## to as computing an expectation of a 
## random variable.

## If you have a random variable X with 
## a density function f(x) and we want to 
## compute the expectation of a function h(x)
## which models the probability of f(X),
## (or the area under the curve of f(x)), 
## then h(x) can be expressed as the integral
## of f(x).

## Sam and Annie from 'Sleepless in Seattle'

## Let A and S represent Sam's and Annie's 
## arrival times at the Empire State Building, 
## where we measure the arrival time as the
## number of hours after noon.

## We assume A and S are independent and 
## uniformly distributed and that Annie 
## arrives somewhere between 10:30 and midnight
## and Sam arrives somewhere between 10:00 and
## 11:30PM.

## Our Questions are:
# 1) What is the probability that Annie 
#    arrives before Sam?
# 2) What is expected difference in arrival 
#    times?

## FIRST QUESTION
# We simulate a large number of values from 
# distribution of (A,S) say, 1000,
# where A and S are independent:


sam <- runif(1000, 10, 11.5) # creating IID uniform distribution with runif
annie <- runif(1000, 10.5, 12)


# We want the probability P(A < S) which is estimated
# by the proportion of simulated pairs (a,s) where a is
# smaller than s

prob <- sum(annie < sam) / 1000
prob

# shaded area shows a < s
plot(sam, annie)
polygon(c(10.5, 11.5, 11.5, 10.5),
        c(10.5, 10.5, 11.5, 10.5), density = 10, angle = 135)


# standard error of this estimate is
sqrt(prob * (1 - prob) / 1000)


# Annie more likely to arrive later, so we model E(A -S)
difference <- annie - sam

# Monte Carlo estimate is the mean of these differences
mc.est <- mean(difference)
mc.est

# Estimated standard error is the standard deviation of diffs
# divided by square root of simulation sample size:
se.est <- sd(difference) / sqrt(1000)
c(mc.est, se.est)


# This one is easy. We generate a large number 
# of random samples of size 2 from a standard 
# normal distribution, then compute the 
# replicate pairs' mean differences, and then
# the mean of those differences.

m <- 1000
g <- numeric(m)
for (i in 1:m) {
  x <- rnorm(2)
  g[i] <- abs(x[1] - x[2])
}

est <- mean(g)
est

### Example of Estimating the MSE of 
### a trimmed mean

# A trimmed mean can be used to estimate the 
# center of a continuous symmetric distribution 
# that is not normal

# You get a trimmed mean by averaging all but 
# the largest and smallest sample observations.

# Here the center is 0, we implement by writing 
# a for loop (could also use replicate() 
# function)

n <- 20
m <- 1000
tmean <- numeric(m)
for (i in 1:m) {
  x <- sort(rnorm(n)) # sort n is vector of 20
  tmean[i] <- sum(x[2:(n-1)]) / (n-2) # trim off first and last 
}

mse <- mean(tmean^2)
mse

# standard error of the mse estimate
sqrt(sum((tmean - mean(tmean))^2)) / m














