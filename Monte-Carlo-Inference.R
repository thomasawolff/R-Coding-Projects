
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


### Estimate MSE of trimmed median

# actually, median is a trimmed mean
# So here we repeat for the median

n <- 20
m = 1000
tmedian <- numeric(m)
for (i in 1:m) {
  x <- sort(rnorm(n))
  tmedian[i] <- median(x)
}
# calculating mean squared error
mse <- mean(tmedian^2)
mse
# calculating standard error
sqrt(sum((tmedian - mean(tmedian))^2))/m

#### ESTIMATING A CONFIDENCE LEVEL


# Often need evaluate cdf of sampling distribution
# of a statistic, when density function is unknown.

# For example, often assume that sampled population
# is normally distributed. If population non-normal,
# true distribution of the estimator may be unknown.

# This is a problem of integration, when you can
# generate the distribution g(X) but the true
# function g(x) is unknown.

### Example of Confidence interval for variance

# Consider confidence interval for variance, is
# sensitive to mild departures from normality. Can
# use MC methods to estimate true confidence level
# when normal theory confidence interval for variance
# is applied to non-normal data.

# Here we calculate 95% Upper Confidence Limit (UCL)
# for random sample size n=20 from a n(0, var=4)
# distribution (sigma-squared is 4 in this case)

n <- 20 # sample size
alpha <- .05 # alpha for 95 UCL
x <- rnorm(n, mean=0, sd=2) # normally distributed data with mean 0 and sd 2
# qchisq is quantile function for chi-sq dist w / df n -1
(UCL <- (n-1) * var(x) / qchisq(alpha, df=n-1))


# Do it several times, Upper Confidence Limit
# (UCL) all contain sigma-squared = 4

# If we do this large number of times, approximately
# 95% of the intervals should contain sigma-squared
# (=4) assuming sampled population is normal with a
# variance sigma-squared.

### Example of MC estimate of confidence level

# Simulation experiment: Repeat large number of times,
# compute the proportion of intervals that contain
# the target parameter

n <- 20
alpha <- .05
UCL <- replicate(1000, expr = {
  x <- rnorm(n, mean = 0, sd = 2)
  (n-1) * var(x) / qchisq(alpha, df = n - 1)
})

# count number of intervals that contain sigma^2=4
sum(UCL > 4)

mean(UCL > 4)

# calculating the Confidence Interval in a function
calcCI <- function(n, alpha0) {
  y <- rnorm(n, mean = 0, sd = 2)
  return((n-1) * var(y) / qchisq(alpha, df = n-1))
}

UCL <- replicate(1000, expr = calcCI(n=20, alpha = 0.05))

sum(UCL > 4)


### Example of Empirical confidence level

# What happens is sampled population is non-normal?
# Suppose have sampled pop of chi-Sq(2) which has a
# variance of 4, but is distinctly non-normal.

# We simply repeat the simulation, replacing N(0,4)
# samples with Chi-Sq(2) samples

UCL <- replicate(1000, expr = {
  x <- rchisq(n, df = 2) # did not use rnorm to sample
  (n-1) * var(x) / qchisq(alpha, df = n-1)
})

sum(UCL > 4)


# In this experiment, only 794 of the intervals contained
# the population variance, which is far less than the 95%
# coverage under normality.

## NOTE: all examples above are 'parametric' in sense that
## distribution of sampled population is specified. MC
## approach here is called 'parametric bootstrap'

## Is in contrast to the 'ordinary bootstrap' where samples
## are generated from observed samples where distribution 
## is NOT specified, is sometimes called 'non-parametric'
## and includes bootstrapping and jackknifing, which can
## also be used to estimate the bias and the standard
## error of an estimate.

## MORE ON SIMULATING SAMPLING DISTRIBUTION WITH MC
# Suppose observe a random sample y1,...,yn
# from exponential density function with unknown
# location parameter theta and you only observe
# sample median M

# simulate the median for exponential function
sim.median <- function(n) {
  return(median(rexp(n)))
}

# replicate 10000 times for a sample size of 21
M <- replicate(10000, sim.median(21))

# draw histogram, use PROB=TRUE so vertical
# axis is probability density function
hist(M, prob=TRUE, main="")

# to confirm simulated draws are from
# sampling density of M, we define samp.med()
# to compute exact density
samp.med <- function(m, n){
  con <- factorial(n) / factorial((n-1)/2)^2
  con * pexp(m)^((n - 1) / 2) * (1 - pexp(m))^((n - 1) / 2) *  dexp(m)
}


# then use curve to overlay exact density on 
# histogram of simulated sample, is a good fit
curve(samp.med(x,21), add=TRUE, col="red")


## CONSTRUCTING A PERCENTILE CONF INTERVAL
# Suppose observe a random sample y1,...,yn
# from exponential density function with unknown
# location parameter theta and you only observe
# sample median M

# compute quantile for 90% CI
# in between %5 and 95%
q <- quantile(M, c(0.05, 0.95))

#         5%       95% 
#    0.3972995 1.1106747 

# So 90% CI is (M-1.1062898,M-0.4011316)

# to illustrate using this CI, suppose we collect
# the following 31 exponential observations 
# stored in vector y (data is collected from
# exponential distribution with theta = 4.5)
y = c(6.11, 5.04, 4.83, 4.89, 4.97, 5.15, 7.98, 4.62,
      6.19, 4.58, 6.34, 4.54, 6.37, 5.64, 4.53, 4.68,
      5.17, 4.72, 5.06, 4.96, 4.70)

median(y)

# width of CI
c(median(y) - q[2], median(y) - q[1])
#      95%       5% 
#   3.859325 4.572700


# actual value of theta is 4.5. If we repeated
# this procedure a large number of times, we
# would find that approximately 90% of the
# intervals would cover theta


## COMPARING ESTIMATORS: THE TAXI PROBLEM
# a person is wandering the streets of a city
# and notices the following numbers of 5 taxis
# that pass by: 34,100,65,81,120

# Can she make an intelligent guess at the number
# of taxis in the city?: Is a problem of statistical
# inference where population is collection of
# taxis driven in city and one wishes to know
# unknown number of taxis N

# Assume taxis are numbered 1 to N, each equally
# likely to be observed

# Consider two possible estimates: (1) the 
# largest taxi number observed; and (2) twice the
# sample mean. 

# Which is a better estimate of the number of
# taxis N?

# We compare these two estimators using a sim

# Simulate taxi numbers from a uniform dist
# with a known number of taxis N and compute
# the two estimates. Repeat many times and
# obtain two empirical sampling distributions.

# Then can compare the two estimators by 
# examining various properties of their respective
# sampling distributions

# taxi() function implements a single simulation.
# Two arguments: actual number of taxis N and
# sample size n. sample() function simulates 
# observed taxi numbers and values of the two
# estimates are stored in variables estimate1
# and estimate2.


taxi <- function(N, n) {
  y <- sample(N, size=n, replace=TRUE)
  estimate1 <- max(y)
  estimate2 <- 2*mean(y)
  c(estimate1=estimate1, estimate2=estimate2)
}

# Lets say the actual number of taxis in the city is 100
# and we observe numbers of n=5 taxis.
taxi(100, 5)


# Lets simulate sampling process 1000 times
EST <- replicate(1000, taxi(100, 5))
str(EST)

# Estimate Bias

# Want unbiased average value of estimator as value of estimator

# We know number of taxis to be 100 and know standard error

# Here we estimate their bias and standard errors
c(mean(EST["estimate1",]) -100, sd(EST["estimate1",])/sqrt(1000))
# [1] -16.5110000   0.4567916
c(mean(EST["estimate2",]) -100, sd(EST["estimate2",])/sqrt(1000))
# [1] 0.5148000 0.8438006


## Estimating mean distance from target
# compute mean absolute error and draw boxplot
absolute.error <- abs(EST - 100)
boxplot(t(absolute.error))








