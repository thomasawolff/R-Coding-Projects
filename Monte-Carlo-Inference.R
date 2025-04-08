
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










