## Probability

## Tosses of a coin where sum of two dice values is > num

dice1 <- 1:6
dice2 <- 1:6
dice3 <- 1:6

# Combination of all possible outcomes between both dice
# tosses is our sample space
tosses <- expand.grid(dice1,dice2)

# Combining probabilities of dice 1 and dice 2
X <- tosses$Var1 + tosses$Var2

# Find probabilities that X greater than 7
# Takes total events > 7 = True divided by sample space length(X)
sum(X > 7)/length(X)

# Find probability that X is odd (not divisible by 2)
sum(X %% 2 != 0)/length(X)

# Combination of all possible outcomes between all dice
tosses_3 <- expand.grid(dice1,dice2,dice3)

# Getting sum of every possible toss. By row.
X <- apply(tosses_3, 1, sum)

# Probability that X will be greater than 14
sum(X > 14)/length(X)

hist(X)


dice1 <- 1:6
dice2 <- 1:6
dice3 <- 1:6
dice4 <- 1:6

tosses_4 <- expand.grid(dice1,dice2,dice3,dice4)
X <- apply(tosses_4, 1, sum)

sum(X < 12)/length(X) # == .902 or 90%
hist(X)

sum(X %% 2 == 0)/length(X) # == .464 or 46%
hist(X)

sum(X < 12 & X %% 2 == 0)/length(X) # == .464 or 46%
hist(X)



## Binomial distribution
# Determines the probability of observing a specified number of successful outcomes in specified number of trials
# Each trial has the same probability
# trials
n = 9
X = 6


fact_n = factorial(n)
fact_X = factorial(X)
fact_notX = factorial(n - X)
all_possible_outcomes = fact_n/(fact_notX * fact_X) # Finding all possible outcomes

p_6_women = 0.8^6
p_3_men = (1-0.8)^3 # The ^3 comes from 9 - 6 = 3
final = all_possible_outcomes*p_6_women*p_3_men # = 0.1761608 or 17%

  
  
# Flip a coin 20 times, what is probability that 9 of them will be heads. Prob of head = 50%

n = 20
x = 9
p = 0.5

fact_n = factorial(n)
fact_x = factorial(x)
fact_notx = factorial(n - x)
all_possible_outcomes = fact_n/(fact_notx*fact_x)

p_heads = p^x
p_tails = p^(n-x)

prob_9_heads = all_possible_outcomes*p_heads*p_tails # = 0.1601791 or 16%

binomialDistribution(10,45,0.55)


dbinom(9, size=20, prob=0.5) # probability of 9 heads on 20 flips
pbinom(9, size=20, prob=0.5) # probability of 9 or fewer heads on 20 flips


pbinom(10, size=45, prob=0.55)


# for looping binomial distribution
prob_vector = c() # Empty vector to store values
for(X in 0:19) {
  n= 20
  x = X
  p = 0.5
  fact_n = factorial(n)
  fact_x = factorial(x)
  fact_notx = factorial(n - x)
  
  all_possible_outcomes = fact_n/(fact_notx*fact_x)
  
  p_outcome1 = p^x
  p_outcome2 = p^(n-x)
  
  prob__heads = all_possible_outcomes*p_outcome1*p_outcome2
  
  prob_vector[X] = prob__heads
}

sum(prob_vector)


## Poisson distribution
# Expresses the probability of a given number of events occurring in a fixed interval of time
# or space if these events occur with a known constant mean rate and independently of the time
# since the last event.
lambda = 2.5 # Average number of goals per World Cup game.
k = 5 # Target value of 5
# exp is Eulers number
prob_5_goals = lambda^k *exp(-lambda)/factorial(k) 

dpois(5,2.5)*100 # gives the Poisson probability

k = c(0:10) # Creating vector of possible goals scored
prob = c() # Empty vector of probabilities

for (i in 0:10) { # looping over probabilities of i number of goals
  prob[i+1] = dpois(i,lambda=lambda)
}

plot(k, prob,
     type = "b", pch = 18, col = 4,
     xlab = "Goals Scored", ylab = "Probability")
  
  

# Normal Distributions example
hist(rnorm(100000,20,2)) # 300 runs, average of 20, SD of 2. Random data

# What is probability that John (avid runner) will take more than 25 minutes?
probability_lessThan25 <- pnorm(25,20,2)
probability_moreThan25 <- 1 - probability_lessThan25
probability_moreThan25*100

# What is probability that John (avid runner) will take less than 17 minutes?
probability_lessThan17 <- pnorm(17,20,2)
probability_lessThan17*100

# What is probability that John (avid runner) will take between 18 and 23 minutes
probability_lessThan18 <- pnorm(18,20,2)
probability_lessThan23 <- pnorm(23,20,2)
probability_between18and23 = (probability_lessThan23 - probability_lessThan18)*100 # = 77.45375



## Uniform distributions
uniform <- runif(100000,2,9) # creates Uniform distribution of data
hist(uniform)
spread <- max(uniform) - min(uniform)
# Probability than any number chosen is less than 4
less_than_4 <- ((4 - min(uniform))/spread)*100 # 28.6% chance


  
## relative risk

setwd('C:/Users/moose_m7y2ik3/Google Drive/R-Course-HTML-Notes')

library(readr)

tiktok <- read_csv('categorical.csv')

prop_table <- prop.table(table(tiktok$group,tiktok$tiktok),margin = 1)

rr <- prop_table[2,2]/prop_table[1,2]



## Correlations

cars <- read_csv('cars.csv')
colnames(cars)

correlation <- cor(cars[,c(13,14)])

sum(is.na(cars) == TRUE)

sum(is.na(cars$city_miles_per_galloon)==TRUE)

sum(is.na(cars$horsepower)==TRUE)

## remove NA's

cars_clean <- na.omit(cars)

nrow(cars_clean)

# Find rows with missing data
na_rows_city <- which(is.na(cars$city_miles_per_galloon)==TRUE)

# Correlation matrix
cor_matrix <- cor(cars_clean[,c(9,13,14,16,19)])

library(corrplot)

corrplot(cor_matrix,method = "circle")

