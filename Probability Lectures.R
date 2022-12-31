## Probability

## Tosses of a coin where sum of two dice values is > num

dice1 <- 1:6
dice2 <- 1:6
dice3 <- 1:6

tosses <- expand.grid(dice1,dice2)

X <- tosses$Var1 + tosses$Var2

sum(X > 7)/length(X)

sum(X %% 2 != 0)/length(X)


tosses_3 <- expand.grid(dice1,dice2,dice3)

X <- apply(tosses_3, 1, sum)

sum(X > 14)/length(X)

hist(X)


dice1 <- 1:6
dice2 <- 1:6
dice3 <- 1:6
dice4 <- 1:6

tosses_4 <- expand.grid(dice1,dice2,dice3,dice4)
X <- apply(tosses_4, 1, sum)

sum(X < 19)/length(X) # == .902 or 90%
hist(X)

sum(X %% 2 == 0)/length(X) # == .464 or 46%
hist(X)

sum(X < 19 & X %% 2 == 0)/length(X) # == .464 or 46%
hist(X)


## Binomial distribution
# trials
n = 9
X = 6

fact_n = factorial(n)
fact_X = factorial(X)
fact_notX = factorial(n - X)
all_possible_outcomes = fact_n/(fact_notX * fact_X)

p_6_women = 0.8^6
p_3_men = (1-0.8)^3
final = all_possible_outcomes*p_6_women*p_3_men

  
  
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

prob_9_heads = all_possible_outcomes*p_heads*p_tails

binomialDistribution(10,45,0.55)


dbinom(9, size=20, prob=0.5) # probability of 9 heads on 20 flips
pbinom(9, size=20, prob=0.5) # probability of 9 or fewer heads on 20 flips


pbinom(10, size=45, prob=0.55)


prob_vector = c()
for(X in 0:10) {
  n= 45
  x = X
  p = 0.55
  fact_n = factorial(n)
  fact_x = factorial(x)
  fact_notx = factorial(n - x)
  
  all_possible_outcomes = fact_n/(fact_notx*fact_x)
  
  p_outcome1 = p^x
  p_outcome2 = p^(n-x)
  
  prob__heads = all_possible_outcomes*p_outcome1*p_outcome2
  
  prob_vector[x] = prob__heads
}

sum(prob_vector)


## Poisson distribution

lambda = 2.5
k = 5
prob_5_goals = lambda^k *exp(-lambda)/factorial(k) 

dpois(5,2.5)*100

k = c(0:10)
prob = c()

for (i in 0:10) {
  prob[i+1] = dpois(i,lambda=lambda)
}

sum(prob)
plot(k,prob)
  
  
  
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

