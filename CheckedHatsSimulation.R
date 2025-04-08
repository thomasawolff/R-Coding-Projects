
# If there are 10 men, how many will correctly recieve their hats

# Using sample() to simulate the experiment

# n = 10 men, store their hats in vector hats, use sample() to permute
# randomly, store in vector mixed.hats

n = 10
hats = 1:n

mixed.hats = sample(hats)

# summing the total number of hats that matched an index matching between the 2 sets
correct <- sum(hats == mixed.hats)


scramble.hats <- function(n) {
  
  hatList <- list()
  
  for (x in 1:2) {
    for (i in x:n) {
      hatList[[length(hatList) + 1]] <- paste0(x,'-',i)
    }
  }
  hats <- noquote(unlist(hatList))
  mixed.hats <- sample(hats)
  return(hats == mixed.hats)
}



# running each trial of 10, 1000 times
matches <- replicate(1000, scramble.hats(10))

# using table to aggregate the data
plot(table(matches)/1000)

# function for propertion no matches
prop.no.matches <- function(n) {
  matches <- replicate(1000, scramble.hats(n))
  sum(matches == 0)/1000
}

prop.no.matches(20)

# we increased the number of hats to 30, but didnt get more matches than we had with 10
matches <- replicate(1000, scramble.hats(30))

# probability of no matches from 2 to 20 hats
many.probs = sapply(2:20, prop.no.matches)

# plotting the probabilities for no matches many.probs
plot(2:20, many.probs, xlab = "Number of Men", ylab="Prob(no matches", abline(h=0.38))




#####################
# Baseball cards #
#####################

cards = c("Mantle","Aaron","Gehrig","Ruth","Schmidt","Mays","Cobb","DiMaggio","Williams","Foxx")


## Simulate the experiment using sample() function

# Assume each purchased card is equally likely to turn up one of the ten players above
# Assuming independent draws, a purchase of 20 cards can be considered a random sample
# taken with replacement from this vector

samp.cards <- sample(cards, size=20, replace=TRUE)
samp.cards

# We have duplicates, but did we get the full set?

# unique values
unique(samp.cards)


# Lets say that a complete set consists of 586 cards, and we purchased 3000 cards. 
# What would be the chance that we get a complete set?

collector <- function(n,m){
  samp.cards <- sample(1:n, size=m, replace=TRUE)
  ifelse(length(unique(samp.cards)) == n, "Yes","No")
}

collector(586, 3000)

table(replicate(100, collector(586, 3000)))



## Buying an Optimal Number of Cards

# We try to make this more interesting.
# What if the person collecting has a strategy of buying
# small sets to improve his chances of getting a complete
# set? Further, let's say packs of cards cost 5 cents each
# but can also buy individual cards from a deal for 25 cents.

# So the card collector will "fill in the holes" at 25 cents
# a pop.

# On average, what is the cost of this plan? Is there an 
# optimal number of cards purchased that will minimize the
# expected cost?

# We write function collect2() to simulate. Is one
# argument, n.purchased, number of cards purchased. We
# assume cost of a "random" card is cost.rcard = 0.05
# and cost of card from dealer is cost.ncard = 0.25.

# We sample n.purchased cards with replacement from the
# complete set represented by integers 1 thru 586.

# We compute number of random unique cards we collect n.cards
# and number we haven't collected n.missed which we buy
# from dealer.

# Therefore, the random total cost will be:

# COST = cost.rcard x n.purchased+cost.ncard x n.missed


collect2 <- function(n.purchased){
  cost.rcard = 0.05
  cost.ncard = 0.25
  
  samp.cards = sample(1:586, size=n.purchased, replace=TRUE)
  n.cards = length(unique(samp.cards))
  n.missed = 586 - n.cards
  return (n.purchased * cost.rcard + n.missed * cost.ncard)
}

# If I decide to buy 800 cards, whats the expected cost?

# running function 500 times with 800 cards bought
costs = replicate(500, collect2(800))

# Summary shows the distribution of random costs
summary(costs)


# So we write new function expected.cost() that takes 100
# samples of cards, each of size n.purchased and computes
# the average total cost. Note that replicate() does the
# repeated simulations and mean() finds expected cost 
# from vector of total costs.

expected.cost = function(n.purchased){
  mean(replicate(100, collect2(n.purchased)))
}

# Since we want to see how total cost varies as 
# a function of number purchased, we define a vector N as 
# numbers of cards to buy to from 500 to 1500
N = 500:1500

# sapply() operates expected.cost() against N
# we plot expected cost against number of cards
# purchased. Grid is overlaid to help locate minimum
ECOST = sapply(N, expected.cost)
plot(N, ECOST, xlab="Cards Purchased",
     ylab="Expected Cost in Dollars")
grid(col="black")

df <- data.frame(N,ECOST)
optimal <- df[df['ECOST'] == min(df['ECOST'])]
abline(v=optimal[1], col="red", lwd=3)
abline(h=optimal[2], col="green",lwd=3)



## Patterns of Dependence in a Sequence

# Sports fans love athletes who exhibit patterns of extreme
# performance. Records are kept of extreme winning or losing
# patterns, such a getting a base hit in baseball....
# ... a "hitting streak"

# In the 2006 baseball season, Chase Utley of the Philadelphia
# Phillies had a hitting streak of 35 games, one of the best
# in baseball history.

# But how "significant" was this streak. Utley was a good
# hitter anyway, so it might be expected.

# As another example, long runs of heads and tails can be
# observed flipping a coin.

# We investigate with Monte Carlo simulation:

## Writing a Function to Compute Streaks

# We represent hitting success or failure with 0's and 1's

y = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1)

# What are the lengths of the hitting streaks?

# Add 0's to front and end to bracket it
# 'where' is vector of TRUE's (if 0) and FALSE's (if 1)
# where = (c(0, y, 0) == 0)
# n = length(y)
# 
# # loc.zeros records locations in sequence where 0's occur
# loc.zeros = (0:(n+1))[where]
# loc.zeros
# 
# # compute length of streaks
# streak.lengths = diff(loc.zeros) - 1
# 
# # we remove the zeros
# streak.lengths = streak.lengths[streak.lengths > 0]
# streak.lengths

# Write function longest.streak by taking the max() of the above vector

longest.streak = function(y){
  where = c(0, y, 0) == 0
  n = length(y)
  loc.zeros = (0:(n+1))[where]
  streak.lengths = diff(loc.zeros)
  streak.lengths = streak.lengths[streak.lengths > 0]
  return(max(streak.lengths))
}

setwd("C:/mdtapps/GitRepositories/R-Coding-Projects")

dat <- read.table("utley2006.txt", header=TRUE, sep="\t")
utley <- as.numeric(dat$H > 0)

longest.streak(utley)


switches <- function(x) {
  switchList = list()
  x <- as.integer((c(0, x, 0) == 0))
  for (i in 1:(length(x)-1)){
    if (x[i] != x[i+1]){
      switchList[i] <- noquote(paste(x[i],x[i+1]))
    }
  }
  return(length(unlist(noquote(switchList))))
}

switches(utley)

# So Utley's longest streak in 2006 was 35 games.

## Writing a Function to Simulate Hitting Data

# We apply Monte Carlo to understand "significance"
# Utley played in 160 games and hit in 116 of them

# What if 116 "hit" games were randomly distributed?
# The what would be length of longest sequence?

# write function random.streak() with binary sequence y as argument
random.streak = function(y) {
  # first, randomly permute y and store in mixed.up.y
  mixed.up.y = sample(y)
  # then find longest streak of 1's in vector mixed.up.y
  return(switches(mixed.up.y))
}

longest.streak(mixed.up.y)

# replicate random.streak 100000 times, store in L
L = replicate(10000, random.streak(utley))
# tabulate values in L and plot
hist(L)
# superimpose line for 2006 season
abline(v=62, lwd=3, col="blue")

text(62,"Utley")












