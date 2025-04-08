

# par(mfrow=c(4,2))
# 
# for (j in 1:8){
#   win = sample(c(-1,1), size=50, replace=TRUE)
#   plot(cumsum(win), type="l", ylim=c(-15,15))
#   abline(h=0)
# }

#par(mfrow=c(1,1))

# function for one game with n = 50 (default) trials
peter.paul <- function(n=50, r=1, c=1) {
  par(mfrow=c(r,c))
  win = sample(c(-1,1), size = n, replace = TRUE)
  #plot(cumsum(win), type="l", ylim=c(-15,15))
  #abline(h=0)
  sum(win)
}

# using replicate to run the function peter.paul() 1000 times
F = replicate(1000, peter.paul())

table(F) # using table() to summarize the data
par(mfrow=c(1,1))
plot(table(F)) # plotting a histogram of the summarized (aggregated) data

dbinom(25, size=50, prob=0.5) # probability of breaking even


# returns a vector (table) of the three values F,L,M for times 
# when cum.win > 0. How often is he winning
peter.paul.winning <- function(n = 50) {
  win = sample(c(-1,1), size = n, replace=TRUE)
  cum.win = cumsum(win)
  print(c(F=sum(win), L=sum(cum.win > 0), M=max(cum.win)))
}

#peter.paul.winning()

W <- replicate(1000, peter.paul.winning())

# how many times was he in the lead?
times.in.lead = W["L",]

# tabulate the simulated values of W using table function and prop.table()
# function will find corresponding relative frequencies
plot(prop.table(table(times.in.lead)))


# next consider his maximum winnings over 50 plays. Store the 1000 simulated
# values of M in the variable maximum.lead, then tabulate the values using table()
# and plot them
maximum.lead = W["M",]
plot(table(maximum.lead))


# To compute the approximate probability that Peter will have a maximum
# winning of 10 or more, we find the values of maximum.lead that are 10
# or greater and divide this by the number of simulation iterations
sum(maximum.lead >= 10)/1000



