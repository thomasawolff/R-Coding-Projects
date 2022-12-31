

simulation_function <- function() {
  
  arrival_rate = 1/5
  customer_number = list(c(0.20,1),c(0.5,2),c(0.15,3),c(0.15,4))
  mean_spend = 20
  sd = 5
  duration = 16*60
  
  
  S = 0
  arrivals = c(0)
  ## Exponential arrival
  while(tail(arrivals,n=1)<= duration) {
    S = S + 1
    arrivals <- cumsum(rexp(S,arrival_rate)) 
  }
  
  
  # We can also do it this way:
  arrivals <- cumsum(rexp(300,1/5))
  arrivals <- arrivals[arrivals < 930]
  
  
  
  n_customers <- function(x) {
    if(x <= 0.20){
      1
    } else if(x <= 0.7) {
      2
    } else if(x <= 0.85) {
      3
    }else { 4}
    
  }
  
  # customers arriving
  customer_number <- c()
  for (i in 1:length(arrivals)) {
    customer_number[i] <- n_customers(runif(1,0,1))
  }
  
  
  # expected_revenue
  expected_revenue <- c()
  for(i in 1:length(customer_number)) {
    expected_revenue[i] <- sum(rnorm(customer_number[i],mean_spend,sd))
  }
  
  hist(expected_revenue)
  
  total_per_day = sum(expected_revenue)
  return(total_per_day)
}


revenue = c()
for (i in 1:10000) {
  revenue[i] <- simulation_function()
}



## M,G,1 problem

lambda <- 1 # average number of arrivals per minute
Mu <- 1 # average number of customers served to minute
sd <- 0.2

arrivalTime <- cumsum(rexp(400,lambda))
serviceTime <- rnorm(400,mean=Mu,sd = sd)

waiting_Time_Function <- function(arrivalTime,serviceTime) {
  waiting_time <- rep(NA,400)
  leaving_time <- rep(NA,400)
  
  waiting_time[1] <- 0
  leaving_time[1] <- arrivalTime[1] + waiting_time[1] + serviceTime[1]
  
  for (i in 2:400) {
    waiting_time[i] <- max(0,leaving_time[i - 1] - arrivalTime[i])
    leaving_time[i] <- arrivalTime[i] + waiting_time[i] + serviceTime[i]
  }
  return(mean(waiting_time))
}


mean_simulations <- rep(NA,400)
for (i in 1:400) {
  arrivalTime <- cumsum(rexp(400,lambda))
  serviceTime <- rnorm(400,mean=Mu,sd = sd)

  mean_simulations[i] <- waiting_Time_Function(arrivalTime,serviceTime)
}

mean(mean_simulations)



# Problem 2: I am the operations manager at a call center. I have been assigned
# the task of determining how many call center reps I will need on an hourly basis
# I checked the incoming calls of customers per day and I have determined
# that we receive about 40 calls per hour and a call takes about 7 minutes.
# How many call center reps are needed to have a waiting time of no more than
# 5 minutes

# This will be an M,G,X inf problem

lambda <- 40 # 40 calls per hour
Mu <- 60/7 # 7 minutes per call
threshold <- 5/60 # 8% is the goal

library(queuecomputer)

inter_arrival_time <- cumsum(rexp(400,rate = lambda))
serviceTime <- rexp(400,rate = Mu)

Model <- queue_step(inter_arrival_time,serviceTime,4)
summary(Model)


mean(print(Model$departures_df$waiting))


call_reps <- seq(1:20)
mwt <- rep(NA,20)

for (K in 1:length(call_reps)) {
  model <- queue_step(inter_arrival_time,serviceTime,K)
  mwt[K] <- mean(print(Model$departures_df$waiting))
}

mwt

call_data <- data.frame(call_reps, mwt)

library(dplyr)
library(ggplot2)

call_data %>% filter(mwt>0) %>% ggplot(aes(x=call_reps,y=mwt))+geom_line()















