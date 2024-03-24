library(dplyr)
library(ggplot2)
library(grid)
library(tidyr)

## NULL Hypothesis: current status-quo
## Alternate Hypothesis: the status that is not happening

# Z-test: observations more than 30

# t-test: observations less than 30

# For samples that assume a normal distribution: T-Test

# For samples that assume non-normal distribution: Mann-Whitney Test

# For two categorical variables: Fisher Test


cats <- MASS::cats

mean_cats_male <- mean(cats$Bwt[cats$Sex=='M'])
mean_cats_female <- mean(cats$Bwt[cats$Sex=='F'])

## Testing for cat weights between male and female

max_bw <- max(cats$Bwt)
min_bw <- min(cats$Bwt)

bin_size = max(max_bw - min_bw)/20

hist_male_bw <- cats[cats$Sex=='M',] %>% ggplot(aes(x = Bwt))+ # creating a histogram of male cat weights
  geom_histogram(bins = 20, binwidth = bin_size)+ # 20 bins with binwidth of 0.095
  geom_vline(xintercept = mean_cats_male, color='red') # a vertical line at mean male cat weight

hist_female_bw <- cats[cats$Sex=='F',] %>% ggplot(aes(x = Bwt))+
  geom_histogram(bins = 20, binwidth = bin_size)+
  geom_vline(xintercept = mean_cats_female, color='red')

# Plots both bargraphs for male and female atop eachother
gridExtra::grid.arrange(hist_male_bw,hist_female_bw)

# Box plots of the cats weights
cats %>% ggplot(aes(x = Sex, y = Bwt)) + geom_boxplot()

# Bwt vs Sex, from cats data. P value is very small, reject NULL hypothesis
# that mean and variance are the same
cat_test <- t.test(Bwt~Sex,data=cats)

# Measuring the difference between the two means mean in group M 0.5404255
dif <- cat_test$estimate[2] - cat_test$estimate[1]


## Testing for differences in cat heart weights

cats %>% ggplot(aes(x = Hwt,fill = Sex)) + geom_density(alpha = 0.4)


male_hw <- cats %>% filter(Sex == 'M') %>% dplyr::select(Hwt)
female_hw <- cats %>% filter(Sex == 'F') %>% dplyr::select(Hwt)

# Reject the NULL hypothesis and accept alternate hypothesis that 
# mean heart weight of male is equal to mean heart weight of female
hw_test <- t.test(male_hw,female_hw)


## Testing for coffee ounces

# 100 random normally distributed values with a mean of 500 and SD of 22
milli_liters <- rnorm(100, mean = 500, 22)

# Very low p-value, reject NULL hypothesis, accept alternate hypothesis
test1 <- t.test(milli_liters, mu = 50)

# Neither reject nor accept the NULL hypothesis with high p-value
test2 <- t.test(milli_liters, mu = 500)


## Testing for pizza delivery times

sample <- c(35,45,55,25,45,60,20,65,35,40)

# t-test on sample. NULL hypothesis delivery = 30 minutes (mean).
# Alternative hypothesis: delivery greater than 30 minutes (true mean)
t.test(sample,mu = 30,alternative = "greater")



cats %>% ggplot(aes(x = Bwt, fill = Sex))+geom_density(alpha=0.4)

## Use Wilcox test for small or non-normal tests
wilcox.test(Bwt~Sex,data=cats)


# Chi-square test in R
smoking <- data.frame(row.names = c('Male','Female'),'smoke'= c(50,20),'not smoke'=c(100,100))
chisq.test(smoking, correct=FALSE)


# Fisher test: categorical variables in a contingency test of 2 x 2 table
MASS::survey

survey_smoking <- MASS::survey %>% dplyr::select(Sex,Smoke) %>% filter(Smoke %in% c('Regul','Never'))

count_table_Pre <- table(survey_smoking$Sex,survey_smoking$Smoke)

count_table <- count_table_Pre[,c('Never','Regul')]

# NULL hypothesis: males are more likely to be regular smokers than females
# Alternate hypothesis: females are more likely to not be regular smokers
# Odds ratio of many trials that male will be smoking more than female divided by the odds male will be smoking

fisher.test(count_table)

# Fisher exact test more accurate than Chi-squared on small samples
# Chi-squared test more accurate than Fisher exact test on large samples

# creating a dataframe from the seatbelts data
seatbelts_df <- data.frame(datasets::Seatbelts)

med <- median(seatbelts_df$drivers)
summary(seatbelts_df$drivers)

# Creating a feature in the data set with values of high or low depending on drivers value above or below median (med)
seatbelts_df<- mutate(seatbelts_df,casualities = case_when(drivers > med ~ 'High',
                                                          drivers <= med ~ 'Low'))

law_test <- table(seatbelts_df$law,seatbelts_df$casualities)
# NULL hypothesis that means are the same and there is no association between law passing and law not passing with casualties magnitude
fisher.test(law_test)
# : p-value = 2.383e-05
# P- value is low, reject NULL hypothesis, there is an association between passing the law and lower casualties
# odds ratio 13.01901: There are 13 times more likely to be casualties if law is not passed 

# Using a t-test:
t.test(drivers~law, data = seatbelts_df)


## Hypothesis testing for binomial distribution:

# number of trials / number of heads * number of tails
factorial(20)/(factorial(10)*factorial(10))*((0.5^10) * (0.5^10))

dbinom(10,20,0.5) # probability of having 10 heads in 20 trials
pbinom(10,20,0.5) # probability of having 10 or less heads in 20 trials

# NULL hypothesis: coin is fair
# Alternate hypothesis: coin is not fair
# In 20 trails, got 4 heads:
pbinom(4,20,0.5)
# less than 1 % chance of 4 heads in 20 trials, less than the 0.5% chance
# Reject NULL hypothesis, accept Alternate hypothesis than coin is not fair


## Democratic party senators: usually are 70% vote for green bill
# Survey sample of 20: 5 yes, 15 no. What is chance that 4 people or less will vote yes for bill out of 20
pbinom(4,20,0.7)



## Bayesian Inference:

prob <- seq(0,1,length.out = 20)
prior <- dbinom(10,20,prob=prob)
plot(1:20,prior,xlab='trials',ylab='probability of heads',type='l',ylim = c(0,0.22),col='blue')

# the distribution of the 20 coin flips with 4 heads
data_estimate <- dbinom(4,20,prob = prob)
lines(1:20,data_estimate,col='red')

# calculating the distribution of the true coin. Weighted distribution of the fair coin and unfair coin
posterior <- (prior + data_estimate)/sum(prior+data_estimate)
lines(1:20,posterior,col='green')






