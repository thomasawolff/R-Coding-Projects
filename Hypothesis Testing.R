library(dplyr)
library(ggplot2)
library(grid)
library(tidyr)

cats <- MASS::cats

mean_cats_male <- mean(cats$Bwt[cats$Sex=='M'])
mean_cats_female <- mean(cats$Bwt[cats$Sex=='F'])

## Testing for cat weights between male and female

max_bw <- max(cats$Bwt)
min_bw <- min(cats$Bwt)

bin_size = max(max_bw - min_bw)/20

hist_male_bw <- cats[cats$Sex=='M',] %>% ggplot(aes(x = Bwt))+geom_histogram(bins = 20, binwidth = 0.095)+geom_vline(xintercept = mean_cats_male, color='red')

hist_female_bw <- cats[cats$Sex=='F',] %>% ggplot(aes(x = Bwt))+geom_histogram(bins = 20, binwidth = 0.095)+geom_vline(xintercept = mean_cats_male, color='red')


gridExtra::grid.arrange(hist_male_bw,hist_female_bw)


cats %>% ggplot(aes(x = Sex, y = Bwt)) + geom_boxplot()

t.test(Bwt~Sex,data=cats)


## Testing for differences in cat weights

cats %>% ggplot(aes(x = Hwt,fill = Sex)) + geom_density(alpha = 0.4)


male_hw <- cats %>% filter(Sex == 'M') %>% dplyr::select(Hwt)
female_hw <- cats %>% filter(Sex == 'F') %>% dplyr::select(Hwt)

hw_test <- t.test(male_hw,female_hw)


## Testing for coffee ounces

milli_liters <- rnorm(100, mean = 500, 22)

test1 <- t.test(milli_liters, mu = 50)

test2 <- t.test(milli_liters, mu = 500)


## Testing for pizza delivery times

sample <- c(35,45,55,25,45,60,20,65,35,40)

t.test(sample,mu = 30,alternative = "greater")


cats %>% ggplot(aes(x = Bwt, fill = Sex))+geom_density(alpha=0.4)

## Use Wilcox test for small or non-normal tests
wilcox.test(Bwt~Sex,data=cats)
