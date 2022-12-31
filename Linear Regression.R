

setwd('C:/Users/moose_m7y2ik3/Google Drive/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Machine Learning with R')

df <- read.csv('student-mat.csv', sep=';')

head(df)

# Are there any NULL values
any(is.na(df))

# Check the structure of the data
str(df)

library(ggplot2)
library(ggthemes)
library(dplyr)

# Take only numeric columns
num.cols <- sapply(df,is.numeric)

# Filter
cor.data <- cor(df[,num.cols])
print(cor.data)


library(corrgram)
library(corrplot)

print(corrplot(cor.data,method='color'))

corrgram(df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)


# Histogram of G3 field in data
ggplot(df, aes(x=G3)) + geom_histogram(bins=20,alpha=0.5,fill='blue')


library(caTools)

set.seed(101)

# Split up data to training and test 
sample <- sample.split(df$G3, SplitRatio = 0.7)

# The training data
train <- subset(df,sample == TRUE)

# Test data
test <- subset(df,sample == FALSE)


# Train and build model
model <- lm(train$G3 ~ ., data = train)

summary(model)


res <- residuals(model)
res <- as.data.frame(res)

ggplot(res,aes(res)) + geom_histogram(fill='blue', alpha=0.5)


# Predictions on test set
G3.predictions <- predict(model,test)

# results as a dataframe
results <- cbind(G3.predictions, test$G3)
colnames(results) <- c('predicted','actual')
results <- as.data.frame(results)

print(head(results))

to_zero <- function(x) {
  if (x < 0) {
    return(0)
  } else {
    return(x)
  }
}

# Apply to_zero function
results$predicted <- sapply(results$predicted,to_zero)

# Mean squared error
mse <- mean((results$actual - results$predicted)^2)

# RMSE
print(mse^0.5)

# SSE 
sse <- sum((results$predicted - results$actual)^2)

# SST 
sst <- sum((mean(df$G3) - results$actual)^2)

R2 <- 1 - sse/sst
