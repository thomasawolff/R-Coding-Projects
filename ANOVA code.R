library(ggplot2)
library(dplyr)

iris %>% ggplot(aes(x = Species,y = Petal.Width, color = Species)) + geom_point() + theme_classic()
iris %>% ggplot(aes(x = Species,y = Petal.Length, color = Species)) + geom_point() + theme_classic()

# Analysis of Variance calculation
#summary(aov(Petal.Width ~ Species, data= iris))

sepal_aov <- aov(Sepal.length ~ Species, data = iris)