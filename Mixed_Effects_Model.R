
library(flexplot)
library(lme4)
library(nlme)

data(alcuse)

# These factors are Beta0j for Random Effects ANOVA
# ~1 is how we tell R that we are fitting an intercept (fixed effects: Gamma-00)
# (1|ID) (Random Effects: uij): # Random Slope Model
# No predictors, modelling the differences in the means
mod <- lmer(ALCUSE~1 + (1|ID), data=alcuse) 
visualize(mod, plot="model")


# Random Intercepts Model with slopes fixed (AGE_14 parameter)
# Added the predictor AGE_14 for random intercepts
# From summary: 
    # Random Effects:
      # Variance:
        # 0.5966 is the variability from fixed mean for each group.
        # 0.4915 is the how much each group differs from the average effect.
    # Fixed Effects:
      # Estimate:
        # 0.65130: Average number of drinks across all groups (clusters)
        # 0.27065: Each year they got older they increased drinks consumed.
rand.intercept <- lmer(ALCUSE~1 + AGE_14 + (1|ID), data=alcuse)
summary(rand.intercept)
visualize(rand.intercept, plot="model")


# Random Slope/Intercept model
rand.slope.int = lmer(ALCUSE~1 + AGE_14 + (AGE_14|ID), data=alcuse)
summary(rand.slope.int)
visualize(rand.slope.int, plot="model")



# Load dataset
data(Arabidopsis)
dim(Arabidopsis)

attach(Arabidopsis)

# Overview of the variables
par(mfrow = c(2,4))
barplot(table(reg), ylab = "Frequency", main = "Region")
barplot(table(popu), ylab = "Frequency", main = "Population")
barplot(table(gen), ylab = "Frequency", las = 2, main = "Genotype")
barplot(table(rack), ylab = "Frequency", main = "Rack")
barplot(table(nutrient), ylab = "Frequency", main = "Nutrient")
barplot(table(amd), ylab = "Frequency", main = "AMD")
barplot(table(status), ylab = "Frequency", main = "Status")
hist(total.fruits, col = "grey", main = "Total fruits", xlab = NULL)

# Transform the three factor variables gen, rock, and nutrient
Arabidopsis[,c("gen","rack","nutrient")] <- lapply(Arabidopsis[,c("gen","rack","nutrient")], factor)
str(Arabidopsis)
# Re-attach after correction, ignore warnings
attach(Arabidopsis)
# Add 1 to total fruits, otherwise log of 0 will prompt error
total.fruits <- log(1 + total.fruits)

table(gen, popu)

any(is.na(Arabidopsis))

# Bad fit for a predictive model
LM <- lm(total.fruits ~ rack + nutrient + amd + status)
summary(LM)
par(mfrow = c(2,2))
plot(LM)

# Generalized linear model
GLM <- gls(total.fruits ~ rack + nutrient + amd + status, method = "ML")
summary(GLM)



lmm1 <- lme(total.fruits ~ rack + nutrient + amd + status, random = ~1|reg, method = "ML")

lmm2 <- lme(total.fruits ~ rack + nutrient + amd + status, random = ~1|popu, method = "ML")

lmm3 <- lme(total.fruits ~ rack + nutrient + amd + status, random = ~1|gen, method = "ML")

lmm4 <- lme(total.fruits ~ rack + nutrient + amd + status, random = ~1|reg/popu, method = "ML")

lmm5 <- lme(total.fruits ~ rack + nutrient + amd + status, random = ~1|reg/gen, method = "ML")

lmm6 <- lme(total.fruits ~ rack + nutrient + amd + status, random = ~1|popu/gen, method = "ML")

lmm7 <- lme(total.fruits ~ rack + nutrient + amd + status, random = ~1|reg/popu/gen, method = "ML")

anova(GLM, lmm1, lmm2, lmm3, lmm4, lmm5, lmm6, lmm7)


ctrl <- lmeControl(opt="optim")

lmm6.2 <- update(lmm6, .~., random = ~nutrient|popu/gen, control = ctrl)

lmm7.2 <- update(lmm7, .~., random = ~nutrient|reg/popu/gen, control = ctrl)

anova(lmm6, lmm6.2, lmm7, lmm7.2)
anova(lmm6.2, lmm7.2)

summary(lmm6.2)

# QQ Plots
par(mfrow = c(1,2))
lims <- c(-3.5,3.5)
qqnorm(resid(GLM, type = "normalized"),xlim = lims, ylim = lims, main = "GLM")
abline(0,1, col = "red", lty =2)
qqnorm(resid(lmm6.2, type = "normalized"),xlim = lims, ylims = lims, main = "lmm6.2")
abline(0,1, col = "red", lty  =2)

summary(lmm6.2)


lmm8 <- update(lmm6.2, .~. + nutrient:amd)
summary(lmm8)
anova(lmm8, lmm6.2)













