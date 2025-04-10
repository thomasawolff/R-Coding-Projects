
library(flexplot)
library(lme4)

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

