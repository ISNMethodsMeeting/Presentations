rm(list = ls()) # Clear workspace

library(nlme) # Linear mixed effects models package
library(car) # ANOVA-style results table
#library(lme4) # Lme4 is another possible package for conducting LMEs but has slightly different syntax

# Load data (estimated amplitude of startle eye-blink response as dependent variable)
# Data is not included with the code as the dataset used during the presentation is not yet published
# But try with your own data and change the variables accordingly!
data <- read.csv("your data")

# Make independent variables into factors (categorical effects, not linear)
data$Subject <- factor(data$Subject)
data$Group <- factor(data$Group)
data$CSType <- factor(data$CSType)
data$CSComplexity <- factor(data$CSComplexity)
#data$Trial <- factor(data$Trial) # Here Trial (or time) was included as a linear effect but it could also be defined as a factor...

# Rename factor levels
# TMS group: experimental = 2, control = 1
# CS type: CS+ = 1, CS- = 0
# CS complexity: complex = 1, simple = 0
data$Group <- plyr::revalue(data$Group, c("1" = "Control", "2" = "Experimental"))
data$CSType <- plyr::revalue(data$CSType, c("0" = "CS-", "1" = "CS+"))
data$CSComplexity <- plyr::revalue(data$CSComplexity, c("0" = "Simple", "1" = "Complex"))

# Linear mixed effects model
# na.action = na.omit option allows to exclude missing (NA) values - check your data for these!
model_1 <- lme(StartleAmplitude ~ Group * Trial * CSType * CSComplexity, random = ~ 1|Subject, data = data, na.action = na.omit)
anova(model_1)

# Use restricted log-likelihood instead of maximizing log-likelihood (default) - REML for model comparison of random effects, ML if you want to look at the model estimates but not do model comparison
model_1b <- lme(StartleAmplitude ~ Group * Trial * CSType * CSComplexity, random = ~ 1|Subject, data = data, na.action = na.omit, method = "REML")
anova(model_1b)

# Increase iterations for the estimation procedure e.g. if model does not converge with the default setting
model_1c <- lme(StartleAmplitude ~ Group * Trial * CSType * CSComplexity, random = ~ 1|Subject, data = data, na.action = na.omit, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
       
# Another model with more complicated random effects structure        
model_2 <- lme(StartleAmplitude ~ Group * Trial * CSType * CSComplexity, random = ~ (CSType*Trial)|Subject, data = data, na.action = na.omit, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8), method = "REML")
# But sadly this model does not converge with the current data

# Let's try yet another model
model_3 <- lme(StartleAmplitude ~ Group * Trial * CSType * CSComplexity, random = ~ Trial|Subject, data = data, na.action = na.omit, method = "REML")
# This model also does not converge...              

# What about this model?
model_4 <- lme(StartleAmplitude ~ Group * Trial * CSType * CSComplexity, random = ~ CSType|Subject, data = data, na.action = na.omit, method = "REML")
# Yes!
anova(model_4)

# Bayesian model comparison for frequentist models with bayestestR package
# Compare the two models that converge and that were estimated with REML
bayestestR::bayesfactor_models(model_4, denominator = model_1b)
# Very small Bayes Factor close to zero = no evidence for either model
