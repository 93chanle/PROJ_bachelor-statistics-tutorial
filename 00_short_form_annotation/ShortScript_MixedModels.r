#============================================================================#

# Linear Mixed Models #
# 
# written by Henrik von Wehrden
# edited by Chan Le

#============================================================================#
#### 0. Load packages ####
#============================================================================#

# Standard package for mixed models
library(lme4)

# Package for linear and non-linear mixed effects models
library(nlme)

# Multiple comparisons package with R
library(multcomp)
# library(ncf)
# library(ape)

#============================================================================#
#### 1. Data Import ####
#============================================================================#

# Load and check data
yields<-read.table("splityield.txt",header=T)
attach(yields)

# Variable names
names(yields)

# Data structure
str(yields)

# Histogram of the response
hist(yield)

#============================================================================#
#### 2. Fitting Models ####
#============================================================================#

# Nested ANOVA mixed model
model <- aov(yield ~ irrigation * density * fertilizer + 
             Error(block/irrigation/density))

# Model summary
summary(model)

# Linear model with interactions
model <- lm(yield ~ block * irrigation * density * yield) 

# Model summary
summary(model)

# Linear mixed effects model with nested random effects (including three-way 
# interactions)
model <- lme(yield ~ irrigation * density * fertilizer, 
           random= ~ 1|block/irrigation/density)

# Model summary
summary(model)

# Linear mixed effects model with nested random effects (no three-way 
# interactions)
model <- lme(yield ~ (irrigation + density + fertilizer)^2,
           random= ~ 1|block/irrigation/density)

# Model summary
summary(model)

# Linear mixed effects model with nested random effects (model fit by maximizing
# log-likelihood)
model.lme <- lme(yield ~ irrigation * density * fertilizer,
  random = ~ 1|block/irrigation/density, method = "ML")

# Model update (remove three-way interaction)
model.lme2 <- update(model.lme, ~ . - irrigation:density:fertilizer)

# Compare two models with ANOVA table
anova(model.lme,model.lme2)

# Linear mixed effects model with nested random effects (pairwise interactions
# with irrigation)
model.lme3 <- lme(yield ~ irrigation * density + irrigation * fertilizer, 
               random = ~ 1|block/irrigation/density)

# Model summary
summary(model.lme3)

#============================================================================#
#### 3. LLMs with package lme4 ####
#============================================================================#

# Load package
library(lme4)

# Linear mixed effects model with nested random effects (including three-way 
# interactions)
model <- lmer(yield ~ irrigation * density * fertilizer + 
                (1|block) + (1|irrigation) + (1|density), yields, REML = F)

# Model summary
summary(model)

# Test estimated coefficients in model
cftest(model)

#============================================================================#
#### 3. Fit and compare models ####
#============================================================================#

model <- lme(yield ~ (irrigation + density + fertilizer)^2, 
             random = ~1|block/irrigation/density)

summary(model)

model2 <- lme(yield ~ irrigation * density + irrigation * fertilizer, 
           random = ~1|block/irrigation/density)

summary(model)

model.lme3 <- update(model.lme2, ~ . - density:fertilizer)

anova(model.lme3,model.lme2)

model.lme4 <- update(model.lme3, ~ . - irrigation:fertilizer)

anova(model.lme3,model.lme4)

model.lme5 <- update(model.lme2, ~ . - irrigation:density)

anova(model.lme5,model.lme2)


### Residual check of model 5

# Histogram of model residual
hist(resid(model.lme5))

# Residual data w.r.t. block
both <- data.frame(block = block,
                      residual = as.vector(resid(model.lme5)))

# Histogram of residual w.r.t. block
par(mfrow=c(2,2))
for (i in unique(both$block)){
    blockResidual <- subset(both, block == i)
    hist(blockResidual$residual, main = paste0("Block ", i), xlab = "Residual")
}

# QQ-plots of residuals w.r.t. block
for (i in unique(both$block)){
    blockResidual <- subset(both, block == i)
    qqnorm(blockResidual$residual, main = paste0("Block ", i), xlab = "Residual")
    qqline(blockResidual$residual)
}

# Residual check of model 3

# Model 3 summary
summary(model.lme3)
str(model.lme3)

# Plot predicted against actual yield values
plot(model.lme3, yield ~ fitted(.))

# QQ-plots of residuals w.r.t. block
qqnorm(model.lme3, ~resid(.)|block)

### Comparison with ANOVA

attach(yields)
names(yields)

# ANOVA model
model<-aov(yield ~ irrigation * density * fertilizer + 
               Error(block/irrigation/density))

summary(model)

# Interaction plots
interaction.plot(fertilizer,irrigation,yield)

interaction.plot(density,irrigation,yield)




