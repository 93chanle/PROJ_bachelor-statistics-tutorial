#============================================================================#

# Analysis of Variance (ANOVA) #
# 
# written by Henrik von Wehrden
# edited by Chan Le

# In this script:

### One-way ANOVA
### Factorial designs
### Split-plot experiment
### ANCOVA

#============================================================================#
#### 1. One-way ANOVA ####
#============================================================================#

### Data Inspection

# Set working directory (optional)
setwd("//R//leuphana//")

# Read data
photo <- read.table("oneway.txt", header = T)
attach(photo)
names(photo)
head(photo, 10)

# Contingency table
table(Growth)

# You can filter data
photo[Photoperiod == "Long", ]

# Default plot() does not always work
plot(Photoperiod, Growth) 

# Need to specify categorical variable as factor
plot(as.factor(Photoperiod), Growth) 

# Alternative: boxplot()
boxplot(Growth ~ Photoperiod) 

### Perform ANOVA

# Preliminary: Fligner-Killeen Test of Homogeneity of variance (non-parametric)
fligner.test(Growth ~ Photoperiod)

# Perform one-way ANOVA (with log-transformed response)
model <- lm(log(Growth) ~ Photoperiod)
anova(model)

# Model summary
summary(model)

# Contingency table
table(Photoperiod)

# Alternative syntax to ANOVA (with original response)
model<-aov(Growth ~ Photoperiod)

# Model summary
summary(model)

### Residual Analysis 

# Visual inspection of normality
hist(resid(model))
plot(model)

# Shapiro-Wilk Normality Test
shapiro.test(residuals(model))

# Komolgornov-Smirnow Test Normality Test 
ks.test(residuals(model), mean(residuals(model)))

library(multcomp)
summary(glht(model,linfct = mcp(Photoperiod = "Tukey")))

#============================================================================#
#### 2. Factorial designs ####
#============================================================================#

### Data Inspection

# Load data
shaun<-read.table("growth.txt",header=T)
attach(shaun)
names(shaun)

# Calculate mean gain wrt to supplement and diet subgroups
meanGain <- tapply(gain, list(diet,supplement), mean)

# Plot and compare values
barplot(meanGain, beside = T, ylim = c(0,30), 
        col = rainbow(3))

# Add legends to plot
legend("bottomright", legend=c("Barley","Oats","Wheat"), fill = rainbow(3))

# Alternative: Boxplots
boxplot(gain ~ supplement * diet, boxwex = 0.3, cex.axis = 0.8, las = 2)

### Build model

# With interactions
model1<-lm(gain ~ diet * supplement)
summary(model1)

# Without interactions
model2<-lm(gain ~ diet + supplement)
summary(model2)

# Hypothesis
summary(glht(model2, linfct = mcp(supplement = "Tukey")))

# Alternative
model <- lm(gain ~ diet * supplement)
summary(model)

# Simplify model
model<-aov(gain ~ diet + supplement)
summary.lm(model)  

#Post-hoc test
TukeyHSD(model1)

#so what would you feed your farm-animals?

library(lme4)
library(nlme)
library(ncf)
library(ape)

#============================================================================#
#### 3. Split-plot experiment ####
#============================================================================#

### Data import

# Load data
yields<-read.table("splityield.txt",header=T)
attach(yields)

# Check data
names(yields)
summary(yields)

# Contingency table
table(irrigation, density, fertilizer)

### Fit models

# Up to three-way interaction
model <- aov(yield ~ irrigation * density * fertilizer)

summary(model)

# Up to two-way interaction
model <- aov(yield ~ (irrigation + density + fertilizer)^2)

summary(model)

# Pairwise interaction terms between irrigation and the others
model <- aov(yield ~ irrigation + density + fertilizer + 
             irrigation:density + irrigation:fertilizer)

summary(model)

# Nested ANOVA with random effects
model <- aov(yield ~ irrigation + density + fertilizer + 
               irrigation:density:fertilizer + irrigation:density + 
               density:fertilizer+ irrigation:fertilizer + 
               Error(block/irrigation/density))

summary(model)

### Visualization

# Boxplots (subgroups of interaction)
boxplot(yield ~ density * fertilizer)

# Interaction plots
interaction.plot(fertilizer, irrigation, yield)

interaction.plot(density, irrigation, yield)

#============================================================================#
#### 4. ANCOVA ####
#============================================================================#

### Data inspection

# Load data
regrowth<-read.table("ipomopsis.txt", header = T)
attach(regrowth)

# Check data
names(regrowth)
summary(regrowth)

# Boxplot of two groups
boxplot(Root ~ Grazing)

# Scatterplot of two groups (red: Grazed, blue: Ungrazed)
plot(Root, Fruit, 
     pch = 16 + as.numeric(as.factor(Grazing)), # shape argument
     col = c("blue","red")[as.factor(Grazing)]) # color argument

# Plot regression line of two groups
abline(lm(Fruit[Grazing=="Grazed"]~Root[Grazing=="Grazed"]), lty=2, col="blue")
abline(lm(Fruit[Grazing=="Ungrazed"]~Root[Grazing=="Ungrazed"]), lty=2, col="red")

### Alternative plot: require package ggplot2
ggplot2::ggplot(data = regrowth, 
                aes(x = Root, y = Fruit, color = Grazing, shape = Grazing)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  theme_bw()

# Calculate mean fruit set of two groups
tapply(Fruit, Grazing, mean)

### Fit models

# Independent T-Test: difference between fruit set and grazing
t.test(Fruit ~ Grazing)

# Does initial root size matter?
ancova <- lm(Fruit ~ Grazing * Root)
summary(ancova)

# Simplify model, delete interaction
ancova2 <- update(ancova, ~ . - Grazing:Root)

# Compare models
anova(ancova, ancova2)

# Simplify model further, delete grazing
ancova3 <- update(ancova2, ~ . - Grazing)

# Compare models
anova(ancova, ancova3)

# Minimal adequate model
summary(ancova2)
