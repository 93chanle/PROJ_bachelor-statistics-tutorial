#============================================================================#

# Linear Regression #
# 
# written by Henrik von Wehrden
# edited by Chan Le

# In this script:

### Normality check
### Correlations
### Linear regression models
### Further examples

#============================================================================#
#### 1. Data import & inspection ####
#============================================================================#

# Set working directory (optional)
setwd("//R//leuphana//")

# Read data from .txt
reg.data <- read.table("regression.txt",header=T)

# Attach object to search path
attach(reg.data)

# Column names
names(reg.data)

# Structure of data
str(reg.data)

# Dimensions of data
dim(reg.data)

# Scatterplot
plot(tannin,growth)

#============================================================================#
#### 2. Normality check ####
#============================================================================#
# Plot histogram
hist(growth)

# Perform Kolmogorov–Smirnov Test
ks.test(growth,mean(growth))

# Perform Shapiro-Wilk Test
shapiro.test(growth)

#============================================================================#
#### 3. Correlations ####
#============================================================================#

# Pearson correlation (parametric)
cor(tannin,growth)
cor.test(tannin,growth)

# Spearman correlation (parametric)
cor.test(tannin,growth,method="spearman")
cor.test(tannin,rank(growth))

# Kendall correlation (parametric)
cor.test(tannin,growth,method="kendall")

#============================================================================#
#### 4. Linear regression model ####
#============================================================================#

# Create model
hvw<-lm(growth~tannin)

# Summary of model
summary(lm(growth~tannin))
summary(hvw)

# Structure of lm() object
str(summary(hvw))

# Extract p-value of tannin
summary(hvw)$coefficients[2,4]

# Extract R squared value
summary(hvw)$r.squared

# Visualize residuals
hist(resid(hvw))
plot(resid(hvw))

#============================================================================#
#### 5. Example: cars ####
#============================================================================#

# Import data
data(cars)
attach(cars)
names(cars)

# Histogram of car speed and displacements
par(mfrow=c(1,2))
hist(speed);hist(dist)

# Create linear regression modlel
model<-lm(speed~dist)

# Post-model analysis
summary(model)
plot(speed~dist) 
abline(model) # Regression line
hist(resid(model))
par(mfrow=c(1,1))

#============================================================================#
#### 6. Example: varachem ####
#============================================================================#

# Load data (required package vegan)
library(vegan)
data(varechem)
??varachem

# Look at data
varechem
dim(varechem)

# Correlation matrix (rounded)
corMat <- round(cor(varechem), d=2)
corMat

# Filter correlations higher than 0.5
corMat > 0.5

# Correlation matrix plot (required package corrplot)
corrplot::corrplot(corMat, diag = T, type = "upper")

# Regression model
summary(lm(growth ~ tannin))

# Visualization
plot(tannin,growth)
abline(lm(growth ~ tannin))

# Regression model through the origin
summary(lm(growth ~ tannin - 1))

# Visualization
plot(tannin,growth, xlim = c(-1, 8), ylim = c(-1, 12))
abline(lm(growth ~ tannin - 1))

# Null model
summary(lm(growth~1))

# Visualization
plot(tannin,growth)
abline(lm(growth~1),col="red")

# Model simplification
model <- lm(growth ~ tannin)
summary(update(model, ~ . -tannin))







