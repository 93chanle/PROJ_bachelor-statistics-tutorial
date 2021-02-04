#============================================================================#

# Linear Regression (1) #
# DATA SET: Rent #
# 
# by Chan Le

# In this script:

### Assumption check
### Simple linear regression with continuous explanatory variables
### Linear regression with nominal variables
### Checking model residual
### Dealing with categorical variables

#============================================================================#
# #### 1. Load data & Descriptive exploration ####
#============================================================================#

library(tidyverse)
library(car)

rentSimple <- read.csv2("https://raw.githubusercontent.com/93chanle/Statistics-Tutorial-2020/master/data/rentSimple.csv")

summary(rentSimple)

# Explanatory variables:
    # area: Area of the apartment (in m2)
    # fromCenter: distance from city center (in km)
    # onFloor: floor number (0 to 4)

# Explained variable:
    # rent: Aparment rent (in $)

#============================================================================#
# #### 2. Assumptions of linear regression ####
#============================================================================#

# Assumption 1: Independence of observations

# Assumption 2: Linear relationship

# Assumption 3: Normality

# Assumption 4: Homoscedasticity

# One way of checking these assumptions is by visual inspection. The funtion
# scatterplotMatrix() from the package car does this for us by plotting histograms
# of all variables and the pairwise scatterplots.

#install.packages("car")
car::scatterplotMatrix(rentSimple, regLine = F, smooth = F)

# Looking into the plot, the response variable rent looks more or less normally
# distributed (assumption 3). The scatterplot of rent with area and fromCenter
# suggest a linear relationship between the variables (assumption 2). No correlation
# can be hinted from the scatterplot between area and fromCenter, suggesting
# independence (assumption 1). Assumption 4 is checked after the model is created.

#============================================================================#
# #### 2. Simple linear regression model ####
#============================================================================#

rentLM <- lm(rent ~ area + fromCenter + onFloor, data = rentSimple)

#============================================================================#
# #### 3. Check model results & residuals ####
#============================================================================#

summary(rentLM)

# This model explains 45.46% the variance in the data (adjusted R2). 
# Two variable area and fromCenter are significant, with p-value < 0.05.
# A squared meter bigger in area pushes the rent up $7.3 on average. 
# Per one km further an apartment is from the city center, the rent goes down 
# approximately $13.2.

# Next, we plot a scatterplot of the residual (X-axis: index of 200 observation,
# Y-axis: residual = predicted value - actual value). We also plot a red line
# showing the mean of all residual.

plot(rentLM$residuals)
abline(h = mean(rentLM$residuals), col = "red")

# In this case, the residual revolves around mean 0, which is a good sign (the
# red line is horizontal at 0). Any other patterns may indicate a problem with 
# some aspect of the linear model.

#============================================================================#
# #### 4. Dealing with categorical variables ####
#============================================================================#

# In this part we use the full rent data. Load data:

{
rentFull <- read.csv2("https://raw.githubusercontent.com/93chanle/Statistics-Tutorial-2020/master/data/rentFull.csv")
rentFull$petsAllowed <- factor(rentFull$petsAllowed)
rentFull$flooringType <- factor(rentFull$flooringType)
}

summary(rentFull)
head(rentFull, 10)

# Looking at the data, we see two new categorial variables:
    # petsAllowed: whether or not house pets are allowed in the apartment.
    # flooringTypes: different options for flooring (vinyl, tiles, carpet, 
    # hardwood)

# In order to put these information into the linear model, a pre-processing step
# is required. It is called one-hot-encoding, or dummification. We do this in R
# using the package fastDummies.

#install.packages("fastDummies")
library(fastDummies)

rentFull <- dummy_columns(rentFull, 
                          select_columns = c("petsAllowed", "flooringType"),
                          remove_selected_columns = T,
                          remove_most_frequent_dummy = T)

# Here, we want to select 2 columns petsAllowed and flooringType to dummify. We
# choose to drop these selected columns in the result data frame. We also want to
# remove one category in each variable (in this case we remove the most frequent
# category, but you can remove any of them, it doesn't change the result). This
# is to prevent perfect multicollinearity,

# Checking the data again.

head(rentFull, 10)

# Fitting the linear model. If you wish to take all variables in the data as 
# explanatory variable, you can put a dot (.) to represent that instead of listing
# all variables out.

rentLM <- lm(rent ~ ., data = rentFull)

# Can you explain the result?

summary(rentLM)

# And the residuals?

plot(rentLM$residuals)
abline(h = mean(rentLM$residuals), col = "red")
