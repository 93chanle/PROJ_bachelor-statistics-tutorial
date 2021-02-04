#============================================================================#

# SIMPLE TESTS #
# DATA SET: CEREAL BRANDS #
# 
# by Chan Le, 16.06.2020

# Link to data set description:
# https://www.kaggle.com/crawford/80-cereals

# In this script:

### Shapiro-Wilk Test of Normality
### Kolmogorov-Smirnov Test
### Chi-square Test
### F-Test
### Simple Independent T-Test

#============================================================================#
#### 1. Load the data ####
#============================================================================#

# Run these lines to load necessary packages and data

# The package for data manipulation in R is tidyverse. If you don't have it yet,
# delete the # in front of the next line and run it once.
# install.packages("tidyverse")
library(tidyverse)

# Load the data from the internet
cereal <- read.csv2("https://raw.githubusercontent.com/93chanle/Statistics-Tutorial-2020/master/cereal.csv", sep = ",") 

# Coerce some variables to the right data format
cereal$rating <- as.numeric(cereal$rating)
cereal$fiber <- as.numeric(cereal$fiber)
cereal$carbo <- as.numeric(cereal$carbo)
cereal$shelf <- as.factor(cereal$shelf)

#============================================================================#
#### 2. Overview, descriptive Statistics, and basic plots ####
#============================================================================#

# Take a look at the whole data set
View(cereal)

# Call the first 5 rows in the data set
head(cereal, 5)

# Look at the summary of the data set
summary(cereal)

# Plot a histogram of the sodium amount in all cereal brands
hist(cereal$sodium)

# Plot a box plot of the sugar amount in all cereal brands
boxplot(cereal$sugars)

# Plot 2 box plots of potassium and sodium amount in one plot
boxplot(cereal$sodium, cereal$potass)

# Plot box plots of sugars amount among different manufacturers
boxplot(sugars ~ mfr, data = cereal)

# Plot a scatter plot of sugars and ratings for all cereal brands
plot(rating ~ sugars, data = cereal)
plot(cereal$sugars, cereal$rating)


#============================================================================#
#### 3. Chi-Squared test ####
#============================================================================#

# Whether cereal brand from a specific manufacturer would affect its placing on
# the shelf

# Print out contingency table
table(cereal$mfr, cereal$shelf)

# Conduct chi-squared test
chisq.test(cereal$mfr, cereal$shelf)

?chisq.test

# Can you draw a conclusion?

#============================================================================#
#### 4. Normality Test, Homogeneity of Variance (F-Test), and Simple Independent T-Test ####
#============================================================================#

# In this part, we want to take a look at the sugar contents of two popular 
# cereal manufacturers, General Mills and Kellogg. We now work with the object
# cerealGPSugar.
cerealGPSugar <- read.csv2("https://raw.githubusercontent.com/93chanle/Statistics-Tutorial-2020/master/cerealGPSugar.csv")

# Let's take a look at the object.
View(cerealGPSugar)

# We want to compare the average sugar portions in the cereal brands between 
# these two manufacturers. An independent T-Test seems to be the right choice. 
# But before that, I need to check for some assumptions:

# 1. Normality assumption: Is the sugar amount normally distributed?
shapiro.test(cerealGPSugar$sugars)

# 2. Homogeneity of Variance: Compare variance between the two groups:
var.test(sugars ~ mfr, data = cerealGPSugar)

# The two assumptions are meet. Let's plot a boxplot to compare them visually.
boxplot(sugars ~ mfr, data = cerealGPSugar)

# Now we can do a T-Test:
t.test(sugars ~ mfr, data = cerealGPSugar)

# Can you draw a conclusion?

#============================================================================#
#### 5. Kolmogorov-Smirnov Test, Welch's T-Test ####
#============================================================================#

# In this part, we want to take a look at the sugar contents of two popular 
# cereal manufacturers, General Mills and Kellogg. We now work with the object
# cerealGPPotass.
cerealGPPotass <- read.csv2("https://raw.githubusercontent.com/93chanle/Statistics-Tutorial-2020/master/cerealGPPotass.csv")

# We want to compare the average potassium portions in the cereal brands 
# between these two manufacturers.

# Instead of Shapiro-Wilk, we can also conduct Kolmogorov-Smirnov Test to
# compare our data to a normal distribution function (pnorm). Result shows
# a p < 0.05, meaning H0 (data follows a normal distribution) is rejected.
ks.test(cerealGPPotass$potass, pnorm)

# Running the F-Test shows that the variance of the two groups are unequal,
# therefore violating another assumption of the simple T-Test.
var.test(potass ~ mfr, data = cerealGPPotass)

# To combat this, we use a more robust version of the simple T-Test called
# Welch's T-Test from the package "rstatix". Go to the documentation and 
# read the description in the result data frame Can you intepret the p-value
# in this case?

# install.packages("rstatix")
library(rstatix)

?t_test

rstatix::t_test(formula = potass ~ mfr, data = cerealGPPotass)



