#============================================================================#

# CORRELATION #
# DATA SET: mtcars, cereal #
# 
# by Chan Le, 28.01.2021

# Link to data set description:
# https://www.kaggle.com/crawford/80-cereals

# In this script:

### Pearson correlation
### Spearman correlation
### Correlation matrix & visualization

#============================================================================#
# #### 1. Pearson Correlation ####
#============================================================================#

# For this part, we use the built-in dataset mtcars in R.

# Using cor(), we can calculate Pearson correlation between two sets of variables.
# Here, we want to know the correlation between mpg and qsec.
cor(mtcars$mpg, mtcars$qsec)

# You can also feed a whole dataframe into cor(). It returns a (symmetric) 
# correlation matrix for all variables inside the dataset.
cor(mtcars)

# you can visualize a correlation matrix for a better overview.
# You can install the package corrplot using the following command (need only
# once, delete the hashtag before install.packages)
# install.packages("corrplot") 
corrplot::corrplot(cor(mtcars), addCoef.col = "black", type = "upper")

# Some correlations are pretty strong. Let's confirm them with a correlation test.
# Correlation test between mpg and disp:
cor.test(mtcars$mpg, mtcars$disp)

# Correlation test between disp and wt:
cor.test(mtcars$disp, mtcars$wt)

# Can you draw a conclusion?

#============================================================================#
# #### 2. Spearman's Correlation ####
#============================================================================#

# Another example. This time we are working with the object cerealNutrients. 
# It contains the nutrients contents of the cereal brands and their ratings.
# Load the data (just run through this part)
{
  cerealNutrients <- read.csv2("https://raw.githubusercontent.com/93chanle/Statistics-Tutorial-2020/master/cerealNutrients.csv")
  cerealNutrients$rating <- as.numeric(cerealNutrients$rating)
  cerealNutrients$fiber <- as.numeric(cerealNutrients$fiber)
  cerealNutrients$carbo <- as.numeric(cerealNutrients$carbo)
}

# Many variables in the dataset are not normally distributed. You can test
# this by either plotting a histogram/boxplot or conducting Shapiro-Wilk 
# test.
hist(cerealNutrients$fiber)

hist(cerealNutrients$rating)

# In these cases, an alternative to Pearson would be Spearman's Rand-Order
# Correlation. We call the same function cor() but with an exrea arguement
# passed in to determine the method used.
cor(cerealNutrients$fiber, cerealNutrients$rating, method = "spearman")

# Same thing applys for the correlation matrix of the whole dataset.
cor(cerealNutrients, method = "spearman")