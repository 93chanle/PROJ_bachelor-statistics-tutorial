#============================================================================#

# ONE-WAY ANOVA #
# DATA SET: PlantGrowth #
# 
# by Anastasiia Malyk (adapted by Chan Le)

# In this script:

### Q-Q Plots
### Barlett & Levene Test - Homogeneity of Variance
### One-way ANOVA

#============================================================================#
# #### 1. Load data & Descriptive exploration ####
#============================================================================#

# We will be using the PlantGrowth data. It consists of 30 cases and 2 variables. 
# The dataset shows results of an experiment that compare yields under a control 
# and two different treatment conditions. More in help().

summary(PlantGrowth)
boxplot(weight ~ group, PlantGrowth)

#============================================================================#
# #### 2. Assumptions of ANOVA: Q-Q Plots, Barlett & Levene Test ####
#============================================================================#

# Assumption 1: All samples are independent, and collected in more than two
# independent categories. If there are less than 2 categories, T-Test analysis 
# should be conducted. 

# As our data has 3 groups (ctrl, trt1, trt2) which are independent, this 
# assumption is satisfied.

# Assumption 2: Dependent variable is continuous.

# This assumption is satisfied as weight is a numeric variable.

# Assumption 3: Each group follows a normal distribution with no major outliers.
# Here, we can take a subset of the data based on each group, then apply the
# Shapiro test on the weight data.

ctrlWeight <- subset(PlantGrowth, group == "ctrl")
shapiro.test(ctrlWeight$weight)

# Alternatively, you can also plot the Q-Q plot to check the normality 
# assumption. Q-Q Plots (Quantile-Quantile plots) are plots of two quantiles 
# against each other. X-axis in the plot reflects normally distributed data, 
# while Y-axis reflects data from our sample. The purpose of Q-Q plots is to 
# find out if two sets of data come from the same distribution. A 45 degree 
# angle is plotted on the Q-Q plot; if the two data sets perfectly come from 
# a common distribution, the points will fall on that reference line.

qqnorm(ctrlWeight$weight)
qqline(ctrlWeight$weight)

# You can try doing the same for the other two groups.

# Assumption 4: Homogeneity of variance.

# Bartlett's null hypothesis states that the variances in each of the groups 
# are the same. With a p-value larger than 0.05, we fail to reject that 
# variances of the groups are homogeneous.
bartlett.test(weight ~ group, data = PlantGrowth)

# Alternatively, Levene Test for equality of variance can also be used here.
# It is less sensitive than the Bartlett test to departures from normality. 
# If you have strong evidence that your data do in fact come from a normal, 
# or nearly normal, distribution, then Bartlett's test has better performance. 
# The function leveneTest() is in the package car.

# install.packages("car")
car::leveneTest(weight ~ group, data = PlantGrowth)

#============================================================================#
# #### 2. Performing One-Way ANOVA ####
#============================================================================#
# The ANOVA output includes the columns F value and Pr(>F) corresponding to the 
# p-value of the test. In our case, ANOVA shows p-value <0.05. Thus, we reject 
# our null hypothesis that the means of the groups are equal. 

anovaWeight <- aov(weight ~ group, data = PlantGrowth)
summary(anovaWeight)

#============================================================================#
# #### 3. Performing Post-hoc Test ####
#============================================================================#
# An ANOVA test tells us if our results are significant overall, but it won't 
# tell us exactly where those differences lie. Therefore, we conduct post-hoc 
# test (TukeyHSD) that performs multiple pairwise comparisons to find out which 
# particular groups have different means. 

TukeyHSD(anovaWeight)

# In the result table, column diff gives the difference in the observed means, 
# lwr gives the lower end point of the interval, upr gives the upper end point 
# and p adj giving the p-value after adjustment for the multiple comparisons.

# It can be seen from the output, that only the difference between trt2 and trt1
# is significant with an adjusted p-value of 0.012. 