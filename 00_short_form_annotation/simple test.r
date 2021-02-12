#============================================================================#

# Simple Tests #
# 
# written by Henrik von Wehrden
# edited by Chan Le

# In this script:

### F-Test - Compare two variances
### Independent T-Test - Compare two means
### Dependent T-Test - Compare two means
### Wilcoxon Test - Compare two medians with rank
### Chi-Squared Test - Test of independence

#============================================================================#
# #### 1. F-Test - Compare two variances ####
#============================================================================#

### Example: Gauster-temperature

# Read data from .txt file
Gaustern<-read.table("f.test.txt",header=T)

# Attach object to search path (to call PopA instead of Gaustern$PopA)
attach(Gaustern)

# Look at object
Gaustern

# Display structure
str(Gaustern)

# Comparing variance of two populations
var.test(PopA,PopB)

# Boxplots for visual comparison
boxplot(PopA,PopB)

# Ploting histograms (par() function multiple plots on the same canvas)
par(mfrow=c(1,2))
hist(PopA)
hist(PopB)

# Adjust a value in PopA
PopA[7]<-50

#============================================================================#
# #### 2. Independent T-Test - Compare two means ####
#============================================================================#

### Example: Stem diameter nesting trees

# Read data from .txt file
Stamm<-read.table("t.test.txt",header=T)

# Attach object to search path
attach(Stamm)

# Look at object
Stamm

# Performing a two-tail T-Test (assuming equal variance)
t.test(Golze,Finguren,var.equal=T)

# Boxplots for visual comparison
par(mfrow=c(1,1))
boxplot(Golze,Finguren)

### Example: Sleep data

# View data
View(sleep)

# Boxplots comparing two groups
plot(extra ~ group, data = sleep)

# T-Test: traditional interface
with(sleep, t.test(extra[group == 1], extra[group == 2]))

## T-Test: formula interface
t.test(extra ~ group, data = sleep)

#============================================================================#
# #### 3. Dependent T-Test - Compare two means ####
#============================================================================#

### Example: Weight loss after raising the younglings?

# Read data from .txt file
Gewicht<-read.table("t.test.paired.txt",header=T)

# Attach object to search path
attach(Gewicht)

# Look at object
Gewicht

# Structure of the object
str(Gewicht)

# Boxplots for visual comparison
boxplot(Gewicht$vorher,Gewicht$nachher)

# Take first column of data and output as vector
Gewicht[,1]

# Performing a two-tail, repeated T-Test
hvw <- t.test(vorher,nachher, paired=T)

# Structure of output object
str(hvw)

# Round up p-values
round(hvw$p.value,digits=2)

# Boxplots for visual comparison
boxplot(vorher,nachher)

#============================================================================#
# #### 4. Wilcoxon Test - Compare two medians with rank ####
#============================================================================#

### Example: Evaluation of the Wiruwaruwolzes?

# Read data from .txt file
Bewertung<-read.table("wilcox.ranksum.txt",header=T)

# Attach object to search path
attach(Bewertung)

# Look at object
Bewertung

# Performing Wilcoxon Test
wilcox.test(Finguren,Golze)

#============================================================================#
# #### 5. Chi-Squared Test - Test of independence ####
#============================================================================#

### Example: Ratio difference within the two forests

# Read data from .txt file
Wald <- read.table("chiquadr.txt",header=T)

# Attach object to search path
attach(Wald)

# Look at object
Wald

# Performing Chi-Squared Test
chisq.test(Wald)

# Extract exptected values
chisq.test(Wald)$expected