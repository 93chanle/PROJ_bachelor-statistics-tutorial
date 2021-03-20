#============================================================================#

# Logistic Regression #
# 
# written by Henrik von Wehrden
# edited by Chan Le

# In this script:

### Logistic Regression
### Variations and Models
### Interactions
### Receiver Operating Characteristics (ROC) Curve, Area under the Curve (AUC)

#============================================================================#
#### 1. Data import & inspection ####
#============================================================================#

# Set working directory
setwd("C:\\R\\R-kurs\\Cordoba")

# Read data from .txt
poly <- read.table("polytraining.txt",header=T)
attach(poly)

# Check name and structure
names(poly)
str(poly)

#============================================================================#
#### 2. Fit models ####
#============================================================================#

### Variable Tree

# Null model
model0 <- glm(Seedl ~ 1, family = binomial)

# Simple logistic regression
model1 <- glm(Seedl ~ Tree, family = binomial)

# Compare models
anova(model0, model1, test = "Chi")

# Question: Is the difference significant? How much is the explained deviance?

### Variable Altitude

model0 <- glm(Seedl ~ 1, binomial)

model2 <- glm(Seedl ~ Altitude, binomial)

anova(model0,model2,test="Chi")

### Variable Altitude LitterC

model0 <- glm(Seedl ~ 1,binomial)

model3 <- glm(Seedl ~ LitterC, binomial)

anova(model0, model3, test="Chi")

### Variable Altitude LitterC

model0 <- glm(Seedl ~ 1, binomial)

model4 <- glm(Seedl ~ baresoil, binomial)

anova(model0, model4, test="Chi")

### Visualization

# A range of possible Tree values
t <- seq(min(Tree) - 2, max(Tree) + 2, 1)

# Scatterplot
plot(Tree, Seedl)

# Predicted Seedl for each value in range of Tree
lines(t, predict(model1, list(Tree = t),type="response"), col = "blue", lwd = 2)

# Alternative: Sunflower plot (a type of scatter plot for high density data)
sunflowerplot(Tree, Seedl, 
              xlab = "Distance from the next seeder tree [m]",
              ylab = "Probability of seedlings occurence",
              font = 2, font.lab = 4)

# Predicted Seedl line
lines(t, predict(model1, list(Tree = t),type="response"), col = "blue", lwd = 2)

### Test for unimodal relationship (adding variable transformation x^2)
model5 <- glm(Seedl ~ Tree + I(Tree^2), binomial) # I(x): use object "as is"

anova(model1, model5, test="Chi")

model6 <- glm(Seedl ~ Altitude + I(Altitude^2), binomial)

anova(model2, model6, test="Chi")

model7 <- glm(Seedl~ LitterC + I(LitterC^2), binomial)

anova(model3, model7, test="Chi")

model8 <- glm(Seedl ~ baresoil + I(baresoil^2), binomial)

anova(model0,model8,test="Chi")

### Visualization

# A range of possible Tree values
t <- seq(min(LitterC) - 2,max(LitterC) + 2, 1)

# Scatterplot
plot(LitterC, Seedl)

# Predicted Seedl for each value in range of Tree
lines(t, predict(model7, list(LitterC = t),type="response"), col = "blue", lwd = 2)

#============================================================================#
#### 3. Interactions ####
#============================================================================#

# Comparing the models
model1 <- glm(Seedl ~ Tree, binomial)

model <- glm(Seedl ~ Tree * Altitude, binomial)

anova(model1, model, test="Chi")

# Manual forward selection
forward1 <- update(model1, ~ . + LitterC + I(LitterC^2))

anova(model1 , forward1, test="Chi")

forward2 <- update(model1, ~ . + Altitude + I(Altitude^2))  

anova(model1, forward2, test = "Chi")

# Automatic forward selection
model <- glm(Seedl ~ Tree + LitterC + I(LitterC^2) + 
               Altitude + I(Altitude^2), binomial)

step(model,direction = "forward")

# Automatic backward selection
model <- glm(Seedl ~ Tree + LitterC + I(LitterC^2) + 
               Altitude + I(Altitude^2), binomial)

step(model, direction = "backward")

# Significant models
model1 <- glm(Seedl ~ Tree, binomial)

model6 <- glm(Seedl ~ Altitude + I(Altitude^2), binomial)

model7 <- glm(Seedl ~ LitterC + I(LitterC^2), binomial)

detach(poly)

#============================================================================#
#### 4. Area under the curve (AUC) ####
#============================================================================#

# Install package ROC through BioManager (optional)
install.packages("BiocManager")
BiocManager::install("ROC")
library(ROC) 

# Read data
poly2 <- read.table("polytest.txt", header = T)
attach(poly2)

# Check data
names(poly2)
str(poly2)

# Validation of Tree
model<- predict(model1, poly2, type="response")
roc <- rocdemo.sca(Seedl, model, dxrule.sca)
AUC(roc)

# Validation of Altitude
model<- predict(model6, poly2, type="response")
roc <- rocdemo.sca(Seedl, model, dxrule.sca)
AUC(roc)

# Validation of LitterC
model<- predict(model7, poly2, type="response")
roc <- rocdemo.sca(Seedl, model, dxrule.sca)
AUC(roc)


