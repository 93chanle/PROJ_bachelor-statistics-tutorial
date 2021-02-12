#load training Data
setwd("C:\\R\\R-kurs\\Cordoba")
poly<- read.table("polytraining.txt",header=T)
attach(poly)
names(poly)
str(poly)


#simple models
model0<-glm(Seedl~1,binomial)
model1<-glm(Seedl~Tree,binomial)
anova(model0,model1,test="Chi")

#significant? how much is the explained deviance?
#and next

model0<-glm(Seedl~1,binomial)
model2<-glm(Seedl~Altitude,binomial)
anova(model0,model2,test="Chi")
#significant? how much is the explained deviance?
#and next
model0<-glm(Seedl~1,binomial)
model3<-glm(Seedl~LitterC,binomial)
anova(model0,model3,test="Chi")
#significant? how much is the explained deviance?
#and next
model0<-glm(Seedl~1,binomial)
model4<-glm(Seedl~baresoil,binomial)
anova(model0,model4,test="Chi")

#graph
plot(Tree,Seedl)
min(Tree);max(Tree)
t<-seq(0,47,1)
lines(t,predict(model1,list(Tree=t),type="response"))

#sunflowerplot
sunflowerplot(Tree,Seedl,xlab="distance from the next seeder tree [m]",ylab="probability of occurrence of seedlings",font=2,font.lab=4)
lines(t,predict(model1,list(Tree=t),type="response"),lwd=2)


#test for unimodal relationship  
model5<-glm(Seedl~Tree+I(Tree^2),binomial)
anova(model1,model5,test="Chi")

model6<-glm(Seedl~Altitude+I(Altitude^2),binomial)
anova(model2,model6,test="Chi")

model7<-glm(Seedl~LitterC+I(LitterC^2),binomial)
anova(model3,model7,test="Chi")

model8<-glm(Seedl~baresoil+I(baresoil^2),binomial)
anova(model0,model8,test="Chi")

#plot unimodal
plot(LitterC,Seedl)
min(LitterC);max(LitterC)
l<-seq(0.1,95,1)
lines(l,predict(model7,list(LitterC=l),type="response"))


#interaction
model<-glm(Seedl~Tree*Altitude,binomial)
anova(model1,model,test="Chi")

#forward manuell
model1<-glm(Seedl~Tree,binomial)
forward1<-update(model1,~.+LitterC+I(LitterC^2))
anova(model1,forward1,test="Chi")
forward2<-update(model1,~.+ Altitude+I(Altitude^2))  
anova(model1,forward2,test="Chi")

#forward automatisch
model<-glm(Seedl~Tree+LitterC+I(LitterC^2)+Altitude+I(Altitude^2),binomial)
step(model,direction="forward")

#backward automatisch
model<-glm(Seedl~Tree+LitterC+I(LitterC^2)+Altitude+I(Altitude^2),binomial)
step(model)

#significant models
model1<-glm(Seedl~Tree,binomial)
model6<-glm(Seedl~Altitude+I(Altitude^2),binomial)
model7<-glm(Seedl~LitterC+I(LitterC^2),binomial)



#AUC
library(ROC) 
detach(poly)
poly2<- read.table("polytest.txt",header=T)
attach(poly2)
names(poly2)
str(poly2)

#Validation of Tree
model<- predict(model1, poly2, type="response")
roc <- rocdemo.sca(Seedl, model, dxrule.sca)
AUC(roc)

#Validation of Altitude
model<- predict(model6, poly2, type="response")
roc <- rocdemo.sca(Seedl, model, dxrule.sca)
AUC(roc)

#Validation of LitterC
model<- predict(model7, poly2, type="response")
roc <- rocdemo.sca(Seedl, model, dxrule.sca)
AUC(roc)


