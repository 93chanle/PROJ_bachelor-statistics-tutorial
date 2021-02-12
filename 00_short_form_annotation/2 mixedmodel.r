library(lme4)

library(multcomp)
library(nlme)
library(ncf)
library(ape)

#mixed model
yields<-read.table("//R//R//splityield.txt",header=T)
attach(yields)
names(yields)
str(yields)
table(block,density,fertilizer)

model<-aov(yield~irrigation*density*fertilizer+Error(block/irrigation/density))
summary(model)

str(yield)
hist(yield)

model<-lm(yield~block*irrigation*density*yield)
summary(model)

test <- as.factor(block:irrigation:density)
model<-lme(yield~irrigation*density*fertilizer,random=~1|block/irrigation/density)
summary(model)

model<-lme(yield~(irrigation+density+fertilizer)^2,random=~1|block/irrigation/density)
summary(model)

model<-lme(yield~irrigation*density+irrigation*fertilizer,random=~1|block/irrigation/density)
summary(model)

model.lme<-lme(yield~irrigation*density*fertilizer,
  random=~1|block/irrigation/density, method="ML")
model.lme2<-update(model.lme,~. -irrigation:density:fertilizer)
summary(model.lme2)
anova(model.lme,model.lme2)


###jetzt mit lme4
library(lme4)
model<-lmer(yield~irrigation*density*fertilizer+(1|block)+(1|irrigation)+(1|density),yields,REML=F)
summary(model)
cftest(model)


model.lme1<-lme(yield~irrigation*density*fertilizer,
  random=~1|block/irrigation/density, method="ML")
  summary(model.lme1)
model.lme2<-update(model.lme1,~. -irrigation:density:fertilizer)
summary(model.lme2)
anova(model.lme1,model.lme2)



model.lme3<-lme(yield~irrigation*density+irrigation*fertilizer,random=~1|block/irrigation/density, method="ML")
summary(model.lme3)

anova(model.lme1,model.lme2,model.lme3)

summary(model.lme3)



model.lme4<-update(model.lme3,~. -irrigation:fertilizer)
summary(model.lme4)

model.lme0<-lme(yield~1,random=~1|block/irrigation/density, method="ML")
summary(model.lme0)
anova(model.lme0,model.lme1,model.lme2,model.lme3,model.lme4)



hist(resid(model.lme3))
both<-cbind(resid(model.lme3),block)

par(mfrow=c(2,2))
for (i in 1:4){
hist(both[both[,2]==i,1])
}

qqnorm(residuals(model.lme3))

str(model.lme3)
summary(model.lme3)
plot(model.lme3,yield~fitted(.))

qqnorm(model.lme3,~resid(.)|block)

#Compare: Anova
yields<-read.table("//R//R//splityield.txt",header=T)
attach(yields)
names(yields)

model<-aov(yield~irrigation*density+irrigation+fertilizer+Error(block/irrigation/density))
summary(model)
summary(model.lme4)

interaction.plot(fertilizer,irrigation,yield)

interaction.plot(density,irrigation,yield)




