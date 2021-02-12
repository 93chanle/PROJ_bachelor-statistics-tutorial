library(lme4)

library(multcomp)
library(nlme)
library(ncf)
library(ape)

#mixed model
yields<-read.table("splityield.txt",header=T)
attach(yields)
names(yields)

model<-aov(yield~irrigation*density*fertilizer+Error(block/irrigation/density))
summary(model)

str(yields)
hist(yield)

model<-lm(yield~block*irrigation*density*yield) 
summary(model)

model<-lme(yield~irrigation*density*fertilizer,random=~1|block/irrigation/density)
summary(model)

model<-lme(yield~(irrigation+density+fertilizer)^2,random=~1|block/irrigation/density)
summary(model)

model<-lme(yield~irrigation*density+irrigation*fertilizer,random=~1|block/irrigation/density)
summary(model)

model.lme<-lme(yield~irrigation*density*fertilizer,
  random=~1|block/irrigation/density, method="ML")
model.lme2<-update(model.lme,~. -irrigation:density:fertilizer)
anova(model.lme,model.lme2)

model.lm3<-lme(yield~irrigation*density+irrigation*fertilizer,random=~1|block/irrigation/density)
summary(model.lm3)

###jetzt mit lme4
library(lme4)
model<-lmer(yield~irrigation*density*fertilizer+(1|block)+(1|irrigation)+(1|density),yields,REML=F)
model.h<-lmer(height.growth~richness.m+(1|PLOT_NO.x)+(1|fac_spp),dat,REML=F)
summary(model)
cftest(model)

model<-lme(yield~(irrigation+density+fertilizer)^2,random=~1|block/irrigation/density)
summary(model)

model<-lme(yield~irrigation*density+irrigation*fertilizer,random=~1|block/irrigation/density)
summary(model)



model.lme3<-update(model.lme2,~. -density:fertilizer)
anova(model.lme3,model.lme2)

model.lme4<-update(model.lme3,~. -irrigation:fertilizer)
anova(model.lme3,model.lme4)

model.lme5<-update(model.lme2,~. -irrigation:density)
anova(model.lme5,model.lme2)

hist(resid(model.lme5))
both<-cbind(resid(model.lme5),block)

par(mfrow=c(2,2))
for (i in 1:4){
hist(both[both[,2]==i,1])
}

qqnorm(residuals(model.lme5)|block)
summary(model.lme3)

str(model.lme3)
summary(model.lme3)
plot(model.lme3,yield~fitted(.))

qqnorm(model.lme3,~resid(.)|block)

#Vergleich: Anova
yields<-read.table("c:\\temp\\splityield.txt",header=T)
attach(yields)
names(yields)

model<-aov(yield~irrigation*density*fertilizer+Error(block/irrigation/density))
summary(model)

interaction.plot(fertilizer,irrigation,yield)

interaction.plot(density,irrigation,yield)




