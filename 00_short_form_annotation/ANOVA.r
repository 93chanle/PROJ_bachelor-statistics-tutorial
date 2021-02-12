#example Oneway ANOVA
setwd("//R//leuphana//")
dat<-read.table("yields.txt",header=T)
attach(photo)
names(photo)
table(Growth)
subset1<-photo[Photoperiod=="long",]
#test for constancy of variance
fligner.test(Growth~Photoperiod)

#1st step inspect your data!
plot(Photoperiod,Growth) #the plot function automatically creates boxplot if explanatory varialbe = factor
boxplot(Growth~Photoperiod)  #alternative, allows to change look of boxes

#ANOVA
model<-lm(log(Growth)~Photoperiod)
anova(model)
summary(model)
table(Photoperiod)
#bzw.
model<-aov(Growth~Photoperiod)
summary(model)

#test for normality
shapiro.test(residuals(model))
ks.test(residuals(model),mean(residuals(model)))
library(multcomp)
summary(glht(model,linfct=mcp(Photoperiod ="Tukey")))

#visual inspection of normality
hist(resid(model))
plot(model)



#factorial design
shaun<-read.table("growth.txt",header=T)
attach(shaun)
names(shaun)

#1st step inspect your data!
#mit barplots
barplot(tapply(gain,list(diet,supplement),mean),beside=T,ylim=c(0,30),col=rainbow(3))
legend ("top",legend=c("Barley","Oats","Wheat"),fill =rainbow(3))
#oder mit boxplots
boxplot(gain~supplement*diet,boxwex=0.3,cex.axis=0.8,las=2)

#inspect mean values
tapply(gain,list(diet,supplement),mean)

#build modell
#1st possibility
model<-aov(gain~diet*supplement)
summary(model)
model1<-lm(gain~diet*supplement)
summary(model1)
model2<-lm(gain~diet+supplement)
summary(model2)

summary(glht(model2,linfct=mcp(supplement ="Tukey")))

#2nd possibility
model<-lm(gain~diet*supplement)
summary(lm(model))

#simplify
model<-aov(gain~diet+supplement)
summary.lm(model)  

#Post Hoc test
TukeyHSD(model1)

#so what would you feed your farm-animals?

library(lme4)
library(nlme)
library(ncf)
library(ape)

#split-plot experiment
yields<-read.table("splityield.txt",header=T)
attach(yields)
names(yields)
summary(yields)
table(irrigation,density,fertilizer)

model<-aov(yield~irrigation*density*fertilizer)
summary(model)
model<-aov(yield~(irrigation+density+fertilizer)^2)
summary(model)
model<-aov(yield~irrigation+density+fertilizer+irrigation:density+irrigation:fertilizer)
summary(model)


model<-aov(yield~irrigation+density+fertilizer+irrigation:density:fertilizer+irrigation:density+density:fertilizer+irrigation:fertilizer+Error(block/irrigation/density))
summary(model)
library(gplots)
boxplot.n(yield~ density* fertilizer)

interaction.plot(fertilizer,irrigation,yield)

interaction.plot(density,irrigation,yield)


#ANCOVA
regrowth<-read.table("ipomopsis.txt",header=T)
attach(regrowth)
names(regrowth)
summary(regrowth)

boxplot(Root~Grazing)
#inspect your data (diamonds=ungrazed plants, triangles=grazede plants)
plot(Root,Fruit,pch=16+as.numeric(Grazing),col=c("blue","red")[as.numeric(Grazing)])

abline(lm(Fruit[Grazing=="Grazed"]~Root[Grazing=="Grazed"]),lty=2,col="blue")
abline(lm(Fruit[Grazing=="Ungrazed"]~Root[Grazing=="Ungrazed"]),lty=2,col="red")

#inspect the mean values of your data
tapply(Fruit,Grazing,mean)

#difference between fruit set and grazing
#t-test
t.test(Fruit~Grazing)

#but does intitial root-size matter?
ancova<-lm(Fruit~Grazing*Root)
summary(ancova)

#simplify model, delete interaction
ancova2<-update(ancova,~.-Grazing:Root)
anova(ancova,ancova2)

#simplify model further, delete Grazing
ancova3<-update(ancova2,~.-Grazing)
anova(ancova,ancova3)

#minimal adequate model
summary(ancova2)