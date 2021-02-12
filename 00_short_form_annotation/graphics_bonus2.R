setwd("//R//leuphana//")

worms<-read.table("worms.txt",header=T)
attach(worms)
names(worms)
summary(worms)

library(gplots)

par(mfrow=c(2,2))

#simple linear model
par(xpd=F)
plot(Worm.density~Soil.pH,xlab="Soil pH",ylab="worm density",cex.lab=1.4,cex.axis=1.2)
abline(lm(Worm.density~Soil.pH),lwd=2,lty=2)
par(xpd=T)
model1<-lm(Worm.density~Soil.pH)
r2<-round(summary(model1)$r.squared,digits=5)
p<-round(summary(model1)$coefficients[2,4],digits=5)
legend("topleft",c("r-square:",paste(r2),"p-value:",paste(p)),merge=FALSE,cex=0.7)  
#boxplot   
boxplot(Soil.pH~Vegetation,top=T,ylab="soil.pH",cex.lab=1.4,cex.axis=1,ylim=c(3.5,6),las=1,boxwex=0.5,col=c("red","blue","white"),xaxt="n")
axis(1,labels=c("Arable","Meadow","Scrub"),at=c(1,3,5),las=2)
#Histogram
hist(Slope,main=" ",ylab="abundance",xlab="slope", cex.lab=1.4,cex.axis=1.2,breaks=20)

worm0<-glm(Damp~1,binomial) 
worm1<-glm(Damp~Soil.pH,binomial)
anova(worm0,worm1,test="Chi")
plot(Soil.pH,Damp,cex=1.5,ylab="Damp",
xlab="soil pH", cex.lab=1.5,pch="?")
wormseq<-seq(3.5,5.7,0.1)
lines(wormseq,predict(worm1,list(Soil.pH=wormseq),type="response"),lwd=3,lty=3)
model<-glm(Damp~Soil.pH,family="binomial")
summary(model)


library(vegan)

##barplots
data(dune.env)
hvw<-t(as.matrix(table(dune.env$Use)))
barplot(hvw)
barplot2(hvw,beside=T)
par(mar = c(8, 4,2, 1) + 0.1)
barplot2(hvw,beside=T,col=c("blue","red","green"),ylim=c(0,10), las=2,cex.axis=1.2,cex=1.6)
par(xpd=T)
legend("topleft",c("Hayfield","Haypastu","Pasture"),
fil=c("blue","red","green"))


#boxplot
data(varechem)
varechem
dim(varechem)
boxplot(varechem[,c(2:11,14)],log="y",las=2,outline=F,boxwex=0.5)

#Bunt
colors()




#Fern diversity
costa<-read.table("areas2.txt",header=T)
attach(costa)
par(mfrow=c(1,3))
plot(SPECIES~vegindmax,xlab="maximum vegetation index",ylab="number of species",
cex.lab=1.4,cex.axis=1.3,cex=1,pch="22")
model1<-lm(SPECIES~vegindmax)
abline(model1,lty=2,lwd=3)
summary(model1)
r2<-round(summary(model1)$r.squared,digits=5)
p<-round(summary(model1)$coefficients[2,4],digits=7)
legend("topright",c("r-square:",paste(r2),"p-value:",paste(p)),merge=FALSE,cex=1.5)          #
plot(SPECIES~MAX3,xlab="maximum 3",ylab="number of species",
cex.lab=1.4,cex.axis=1.3,cex=1.6,lty=19)
model1<-lm(SPECIES~MAX3)
abline(model1,lty=2,lwd=3)
summary(model1)
r2<-round(summary(model1)$r.squared,digits=5)
p<-round(summary(model1)$coefficients[2,4],digits=7)
legend("topright",c("r-square:",paste(r2),"p-value:",paste(p)),merge=FALSE,cex=1.5)
plot(SPECIES~MAX4,xlab="maximum4",ylab="number of species",
cex.lab=1.4,cex.axis=1.3,cex=1.6,lty=19)
model1<-lm(SPECIES~MAX4)
abline(model1,lty=2,lwd=3)
summary(model1)
r2<-round(summary(model1)$r.squared,digits=5)
p<-round(summary(model1)$coefficients[2,4],digits=7)
legend("topright",c("r-square:",paste(r2),"p-value:",paste(p)),merge=FALSE,cex=1.5)


##3 combined plots
par(mfrow=c(1,3))
#x~y plot
pro<-read.table("all_enviro4.txt",header=T)
attach(pro)
names(pro)
plot(annpre_mean,median_pro,col="grey" 
,pch=3,cex.axis=1.2,xlab="precipitation",ylab="productivity in gC/square-metre",cex.lab=1.3,axes=F)
axis(1,at=seq(50,350,50))
axis(2)
box("plot")
model1<-lm(median_pro~annpre_mean)
summary(model1)
abline(model1,lwd=2)
detach(pro)
#boxplot
coeff<-read.table("coeff_vs_pre.txt",header=T)
attach(coeff)
boxplot(coeff_var~annpre_mean, ylim=c(0,50),outline=FALSE,
cex.lab=1.5,cex.axis=0.8,las=2,xlab="precipitation",ylab="coefficient of variance",axes=F)
axis(1,at=(seq(50,350,50)-5)/10,labels=seq(50,350,50))
axis(2)
box("plot")
#barplot
years<-read.table("hvw_med.txt",header=T)
attach(years)
names(years)
steps<-c(1,5,10,15,20)
mp<-barplot(med,cex.axis=1.3,cex.lab=1.3,xlab="years",
ylab="productivity in gC/square-metres",ylim=c(110,220),xpd=F,axes=F)
axis(1,at=mp[steps],pos= 112,labels=year[steps], tick= FALSE, font=1, cex.axis =1.3,las=1)
axis (2)


#species area curves
multi<-read.table("species_estimate.txt",header=T)
attach(multi)
plot(plots,mountain.shrub,xlim=c(0,200),ylim=c(0,310),xlab="number of relevÈs (‡ 100 square-metres)",ylab="number of species",
type="l",lty=3,lwd=2,cex.lab=1.5,cex.axis=1.3)
points(plots,salt,type="l",col="black",lty=4,lwd=2)
points(plots,mountain.steppe,type="l",col="black",lty=8,lwd=2)
points(plots,caragana,type="l",col="grey",lty=8,lwd=2)
points(plots,steppe,type="l",col="grey",lty=9,lwd=2)
points(plots,anabasis,type="l",col="grey4",lty=1,lwd=2)
points(plots,desert.shrub,type="l",col="grey",lty=4,lwd=2)
points(plots,haloxylon,type="l",col="black",lty=5,lwd=2)
legend("topright",c("alpine","salt","mountain","Caragana","steppe","Anabasis","desert scrub","Haloxylon"),
col=c("black","black","black","grey","grey","grey4","grey","black"),lty=c(3,4,8,8,9,1,4,5),
lwd=c(2,2,2,2,2,2,2,2),merge=FALSE)

spec<-read.table("zonal2.txt",header=T)
attach(spec)
names(spec)

par(mfrow=c(1,3))

#altitude
altitude<-alt_srtm
y1<-SPECIES
y2<-log(SPECIES)
par(mar=c(5,5,4,5))
plot(altitude,y1,ylab="",pch=25,cex.lab=1.5,cex.axis=1.5)
model1<-lm(SPECIES~alt_srtm)
abline(model1,lwd=2)
par(new=T)
plot(altitude,y2,ylab="number of species",yaxt='n',col='grey',pch=24,cex.lab=1.5,cex.axis=1.5)
axis(4,at=pretty(range(y2)),cex.axis=1.5) 
model2<-lm(y2~alt_srtm)
abline(model2,lty=3,lwd=2)

#precipitation
precipitation<-PRE_ALL
y1<-SPECIES
y2<-log(SPECIES)
 par(mar=c(5,5,4,5))
plot(precipitation,y1,ylab="",pch=25,size=0.5,cex.lab=1.5,cex.axis=1.5)
model1<-lm(SPECIES~PRE_ALL)
abline(model1,lwd=2)
par(new=T)
plot(precipitation,y2,ylab="",yaxt='n',col='darkgrey',pch=24,cex.axis=1.5,cex.lab=1.5)
axis(4, at=pretty(range(y2)),cex.axis=1.5) 
model2<-lm(y2~PRE_ALL)
abline(model2,lty=3,lwd=2)

#Productivity
productivity<-PRO
y1<-SPECIES
y2<-log(SPECIES)
 par(mar=c(5,5,4,5)) 
plot(productivity,y1,ylab="",pch=25,cex.lab=1.5,cex.axis=1.5)
axis(1,cex.lab=1.5,cex.axis=1.5)
axis(2,cex.lab=1.5,cex.axis=1.5)
model1<-lm(SPECIES~PRO)
abline(model1,lwd=2)
par(new=T)
plot(productivity,y2, 
ylab="",yaxt='n',col='grey',pch=24,cex.axis=1.5,cex.lab=1.5)
axis(4,at=pretty(range(y2)),cex.axis=1.5)
model2<-lm(y2~PRO)
abline(model2,lty=3,lwd=2)
axis(4,at=mean(y2),pos= 700,labels=c(""), tick= FALSE, 
font=0.75, cex.axis =1,las=3)
mtext("log(number of species)", side=4, cex=1, line=3) 



##diversity model
library(vegan)
data(varespec)
data(varechem) 
#Question: Which variable from varechem explains the diversity of species from varespec
#Note: varespec should pe transformed to presence/absence data

library(car)
varespec2<-ifelse(varespec>0,1,0)  ###replace all value >0 with 1
divers<-apply(varespec2,1,sum)     ###calculate diversity per plot
both<-cbind(varechem,divers) ####combine datasets
attach(both)      
model<-lm(divers~N+P+K+Ca+Mg+S+Al+Fe+Mn+Zn+Mo+Baresoil+Humdepth+pH)   #full model
summary(model)    
model2<-step(model)        ##reduce model
round(cor(varechem,method="pearson"),digits=2)  ###check correlations of predictors
biplot(prcomp(varechem,scale=T,center=T))      ###pca of all predictors
model<-glm(divers~N+P+K+Ca+Fe+Mn+Mo+Humdepth,poisson)    ##reduced models
vif(model)         ##values should be lower than 10 (or 5)
summary(model)
model2<-update(model,~.-Ca)     ##reduce model
model3<-update(model2,~.-Fe)
model4<-update(model3,~.-N)
model5<-update(model4,~.-Mo)
model6<-update(model5,~.-K)
model7<-update(model6,~.-P)
model8<-update(model7,~.-Mn)
summary(model8)    ###suggested model

##statistical fishing

newmatrix<-matrix(ncol=3,nrow=14)  ###create new result matrix (empty)
for (i in 1:14){ ####start loop
predictor<-both[,i]    ##use predictor in the loop
model<-lm(both$divers~predictor)   ##model
newmatrix[i,1]<-round(summary(model)$coefficients[2,4],digits=3)    ##safe p-values in the matrix
newmatrix[i,2]<-round(summary(model)$r.squared,digits=3)   ##safe r-square in the matrix
newmatrix[i,3]<-names(both)[i]  ##safe name in the matrix
}
colnames(newmatrix)<-c("p-value","r-square","predictor")  ##label collumn names
newmatrix  ##inspect matrix

finalmodel<-lm(divers~Mn+Baresoil+Humdepth)  ##final model?
vif(finalmodel) ###yes?
summary(finalmodel)  ##No!

finalmodel2<-lm(divers~Baresoil)  ##Really minimum adequate model?
summary(finalmodel2)  ##Finally!



## Rarefaction

data(BCI)
BCI01<-ifelse(BCI>0,1,0)
div<-apply(BCI01,1,sum)
boxplot(div)
(raremax <- min(rowSums(BCI)))
Srare <- rarefy(BCI, raremax)
S <- specnumber(BCI) #
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)


data(ToothGrowth)
attach(ToothGrowth)
table(supp,dose)#len 
names(ToothGrowth)
dose1<-as.factor(dose)
str(ToothGrowth)
model<-lm(len~dose1*supp)
summary(model)
model1<-aov(len~dose1*supp)
summary(model1)
boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, at = 1:3 - 0.2,
        subset = supp == "VC", col = "yellow",
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Vitamin C dose mg",
        ylab = "tooth length",
        xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs = "i",xaxt="n")
boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
        boxwex = 0.25, at = 1:3 + 0.2,
        subset = supp == "OJ", col = "orange",xaxt="n")
        axis(1)
axis(4)
legend(2, 9, c("Ascorbic acid", "Orange juice"),
       fill = c("yellow", "orange"))
model<-lm(len~dose*supp)
summary(model)


library(cluster)
?agnes#method="ward"
?cutree
model<-agnes(varespec,method="ward")
plot(model)
cuts<-cutree(model,k=3)
#make a boxplot of Al, 
#and check for significant differences
#against groups
dca<-decorana(varespec)
dcax<-cbind(scores(dca,type="sites"),cuts)
par(mfrow=c(1,2))
plot(dcax[,1],dcax[,2],pch=dcax[,5],cex=3,xlab="first axis",ylab="second axis")
legend("topright",legend=c("1","2","3"),pch=c(1,2,3),cex=1.5)
boxplot(varechem[,7]~cuts)



