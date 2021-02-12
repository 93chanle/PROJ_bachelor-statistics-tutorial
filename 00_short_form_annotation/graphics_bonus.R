?barplot
barplot(VADeaths, plot = FALSE)
barplot(VADeaths, plot = FALSE, beside = TRUE)
barplot(VADeaths, plot = T, beside = TRUE)

?pie
require(grDevices)
pie(rep(1, 24), col = rainbow(24), radius = 0.9)
data(iris)
str(iris)
pie(table(iris$Species))
barplot(apply(VADeaths,2,mean))
barplot(apply(VADeaths,2,var))

boxplot(iris[,c(1:4)])
iris$Species2<-iris$Species
names(iris)
iris$Species2<-as.numeric(iris$Species)
plot(iris$Sepal.Length~iris$Petal.Length,pch=iris$Species2,xlim=c(0,8),ylim=c(2,8))
abline(lm(iris$Sepal.Length~iris$Petal.Length))
points(iris$Sepal.Width~iris$Petal.Width,pch=iris$Species2,col="gray40")
abline(lm(iris$Sepal.Width~iris$Petal.Width,col="gray40"))

par(mfrow=c(2,2))
for(i in 1:4){
	hist(iris[,i],main=colnames(iris)[i])
	}
	




