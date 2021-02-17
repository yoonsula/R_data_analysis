head(iris)
tail(iris)
summary(iris)
str(iris)
plot(iris)
plot(iris$Sepal.Length)
Sepal_Length<-iris[,1]
Petal_Length<-iris[,3]
temp<-cbind(Sepal_Length,Petal_Length)
boxplot(temp)
plot(iris$Sepal.Length, iris$Petal.Length, pch=as.numeric(iris$Species))

install.packages("caret")
library(caret)
featurePlot(iris[,1:4],iris$Species)
