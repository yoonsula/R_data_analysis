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

x<-0:2
x
str(x)
x1<-as.numeric(x)
x1
str(x1)
x2<-as.data.frame(x1)
x2
str(x2)
x3<-as.character(x1)
x3
str(x3)
x4<-as.factor(x1)
x4
str(x4)

str(iris)
View(iris)
NewData<-iris[,1:4]
head(NewData)
names(NewData)<-c("SepalLength","SepalWidth","PetalLength","PetalWidth")
head(NewData)
NewData2<-NewData$SepalLength
head(NewData2)
NewData3<-NewData[1:4,]
NewData3

iris_test<-iris
iris_test[c(5,7,8,20,60,100),1]<-NA
iris_test[c(1,2,3),3]<-NA
iris_test[!complete.cases(iris_test),]
mapply(mean,iris_test[1:4],na.rm=TRUE)

install.packages("DMwR")
library(DMwR)
iris_test[!complete.cases(iris_test),]
centralImputation(iris_test[1:4]) [c(1,2,3,5,7,8,20,60,100),]
median(iris$Sepal.Length)
test<-centralImputation(iris_test[1:4])[c(1,2,3,5,7,8,20,60,100),]
mapply(mean,test[1:4],na.rm=TRUE)
knnImputation(iris_test[1:4])[c(1,2,3,5,7,8,20,60,100),]
test2<-knnImputation(iris_test[1:4])[c(1,2,3,5,7,8,20,60,100),]
mapply(mean,test2[1:4],na.rm=TRUE)

boxplot(iris)
plot(iris$Sepal.Length, iris$Sepal.Width)
