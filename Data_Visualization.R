getwd()
setwd("C:/Users/YOONSU/Desktop/R_data_practice")
employee<-read.csv("employees_kr.csv")
head(employee)

#연도에 따른 인센티브 차이
sub2008<-subset(employee,employee$Year==2008)
head(sub2008)
hist(sub2008$Incentive)
sub2009<-subset(employee,employee$Year==2009)
hist(sub2009$Incentive)

#성별에 따른 인센티브 차이
subMan<-subset(employee,employee$Sex=="M")
hist(subMan$Incentive)
subWoman<-subset(employee,employee$Sex=="F")
hist(subWoman$Incentive)

#협상여부에 따른 인센티브 차이
subNego<-subset(employee, employee$Nego==TRUE)
hist(subNego$Incentive)
subNoNego<-subset(employee, employee$Nego==FALSE)
hist(subNoNego$Incentive)

#R그래프 전체 구성 결정
split.screen(c(2,1))
screen(1)
plot(1:10)
screen(2)
plot(10:1)
screen(2)
plot(1:10)

split.screen(c(2,3))
screen(3)
plot(1:10)
screen(4)
plot(10:1)
screen(1)
plot(1:100)
screen(6)
plot(100:1)

par(mfrow=c(2,1))
plot(1:10)
plot(10:1)

#R그래프 옵션
x<-seq(1,10,0.1)
y<-exp(x)
plot(x,y)
plot(x,y,main="EXP_GRAPH",xlab="Time",ylab="Income increase")
