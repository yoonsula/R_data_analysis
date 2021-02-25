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

#R그래프 그리기
abc<-c(260,300,250,280,310)
def<-c(180,200,210,190,170)
ghi<-c(210,250,260,210,270)
plot(abc,type="o",col="red",ylim=c(0,400), axes=F, ann=F)
axis(1, at=1:5, lab=c("A","B","C","D","E"))
axis(2, ylim=c(0,400))
title(main="Fruit",col.main="red",font.main=4)
title(xlab="Day",col.lab="black")
title(ylab="Price",col.lab="blue")
lines(def,type="o",pch=21,col="green",lty=2)
lines(ghi,type="o",pch=22,col="blue",lty=2)
legend(3,400,c("Orange","Apple","Banana"),cex=0.8,
       col=c("red","green","blue"),pch=21,lty=1:3)
#cex:글자크기   lty:대시선

#기본 R그래프 그리기
x<-c(50,40,32,68,120,92)
barplot(x, names="Total Sale Amount")
x_matrix<-matrix(c(50,40,32,68,120,92),3,2)
x_matrix
split.screen(c(1,2))
screen(1)
barplot(x_matrix, names=c("Korea","America"))
screen(2)
barplot(x_matrix, names=c("Korea","America"),beside=T)
abc<-c(50,40,32,68,120,92)
barplot(abc,main="abc",xlab="season", ylab="sales", names.arg=
          c("A","B","C","D","E","F"),border="blue",density=c(10,30,50,80,90,92))

abc<-c(110,300,150,280,310)
def<-c(180,200,210,190,170)
ghi<-c(210,150,260,210,70)
B_Type2<-matrix(c(abc,def,ghi),5,3)
B_Type2
barplot(B_Type2,main="Ball Type별 시즌의 판매량",xlab="Ball Type",
        ylab="매출",beside=T,names.arg=c("Baseball","Soccerball","Beachball"),
        border="blue",col=rainbow(5),ylim=c(0,400))
legend(13,400,c("A시즌","B시즌","C시즌","D시즌","E시즌"),cex=0.7,
       fill=rainbow(5))
barplot(t(B_Type2), main="시즌별 볼타입에 따른 판매량",xlab="Season",
        ylab="Price",beside=T,names.arg=c("A","B","C","D","E"),
        border="blue",col=rainbow(3),ylim=c(0,400))
legend(13,400,c("Baseball","Soccerball","Beachball"),cex=0.7,fill=rainbow(3))

#데이터 누적 형태로 보여 주는 예
barplot(t(B_Type2), main="시즌별 볼타입에 따른 판매량(누적 표시형)",
        xlab="Season",ylab="매출",names.arg=c("A","B","C","D","E"),
        border="blue", col=rainbow(3),ylim=c(0,1000))
legend(4,1000,c("Baseball","Soccerball","Beachball"),cex=0.7,fill=rainbow(3))

#점그래프 그리기
x
dotchart(x,labels=c("A","B","C","D","E","F"),pch=22)

#히스토그램 그리기
x<-c(1,2,1,4,5,4,5,2,3,5,2,6,7,3,7,8,6,5,4,7,7,6,5,7,8,9,8)
hist(x,xlim=c(0,10),ylim=c(0,6),nclass=12,main="Call number of #1 Topic")
#nclass: 계급구간의 수

#원그래프 그리기
T_sales<-c(210,110,400,550,700,130)
pie(T_sales)
pie(T_sales,init.angle=90,col=rainbow(length(T_sales)),
    labels=c("Mon","Tue","Wed","Thu","Fri","Sat"))
legend(1,1,c("Mon","Tue","Wed","Thu","Fri","Sat"),cex=0.7,
       fill=rainbow(length(T_sales)))
#3차원 파이 그래프 그리기
install.packages("plotrix")
library(plotrix)
T_sales<-c(210,110,400,550,700,130)
week<-c("Mon","Tue","Wed","Thu","Fri","Sat")
ratio<-round(T_sales/sum(T_sales)*100,1)
label<-paste(week,"\n",ratio,"%")
pie3D(T_sales,main="주간 매출 변동",col=rainbow(length(T_sales)),
      cex=0.8,labels=label)
legend(0.8,1.4,c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),cex=0.7,fill=rainbow(length(T_sales)))
pie3D(T_sales,main="주간 매출 변동", col=rainbow(length(T_sales)), cex=0.8,  labels=label, explode=0.05)

#박스그래프 그리기
boxplot(abc,def,ghi)
boxplot(abc,def,ghi,col=c("yellow","cyan","green"),names=c("baseball","soccerball","beachball"),horizontal=TRUE)

#그래프 그리기의 부가적인 기능
plot(1:10)
par(new=T)
plot(10:1)
plot.new()
plot(1:100)

plot.new()
plot(-4:4,-4:4,type='n')
#type="n" =>좌표를 찍지 않음 
points(rnorm(200),rnorm(200),pch="+",col="red")
par(new=T)
points(rnorm(200),rnorm(200),pch="o",col="cyan")
#rnorm:난수발생, dnorm:확률밀도함수, pnorm:누적분포함수, qnorm:분위수

#꺾은선 그래프
x<-c(1:10)
y<-c(x*x)
y
plot(x,y,type='n',main="Title")
for(i in 1:5) lines(x,(y+i*5),col=i,lty=i)

#선분, 화살표, 사각형, 문자열, 직선 그리기
x<-c(1,3,6,8,9)
y<-c(12,56,78,32,9)
plot(x,y)
segments(6,78,8,32)  #점들을 연결
arrows(3,56,1,12)    #화살표
rect(4,20,6,30,density=10) #사각형 그리기, 밀도는 3

text(4,40,"sample",srt=55)  #각도 55
mtext("상단의 문자열입니다",side=3) #mtext:그래프 위에 글자 출력
mtext("우측의 문자열입니다",side=4)
box(lty=2,col="red")
axis(1,pos=40,at=0:10,col=2)
axis(2,pos=5,at=10:60,col=3)

#연습문제
x<-c(1:10)
y<-exp(x)
plot(x,y,type='n',main="Title")
for(i in 1:10) lines(x,(y+i*5),col=i,lty=i)
y<-x*x
plot(x,y,type='n',main="Title")
for(i in 1:10) lines(x,(y+i*5),col=i,lty=i)

mtext("Right Side Text", side=4,adj=0.5)  
#adj:텍스트의 위치(0:왼쪽 또는아래, 1:오른쪽 또는 위, 0.5:가운데)
abline(1,2) #직선을 추가함. a:절편, b:기울기
box(lty=2,col="red")
axis(1,pos=50, at=0:10, col=2)
axis(2,pos=6, at=0:100,col=3)

#두 종류 그래프 조합하기
x<-c(1,2,1,4,5,4,5,2,3,5,2,6,7,3,7,8,6,5,4,7,7,6,5,7,8,9,8)
par(mfrow=c(1,2))
hist(x)
hist(x,probability=T, main="Histogram with density line")
lines(density(x))

#그래프 종류 소개
#Sunflowerplot 그래프
zz<-iris[,1:2]
sunflowerplot(zz)

#Stars 그래프
data(mtcars)
head(mtcars)
stars(mtcars[,1:4])

#Persp,Contour 그래프
x1<-seq(-3,3,length=50)
x2<-seq(-4,4,length=60)
f<-function(x1,x2){
  x1^2+x2^2+x1*x2
}
y = outer(x1,x2,FUN=f)
persp(x1,x2,y)
contour(x1,x2,y)
image(x1,x2,y)