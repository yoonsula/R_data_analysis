#ggplot2 패키지
install.packages('ggplot2')
library(ggplot2)
diamonds
g<-diamonds[order(diamonds$table),]
head(g)
tail(g)
gg<-ggplot(diamonds,aes(x=carat, y=price))  #aes:그림의 축
gg+geom_point(size=1,shape=2,color="steelblue",stroke=1)
#size:점의 크기, shape:도형, stroke:각 점의 외각선 굵기
gg+geom_point(aes(size=carat,shape=cut,color=color,stroke=carat))

gg1<-gg+geom_point(aes(color=color))
gg2<-gg1+labs(title="Diamonds",x="Carat Layer", y="Price Layer")
print(gg2)

gg1<-gg+geom_point(aes(color=color))
gg2<-gg1+labs(title="Diamonds",x="Carat", y="Price")
gg2+theme(text=element_text(color="red"))
#theme:그래프 설정 변경, element_text:그래프요소 중 텍스트부분을 지칭

gg3<-gg2+theme(plot.title=element_text(size=25),axis.title.x=
                 element_text(size=20),axis.title.y=element_text(size=20),
               axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))

gg3+labs(title="Plot Title \nSecond Line of plot Title")+
  theme(plot.title=element_text(face="bold",color="steelblue",lineheight=1))

gg3+scale_colour_manual(name='Legend',values=c('D'='grey','E'='red',
                                               'F'='blue','G'='yellow','H'='black','I'='green','J'='firebrick'))

gg3+coord_cartesian(xlim=c(0,3),ylim=c(0,5000))+geom_smooth()
#coord_cartesian:그림에서 x축,y축의 크기를 변경
#geom_smooth: 추세선을 추가한다.

gg3+coord_flip()   #x축과 y축을 변경

gg3+theme(plot.background=element_rect(fill="yellowgreen"),
          plot.margin=unit(c(3,2,3,2),"cm"))
#plot.margin:여백공간설정, 4개의 숫자는 각각 위,오른쪽,아래,왼쪽

p1<-gg3+geom_hline(yintercept=5000,size=2,
                   linetype="dotted",color="blue")
print(p1)

#ggplot2 그래프 응용 사례
# 1
options(scipen=999)
library(ggplot2)
theme_set(theme_bw())
data("midwest",package = "ggplot2")
head(midwest)
gg<-ggplot(midwest,aes(x=area, y=poptotal))+
  geom_point(aes(col=state, size=popdensity))+
  geom_smooth(method="loess",se=F)+  #smooth():추세선, se:표준요차
  xlim(c(0,0.1))+ylim(c(0,500000))+
  labs(subtitle="Area Vs Population", y="Population", x="Area",
       title="Scatterplot",caption="Source:midwest")
plot(gg)

# 2
library(ggplot2)
data(mpg,package="ggplot2")
theme_set(theme_bw())
g<-ggplot(mpg, aes(cty,hwy))
g + geom_point() +geom_smooth(method="lm",se=F)+
  labs(subtitle="mpg: city vs highway mileage", y="hwy",x="cty",
       title="Scatterplot with overlapping points",
       caption="Source: midwest")
