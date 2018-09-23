#1
data<-rnorm(100, 1.5, 2)
dataframe<-data.frame(data)
install.packages("ggplot2")
library(ggplot2)
ggplot(dataframe, aes(data))+geom_histogram(binwidth = 0.5)+xlim(-4, 10)+ggtitle("Yuchen Shi, March 5th, 2018")

#2
install.packages("dplyr")
library(dplyr)
dataframe%>%filter(data>0)%>%count()
1-pnorm(0, 1.5, 2)

#3
sampleMeans=c()
for (i in 1:100){
  sampleMeans[i]=mean(rnorm(20, 1.5, 2))
}
dataframe2<-data.frame(sampleMeans)
ggplot(dataframe2, aes(sampleMeans))+geom_histogram(binwidth=0.5)+xlim(-4, 10)+ggtitle("Yuchen Shi, March 5th, 2018")
dataframe2%>%filter(sampleMeans>0)%>%count()
1-pnorm(0, 1.5, 2/sqrt(20))

#5
data2<-rcauchy(100, location=1.5, scale=2)
dataframe4<-data.frame(data2)
ggplot(dataframe4, aes(data2))+geom_histogram(binwidth = 0.5)+xlim(-4, 10)+ggtitle("Yuchen Shi, March 5th, 2018")

dataframe4%>%filter(data2>0)%>%count()
1-pcauchy(0, location=1.5, scale=2)

sampleMeans2=c()
for (i in 1:100){
  sampleMeans2[i]=mean(rcauchy(20, location=1.5, scale=2))
}
dataframe5<-data.frame(sampleMeans2)
ggplot(dataframe5, aes(sampleMeans2))+geom_histogram(binwidth = 0.5)+xlim(-4, 10)+ggtitle("Yuchen Shi, March 5th, 2018")
dataframe5%>%filter(sampleMeans2>0)%>%count()

1-pcauchy(0, location=1.5, scale=2)