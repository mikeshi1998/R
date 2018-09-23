rm(list=ls())

#1 Probability
psurvive1=(4/6)*(4/6)
psurvive2=0.5*(3/6)*(3/6)+0.5*(5/6)*(5/6)
psurvive1
psurvive2
psurvive1<psurvive2

#2 Bayes
p50<-list(x=0.65, p=0.5)
p90<-list(x=0.75, p=0.9)
library(TeachBayes)
beta.select(p50, p90)
beta_draw(c(21.77, 11.88))
beta_interval(0.75, c(21.77, 11.88))

#3 Base R Plots

library(datasets)
View(USArrests)
View(state.abb)
data <- USArrests
data$abb <- state.abb
plot(data$Rape, USArrests$Murder, xlab="Rape Arrests (per 100,000)", ylab="Murder Arrests (per 100,000)")
abline(lsfit(data$Rape, data$Murder), col="red")
text(data$Rape, data$Murder, labels = data$abb)

#4 A Grammar of Graphics
mydata100 <- read.csv("~/Desktop/mydata100.csv")
mydata100 <- mydata100[complete.cases(mydata100),]
View(mydata100)
library(ggplot2)
ggplot(mydata100, aes(workshop, fill=workshop))+geom_bar()
ggplot(mydata100, aes(x=workshop, y=posttest))+geom_point()+geom_boxplot()
ggplot(mydata100, aes(pretest, posttest))+geom_point(aes(shape=gender))+geom_smooth(method="lm", aes(linetype=gender))
ggplot(mydata100, aes(pretest, posttest))+geom_point()+geom_smooth(method="lm")+facet_grid(gender~.)
