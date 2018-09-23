rm(list=ls())

# 1
((0.7-0.6)/2)/((1-0.7)/(1000-5-1))

# 2

b  <- read.csv("~/Desktop/ITP 350/Cars93.txt", sep="")

library(dplyr)
library(rpart)
rpart_model <- rpart(Price~Horsepower+Weight, data=b, method="anova")
library(rpart.plot)
rpart.plot(rpart_model)

rpart_model2<-rpart(Price~., data=b, method="anova")
rpart.plot(rpart_model2)

# 3
c<-b%>%select(Price, MPG.city, MPG.highway, EngineSize, Horsepower, RPM, Rev.per.mile, Fuel.tank.capacity, Length, Width)
library(ggdendro)
hc<-hclust(dist(c))
ggdendrogram(hc)
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
A2Rplot(hc, k = 4, boxes = FALSE, col.up = "gray50", col.down = c("#FF6B6B", "#4ECDC4", "#556270", "#5DB944"))

# 4
c<-na.omit(c)
c<-scale(c)
head(c)
library(factoextra)
distance<-get_dist(c)
fviz_dist(distance, gradient=list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
k2<-kmeans(c, centers=2, nstart=25)
k3<-kmeans(c, centers=3, nstart=25)
k4<-kmeans(c, centers=4, nstart=25)
k5<-kmeans(c, centers=5, nstart=25)
p2<-fviz_cluster(k2, geom="point", data = c)
p3<-fviz_cluster(k3, geom="point", data = c)
p4<-fviz_cluster(k4, geom="point", data = c)
p5<-fviz_cluster(k5, geom="point", data = c)
library(gridExtra)
grid.arrange(p2, p3, p4, p5, nrow=2)
fviz_nbclust(c, kmeans, method="wss")
fviz_cluster(k4, data = c)