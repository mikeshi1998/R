rm(list=ls())

library(readxl)
library(knitr)
a <- read_excel("~/Downloads/sr28abxl/ABBREV.xlsx")
kable(head(a,10))

library(dplyr)
category<-data.frame(do.call(rbind,strsplit(as.character(a$Shrt_Desc),",")))
View(category)
a$category <- category$X1
View(a)
c<- a%>% 
  select(Food=Shrt_Desc, category,
         Energ_Kcal, `Protein_(g)`,`Carbohydrt_(g)`, 
         `Fiber_TD_(g)`,`Sugar_Tot_(g)`, `Lipid_Tot_(g)`,
         `Cholestrl_(mg)`,`Sodium_(mg)`,`Potassium_(mg)`,
         Vit_A_IU,`Vit_C_(mg)`,`Calcium_(mg)`,`Iron_(mg)`)
View(c)

summary(c$Energ_Kcal)
plot(c$Energ_Kcal, ylab="Energy in kilocalories")
hist(c$Energ_Kcal, xlab="Energy in kilocalories")

c%>% 
  group_by(category)%>%
  summarise(AverageEnergy=mean(Energ_Kcal))%>%
  arrange(AverageEnergy)%>%
  head(10)

library(hexbin)
library(RColorBrewer)
lr1<-lm(c$Energ_Kcal~c$`Lipid_Tot_(g)`)
summary(lr1)

lr2<-lm(c$Energ_Kcal~c$`Carbohydrt_(g)`)
summary(lr2)

lr3<-lm(c$Energ_Kcal~c$`Protein_(g)`)
summary(lr3)

lr4<-lm(c$Energ_Kcal~c$`Sugar_Tot_(g)`)
summary(lr4)



x1<-hexbinplot(c$Energ_Kcal~c$`Lipid_Tot_(g)`,  type=c("r"), col.line = "red", lwd="3", xlab="Lipid(g)", ylab="Energy(Kcal)")
x2<-hexbinplot(c$Energ_Kcal~c$`Protein_(g)`, type=c("r"), col.line = "red", lwd="3", xlab="Protein(g)", ylab="Energy(Kcal)")
x3<-hexbinplot(c$Energ_Kcal~c$`Sugar_Tot_(g)`, type=c("r"), col.line = "red", lwd="3", xlab="Sugar(g)", ylab="Energy(Kcal)")
x4<-hexbinplot(c$Energ_Kcal~c$`Carbohydrt_(g)`,type=c("r"), col.line = "red", lwd="3", xlab="Carbohydrate(g)", ylab="Energy(Kcal)")
library(gridExtra)    
grid.arrange(x1,x2,x3,x4,nrow=2)

library(corrplot)
C<-cor(na.omit(c[3:15]))
corrplot(C, method="circle")


#protein source - meat 
meat<-c%>%filter(category==c("CHICKEN","LAMB","PORK","FISH","DUCK","BEEF"))
library(ggplot2)
library(reshape2)
meat.m<-melt(data=meat,id.vars = 'category',measure.vars = c("Protein_(g)","Lipid_Tot_(g)"))
ggplot(meat, aes(category))+geom_bar()
ggplot(meat, aes(category, meat$Energ_Kcal))+geom_violin()+ylab("Energy(Kcal)")
ggplot(meat.m)+geom_boxplot(aes(x=category,y=value,color=variable))+scale_y_continuous(limits=c(0,50))


#bread 
bread<-c%>%filter(category=="BREAD")
bread.m<-melt(data=bread,id.vars = 'category',measure.vars = c("Protein_(g)","Lipid_Tot_(g)","Carbohydrt_(g)"))
ggplot(bread.m)+geom_boxplot(aes(x=category,y=value,color=variable))

bread%>%ggplot(aes(x=`Carbohydrt_(g)`,y=Energ_Kcal))+geom_point()+geom_smooth(method="lm")
geom_text(aes(label=Food))

goodBread<-bread%>%arrange(`Carbohydrt_(g)`)%>%head(10)

#vegetable
cabbage<-c%>%filter(category=="CABBAGE")
celery<-c%>%filter(category=="CELERY")
broccoli<-c%>%filter(category=="BROCCOLI")
carrot<-c%>%filter(category=="CARROTS")
lettuce<-c%>%filter(category=="LETTUCE")
kale<-c%>%filter(category=="KALE")
spinach<-c%>%filter(category=="SPINACH")
cauliflower<-c%>%filter(category=="CAULIFLOWER")

vegetable<-rbind(cabbage, celery, broccoli, carrot, lettuce, kale, spinach, cauliflower)

vegetable.m<-vegetable%>%mutate(Fiber100mg=10*`Fiber_TD_(g)`)%>%melt(id.vars = 'category',measure.vars = c("Fiber100mg","Vit_C_(mg)"))
ggplot(vegetable.m)+geom_boxplot(aes(x=category,y=value,color=variable))

vegetable%>%ggplot(aes(x=vegetable$`Fiber_TD_(g)`, y=vegetable$`Vit_C_(mg)`))+geom_point()+geom_smooth(method="lm")+geom_text(aes(label=Abb2))+xlab("Fiber(g)")+ylab("Vitamin C(mg)")
geom_text(aes(label=Food))
goodVegetable<-vegetable%>%arrange(desc(`Vit_C_(mg)`))%>%head(10)


library(rpart)
rpart_model <- rpart(Energ_Kcal~`Lipid_Tot_(g)`+`Sugar_Tot_(g)`+`Carbohydrt_(g)`+`Protein_(g)`, data=a, method="anova")library(rpart.plot)
rpart.plot(rpart_model)


d<-na.omit(c)%>%select(Energ_Kcal, `Protein_(g)`, `Carbohydrt_(g)`, `Fiber_TD_(g)`, `Sugar_Tot_(g)`, `Lipid_Tot_(g)`)
library(factoextra)
fviz_nbclust(d, kmeans, method="wss")
k3<-kmeans(d, centers=3, nstart=25)
k4<-kmeans(d, centers=4, nstart=25)
p3<-fviz_cluster(k3, geom="point", data = d)
p4<-fviz_cluster(k4, geom="point", data = d)
grid.arrange(p3, p4, nrow=1)



