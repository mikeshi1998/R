rm(list=ls())
#1
library(rvest)
w1<-read_html("http://www.numbeo.com/cost-of-living/rankings.jsp")
t1<-html_nodes(w1, "table")
CoL<-html_table(t1[[3]], fill=TRUE)
View(CoL)
library(dplyr)
library(tidyr)
names(CoL)

CoL<-rename(CoL, place="City", col="Cost of Living Index", rent="Rent Index", colWrent="Cost of Living Plus Rent Index", grocs="Groceries Index", rest="Restaurant Price Index", ppi="Local Purchasing Power Index")

locale<-data.frame(do.call(rbind, strsplit(CoL$place, ",")))

CoL$metro<-as.character(locale$X1)
CoL$country<-as.character(locale$X2)
CoL$USA<-as.character(locale$X3)

CoL$country<-ifelse(nchar(CoL$country)==3, "USA", CoL$country)
CoL$country<-ifelse(CoL$place=="St. John's, NL, Canada", "Canada", CoL$country)
CoL$country<-ifelse(CoL$place=="Nanaimo, BC, Canada", "Canada", CoL$country)
CoL%>%filter(country=="USA")%>%count()

CoL$USA<-ifelse(CoL$country=="USA", "USA", "Non-USA")

library(ggplot2)
ggplot(CoL, aes(ppi, col))+geom_point(aes(color=USA, shape=USA))+geom_smooth(method="lm", aes(linetype=USA))

#2
library(corrplot)
C<-cor(CoL[, c(3:8)])
corrplot(C, method="circle")

#3
cities<-select(CoL, metro)
cities<-cities%>%mutate(characters=nchar(CoL$metro))
library(stringr)
cities<-cities%>%mutate(numOfe=str_count(tolower(CoL$metro), "e"))
cities<-cities%>%mutate(percentOfe=numOfe/characters)
View(cities)
ggplot(cities, aes(cities$characters, cities$percentOfe))+geom_point()+geom_smooth(method="lm")+xlab("Number of Characters")+ylab("Percentage of e characters")
ggplot(cities, aes(cities$characters, cities$percentOfe))+geom_point()+geom_smooth(method="loess")+xlab("Number of Characters")+ylab("Percentage of e characters")
library(acepack)
cor(cities$characters, cities$percentOfe)
cor(cities$characters, cities$percentOfe, method='spearman')
argmax=ace(cities$characters, cities$percentOfe)
cor(argmax$tx, argmax$ty)

#4
countries<-as.data.frame(CoL%>%select(country)%>%unique())
library(countrycode)
CoL$cntry_code<-countrycode(CoL$country, 'country.name', 'iso2c')
w2<-read_html("https://en.wikipedia.org/wiki/List_of_sovereign_states_and_dependent_territories_by_continent_(data_file)")
t2<-html_nodes(w2, "table")
id<-html_table(t2[[3]], fill=TRUE)
head(id<-as_tibble(id[, 1:2]))
id<-rename(id, cont_code="CC", cntry_code="a-2")
CoL<-CoL%>%inner_join(id, by="cntry_code")

CoL$cont_code<-ifelse(is.na(CoL$cont_code), "NA", CoL$cont_code)
ggplot(CoL, aes(ppi, col))+geom_point()+geom_text(aes(label=cntry_code, size=0.1))+geom_abline(slope=1, intercept=0)+facet_wrap(~cont_code)
