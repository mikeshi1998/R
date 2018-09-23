rm(list=ls())

# Stage 1
sss1<-read.csv("~/Desktop/ITP 350/SampleSubmissionStage1.csv")
head(sss1)
(n<-dim(sss1)[1])
set.seed(202839)
sss1$Pred<-runif(n, 1/3, 2/3)
summary(sss1$Pred)
write.csv(sss1, "submission.csv", row.names=F)

# Visualizations
library(dplyr)
Teams<-read.csv("~/Desktop/ITP 350/DataFiles/Teams.csv")
a<-read.csv("~/Desktop/ITP 350/DataFiles/NCAATourneyDetailedResults.csv")
b<-inner_join(a, Teams, by=c("WTeamID"="TeamID"))%>%mutate(FieldGoalPercentage=WFGM/WFGA)%>%mutate(ThreePointersPercentage=WFGM3/WFGA3)%>%mutate(ScoreDifference=WScore-LScore)%>%group_by(TeamName)%>%summarize(AverageThreePointersMade=mean(WFGM3), AverageOffensiveRebounds=mean(WOR), AverageFieldGoalPercentage=mean(FieldGoalPercentage), AverageThreePointersPercentage=mean(ThreePointersPercentage))
c<-inner_join(a, Teams, by=c("WTeamID"="TeamID"))%>%group_by(TeamName)%>%count()%>%rename(NumberOfWins=n)
d<-inner_join(b, c, by="TeamName")%>%arrange(desc(NumberOfWins))
head(d)

e<-d%>%head(10)
library(ggplot2)
ggplot(e, aes(TeamName, NumberOfWins))+geom_bar(aes(reorder(TeamName, NumberOfWins)), stat="identity")+xlab("Team Name")+ylab("Number of Wins")+ggtitle("Number of Wins for the Top 10 Teams in NCAA Tourney")+coord_flip()

ggplot(d, aes(AverageThreePointersMade, NumberOfWins))+geom_point()+geom_smooth(method="lm")+ggtitle("Relationship Between Average Three Pointers Made and the Number of Wins in NCAA Tourney")

ggplot(d, aes(AverageOffensiveRebounds, NumberOfWins))+geom_point()+geom_smooth(method="lm")+ggtitle("Relationship between Average Offensive Rebounds and the Number of Wins in NCAA Tourney")

ggplot(e, aes(TeamName, AverageOffensiveRebounds))+geom_bar(aes(reorder(TeamName, -AverageOffensiveRebounds)), stat="identity")+xlab("Team Name")+ylab("Average Number of Offensive Rebounds")+ggtitle("Average Number of Offensive Rebounds for the Top 10 Teams in NCAA Tourney")

ggplot(d, aes(AverageFieldGoalPercentage, AverageThreePointersPercentage))+geom_point()+geom_smooth(method="lm")+ggtitle("Relationship between Field Goal Percentage and Three Pointers Percentage for the Winning Teams")

p1<-ggplot(e, aes(TeamName, AverageThreePointersPercentage))+geom_bar(aes(reorder(TeamName, -AverageThreePointersPercentage)), stat = "identity")+coord_flip()
p2<-ggplot(e, aes(TeamName, AverageFieldGoalPercentage))+geom_bar(aes(reorder(TeamName, -AverageFieldGoalPercentage)), stat = "identity")+coord_flip()
library(gridExtra)
grid.arrange(p1, p2, ncol=1)

f<-a%>%mutate(ScoreDifference=WScore-LScore)%>%inner_join(Teams, by=c("WTeamID"="TeamID"))%>%inner_join(e, by="TeamName")%>%select(TeamName, ScoreDifference)
ggplot(f, aes(TeamName, ScoreDifference))+geom_boxplot()+ggtitle("Score Differences in All Games Won")


