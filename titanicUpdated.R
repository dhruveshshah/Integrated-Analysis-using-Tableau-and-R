titanicNew<-read.csv("titanicNew.csv")
titanicUpdated<-titanicNew
SurvivedNum<-ifelse(titanicUpdated$Survived=="Not Survived",0,1)
titanicUpdated<-data.frame(titanicUpdated,SurvivedNum)

SexN<-ifelse(titanicUpdated$Sex=="male",1,0)
titanicUpdated<-data.frame(titanicUpdated,SexN)

EmbarkedN<-ifelse(titanicUpdated$Embarked=="Southampton",1,
                  ifelse(titanicUpdated$Embarked=="Cherbourg",2,0))
titanicUpdated<-data.frame(titanicUpdated,EmbarkedN)


write.csv(titanicUpdated,"titanicUpdated.csv")

titanic.scaled<-scale(data.frame(titanic$Age,titanic$Parch,
                                 titanic$SibSp,titanic$Fare))
colnames(titanic.scaled)
totwss<-vector()
btwss<-vector()
for(i in 2:15)
{
  set.seed(1234)
  temp<-kmeans(titanic.scaled,centers=i)
  totwss[i]<-temp$tot.withinss
  btwss[i]<-temp$betweenss
}

plot(totwss,xlab="Number of Cluster",type="b",
     ylab="Total Within Sum of Square")
plot(btwss,xlab="Number of Cluster",type="b",
     ylab="Total Between Sum of Square")

install.packages("Rserve")
library(Rserve)
Rserve()

