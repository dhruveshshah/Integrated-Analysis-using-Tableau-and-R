#Getting the data
titanic<-read.csv("titanic.csv")
dim(titanic)

#Replacing the missing values for age with their mean
meanAge<- sum(na.omit(titanic$Age))/length(na.omit(titanic$Age))
meanAge

titanic$Age[is.na(titanic$Age)]<-meanAge
titanic$Age<-round(titanic$Age)

#Adding Age Category Column
titanic$AgeCat[titanic$Age>=0 & titanic$Age<=16]<-"0-16"
titanic$AgeCat[titanic$Age>=17 & titanic$Age<=32]<-"17-32"
titanic$AgeCat[titanic$Age>=33 & titanic$Age<=48]<-"33-48"
titanic$AgeCat[titanic$Age>=49 & titanic$Age<=64]<-"49-64"
titanic$AgeCat[titanic$Age>=65]<-"65 and Above"

#Replacing integer values of  and with their meaningful labels
titanic$Survived[titanic$Survived==0]<-"Not Survived"
titanic$Survived[titanic$Survived==1]<-"Survived"

#Converting integer vectors into factor variables
titanic$Pclass<-factor(titanic$Pclass)
titanic$AgeCat<-factor(titanic$AgeCat)
titanic$Survived<-factor(titanic$Survived)
titanic$Embarked<-as.character(titanic$Embarked)
titanic$Embarked[titanic$Embarked=="S"]<-"Southampton"
titanic$Embarked[titanic$Embarked=="C"]<-"Cherbourg"
titanic$Embarked[titanic$Embarked=="Q"]<-"Queenstown"
titanic$Embarked<-factor(titanic$Embarked)

#Removing redundant variables
titanic<-titanic[c(-9,-11)]
str(titanic)
View(titanic)


write.csv(titanic,"C:\\Users\\Dhruvesh\\Desktop\\Data Visualization\\Kmean\\titanicNew.csv")

#Converting SibSp and Parch into categorical variables 
decision_tree<-titanic
SibSpCat=ifelse(decision_tree$SibSp >=3,">=3","<3")
decision_tree<-data.frame(decision_tree,SibSpCat)
decision_tree$SibSpCat<-as.factor(decision_tree$SibSpCat)
ParchCat=ifelse(titanic$Parch >=3,">=3","<3")
decision_tree<-data.frame(decision_tree,ParchCat)
decision_tree$ParchCat<-as.factor(decision_tree$ParchCat)

set.seed(1)
#Separating training and test data
test<-sample(1:nrow(decision_tree),nrow(decision_tree)/3)
test
train=-test
training_data=decision_tree[train,]
testing_survived=decision_tree$Survived[test]

#Plotting decision tree
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


tree_model=rpart(Survived ~ Pclass + Sex + AgeCat + Embarked + SibSpCat + ParchCat,
                 data=training_data,method="class",control=rpart.control(minsplit=10,cp=0.00))
fancyRpartPlot(tree_model,sub="decision_tree")
tree_predict=predict(tree_model,testing_data,type="class")

#Calculating the misclassification error
mean(tree_predict!=testing_survived)

