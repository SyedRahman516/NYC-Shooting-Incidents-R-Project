rm(list = ls())

Timeofday=read.csv(file.choose(),header=T)
attach(Timeofday)
names(Timeofday)
summary(Timeofday)

library(class)

standardized.Latitude=scale(Latitude) #Longitutde and Latitude is redundant but
#I kept it because I wanted to standardize it and boro is there to make sense out of the Long/Lat
standardized.Longitude=scale(Longitude)

Input.standard=cbind(standardized.Latitude,standardized.Longitude,as.factor(BORO))
#5 fold cross validation
accuracy=matrix(0,10,5)

set.seed(2)
folds=sample(1:5,nrow(Input.standard),replace=TRUE)
for (j in 1:10)
{
  for(i in 1:5)
  {
    train.standard=Input.standard[folds!=i,]
    test.standard=Input.standard[folds==i,]
    train.truevalue=Time_of_Day[folds!=i]
    test.truevalue=Time_of_Day[folds==i]
    
    knn.pred=knn(train.standard,test.standard,train.truevalue,k=j, use.all =  FALSE)#problem
    
    table(knn.pred,test.truevalue)
    accuracy[j,i]=mean(knn.pred==test.truevalue)
    
  }
}
cv.accuracy=apply(accuracy,1,mean)
which.max(cv.accuracy)
#KNN 1 is the highest accuracy at .52
#### Classification Tree
rm(list = ls())
TimeDay<-read.csv(file.choose(),header=T)
library(tree) 
names(TimeDay)
attach(TimeDay)
summary(TimeDay)

set.seed(1)
train=sample(nrow(TimeDay),nrow(TimeDay)*0.8)

tree.model=tree(as.factor(Time_of_Day)~ BORO+Longitude+Latitude,TimeDay,subset =train)#problem
TimeDay.test=TimeDay[-train,]
Time_of_Day.test=Time_of_Day[-train]


cv.model=cv.tree(tree.model,K=10,FUN=prune.misclass)
cv.model 

prune.model=prune.tree(tree.model,best=3)
plot(prune.model)
text(prune.model,pretty=0) 

prunetree.pred=predict(prune.model,TimeDay.test,type="class")

