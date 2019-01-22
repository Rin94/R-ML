##How it works?
tee<-c(1,1,1,2,2,3,4,5,5,6,6,7,7,7,7)
at1<-c(1,1,1,2,2,3,4,5,5)
at2<-c(6,6,7,7,7,7)
bt1<-c(1,1,1,2,2,3,4)
bt2<-c(5,5,6,6,7,7,7,7)

sdr_a<-sd(tee)-(length(at1)/length(tee)*sd(at1)+length(at2)/length(tee)*sd(at2))
sdr_b<-sd(tee)-(length(bt1)/length(tee)*sd(bt1)+length(bt2)/length(tee)*sd(bt2))

## read the archive
wine<-read.csv(file.choose(),stringsAsFactors = TRUE)


## examine the archive 
str(wine)
summary(wine)
sum(is.na(wine))

## correlation
library(psych)
pairs.panels(wine[colnames(wine)])
cor(wine)

### hist
hist(wine$quality)

## make the split
set.seed(2)
train_sample<-sample(4898,3750)
train_sample
wine_train<-wine[train_sample,]
wine_test<-wine[-train_sample,]

str(wine_test)
str(wine_train)

## using rpart
library(rpart)
## m<-rpart(dv~iv, data=mydata)
#p<-predict(m,test,type="vector")
###vector->numeric values
###class->classification classes
###prob->predicted class probabilites
winemodel<-rpart(quality~.,data=wine_train)
winemodel

###Visualizing decision trees
library(rpart.plot)
rpart.plot(winemodel,digits = 3)
#extra parameters
rpart.plot(winemodel,digits=4,fallen.leaves = TRUE,type=3,extra = 101)

###Evaluating model performance
p.rpart<-predict(winemodel,wine_test)

summary(p.rpart)
summary(wine_test$quality)

cor(p.rpart,wine_test$quality)


#MAE Mean Absolute Error

MAE<-function(actual,predicted){
  mean(abs(actual-predicted))
}

MAE(p.rpart,wine_test$quality)

mean(wine_train$quality)

MAE(5.87,wine_test$quality)

library(RWeka)
