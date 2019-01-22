####K-means###

###Read the SNS DATA FOR TEENAGERS 
sns<-read.csv("snsdata.csv")
str(sns)
#check missing values
table(sns$gender,useNA = "ifany")
summary(sns$age)#max age and min age are unreasonable for teenagers

###Data Preparation- dummy missing values

sns$female<-ifelse(sns$gender=="F" & !is.na(sns$gender),1,0)
sns$nogender<-ifelse(is.na(sns$gender),1,0)

table(sns$female)
table(sns$nogender)

###Data Preparation - Inputing missing values
sns$age<-ifelse(sns$age>=13&sns$age<20,sns$age,NA)
mean(sns$age, na.rm=TRUE)
#calcula la media de la edad por los valores de año
aggregate(data=sns,age~gradyear,mean,na.rm=TRUE)
#ave() regresa un vector con el grupo de medias repetidas, asi que el resultado
#es igual al vector original
ave_age<-ave(sns$age, sns$gradyear,FUN=function(x) mean(x,na.rm=TRUE))
ave_age
sns$age<-ifelse(is.na(sns$age),ave_age,sns$age)
summary(sns$age)

###Fit the data test
install.packages("stats")
library(stats)
interests<-sns[5:40]
interests
#normalize the interest-lapply returns matrix, as.data.frame convert to dataframe
interests_xyz<-as.data.frame(lapply(interests,scale))
interests_xyz
set.seed(2345)
sns_clusters<-kmeans(interests_xyz,5)

###Evaluate model performance
sns_clusters$size

sns_clusters$centers

sns_clusters$cluster
sns$cluster_id<-sns_clusters$cluster

###improving model performance

sns[1:5,c("cluster_id","gender","age","friends")]

aggregate(data=sns,age~cluster_id,mean)
aggregate(data=sns,female~cluster_id,mean)

aggregate(data=sns,friends~cluster_id,mean)
