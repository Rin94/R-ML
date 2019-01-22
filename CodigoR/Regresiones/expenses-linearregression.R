insurance<-read.csv(file.choose(),stringsAsFactors = TRUE)

#gain information of the data
str(insurance)
summary(insurance)
summary(insurance$charges)#charges equals expenses
hist(insurance$charges)

#make correlation 
colnames(insurance)
cor(insurance[c("age","bmi","children","charges")])

#scatterplot matrix
pairs(insurance[c("age","bmi","children","charges")])
#aumented our dataset
install.packages("psych")
library(psych)
pairs.panels(insurance[c("age","bmi","children","charges")])

#made our model
ins_model<-lm(charges~age+children+bmi+sex+smoker+region,data=insurance)
ins_model
summary(ins_model)

#improving our model 
insurance$age2<-insurance$age^2
insurance$bmi30<-ifelse(insurance$bmi>=30,1,0)

ins_model2<-lm(charges~age+age2+children+bmi+sex+bmi30*smoker+region,data=insurance)

summary(ins_model2)
