###Árboles de decición usando C5.0

###read the credit.csv dataset
credit<-read.csv(file.choose())

##check de columns of the data set, like age, housing, dependents
str(credit)

## check the features as factors
table(credit$checking_balance)
table(credit$savings_balance)
## check for distribution of some values
summary(credit$months_loan_duration)
summary(credit$amount)
## make y credit default<- factor (yes, no)
credit$default
credit$default <- factor(credit$default,
                               levels=c(1,2),
                               labels=c("No","Si"))

# credit table score
table(credit$default)

## Data preparation- creating random training and test datasets
set.seed(123)
train_sample<-sample(1000,900)
str(train_sample)

credit_train<-credit[train_sample,]
credit_test<-credit[-train_sample,]

# verify the dataset of train
prop.table(table(credit_train$default))

#veryfi the dataset of test
prop.table(table(credit_test$default))


##Training a model on the data

library(C50)

credit_model<-C5.0(credit_train[-17],credit_train$default,type="class")
credit_model ##gives info of the decision tree

summary(credit_model)

library(gmodels)
credit_pred<-predict(credit_model,credit_test)
CrossTable(credit_test$default,credit_pred, prop.chisq = FALSE,
           prop.c = FALSE, prop.r=FALSE, dnn=c("actual default", "predict default"))

##Improving performance

#using trials for punning the tree

model_2<-C5.0(credit_train[-17],credit_train$default,type="Class",trials = 10)
model_2
summary(model_2)

credit_boost<-predict(model_2,credit_test)

table(credit_boost)

CrossTable(credit_test$default, credit_boost, prop.chisq = FALSE, prop.c = FALSE,
           prop.r = FALSE,
           dnn=c("actual defaul", "predicted default"))


### mejorar un tipo de error que sea menos costoso, vamos a mejorar la specfificidad
matrix_dimensions<-list(c("no","yes"), c("no","yes"))
names(matrix_dimensions)<-c("predicted","actual")

matrix_dimensions

error_cost<-matrix(c(0,1,4,0),nrow = 2, dimnames = matrix_dimensions)

error_cost

credit_cost<-C5.0(credit_train[-17],credit_train$default,type="Class",costs=error_cost)
credit_cost_pred<-predict(credit_cost,credit_test)

matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions
error_cost <- matrix(c(0, 1, 1, 0), nrow = 2,
                     dimnames = matrix_dimensions)
error_cost
credit_cost <- C5.0(credit_train[-17], credit_train$default, trials = 8,
                    costs =error_cost)
credit_cost_pred<-predict(credit_cost,credit_test)

CrossTable(credit_test$default, credit_cost_pred, prop.chisq = FALSE, prop.c = FALSE,
           prop.r = FALSE,
           dnn=c("actual defaul", "predicted default"))

