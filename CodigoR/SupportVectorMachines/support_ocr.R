letters<-read.csv(file.choose())
str(letters)
letters_train<-letters[1:16000,]
letters_test<-letters[16001:20000,]

library(e1071)
library(kernlab)
## suport vector m<-ksvm(target~predictors,data=mydata,kernel="rbfdot",c=1)

letter_model<-ksvm(letter~.,data=letters_train, kernel="vanilladot")
letter_model

letter_predictions<-predict(letter_model,letters_test)
letter_predictions <- predict(letter_model, letters_test)
head(letter_predictions)

#check other letters
table(letter_predictions, letters_test$letter)
agreement <- letter_predictions == letters_test$letter
table(agreement)
#accuracy table
prop.table(table(agreement))


### improving the model, witho other kernel

letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train,
                              kernel = "rbfdot")
letter_predictions<-predict(letter_classifier_rbf,letters_test)
table(letter_predictions, letters_test$letter)
agreement <- letter_predictions == letters_test$letter
table(agreement)
