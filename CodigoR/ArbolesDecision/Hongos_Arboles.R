### Arboles de decision

#posion mushrooms clasificator

## 1 exploring an preparing the data
hongos<-read.csv(file.choose(),stringsAsFactors = TRUE)
str(hongos)## describe the types of data
hongos$veil_type<-NULL#By assigning null a vector, R eliminates the feature from the mushrooms dataframe

table(hongos$type)#Ver los tipos de hongos
summary(hongos)
tail(hongos)
## 2 Training a model on the data
set.seed(123)
train_sample<-sample(8124,5686)
str(train_sample)
hongos_train<-hongos[train_sample,]
hongos_test<-hongos[-train_sample,]

# verify the dataset of train
prop.table(table(hongos_train$type))

#veryfi the dataset of test
prop.table(table(hongos_test$type))

library(C50)

modelo_hongos<-C5.0(hongos_train[-1],hongos_train$type,type="class")

modelo_hongos

summary(modelo_hongos)

library(gmodels)

hongos_pred<-predict(modelo_hongos,hongos_test)

CrossTable(hongos_test$type,hongos_pred, prop.chisq = FALSE,
           prop.c = FALSE, prop.r=FALSE, dnn=c("actual default", "predict default"))


##install.packages("RWeka") #1Rule
#dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_152.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
#library(RWeka)

#hongos_1R<-OneR(type~.,data=hongos) 
#hongos_1R

## 3 Evaluar el performance del modelo

#summary(hongos_1R)

## 4 Mejorar el perfomance del modelo

#hongos_JRip<-JRip(type~ .,data=hongos )
#hongos_JRip

