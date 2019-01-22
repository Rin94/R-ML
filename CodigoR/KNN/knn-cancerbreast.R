## read the csv
wbcd<-read.csv(file.choose(),stringsAsFactors = FALSE)
##Str as describe on python
str(wbcd)
#drop the first column id
wbcd<-wbcd[-1]
# we are searching the diagnosis of cancer
table(wbcd$diagnosis)

wbcd$diagnosis<-factor(wbcd$diagnosis, levels=c("B","M"), labels = c("Benign","Malignant"))

#buscar porcentajes de  las masas

round(prop.table(table(wbcd$diagnosis))*100,digits = 1)

#look for some characteristics
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])
#como vemos, el radio tiene medidas muy diferentes entre el area y el smoothnees en tamaño
#por eso debemos de normalizar los datos

normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

#hacer pruebas en nuestra funcion de normalización
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))
normalize(c(100,200,300,400,500))
#normalizar el dataset, con la funcion lapply y regresarlo como dataframe
wbcd_n<-as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

#data preparation @[row,column]
wbcd_train<-wbcd_n[1:469, ]
wbcd_test<-wbcd_n[470:569,]

#labels data preparations
wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]

#training a model on the data

#install packages where is the knn algorithm
install.packages("class")

library("class")

#aplicar el algoritmo knn
# knn buen numero de vecinos es la raiz cuadrada total de los datos sqrt(469)
wbcd_pred<-knn(train=wbcd_train,test=wbcd_test, cl=wbcd_train_labels,k=21)

install.packages("gmodels")
library(gmodels)

#ver el performance del algoritmo
CrossTable(x=wbcd_test_labels,y=wbcd_pred,prop.chsq=FALSE)

#improving algorithm

#using z score instead of normalization

wbcd_z<-as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

wbcd_train<-wbcd_z[1:469,]
wbcd_test<-wbcd_z[470:569,]
wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]

wbcd_tes_pred<-knn(train = wbcd_train,test = wbcd_test, cl=wbcd_train_labels,k=21)
CrossTable(x=wbcd_test_labels,y=wbcd_tes_pred,prop.chisq = FALSE)
