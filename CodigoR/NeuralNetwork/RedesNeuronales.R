#Cargando los datos
concrete<-read.csv("concrete.csv")
str(concrete)
#Vamos a predecir la fortaleza del semento, la mayoria son numericos
#Vamos a normalizar de o a 1 Usando la funcion de minimos y maximos

normalize<-function(x){
  return ((x-min(x))/ (max(x)- min(x)))
  }

concrete_norm<-as.data.frame(lapply(concrete, normalize))
#lappy nos evita usar ciclo, donde le decimos que todas las columnas, le aplica la funci??n normalize

#Comprobamos si estan normalizados
summary(concrete_norm $ strength)
#Comprobamos con los valores normales
summary(concrete $ strength)

concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]
install.packages("neuralnet")
library(neuralnet)
set.seed(12354)

#m<- neuralnet(target-predictors, data= mydata, hidden=1)
concrete_model<-neuralnet(strength ~ cement +slag + ash + water + superplastic + coarseagg + fineagg +age, data=concrete_train)

plot(concrete_model, rep="best")

#Evaluar el desempenio del modelo
#Pasarle nuestro modelo, con muestros datos de prueba, la parte es la comlumna, no se pasa la columna 9
model_result<-compute(concrete_model, concrete_test[1:8])

#vamos a guardar los resultados de nuestra prediccion
predicted_strength<-model_result$net.result

#Correlacion para no clasificacion
cor(predicted_strength,concrete_test$strength)[,1]


concrete_model2<-neuralnet(strength ~ cement +slag + ash + water + superplastic + coarseagg + fineagg +age, data=concrete_train,hidden=5)
plot(concrete_model2, rep="best")

model_result2<-compute(concrete_model2, concrete_test[1:8])
predicted_strength2<-model_result2$net.result
cor(predicted_strength2,concrete_test$strength)
