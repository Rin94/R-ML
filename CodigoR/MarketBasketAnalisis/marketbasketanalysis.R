#Market Basket Analysis- tecnica recomendada para citas, dating sites, finding dangeous among medications
#9832 TRANSACTIONES (30 TRANSACTION PER HOUR), SE REMOVI?? EL VALOR DE MARCA.
#M??s granularidad,m??s datos
#cada transacci??n cada liea

#Matriz esparcida la mayoria de las transacciones tiene 0

install.packages("arules")

library("arules")

groceries <- read.transactions("groceries.csv",sep=",")#entienda que es una base de datos transaccional, separacion por comas
#densidad cuantos unos hay
#mean numero de transacciones, min 1, max 32 (n??mero de transacci??es) 
#guardad unos o cero
summary(groceries)

inspect(groceries[1:5])
itemFrequency(groceries[,1:3])
itemFrequencyPlot(groceries, support=0.2)#aparezca el 10% de las observaciones
itemFrequencyPlot(groceries, topN=20)
image(groceries[1:5])#x articulos y transacciones
image(sample(groceries, 100))

#apriori
apriori(groceries)

groceryrules<-apriori(groceries, parameter = list(support=0.006, confidence=0.25, minlen=2))
groceryrules
summary(groceryrules)
inspect(groceryrules[1:100])#mostrar las reglas del 1 a la especificada
#se debe de buscar mucho lift para ver que no sea coincdencia

inspect(sort(groceryrules,by="lift")[1:10])#ordenar por lift


#funcion subset() para objetos que queramos juntos

berryrules<-subset(groceryrules, items %in% "berries")

inspect(berryrules)

milkrules<-subset(groceryrules, items %in%c ("whole milk",'berries'))

inspect(sort(milkrules,by="lift")[1:30])
#guardar resultados

write(groceryrules, file = "groceryrules.csv",sep=",", quote=TRUE, row.names=FALSE)