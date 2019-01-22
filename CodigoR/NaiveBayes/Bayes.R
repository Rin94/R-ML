#Bayes-Naive Bayes--- hagamos un resumen de la parte teoríca
sms_raw<-read.csv(file.choose(),stringsAsFactors = FALSE)

# Info of dataset
str(sms_raw)

sms_raw$type<-factor(sms_raw$type)

##convertir a factor

sms_raw$type<-factor(sms_raw$type)

str(sms_raw$type)

table(sms_raw$type)


### Data preparation - cleaning 

install.packages("tm")

library(tm)

##corpus coleccion de documentos de textos, 
sms_corpus<-VCorpus(VectorSource(sms_raw$text))

print(sms_corpus)

# visualizar los datos del corpus
inspect(sms_corpus[1:2])

as.character(sms_corpus[[1]])

## visualizar más de dos documentos usar la funcion lappy

lapply(sms_corpus[1:2], as.character)

## Limpiar el corpus, transformar los datos a minusculas

sms_corpus_clean<-tm_map(sms_corpus, content_transformer(tolower))

as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

## remover numeros
sms_corpus_clean<-tm_map(sms_corpus_clean,removeNumbers)

## Our next task is to remove filler words such: to, and, but, or from our
#SMS message. These terms are known as STOP WORDS, cuz do not provide much
#useful inofrmation and last remove these words

sms_corpus_clean<-tm_map(sms_corpus_clean,removeWords,stopwords())

## Next remove any punctiation from the text messages

sms_corpus_clean<-tm_map(sms_corpus_clean,removePunctuation)

removePunctuation("hello...,...world")

## Reducing words to ther root by the process called STEMMING

install.packages("SnowballC")
library(SnowballC)

wordStem(c("learn", "learned", "learning","learns"))
wordStem(c("videogame","game","gaming","gamer"))

sms_corpus_clean<-tm_map(sms_corpus_clean, stemDocument)

## REMOVE white_spaces

sms_corpus_clean<-tm_map(sms_corpus_clean, stripWhitespace)

print(as.character(sms_corpus[1:3]))
as.character(sms_corpus[1:3])
print(as.character(sms_corpus_clean[1:3]))
as.character(sms_corpus_clean[1:3])

### Data preparation

##Now that the data are processed to our liking, the final step is to split
#the messages into invidual components through a process called tokenization

#Tokenizer

#rows: documents, columns:words

#hace una columna de cuantas veces se muestra una palabra

sms_dtm<-DocumentTermMatrix(sms_corpus_clean)
sms_dtm2<-DocumentTermMatrix(sms_corpus,
                             control = list(tolower=TRUE,
                                            removeNumbers=TRUE,
                                            stopwords=TRUE,
                                            removePunctuation=TRUE,
                                            stemming=TRUE))

sms_dtm

sms_dtm2

##the order of preparation matters


### Data preparation- creating training and test datasets

sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4170:5574,]

sms_train_labels<-sms_raw[1:4169,]$type
sms_test_labels<-sms_raw[4170:5574,]$type

prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

### Visualizing text data - WORD CLOUDS

install.packages("wordcloud")
library(wordcloud)

wordcloud(sms_corpus_clean,min.freq = 50, random.order = FALSE)
options(warnings=2) 

spam<-subset(sms_raw,type=="spam")
ham<-subset(sms_raw,type=="ham")

wordcloud(spam$text,max.words = 40,scale=c(3,0.5))
wordcloud(ham$text,max.words = 40, scale=c(3,0.5))

### Data preparation

## para hacerlo más limpio vamos a reducir el numero de caracteristicas, 
##quitando palabras que tengan menos de 5 menciones usando
#FINDFREQTERMS

findFreqTerms(sms_dtm_train,5)

sms_freq_words<-findFreqTerms(sms_dtm_train,5)

str(sms_freq_words)

sms_dtm_freq_train<-sms_dtm_train[,sms_freq_words]             
sms_dtm_freq_test<-sms_dtm_test[,sms_freq_words]

##hacer una funcion que nos diga si es spam o no

convert_counts<-function(x){
  x<-ifelse(x>0,"Yes","No")
}

sms_train<-apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test<-apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

install.packages("e1071")
library(e1071)

#building the classifier

sms_classifier<-naiveBayes(sms_train,sms_train_labels)

#making predictions

sms_tes_pred<-predict(sms_classifier,sms_test)

#example
library(gmodels)
CrossTable(sms_tes_pred,sms_test_labels,prop.chisq = FALSE, prop.t = FALSE,
           dnn = c("predicted","actual"))

## Mejorando con el laplace

sms_classifier2<-naiveBayes(sms_train,sms_train_labels,laplace = 1)
sms_tes_pred2<-predict(sms_classifier2,sms_test)

library(gmodels)
CrossTable(sms_tes_pred2,sms_test_labels,prop.chisq = FALSE, prop.t = FALSE,
           dnn = c("predicted","actual"))
