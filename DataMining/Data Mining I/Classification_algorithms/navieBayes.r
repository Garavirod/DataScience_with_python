library(e1071)
library(caret)
#Caragmos el datast
divorcios <- read.csv("../rodrigo/Documentos/Mineria de Datos/DataScience/DataSets/DivorciosDataset.csv")
#Generamos la semilla
set.seed(2018)
#Establecemos nuestro conjunto de entrenamiento como el 80% de los datos
t.ids <- createDataPartition(divorcios$marital_status, p=0.80, list = F)

#El odelo me dará las probabilidades apriori de que una persona esté casad, divrociada etc..
modelo <- naiveBayes(marital_status ~ ., data = divorcios[t.ids,])

modelo
#HAcemos una predicción usando el modelo y el resto de varibales 
prediction <- predict(modelo,divorcios[-t.ids,])

#Mostramos el resultado con una matriz de confusión, es decir cunatas predicciones se aciertan o cuantas se fallan
tab <- table(divorcios[-t.ids,]$marital_status, prediction,dnn=c("Actual","Predicha"))
confusionMatrix(tab)
