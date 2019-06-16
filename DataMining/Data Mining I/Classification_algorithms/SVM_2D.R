# Se eliminan todas las variables para evitar conflictos
rm(list = ls())
# Descargan los datos. Requiere conexión a internet
load(url("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/ESL.mixture.rda"))

datos <- data.frame(ESL.mixture$x, y = ESL.mixture$y)
# Al tratarse de un problema de clasificación se convierte la variable
# respuesta en factor
datos$y <- as.factor(datos$y)
head(datos)

ggplot(data = datos, aes(x = X1, y = X2, color = y)) + geom_point(size = 2.5) + 
  theme_bw() + theme(legend.position = "none")

library(e1071)
# Como los datos se han simulado en una misma escala, no es necesario
# estandarizarlos si no fuese así, es muy importante hacerlo.
set.seed(1)
svm_cv <- tune("svm", y ~ X1 + X2, data = datos, kernel = "radial", ranges = list(cost = c(0.001, 
                                                                                           0.01, 0.1, 1, 5, 10, 20), gamma = c(0.5, 1, 2, 3, 4, 5, 10)))

ggplot(data = svm_cv$performances, aes(x = cost, y = error, color = as.factor(gamma))) + 
  geom_line() + geom_point() + labs(title = "Error de clasificación vs hiperparámetros C y gamma", 
                                    color = "gamma") + theme_bw() + theme(legend.position = "bottom")


svm_cv$best.parameters

modelo_svm_rbf <- svm_cv$best.model

#Visualizacion del calsificador

# Se interpolar puntos dentro del rango de los dos predictores X1 y X2.
# Estos nuevos puntos se emplean para predecir la variable respuesta acorde
# al modelo y así colorear las regiones que separa el hiperplano.

# Rango de los predictores
rango_X1 <- range(datos$X1)
rango_X2 <- range(datos$X2)

# Interpolación de puntos
new_x1 <- seq(from = rango_X1[1], to = rango_X1[2], length = 75)
new_x2 <- seq(from = rango_X2[1], to = rango_X2[2], length = 75)
nuevos_puntos <- expand.grid(X1 = new_x1, X2 = new_x2)

# Predicción según el modelo de los nuevos puntos
predicciones <- predict(object = modelo_svm_rbf, newdata = nuevos_puntos)

# Se almacenan los puntos predichos para el color de las regiones en un
# dataframe
color_regiones <- data.frame(nuevos_puntos, y = predicciones)

ggplot() + # Representación de las 2 regiones empleando los puntos y coloreándolos
  # según la clase predicha por el modelo
  geom_point(data = color_regiones, aes(x = X1, y = X2, color = as.factor(y)), 
             size = 0.5) + # Se añaden las observaciones
  geom_point(data = datos, aes(x = X1, y = X2, color = as.factor(y)), size = 2.5) + 
  # Se identifican aquellas observaciones que son vectores soporte
  geom_point(data = datos[modelo_svm_rbf$index, ], aes(x = X1, y = X2, color = as.factor(y)), 
             shape = 21, colour = "black", size = 2.5) + theme_bw() + theme(legend.position = "none")

