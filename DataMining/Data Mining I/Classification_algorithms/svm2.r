set.seed(10111) #Establecemos la semilla

coordenadas <- matrix(rnorm(40), 20, 2) #creamos las cordenadas del plano

colnames(coordenadas) <- c("X1", "X2") #Nombres de las columnas del dataframe

y <- c(rep(-1, 10), rep(1, 10)) #Rango del eje Y

coordenadas[y == 1, ] <- coordenadas[y == 1, ] + 1

datos <- data.frame(coordenadas, y) #El data frame so ls cordenas estableciadas en la linea 2

ggplot(data = datos, aes(x = X1, y = X2, color = as.factor(y))) + geom_point(size = 6) + 
  theme_bw() + theme(legend.position = "none")


# Se convierte la variable respuesta a factor
datos$y <- as.factor(datos$y)

# Para que la función svm() calcule el Support Vector Classifier, se tiene
# que indicar que la función kernel es lineal.
modelo_svm <- svm(formula = y ~ X1 + X2, data = datos, kernel = "linear", cost = 10, 
                  scale = FALSE)

#Resumen del SVM
summary(modelo_svm)

# Índice de las observaciones que actúan como vector soporte
modelo_svm$index


plot(modelo_svm, datos)
