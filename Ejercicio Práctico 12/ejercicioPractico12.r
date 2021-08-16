# Ejercicio practico 12: Regresion Logistica
# Grupo 3
# Integrantes:  Ignacio Barahona    - 20.053.896-k
#               Hernan Pinochet     - 20.186.654-5
#               Dennis Urrutia      - 20.054.672-5

library(ggplot2)
library(lmtest)
library(psych)
library(corrplot)
library(gridExtra)
library(dplyr)
library(ggpubr)
library(lattice)
library(caret)
library(car)
library(pROC)

# Se obtiene el directorio
dir <- "C://Nano//Universidad//Semestre Actual//IME//Tareas//Practico 12//Phishing.csv"
file <- file.path(dir)

# Se lee el archivo como csv
phishing <- read.csv(file = file, encoding = "UTF-8")

# Se carga la semilla
seed <- 199902
sample_size <- 500

poblation_size <- nrow(phishing)

set.seed(seed)


# Cargar los datos.
sample <- phishing[sample(1:poblation_size, sample_size), ]

# Matriz de correlación
cor_matrix <- cor(sample)
gra1 <- corrplot(cor_matrix, method='number',type='upper')

# Se obtiene la primera fila de la matriz
correlations <- sort(abs(cor_matrix[1,]))

# Se tomaran las últimos 5 variables puesto que son las que tienen una mayor correlación
# con la variable de respuesta

ip_address <- factor(sample$having_IP_Address)
sample$having_IP_Address <- NULL
sample <- cbind(ip_address, sample)

# Separar conjuntos de entrenamiento y prueba.
n <- nrow(sample)
n_training <- floor(0.8 * n)
sample_model <- sample.int(n = n, size = n_training , replace = FALSE)
training <- sample[sample_model , ]
test <- sample[-sample_model , ]

# Ajustar modelo nulo.
null <- glm(ip_address ~ 1,
           family = binomial(link = "logit"),
           data = training)

# Ajustar modelo completo.
cat("\n\n")
complete <- glm(ip_address ~ Shortining_Service + Links_pointing_to_page + HTTPS_token + double_slash_redirecting + Abnormal_URL + Redirect + popUpWidnow,
                family = binomial(link = "logit"),
                data = training)

# Ajustar modelo con regresión escalonada.
cat("Modelo con regresión escalonada\n")
cat("--------------------------------------\n")
best <- step(null ,
            scope = list(lower = null , upper = complete),
            direction = "both",
            trace = 0)

print(summary(best))

# Se obtiene el mejor modelo posible con las variables elegidas
best1 <- glm(ip_address ~ Redirect + Links_pointing_to_page + HTTPS_token,
            family = binomial(link = "logit"),
            data = training)

print(summary(best1))

# ROC

# Evaluar el modelo con el conjunto de entrenamiento.
cat("Evaluación del modelo a partir del conjunto de entrenamiento :\n")
prediction <- predict(best1 , training , type = "response")

umbral <- 0.5
prediction_e <- sapply(prediction, function(p) ifelse(p >= umbral , "1", "0"))
prediction_e <- factor(prediction_e, levels = levels(sample[["ip_address"]]))

ROC_e <- roc(training[["ip_address"]], prediction)
plot(ROC_e)

matriz_e <- confusionMatrix(prediction_e, training[["ip_address"]])
print(matriz_e)

# Evaluar el modelo con el conjunto de prueba.
cat("Evaluación del modelo a partir del conjunto de prueba :\n")
probs_p <- predict(best1 , test , type = "response")

prediction_p <- sapply(probs_p, function(p) ifelse(p >= umbral , "1", "0"))
prediction_p <- factor(prediction_p, levels = levels(sample[["ip_address"]]))

ROC_p <- roc(training[["ip_address"]], prediction)
plot(ROC_p)

matriz_p <- confusionMatrix(prediction_p, test[["ip_address"]])
print(matriz_p)

#====== Se verifican condiciones =====#

# Se realiza un test de anova para saber que tan bueno es el modelo.
cat("\n\n")
cat("Likelihood Ratio Test para los modelos\n")
cat("--------------------------------------\n")
print(anova(best1, test = "LRT"))


# Verificación de multicolinealidad.
cat("Verificación de colinealidad\n")
cat("--------------------------------------\n")
cat("\nVIF:\n")
vifs <- vif(best)
print(vifs)
cat("\nPromedio VIF: ")
print(mean(vifs))

# Al ser Vif = 1.035642 > 1 y <5, existe una pequeña colinealidad, por lo que estaría correcto el modelo

# Independencia de los residuos.
cat("Verificación de independencia de los residuos\n")
cat("--------------------------------------\n")
print(durbinWatsonTest(best1 , max.lag = 5))

# Detectar posibles valores atípicos.
cat("Identificación de posibles valores atípicos\n")
cat("--------------------------------------\n")
plot(best1)


# Al ver los gráficos de valores atípicos, se puede observar que hay unos valores
# de residuos atípicos, sin embargo como no son tantos, no es un impedimento para 
# realizar una buena clasificación.

# Obtener los residuos y las estadísticas.
output <- data.frame(predicted.probabilities = fitted(best1))
output [["standardized.residuals"]] <- rstandard(best1)
output [["studentized.residuals"]] <- rstudent(best1)
output [["cooks.distance"]] <- cooks.distance(best1)
output [["dfbeta"]] <- dfbeta(best1)
output [["dffit"]] <- dffits(best1)
output [["leverage"]] <- hatvalues(best1)

# Evaluar residuos estandarizados que escapen a la normalidad.
# 95 % de los residuos estandarizados deberían estar entre
# -1.96 y 1.96, y 99 % entre -2.58 y 2.58.
suspects1 <- which(abs(output [["standardized.residuals"]]) > 1.96)
suspects1 <- sort(suspects1)
cat("\n\n")
cat("Residuos estandarizados fuera del 95 % esperado\n")
cat("------------------------------------------------\n")
print(rownames(training[suspects1, ]))

# Revisar casos con distancia de Cook mayor a uno.
suspects2 <- which(output [["cooks.distance"]] > 1)
suspects2 <- sort(suspects2)
cat("\n\n")
cat("Residuales con una distancia de Cook alta\n")
cat("-----------------------------------------\n")
print(rownames(training[suspects2, ]))

# Revisar casos cuyo apalancamiento sea más del doble
# o triple del apalancamiento promedio.
leverage.mean <- ncol(training) / nrow(sample)
suspects3 <- which(output [["leverage"]] > leverage.mean)
suspects3 <- sort(suspects3)
cat("\n\n")
cat("Residuales con levarage fuera de rango (> ")
cat(round(leverage.mean , 3), ")", "\n", sep = "")
cat("--------------------------------------\n")
print(rownames(training[suspects3, ]))

# Revisar casos con DFBeta >= 1.
suspects4 <- which(apply(output [["dfbeta"]] >= 1, 1, any))
suspects4 <- sort(suspects4)
names(suspects4) <- NULL
cat("\n\n")
cat("Residuales con DFBeta sobre 1\n")
cat("-----------------------------\n")
print(rownames(training[suspects4, ]))

# Detalle de las observaciones posiblemente atípicas.
suspects <- c(suspects1, suspects2, suspects3, suspects4)
suspects <- sort(unique(suspects))
cat("\n\n")
cat("Casos sospechosos\n")
cat("-----------------\n")
print(training[suspects , ])
cat("\n\n")
print(output[suspects , ])


sample <- phishing[sample(1:poblation_size, sample_size), ]
# Separar conjuntos de entrenamiento y prueba.
n <- nrow(sample)
n_training <- floor(0.8 * n)
sample_model <- sample.int(n = n, size = n_training , replace = FALSE)
training <- sample[sample_model , ]
test <- sample[-sample_model , ]

# Evaluar poder predictivo utilizando evaluación cruzada ------------------
cat("##### Evaluar poder predictivo utilizando evaluación cruzada ##### \n")

# Calcular error cuadrado promedio para el conjunto de entrenamiento.
mse_training <- mean( complete$residuals ** 2)
cat("MSE para el conjunto de entrenamiento :", mse_training , "\n")

# Hacer predicciones para el conjunto de prueba .
predictions <- predict(complete, test)

# Calcular error cuadrado promedio para el conjunto de prueba.
error <- test[["having_IP_Address"]] - predictions
mse_test <- mean( error ** 2)
cat ("MSE para el conjunto de prueba :", mse_test , "\n")
cat ("Diferencia de MSE:",abs(mse_training - mse_test) , "\n")

# Luego de analizar la diferencia de los errores cuadrados medios de las
# muestras de entrenamiento y prueba se determina que el modelo no es optimo
# pero esta en el rango aceptable, con una puntuación de 5.959824 entre ambos MSE.

# Por último se puede concluir que se tiene un modelo bueno pero se podría mejorar
# utilizando otros predictores o normalizando estos.


