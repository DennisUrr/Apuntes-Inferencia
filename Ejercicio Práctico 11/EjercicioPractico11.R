# Grupo 3
# Integrantes:  Ignacio Barahona    - 20.053.896-k
#               Hernan Pinochet     - 20.186.654-5
#               Dennis Urrutia      - 20.054.672-5

#Se importan librerias a utilizar
library(ggplot2)
library(lmtest)
library(psych)
library(corrplot)
library(gridExtra)
library(car)
library(dplyr)
library(ggpubr)


#Ejercicio practico 11
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


# Lectura de archivo ------------------------------------------------------

datos.todos <- read.csv2(
  file = "C:\\Users\\ohcan\\OneDrive\\Escritorio\\Usach 1-2021\\Inferencias y modelos estadisticos\\Ejercicio practico 11\\body.csv"
)


# Seleccion de muestra ----------------------------------------------------

set.seed(199902)
# Tomamos la  muestra con 50 mujeres ya que la semilla es par.
# Asumiendo que Gender == 0, corresponde a mujeres.
i <- which(datos.todos[["Gender"]] == 0)
N <- 50
i <- sample(i, N)
datos <- datos.todos[c(i), ]


# Condiciones para uso de RLM
# 1. Las variables predictoras deben ser cuantitativas o dicotómicas
#   -> Esta condicion se comprueba ya que como se mostrara en el desarrollo
#   -> todas las variables son cuantitativas.
# 2. La variable de respuesta debe ser cuantitativa y continua, sin 
# restricciones para su variabilidad.
#   -> Esta condicion se comprueba ya que la varoable de respuesta cumple con
#   -> las condiciones mencionadas.
# 3. Los predictores deben tener algún grado de variabilidad. Esto significa 
# que su varianza no debe ser igual a cero. En otras palabras, no pueden ser
# constantes.
# 4. No debe existir multicolinearidad. Esto significa que no deben existir 
# relaciones lineales fuertes entre dos o más predictores (coeficientes de 
# correlación altos).
#   -> Esta se comprueba observando la matriz de correlacion realizada y
#   -> comparando las variables predictoras obtenidas en el modelo final.
#   -> (no existen coeficientes altos entre ellas)
# 5. Los residuos deben ser homocedásticos (con varianzas similares) para cada 
# nivel de los predictores.
#   -> Dado el grafico presentado de los residuos se puede aceptar esta
#   -> condicion.
# 6. Los residuos deben seguir una distribución cercana a la normal centrada en 
# cero.
#   -> Dado el test de shapiro realizado se cumple esta condicion ya que se
#   -> obtiene p = 0.414 y se acepta la normalidad con 90 % de confianza.
# 7. Los valores de la variable de respuesta son independientes entre sí.
# 8. Cada predictor se relaciona linealmente con la variable de respuesta.



# Desarrollo --------------------------------------------------------------

# Matriz de correlacion
cor.matriz <- cor(datos)
gra1 <- corrplot(cor.matriz, method='number',type='upper')

# Ajustar modelo. Se realizan distintos modelos, en donde cada uno aumenta
# su capacidad predictiva, esto debido a la eliminacion de variables con poca
# significacion.

modelo <- lm(Height ~ ., data = datos)
# De este modelo se logro identificar 5 variables predictoras con alta 
# significacion, estas son Biacromial.diameter, Biiliac.diameter, Knees.diameter
# Waist.Girth y Hip.Girth

modelo1 <- lm(Height ~ Biacromial.diameter + Biiliac.diameter + Knees.diameter + Waist.Girth + Hip.Girth, data = datos)
# De este segindo modelo se logro descartar 1 variable predictora con baja 
# significacion la cual fue Knees.diameter.
cat("##### Summary modelo 1 #####")
print(summary(modelo1))

modelo2 <- lm(Height ~ Biacromial.diameter + Biiliac.diameter + Waist.Girth + Hip.Girth, data = datos)
# Con este utimo modelo, se logra comprobar que no existe variacion en la
# significancia de las variables predictoras, por lo que este seria el modelo
# definitivo.
cat("##### Summary modelo 2 #####")
print(summary(modelo2))

# Luego de realizar los distintos modelos, se logra identificar que el mejor
# modelo encontrado es:

#    Heigth = 70.3965 + 1.6328*Biacromial.diameter + 0.8296*Biiliac.diameter - 0.6181*Waist.Girth + 0.5957*Hip.Girth

# Tambien destacar que el coeficiente de determinacion (R-square) aumenta, en 
# cada iteracion, podemos confirmar que el modelo va mejorando.

# Residuos

# Graficar los residuos .
b_4 <- modelo2$coefficients [5]
b_3 <- modelo2$coefficients [4]
b_2 <- modelo2$coefficients [3]
b_1 <- modelo2$coefficients [2]
b_0 <- modelo2$coefficients [1]
residuos <- datos[["Height"]] - (b_0 + b_1 * datos[["Biacromial.diameter"]] + b_2 * datos[["Biiliac.diameter"]] + b_3 * datos[["Waist.Girth"]] + b_4 * datos[["Hip.Girth"]]) 
datos2 <- data.frame(datos , residuos)

r <- ggscatter(
  datos2 , x = "Height", y = "residuos",
  color = " blue ", fill = " blue ", xlab = "Height",
  ylab = "Residuo",
  title = "Grafico Residuos")

r <- r + geom_hline( yintercept = 0, colour = "red")
print (r)

# Verificar normalidad de los residuos .
cat("###Prueba de normalidad para los residuos### \n")
print(shapiro.test(datos2$residuos))
# obteniendo un p = 0.414, por ende se concluye con 90% de
# confianza que sigue una distribucion cercana a la normal.

# Evaluar poder predictivo utilizando evaluacion cruzada ------------------
cat("##### Evaluar poder predictivo utilizando evaluacion cruzada #####")
# Crear conjuntos de entrenamiento y prueba .
set.seed (199902)
cantidad_datos <- nrow(datos)

n_entrenamiento <- floor (0.7 * cantidad_datos)
muestra <- sample.int(n = cantidad_datos, size = n_entrenamiento , replace = FALSE )
entrenamiento <- datos[ muestra, ]
prueba <- datos[- muestra, ]

# Ajustar modelo con el conjunto de entrenamiento .
modelo3 <- lm(Height ~ Biacromial.diameter + Biiliac.diameter + Waist.Girth + Hip.Girth , data = entrenamiento )
print (summary(modelo3))

# Calcular error cuadrado promedio para el conjunto de entrenamiento.
mse_entrenamiento <- mean( modelo3$residuals ** 2)
cat("MSE para el conjunto de entrenamiento :", mse_entrenamiento , "\n")

# Hacer predicciones para el conjunto de prueba .
predicciones <- predict(modelo3, prueba )

# Calcular error cuadrado promedio para el conjunto de prueba.
error <- prueba[["Height"]] - predicciones
mse_prueba <- mean( error ** 2)
cat ("MSE para el conjunto de prueba :", mse_prueba , "\n")
cat ("Diferencia de MSE:",abs(mse_entrenamiento - mse_prueba) , "\n")


# Luego de analizar la diferencia de los errores cuadrados medios de las
# muestras de entrenamiento y prueba se determina que el modelo no es optimo
# pero esta en el rango aceptable, con una puntuacion de 6.93976 entre ambos MSE.



