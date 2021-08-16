# Ejercicio practico 7: Anova para muestras independientes
# Grupo 3
# Integrantes:  Ignacio Barahona    - 20.053.896-k
#               Hernan Pinochet     - 20.186.654-5
#               Dennis Urrutia      - 20.054.672-5

#Se importan librerias a utilizar
library(ggpubr)
library(ggplot2)
library(ez)
library(dplyr)

#Ejercicio practico 7, desarrollo de problema A

################################################################################
# Problema A
# La avicultura de carne es un negocio muy lucrativo, y cualquier método que 
# ayude al rápido crecimiento de los pollitos es beneficioso, tanto para las 
# avícolas como para los consumidores no veganos. En el paquete datasets de R 
# (importado nativamente) est? el conjunto de datos chickwts con los resultados 
# de un experimento hecho (supuestamente en 1948) para medir la efectividad de 
# varios suplementos alimenticios en la tasa de crecimiento de las aves, en 
# donde pollitos recién nacidos se separaron aleatoriamente en seis grupos, y 
# a cada grupo se le dio un suplemento distinto por seis semanas. Se reportan 
# los pesos, en gramos, alcanzados por los pollitos. Para productores de la 
# 7? región, es especialmente importante saber si deberían usar suplementos 
# basados en linaza (linseed), soya (soybean), habas (horsebean) o carne 
# (meatmeal).
################################################################################
# Análisis del problema
# Al decir "es especialmente importante saber si deberían usar suplementos
# basados en", se espera encontrar si existe alguna diferencia en las medias
# de los pesos de los pollitos según el suplemento dado, si es así, cual es
# mas eficiente, es decir, cual de todas las medias es mayor(definiendo
# eficiencia como mayor peso).
################################################################################
cat("################################\nResolución problema A\n\n")

#Se cargan los datos asociados al estudio de los pollitos
pollitos <- chickwts

#Se deja la tabla pollitos con los 4 alimentos dados
pollitos <- filter(pollitos, pollitos$feed %in% c("linseed", "soybean", "horsebean", "meatmeal"))

# Para poder realizar un procedimiento Anova es necesario cumplir ciertas
# condiciones
#   1. La escala con que se mide la variable dependiente tiene las propiedades 
#   de una escala de intervalos iguales.
#   2. Las k muestras son obtenidas de manera aleatoria e independiente desde
#   la(s) población(es) de origen.
#   3. Se puede suponer razonablemente que la(s) población(es) de origen 
#   sigue(n) una distribución normal.
#   4. Las k muestras tienen varianzas aproximadamente iguales.

# LA PRIMERA CONDICION se verifica ya que la variable dependiente (weight) efectivamente
# se encuentra en intervalos de igual tamaño, específicamente en intervalos
# de 1 gramo.

# LA SEGUNDA CONDICION se verifica debido al proceso de realización del 
# estudio, explicado en el enunciado, ya que separaron de forma aleatoria la
# población en 4 grupos y a cada grupo se aplico un distinto suplemento.

# LA TERCERA CONDICION se debe realizar un gráfico Q-Q para determinar este
# supuesto

Grafico_QQ <- ggqqplot(data = pollitos,
                       x = "weight",
                       color = "blue",
                       title = "Gr?fico Q-Q pollitos")
print(Grafico_QQ)

# Dado el gráfico Q-Q obtenido, se verifica que la distribución se asemeja
# a la normal, ademas se establece la utilización de un nivel
# de significación alpha=0.05

alpha <- 0.05

# LA CUARTA CONDICION asociada a la homocedasticidad se comprobara luego de
# realizar el procedimiento ANOVA

#REVISAR HIPOTESIS
# Hipótesis planteadas
# H0: El peso promedio de los pollitos es igual para todos los suplementos.
# H1: Existe al menos un peso promedio de pollitos diferente dependiendo del
#     suplemento.

#Se calcula la cantidad de datos del estudio para luego agregar las instancias
#al dataframe
Length_data <- nrow(pollitos)
#Se crean las instancias
instancia <- factor(seq(1,Length_data,by = 1))
#Se crea un nuevo dataframa para el uso del procedimiento ANOVA
datos.long <- cbind(instancia, pollitos)

# Luego de realizar estas comprobaciones se pasa a hacer el procedimiento
# anova, mediante el uso de ezANOVA()

Prueba_ANOVA <- ezANOVA(data = datos.long,
                        dv = weight,
                        between = feed,
                        wid = instancia,
                        return_aov = TRUE)
print(Prueba_ANOVA)

Grafico_TamEfecto <- ezPlot(data = datos.long,
                            dv = weight,
                            wid = instancia,
                            between = feed,
                            type = 3,
                            y_lab = "Peso promedio de pollitos segun suplemento",
                            x = feed)
print(Grafico_TamEfecto)

# PARA LA CUARTA CONDICION DE COMPROBACION
# Del procedimiento anterior se puede extraer el test de Levene, este permite
# comprobar la 4ta condición asociada a la homocedasticidad.
# valor p obtenido para el test de levene es p = 0.5633959
# Como el valor p > alpha se falla al rechazar la hipótesis nula de este test,
# es decir que no hay evidencia significativa para que sugiera diferencias
# significativas entre las varianzas.

# ANALISIS DE RESULTADOS PRUEBA ANOVA
#   Valor de p de la prueba ANOVA = 9.22719e-05
#   alpha = 0.05
# En este caso p < alpha por lo que se rechaza la hipótesis nula. Entonces
# se puede concluir con un 95% de confianza que el peso promedio de los pollitos
# es diferente en al menos uno de los suplementos comparados.

# ANÁLISIS post-hoc
# Para el análisis post-hoc se utilizara la prueba HSD de Tukey, así buscar
# diferencias significativas entre las medias.

Prueba_posthoc <- TukeyHSD(Prueba_ANOVA[["aov"]])

# ANALISIS PRUEBA POST-HOC
# Para la prueba de TukeyHSD se destacan las columnas diff y p adj.
# la primera indica la diferencia de media entre los grupos asociados y 
# la segunda nos permite comparar con el nivel de significación, así 
# determinar cuales se consideran diferencias significativas.
# Considerando lo anterior se detallan aquellos con un 
#   p adj < alpha
# 
# $feed
#                         diff         lwr       upr     p adj
# meatmeal-horsebean 116.70909  54.093130 179.32505 0.0000620
# soybean-horsebean   86.22857  26.893243 145.56390 0.0019171
# 
# Estos son considerados como diferencias significativas.
# De los datos presentados anteriormente se puede destacan los p adj de:
#   meatmeal-horsebean 
#   soybean-horsebean
# 
# El p adj menor es del meatmeal-horsebean, que sería 
# la diferencia más significativa de la muestra. Además se corrobora puesto
# que es la mayor diferencia (diff)
#
# De esto se puede interpretar que se coincide en la existencia de al menos
# una diferencia significativa, en este caso la diferencia más grande es 
# entre los suplementos meatmeal-horsebean, por lo que si existe mayor 
# eficacia en relación al suplemento entregado, siendo meatmeal el suplemento
# con mayor eficacia en la alimentación de los pollitos.
  







