# Ejercicio práctico 8: Anova para muestras correlacionadas
# Grupo 3
# Integrantes:  Ignacio Barahona    - 20.053.896-k
#               Hernán Pinochet     - 20.186.654-5
#               Dennis Urrutia      - 20.054.672-5

#Se importan librerías a utilizar
library(ggpubr)
library(ggplot2)
library(ez)


#Ejercicio practico 8, desarrollo de problema B

################################################################################
# El siguiente es un resumen de la descripción de un famoso experimento:
# 
#   Naming the ink color of color words can be difficult. For example, if asked 
# to name the color of the word "blue" is difficult because the answer (red) 
# conflicts with the word "blue." This interference is called "Stroop 
# Interference" after the researcher who first discovered the phenomenon. 
# This case study is a classroom demonstration. Students in an introductory 
# statistics class were each given three tasks. In the "words" task, students 
# read the names of 60 color words written in black ink; in the "color" task, 
# students named the colors of 60 rectangles; in the "interference" task, 
# students named the ink color of 60 conflicting color words. The times to read 
# the stimuli were recorded. There were 31 female and 16 male students.
# El siguiente código R define los datos que se obtuvieron en este estudio. 
# 
# Con estos datos, responda la siguiente pregunta de investigación: 
# ¿Hay diferencias en los tiempos entre tareas?
################################################################################
# Análisis del problema
# Se realizan un análisis anova. Así mediante el
# análisis de los resultados, y los gráficos obtenidos poder identificar
# si existe diferencia en los tiempos entre tareas.
################################################################################
cat("################################\nResolucion problema B\n\n")

#Se cargan los datos asociados al estudio
texto <- ("
      gender words colors interfere
      1 19 15 31
      1 21 20 38
      1 9 25 38
      1 21 19 32
      1 16 15 29
      1 16 16 36
      1 17 23 34
      1 21 19 44
      1 9 14 42
      1 23 17 37
      1 18 19 31
      2 26 24 33
      2 18 19 44
      2 17 21 31
      2 12 5 44
      2 21 17 35
      2 19 21 34
      2 16 15 32
      2 13 22 47
      2 13 24 29
      2 15 19 38
      2 15 26 42
")
datos <- read.table(textConnection(texto), header = TRUE)

#  CONDICIONES PARA USO DE ANOVA
# Para poder realizar un procedimiento Anova para muestras correlacionadas es necesario cumplir ciertas
# condiciones:
# 1. La escala con que se mide la variable dependiente tiene las propiedades de 
# una escala de intervalos iguales.
# 2. Las mediciones son independientes al interior de cada grupo.
# 3. Se puede suponer razonablemente que la(s) población(es) de origen sigue(n)
# una distribución normal.
# 4. La matriz de varianzas-covarianzas es esférica. Como explica Horn, esta 
# condición establece que las varianzas entre los diferentes niveles de las 
# medidas repetidas deben ser iguales.

# LA PRIMERA CONDICIÓN se verifica ya que la variable dependiente (tiempo) si
# se encuentra en intervalos de igual tamaño, específicamente en intervalos
# de 1 segundo.

# LA SEGUNDA CONDICIÓN se verifica debido a que el resultado de cada grupo es 
# independiente del resto, ademas se supone que el investigador al momento de
# realizar el estudio considero este punto

# LA TERCERA CONDICIÓN se debe realizar un gráfico Q-Q para determinar este
# supuesto, primero se crea el dataframe a utilizar, luego se realiza el
# gráfico q-q

#Crear el dataframe.
Tiempos <- c( datos$words, datos$colors, datos$interfere)
Tasks <- c(rep("Words", length (datos$words)),
            rep ("Colors", length (datos$colors)),
            rep ("Interfere", length (datos$interfere)))

Tasks <- factor (Tasks)
Genders <- factor (rep(datos$gender, 3))
Length_data <- nrow(datos)
Instancia <- factor (rep(seq (1, Length_data, by=1), 3))
datos.long <- data.frame (Instancia, Genders, Tasks , Tiempos)

# Comprobación de normalidad, se realiza gráfico Q-Q
Grafico_QQ <- ggqqplot (datos.long ,
                        x = "Tiempos",
                        y = "Tasks",
                        color = "Tasks",
                        title = "Grafico Q-Q Tasks")
Grafico_QQ <- Grafico_QQ + facet_wrap (~ Tasks )
Grafico_QQ <- Grafico_QQ + rremove ("x.ticks") + rremove ("x.text")
Grafico_QQ <- Grafico_QQ + rremove ("y.ticks") + rremove ("y.text")
print(Grafico_QQ)

# Dado el gráfico Q-Q obtenido, se verifica que la distribución se asemeja
# a la normal, por esto se establece la utilización de un nivel de 
# significación alpha=0.05

alpha <- 0.05

# LA CUARTA CONDICIÓN asociada a la esfericidad se realizará luego del
# procedimiento de ANOVA con la prueba de esfericidad de Mauchly

# SE REALIZAN HIPOTESIS PARA CADA PRUEBA A REALIZAR

# Hipótesis planteadas
# H0: El tiempo promedio en las tareas son iguales.
# H1: Al menos uno de los tiempos promedios en las tareas es distinto.

# Luego de realizar estas comprobaciones se pasa a hacer el procedimiento
# anova, mediante el uso de ezANOVA()

#Procedimiento de Anova para muestras correlacionadas para los datos de la tabla
Prueba_ANOVA <- ezANOVA(data = datos.long,
                        dv = Tiempos,
                        within = Tasks,
                        wid = Instancia,
                        return_aov = TRUE)

# PARA LA CUARTA CONDICIÓN DE COMPROBACIÓN, se estudia los resultados
# obtenidos de la prueba ANOVA, específicamente la prueba de esfericidad de 
# Mauchly

# ANÁLISIS PRUEBA DE ESFERICIDAD DATOS.
# Considerando el valor de alpha, y el p obtenido de la prueba de esfericidad de 
# Mauchly.
# alpha = 0.05  y   p = 0.3539676      
# Dado estos resultados como p > alpha, se desprende que los datos si cumplen 
# con la condición de esfericidad

# ANÁLISIS PRUEBA MAUCHLY DATOS.
cat("\nResultados prueba MAuchly para datos:\n\n")
print(Prueba_ANOVA$`Mauchly's Test for Sphericity`)

#ANÁLISIS DE RESULTADOS PRUEBA ANOVA
# ANÁLISIS PRUEBA ANOVA DATOS.
cat("\nResultados prueba ezANOVA para datos:\n\n")
print(Prueba_ANOVA$ANOVA)

# alpha = 0.05  y   p = 1.312004e-16
# Dado estos resultados como p < alpha, se rechaza la hipótesis nula en favor 
# de la hipótesis alternativa. Así, se concluye con 95% de confianza que
# existen diferencias significativas entre al menos dos de las tareas comparadas.

cat("\nDado estos resultados como p < alpha, se rechaza la hipótesis nula en favor 
de la hipótesis alternativa. Así, se concluye con 95% de confianza que existen 
diferencias significativas entre al menos dos de las tareas comparadas.\n")

#SE CONSTRUYEN LOS GRÁFICOS ASOCIADOS PARA EL TAMAÑO DEL EFECTO
# Gráfico para tamaño del efecto para hombres
Grafico_TamEfecto <- ezPlot(data = datos.long,
                            dv = Tiempos,
                            wid = Instancia,
                            within = Tasks,
                            type = 3,
                            y_lab = "Tiempo de lectura en segundos",
                            x = Tasks)
print(Grafico_TamEfecto)

# ANÁLISIS post-hoc
# Para el análisis post-hoc se utilizará el procedimiento post -hoc de Holm, 
# así buscar diferencias significativas.

Post_Hoc_holm <- pairwise.t.test (datos.long$Tiempos,
                                  datos.long$Tasks,
                                  p.adj = "holm",
                                  paired = TRUE )
cat ("\n\nCorreccion de Holm\n")
print (Post_Hoc_holm)

# ANÁLISIS PRUEBA POST-HOC
# La corrección se basa en la comparación de todas las medias, reduciendo la 
# probabilidad de cometer un error de tipo II. Donde las diferencias 
# significativas son aquellas con un p < alpha
#
# Dado los valores obtenidos, se obtienen dos diferencias significativas
# 
#           Colors  Interfere
# Interfere 8.0e-10 -        
#    Words  0.16    3.8e-10  
# 
# Las diferencias significativas son:
#       * Colours e Interfere
#       * Words e Interfere
# 
# Se destaca que la diferencia más significativa es entre Words e Interfere, también esto se puede
# observar en el gráfico de tamaño de efecto puesto que la distancia entre los puntos (diferencia entre las medias)
# es mayor entre Words e Interfere




