# Ejercicio práctico 9
# Grupo 3
# Integrantes:  Ignacio Barahona    - 20.053.896-k
#               Hernán Pinochet     - 20.186.654-5
#               Dennis Urrutia      - 20.054.672-5
#librerías a utilizar
library(ggpubr)
library(ggplot2)
library(tidyr)

#=========================Problema A============================
# En trabajo de título de un estudiante del DIINF, se reportan los siguientes tiempos de ejecución (en milisegundos) 
# medidos para dos versiones de un algoritmo genético para resolver instancias del problema del vendedor viajero 
# disponibles en repositorios públicos. ¿Es uno de los algoritmos más rápido que el otro?

texto <-("
        Instancia 'Tiempo A6 (ms)' Instancia 'Tiempo B12 (ms)'
        'fl1400' 337977 'd1291' 335566
        'pcb1173' 303634 'd657' 52696
        'rat575' 33349 'fl1577' 3192222
        'rl1323' 243679 'nrw1379' 393213
        'u1060' 3453176 'pr1002' 162808
        'u1432' 398653 'pr2392' 8765321
        'u1817' 876432 'rat783' 76857
        'u2152' 3073534 'rl1304' 231254
        'u574' 112326 'rl1889' 854213
        'u724' 55026 'vm1084' 543215
        ")
datos1 <- read.table(textConnection(texto), header = TRUE)

#Crear el dataframe falta cambiar el dataframe por los dos tiempos

Tiempos <- c(datos1$Tiempo.A6..ms., datos1$Tiempo.B12..ms.)
Algoritmos <- c(rep("Algoritmo A6", length(datos1$Tiempo.A6..ms.)),
                rep("Algoritmo B12", length(datos1$Tiempo.B12..ms.)))

Algoritmos <- factor(Algoritmos)

Instancias <- c(rep(datos1$Instancia), rep(datos1$Instancia.1))

Instancias <- factor(Instancias)
 
datos1_final <- data.frame(Instancias, Algoritmos, Tiempos)

#Se realiza un gráfico histograma para ver la simetría de los datos
Histograma <- gghistogram(datos1_final,
                          x = "Tiempos",
                          bins = 10,
                          xlab = "Tiempos",
                          ylab = "Frecuencia",
                          color = "red",
                          fill = "red" )

#Como los datos son muy asimétricos, se realiza una transformación logarítmica

log_tiempos <- log(datos1$Tiempos)

log_tiemposA6 <- log(datos1$Tiempo.A6..ms.)
log_tiemposB12 <- log(datos1$Tiempo.B12..ms.)

datos1_log <- data.frame(Instancias, Algoritmos, log_tiempos)

datos1_tiempo_log <- data.frame(datos1$Instancia, log_tiemposA6, datos1$Instancia.1, log_tiemposB12)

#Se realiza un gráfico histograma para ver la simetría de los datos
Histograma_log <- gghistogram(datos1_log,
                          x = "log_tiempos",
                          bins = 10,
                          xlab = "Tiempos",
                          ylab = "Frecuencia",
                          color = "red",
                          fill = "red" )
#Se realiza un gráfico que contiene los dos histogramas

Histograma_final <-  ggarrange (Histograma , Histograma_log , ncol = 2 , nrow = 1 )

titulo <- text_grob ( "Efecto de la transformación logarítmica " ,
                        face = "bold" ,
                        size = 14 )

Histograma_final <- annotate_figure ( Histograma_final , top = titulo )

print ( Histograma_final )

#Como se puede apreciar en los histogramas, la transformación de los datos conllevó una mejor
#distribución de estos.


# Comprobación de normalidad, se realiza gráfico Q-Q
Grafico_QQ <- ggqqplot (datos1_log,
                        x = "log_tiempos",
                        y = "Algoritmos",
                        color = "Algoritmos",
                        title = "Grafico Q-Q Algoritmos")
Grafico_QQ <- Grafico_QQ + facet_wrap (~ Algoritmos )
Grafico_QQ <- Grafico_QQ + rremove ("x.ticks") + rremove ("x.text")
Grafico_QQ <- Grafico_QQ + rremove ("y.ticks") + rremove ("y.text")
print(Grafico_QQ)

# Como se puede ver en el gráfico Q-Q, los datos se distribuyen normalmente, por lo que se piensa
# utilizar un T test

# H0: existe una diferencia en los tiempos de ejecución de los algoritmos
# HA: no existe una diferencia en los tiempos de ejecución de los algoritmos

alfa <- 0.05

prueba <- t.test(x=datos1_tiempo_log$log_tiemposA6,
                 y=datos1_tiempo_log$log_tiemposB12,
                 alternative = "two.sided",
                 conf.level = 1-alfa)
print(prueba)

# Considerando alpha = 0, 05 y el valor p = 0.7115, en una prueba
# bilateral. Puesto que p > alpha, se falla al rechazar
# la hipótesis nula, por lo que se concluye con 95% de confianza que no existe una diferencia estadísticamente
# significativa en los tiempos de ejecución de los algoritmos.


#=========================Problema C============================
# En trabajo de título de un estudiante del DIINF, se reportan los siguientes tiempos de ejecución ('Tpo' en milisegundos)
# medidos para dos versiones de un algoritmo genético (A6 y B12) para resolver instancias del problema del vendedor
# viajero disponibles en repositorios públicos. ¿Es uno de los algoritmos más rápido que el otro?

texto2 <-("
          Instancia 'Tpo A6' 'Tpo B12'
          'rat575' 33349 32444
          'u724' 55026 64019
          'd657' 43352 52696
          'rat783' 65076 76857
          'u574' 112326 123456
          'pr1002' 136262 162808
          'fl1577' 3234574 3192222
          'nrw1379' 335608 393213
          'd1291' 268964 335566
          'u1432' 398653 472597
          'pcb1173' 303634 234658
          'fl1400' 337977 430748
          'u2152' 3073534 3253423
          'rl1323' 243679 132654
          'rl1304' 342321 231254
          'u1817' 876432 672542
          'vm1084' 413672 543215
          'rl1889' 1876432 854213
          'pr2392' 6764986 8765321
          'u1060' 3453176 432876
          ")

datos2 <- read.table(textConnection(texto2), header = TRUE)

Tiempos <- c(datos2$Tpo.A6, datos2$Tpo.B12)
Algoritmos <- c(rep("Algoritmo A6", length(datos2$Tpo.A6)),
                rep("Algoritmo B12", length(datos2$Tpo.B12)))

Algoritmos <- factor(Algoritmos)

Instancias <- c(rep(datos2$Instancia, 2))

Instancias <- factor(Instancias)

datos2_final <- data.frame(Instancias, Algoritmos, Tiempos)

# Comprobación de normalidad, se realiza gráfico Q-Q
Grafico_QQ_2 <- ggqqplot (datos2_final,
                        x = "Tiempos",
                        y = "Algoritmos",
                        color = "Algoritmos",
                        title = "Grafico Q-Q Algoritmos")
Grafico_QQ_2 <- Grafico_QQ_2 + facet_wrap (~ Algoritmos )
Grafico_QQ_2 <- Grafico_QQ_2 + rremove ("x.ticks") + rremove ("x.text")
Grafico_QQ_2 <- Grafico_QQ_2 + rremove ("y.ticks") + rremove ("y.text")
print(Grafico_QQ_2)

# Al tener datos atípicos (y algunos que se encuentran muy alejados de la normal) en ambos gráficos Q-Q, no se puede
# decir que son datos distribuidos normalmente, por lo que se utilizará la prueba no paramétrica
# de Wilcoxon-Manmn-Whitney

# Primera Condición
#   Se asume que las muestras son independientes puesto que las muestras son tomadas a dos conjuntos de elementos
#   distintos y ademas los valores de una muestra no revelan los valores de otra muestra.
#   Se puede decir que la escala de datos es ordinal al ser la diferencia de tiempos alta en ciertos casos.

# H0: no existe una diferencia en los tiempos de ejecución de los algoritmos
# HA: existe una diferencia en los tiempos de ejecución de los algoritmos

alfa <- 0.05

prueba2 <- wilcox.test(datos2$Tpo.A6,
                      datos2$Tpo.B12,
                      alternative = "two.sided",
                      paired = TRUE,
                      conf.level = 1-alfa)
print(prueba2)

# Considerando alpha = 0,05 y el valor p = 0.8695, en una prueba
# bilateral. Puesto que p > alpha, se falla al rechazar la hipótesis nula,
# por lo que se concluye con 95% de confianza que no existe una diferencia estadísticamente
# significativa en los tiempos de ejecución de los algoritmos.

#=========================Problema E============================
# Una compañía de cosméticos hizo una prueba preliminar de su nueva crema quitamanchas, en que 30 personas fueron
# separadas aleatoriamente en tres grupos de 10 voluntarios/as: uno de control, a quienes se les entregó una crema 
# placebo (humectante solamente); otro que usaron la crema quitamanchas que la compañía comercializa actualmente; 
# y el último que usaron el nuevo producto. A todos se les dijo que usaban la crema nueva de última generación. 
# Dos personas del grupo de control y una del grupo con la crema existente abandonaron el estudio. Para el resto,
# se reportaron los siguientes números de manchas removidas al finalizar el tiempo de prueba:
# ¿Hay diferencia en la cantidad de manchas que quita cada crema?
texto3 <-("
          Nueva Actual Control
          81 48 18
          32 31 49
          42 25 33
          62 22 19
          37 30 24
          44 30 17
          38 32 48
          47 15 22
          49 40 --
          41 -- --
          ")
datos3 <- read.table(textConnection(texto3), header = TRUE, na.strings = "--")

# Se transforman los datos a formato largo
datos3_largo <- gather(data = datos3,
                       key = "Grupos",
                       value = "Manchas")

datos3_largo[["Grupos"]] <- factor(datos3_largo[["Grupos"]])

# Se crea una secuenca con el largo de los datos
instancia <- seq(1,30,1)

# Se crea el dataframe
datos3_dataframe <- data.frame(instancia, datos3_largo)

# Se quitan los valores nulos
datos3_dataframe <- datos3_dataframe[complete.cases(datos3_dataframe),]

# Dado que se tienen 3 muestras independientes corresponde a la utilización de 
# métodos no paramétricos (enunciado) para más de dos muestras, por lo tanto se 
# utilizará Kruskal Wallis

# Hay que cumplir las siguientes condiciones:
# 1. Las muestras son independientes y aleatorias
# 2. La variable dependiente es continua.
# 3. Las mediciones tienen al menos escala ordinal.

# En la primera condición se asume que los datos fueron tomados de forma aleatoria
# y son independientes entre si (se puede ver por el enunciado del problema)

# La segunda condición se puede ver que son continuas por los datos que contiene

# La tercera condición se cumple evidentemente 

# Comprobación de distribución de las observaciones, se realiza gráfico de caja
Grafico_caja <- ggboxplot (datos3_dataframe,
                          x = "Grupos",
                          y = "Manchas",
                          xlab = "Grupos",
                          color = "Grupos",
                          add = "jitter",
                          title = "Grafico Caja Grupos")
print(Grafico_caja)

# Como se puede ver en el gráfico, estas no tiene una distribución conocida, por lo que se usará Kruskal-Wallis

# H0: no hay diferencia en la cantidad de manchas que quita cada crema
# HA: Hay diferencia en la cantidad de manchas que quita cada crema

prueba_Kruskal <- kruskal.test(Manchas ~ Grupos, datos3_dataframe)

print(prueba_Kruskal)

# Dado que el alfa es 0.05, el p-value es menor que el alfa, (0.05 > 0.01228) se rechaza H0, por
# lo que se puede decir con un 95% de nivel confianza de que hay diferencia en la cantidad de manchas que quita cada crema

# Como el p-value es menor que alfa, se procede a realizar un análisis post-hoc para determinar donde existen las 
# diferencias

prueba_post_hoc <- pairwise.wilcox.test(datos3_dataframe[["Manchas"]], 
                                        datos3_dataframe[["Grupos"]],
                                        p.adjust.method = "BH") 
print(prueba_post_hoc)


# Se puede ver que hay una diferencia significativa entre la crema nueva y al crema actual con un p de 0,016
# y la crema nueva con la crema control con un p de 0.049
