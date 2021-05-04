# Hernan Pinochet
# Ignacio Barahona
# Dennis Urrutia

# Se sabe que la lactancia estimula una pérdida de masa ósea para proporcionar cantidades de calcio adecuadas
# para la producción de leche. Un estudio intentó determinar si madres adolescentes podían recuperar niveles más
# normales a pesar de no consumir suplementos (Amer. J. Clinical Nutr., 2004; 1322-1326). El estudio obtuvo las
# siguientes medidas del contenido total de minerales en los huesos del cuerpo (en gramos) para una muestra de
# madres adolescentes tanto durante la lactancia (6-24 semanas postparto) y posterior a ella (12-30 semana postparto)
# ¿Sugieren los datos que el contenido total de minerales en los huesos del cuerpo durante el posdestete excede el
# de la etapa de lactancia por más de 25 g?

library(ggplot2)
library(ggpubr)
library(dplyr)

print("================== Se inicia la resolución del problema A ========================")

instancia <- seq(1, 10, 1)
muestra_lactancia <- c(1928, 2549, 2825, 1924, 1628, 2175, 2114, 2621, 1843, 2541)
muestra_posdestete <- c(2126, 2885, 2895, 1942, 1750, 2184, 2164, 2626, 2006, 2627)

diferencia <- muestra_posdestete - muestra_lactancia

muestra_dataframe <- data.frame(instancia, muestra_lactancia, muestra_posdestete, diferencia)

ggqqplot(data = muestra_dataframe ,
         x = "diferencia",
         color = "deeppink3",
         xlab = "Teórico",
         ylab = "Muestra",
         title = "Gráfico Q-Q muestra v/s distr. normal")

#Como las muestras fueron elegidas al azar, se puede asumir que son independientes. (son muestras pareadas)

#Puesto que en los gráficos muestra que los valores se distribuyen de manera cercana a 
#la normal, se puede seguir con el método

#muL_0 = median(muestra_lactancia)
#muP_0 = median(muestra_posdestete)

#H_0 : mu_posdestete - mu_lactancia >= 25
#H_A : mu_posdestete - mu_lactancia < 25


mu_0 = 25
alfa <- 0.025

# prueba_unilateral1 <- t.test(diferencia ,
#                            alternative = "less",
#                            mu = mu_0,
#                            conf.level = 1 - alfa)


prueba_unilateral <- t.test(x = muestra_posdestete,
                           y = muestra_lactancia,
                           paired = TRUE ,
                           alternative = "less",
                           mu = mu_0,
                           conf.level = 1 - alfa)

if(prueba_unilateral[3] > alfa){
  print("como el valor p es mayor al nivel de significación, se falla al rechazar H_0")
  print("se puede asegurar con un 97.5% de confianza que el contenido total de minerales en los huesos del cuerpo durante el posdestete excede el de la etapa de lactancia por más de 25 g")
} else {
  print("No hay evidencia suficiente para rechazar la hipótesis H_0")
  print("Se necesita mayor información para poder fallar a favor de H_0")
}

if(between(mu_0, prueba_unilateral[4]$conf.int[1], prueba_unilateral[4]$conf.int[2])){
  print("Como la diferencia de medias está entre el intervalo de confianza, se falla al rechazar H_0")
} else {
  print("Como la diferencia de medias no está entre el intervalo de confianza, No hay evidencia suficiente para rechazar la hipótesis H_0") 
}

print("==================================================================================")
# La avicultura de carne es un negocio muy lucrativo, y cualquier método que ayude al rápido crecimiento de los
# pollitos es beneficioso, tanto para las avícolas como para los consumidores. En el paquete datasets de R están
# los datos (chickwts) de un experimento hecho para medir la efectividad de varios suplementos alimenticios en
# la tasa de crecimiento de las aves. Pollitos recién nacidos se separaron aleatoriamente en 6 grupos, y a cada grupo
# se le dio un suplemento distinto. Para productores de la 7ª región, es especialmente importante saber si existe
# diferencia en la efectividad entre el suplemento basado en linaza (linseed) y el basado en habas (horsebean).

#se cargan los datos de los pollitos
pollitos <- chickwts

pollitos_linaza_1 <- pollitos %>% group_by(feed='linseed') 

pollitos_habas_1 <- pollitos %>% group_by(feed='horsebean')

pollitos_linaza <- pollitos_linaza_1$weight
pollitos_habas <- pollitos_habas_1$weight

#H_0: no existe diferencia en la efectividad entre suplementos (mu_A - mu_B = 0)
#H_A: existe diferencia en la efectividad entre suplementos (mu_A - mu_B != 0)


instancia  <- seq(1, 71, 1)

pollitos_linaza_dataframe <- data.frame(pollitos_linaza)

pollitos_habas_dataframe <- data.frame(pollitos_habas)

# Verificar  si la  primera  muestra  se  distribuye  de  manera  cercana
# a la  normal.
ggqqplot(data = pollitos_linaza_dataframe,
         x = "pollitos_linaza",
         color = "darkgreen",
         xlab = "Teórico",
         ylab = "Muestra",
         title = "Gráfico Q-Q muestra A v/s distr. normal")

# Verificar  si la  primera  muestra  se  distribuye  de  manera  cercana
# a la  normal.
ggqqplot(data = pollitos_habas_dataframe,
         x = "pollitos_habas",
         color = "red",
         xlab = "Teórico",
         ylab = "Muestra",
         title = "Gráfico Q-Q muestra B v/s distr. normal")
#Se puede ver que las muestras no son pareadas y son independientes
#Puesto que el gráfico muestra que los valores se distribuyen de manera cercana a 
#la normal, se puede seguir con el método

mu_0 = 0
alfa <- 0.01

prueba_bilateral_pollitos  <- t.test(x = pollitos_linaza,
                                     y = pollitos_habas,
                                     paired = FALSE ,
                                     alternative = "two.sided",
                                     mu = mu_0,
                                     conf.level = 1 - alfa)

# Calcular  la  diferencia  entre  las  medias.
mu_A <- prueba_bilateral_pollitos$estimate[1]
mu_B <- prueba_bilateral_pollitos$estimate[2]
diferencia_mu <- mu_A - mu_B

if(prueba_bilateral_pollitos[3] > alfa){
  print("como el valor p es mayor al nivel de significación, se falla al rechazar H_0")
  print("se puede asegurar con un 99% de confianza que no existe variación en los pesos de los pollitos por la alimentación que están recibiendo")
  
} else {
  print("No hay evidencia suficiente para rechazar la hipótesis H_0")
  print("Se necesita mayor información para poder fallar a favor de H_0")
}
