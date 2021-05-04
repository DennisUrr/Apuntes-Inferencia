# Hernan Pinochet
# Ignacio Barahona
# Dennis Urrutia

# El art�culo "An Introduction to Some Precision and Accuracy of Measurement Problems" 
# (Journal of Testing and Evaluation 1982) estudi� si la ropa de dormir de ni�os cumpl�a 
# la norma que se�ala que el tiempo de permanencia de llamas ha de ser 9,75 s en promedio.
# Con una muestra de tiras de ropa tratada observaron los siguientes tiempos de permanencia
# de llamas (en segundos, usando punto en vez de coma decimal):
# 9.85 9.94 9.88 9.93 9.85 9.95 9.75 9.75 9.95 9.77 9.83 9.93 9.67 9.92 9.92 9.87 9.74
# 9.89 9.67 9.99
# �A qu� conclusi�n deber�an haber llegado los autores del art�culo?
library(ggplot2)
library(ggpubr)
library(dplyr)

print("================== Se inicia la resoluci�n del problema D ========================")

muestraD <- c(9.85, 9.94, 9.88, 9.93, 9.85, 9.95, 9.75, 9.75, 9.95, 9.77, 9.83, 9.93, 9.67, 9.92, 9.92, 9.87, 9.74, 9.89, 9.67, 9.99)

muestraD_dataframe <- data.frame(muestraD)

#Como las muestras fueron elegidas al azar, se puede asumir que son independientes.

ggqqplot(data = muestraD_dataframe ,
         x = "muestraD",
         color = "steelblue",
         xlab = "Te�rico",
         ylab = "MuestraD",
         title = "Gr�fico Q-Q muestra v/s distr. normal")
#Puesto que el gr�fico muestra que los valores se distribuyen de manera cercana a 
#la normal, se puede seguir con el m�todo

#Se realizar� una prueba bilateral puesto que no se conoce el signo potencial de diferencia

#H_0: el tiempo de permanencia de llamas es 9.75 s en promedio (mu_0 = 9.75)
#H_A: el tiempo de permanencia de llamas no es 9.75 s en promedio (mu_A != 9.75)

# Calcular  estimadores  puntuales y estad�sticos �tiles.
muD_0 = 9.75 
alfa  <- 0.05

prueba_bilateral <- t.test(muestraD ,
                           alternative = "two.sided",
                           mu = muD_0,
                           conf.level = 1 - alfa)

if(prueba_bilateral[3] > alfa){
  print("como el valor p es mayor al nivel de significaci�n, se falla al rechazar H_0")
} else {
  print("No hay evidencia suficiente para rechazar la hip�tesis H_0")
  print("Se necesita mayor informaci�n para poder fallar a favor de H_0")
}

if(between(muD_0, prueba_bilateral[4]$conf.int[1], prueba_bilateral[4]$conf.int[2])){
  print("Como la media est� entre el intervalo de confianza, se falla al rechazar H_0")
} else {
  print("Como la media no est� entre el intervalo de confianza, No hay evidencia suficiente para rechazar la hip�tesis H_0") 
}

print("==================================================================================")
#Un estudio, encargado por bomberos, investig� si los sistemas aspersores de prevenci�n, 
#que deben ser instalados en los edificios de m�s de 4 pisos construidos despu�s de 2001,
#cumplen con la norma que les obliga a que el tiempo promedio de activaci�n no sobrepase
#los 25 s. Con una serie de pruebas obtuvieron la siguiente muestra:
#  27 41 22 27 23 35 30 33 24 27 28 22 24
#El estudio sugiere que la norma no se est� cumpliendo. �Sugieren los datos esta conclusi�n?

print("================== Se inicia la resoluci�n del problema E ========================")

muestraE <- c(27, 41, 22, 27, 23, 35, 30, 33, 24, 27, 28, 22, 24)

#H_0: el estudio sugiere que NO se est� cumpliendo la norma (mu_0 >= 25)
#H_A: el estudio sugiere que se est� cumpliendo la norma (mu_0 < 25)


#Como las muestras fueron elegidas al azar, se puede asumir que son independientes (son distintos edificios).

muestraE_dataframe <- data.frame(muestraE)

ggqqplot(data = muestraE_dataframe ,
         x = "muestraE",
         color = "steelblue",
         xlab = "Te�rico",
         ylab = "MuestraE",
         title = "Gr�fico Q-Q muestra v/s distr. normal")

#Puesto que el gr�fico muestra que los valores se distribuyen de manera cercana a 
#la normal, se puede seguir con el m�todo

#Se realizar� una prueba unilateral puesto que se necesita comprobar que el tiempo promedio de activaci�n sea mayor a 25s

# Calcular  estimadores  puntuales y estad�sticos �tiles.
muE_0 = 25
alfa  <- 0.025

prueba_unilateral <- t.test(muestraE ,
                            alternative = "less", #el contrario a lo que se quiere probar
                            mu = muE_0,
                            conf.level = 1 - alfa)

if(prueba_unilateral[3] > alfa){
  print("como el valor p es mayor al nivel de significaci�n, se falla al rechazar H_0")
  print("Los datos sugieren la conclusi�n dada en la pregunta, puesto que como no se rechaza H_0, se puede asegurar con un 97,5% que la media del tiempo promedio de activaci�nes es mayor a 25 segundos")
} else {
  print("No hay evidencia suficiente para rechazar la hip�tesis H_0")
  print("Se necesita mayor informaci�n para poder fallar a favor de H_0")
}

if(between(muE_0, prueba_unilateral[4]$conf.int[1], prueba_unilateral[4]$conf.int[2])){
  print("Como la media est� entre el intervalo de confianza, se falla al rechazar H_0")
} else {
  print("Como la media no est� entre el intervalo de confianza, No hay evidencia suficiente para rechazar la hip�tesis H_0") 
}

print("==================================================================================")
