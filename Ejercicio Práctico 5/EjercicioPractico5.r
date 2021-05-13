# Nombres:
# Hernan Pinochet
# Dennis Urrutia

# Se importan las librerias a utilizar
library(pwr)

## ------------- Enunciado ----------------

# Los siguientes datos vienen de un estudio hecho por Rothman & Keller (1972)
# (Journal of chronic diseases, 25(12), 711-716) sobre la incidencia de la cantidad de alcohol y
# de tabaco que se consume en el riesgo de padecer cáncer oral.

# Se almacenan los datos del enunciado en variables

# Se crea y almacena tabla alcohol
cancer_oral_alcohol <- c(43, 89, 109, 242)
controles_alcohol <- c(108, 141, 91, 107)

alcohol <- data.frame(
  alcohol = factor("0", "1-9", "10-44", "45+"),
  cancer_oral = cancer_oral_alcohol,
  controles = controles_alcohol
)

# Se crea y almacena tabla de tabaco
cancer_oral_tabaco <- c(26, 66, 248, 143)
controles_tabaco <- c(85, 97, 197, 68)

tabaco <- data.frame(
  tabaco = factor("0", "1-19", "20-39", "40+"),
  cancer_oral = cancer_oral_tabaco,
  controles = controles_tabaco
)

## --------- Problema 1 ------------

# Suponiendo que la diferencia en la proporción de personas que desarrollan la enfermedad entre quienes
# fuman de 20 a 39 cigarrillos al día y aquellos que fuman 40 o más unidades por día es de 0.18. ¿Cuánta gente
# deberíamos entrevistar para obtener un intervalo de confianza del 95% y poder estadístico de 80%?
#n ?

# Se almacena alfa y poder dados en el enunciado
alfa <- 0.05
poder <- 0.8

# Se calculan las proporciones necesitadas del tabaco establecidos por el enunciado

# Proporción para el rango entre 20 y 39 cigarros al dia
n_20.39.total <- tabaco[["cancer_oral"]][3] + tabaco[["controles"]][3]
prop_20.39 <- tabaco[["cancer_oral"]][3]/n_20.39.total

# Proporción para el rango entre mas de 40 cigarros al dia
n_40.total <- tabaco[["cancer_oral"]][4] + tabaco[["controles"]][4]
prop_40 <- tabaco[["cancer_oral"]][4]/n_40.total

#En base a las proporciones anteriores, se asume una proporción esperada para cada una de las anteriores proporciones
#con el fin de que la diferencia de proporciones de 0.18
prop_20.30_esp <- 0.55
prop_40_esp <- 0.73

dif_prop_20.30_40 <- 0.18

#Se calcula el tamaño del efecto para las proporciones esperadas para posteriormente utilizarlo en la función pwr.2p.test
h <- ES.h(prop_20.30_esp, prop_40_esp )

#Como se tiene el nivel de significación, rl tamaño del efecto y el poder, se utiliza esta función para obtener n
datos <- pwr.2p.test(h=h,n=NULL, sig.level = alfa, power = poder)

#Función que hace exactamente lo mismo, solo que en vez del tamaño del efecto, utiliza las proporciones esperadas
# dato1 <- power.prop.test(p1=prop_20.30_esp, p2=prop_40_esp, sig.level = alfa, power = poder, alternative = "two.sided")

#Luego n es:
n <- datos$n

## --------------- Problema 2 --------------------

# Estudios previos habían determinado que la incidencia de cáncer oral en la población general que fuma entre
# 20 y 39 cigarrillos al día era de 5%. ¿Respaldan estos datos tal estimación?

# Se almacena p0 indicado por enunciado
p0 <- 0.05

# Realizanos la docima de hipotesis
#H_0: p = p0
#H_1: p != p0

# Se extraen los datos necesarios por el rango de 20 y 39 cigarrillos
cancer_oral_tabaco_n <- tabaco[["cancer_oral"]][3]
controles_tabaco_n <- tabaco[["controles"]][3]

# Se obtiene el total de datos
total <- cancer_oral_tabaco_n + controles_tabaco_n

# Se calcula la proporción de datos
prop <- cancer_oral_tabaco_n/total

# Y por ultimo se realiza el ptest para calcular el valor p y realizar una comparación con alfa para rechazar
# o no la hipótesis nula
ptest <- prop.test(
  x = cancer_oral_tabaco_n,
  n = total,
  p = p0,
  alternative = "two.sided",
  conf.level = 1-alfa,
  correct = FALSE
)

#Como el p valor es menor que alfa, se falla al rechazar H0, por lo tanto se puede decir con un 95% de confianza que la incidencia
#del cáncer oral de quien fuma entre 20 y 39 cigarrillos al día es de un 5%


## --------------------- Problema 3 -------------------

# Según estos datos, ¿da lo mismo fumar diariamente entre 1 y 2 paquetes de cigarrillos que hacerlo más de
# dos paquetes?
  
#H_0:  casos de incidencia 1 y 2 paquetes de cigarros - casos de incidencia 2 o mas paquetes = 0
#H_1: casos de incidencia 1 y 2 paquetes de cigarros - casos de incidencia 2 o mas  paquetes != 0

ptest2 <- prop.test(
  x = c(248, 143),
  n = c(248+197, 143+68),
  alternative = "two.sided",
  conf.level = 1 - alfa
) 

# Se estudia la variable p con respecto alfa
if(ptest2$p.value > alfa){
  print("como el valor p es mayor al nivel de significación, se falla al rechazar H_0")
  print("Los datos sugieren la conclusión dada en la pregunta, puesto que como no se rechaza H_0, se puede asegurar con un 95% que fumar 1 o 2 paquetes de cigarrilos da lo mismo que fumar de 2 paquetes")
} else {
  print("No hay evidencia suficiente para rechazar la hipótesis H_0")
  print("Se necesita mayor información para poder fallar a favor de H_0")
}