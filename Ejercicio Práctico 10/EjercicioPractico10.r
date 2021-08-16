# Ejercicio practico 10: REMUESTREO Y MÉTODOS ROBUSTOS
# Grupo 3
# Integrantes:  Ignacio Barahona    - 20.053.896-k
#               Hernan Pinochet     - 20.186.654-5
#               Dennis Urrutia      - 20.054.672-5

#Se importan librerias a utilizar
library(ggpubr)
library(ggplot2)
library(ez)
library(dplyr)
library (simpleboot)
library(WRS2)

#Ejercicio practico 10
################################################################################
# Preguntas propuestas por el equipo
# Asociado a -> La comparación de las medias de dos grupos independientes
#### PREGUNTA 1 ###
# La cantidad de hijos nacidos vivos (s4) es similar entre mujeres casadas
# y mujeres solteras (sexo) (ecivil).
#### ANALISIS
# --- los grupos independientes son mujeres casadas y mujeres solteras ----
# --- se busca comparar media entre ambos grupos ---
########
# Asociado a -> La comparación de más de dos medias independientes.
#### PREGUNTA 2 ###
# El numero de personas en el hogar (numper) es similar para viviendas en 
# situación ocupa (V13) "propia pagada", "Arrendada con contrato" y "Ocupación 
#irregular", en la región metropolitana.
#### ANALISIS
# --- Existen 3 muestras, "propia pagada", "Arrendada con contrato" y "Ocupación
# irregular"---
# --- se busca identificar la diferencia entre estas medias ---
########
################################################################################

################################################################################
#LECTURA DE ARCHIVO Datos-Casen-V2
#El archivo entregado esta en formato .xls por lo que tira error al intentar
#leer con read.csv2, por ende, se debe guardar en formato csv delimitado por
#comas, de esta forma se lee correctamente.

# Indicar directorio donde se encuentra el archivo
dir <- "C:/Users/Dennis/Documents/Clases/IME/Apuntes-Inferencia/Ejercicio Práctico 10/Datos-Casen-v2.csv"

# Lectura del archivo que contiene los datos
poblacion <- read.csv2(dir, encoding = 'UTF-8')

################################################################################
# SE FIJA EL SEED PARA TOMAR UNA MUESTRA ALEATORIA
semilla <- 350
set.seed(semilla)

# SE CALCULA EL TAMANO DE LA POBLACION Y SE DEFINE EL TAMANO DE LA MUESTRA
tam.poblacion <- nrow(poblacion)
tam.muestra <- 300

# Se obtiene una muestra de 300 datos utilizando la semilla.
muestra <- poblacion[sample(1:tam.poblacion, tam.muestra),]


#=====================================PREGUNTA 1================================
# La cantidad de hijos nacidos vivos (s4) es similar entre mujeres casadas
# y mujeres solteras (sexo) (ecivil).

muestra.solteras <- filter(muestra, muestra$ecivil %in% c("Soltero(a)"), muestra$sexo %in% c("Mujer"))

cant.hijos.solteras <- muestra.solteras$s4

muestra.casadas <- filter(muestra, muestra$ecivil %in% c("Casado(a)"), muestra$sexo %in% c("Mujer"))

cant.hijos.casadas <- muestra.casadas$s4

alfa <- 0.05

# Se realiza la prueba de Shapiro

shapiro.solteras <- shapiro.test(cant.hijos.solteras)

print(shapiro.solteras)

shapiro.casadas <- shapiro.test(cant.hijos.casadas)

print(shapiro.casadas)

# Shapiro solteras
# Shapiro-Wilk normality test
# data:  cant.hijos.solteras
# W = 0.71675, p-value = 1.674e-08
# 
# Shapiro casadas
# 
# Shapiro-Wilk normality test
# 
# data:  cant.hijos.casadas
# W = 0.91622, p-value = 0.1108

# En base al test de shapiro se puede concluir con un 95% de confianza que la muestra de la cantidad de hijos vivos
# de las mujeres solteras no se distribuye normalmente y la cantidad de hijos vivos
# de las mujeres casadas se distribuye normalmente


#=====================================REMUESTREO================================

# Se calcula la diferencia observada entre las medias muestrales

media.solteras <- mean(cant.hijos.solteras)
media.casadas <- mean(cant.hijos.casadas)
diferencia.medias <- media.solteras - media.casadas

# Se construye la distribución bootstrap
# H0: La diferencia de medias de la cantidad de hijos nacidos vivos en mujeres casadas y mujeres solteras es 0 
# HA: La diferencia de medias de la cantidad de hijos nacidos vivos en mujeres casadas y mujeres solteras es distinto de 0


B <- 9999

valor.nulo <- 0

distribucion.bootstrap <- two.boot(cant.hijos.solteras, 
                                   cant.hijos.casadas,
                                   FUN = mean,
                                   R = B)

desplazamiento <- mean(distribucion.bootstrap[["t"]]) - valor.nulo
distribucion_nula <- distribucion.bootstrap[["t"]] - desplazamiento

p <- (sum(abs(distribucion_nula) > abs(diferencia.medias)) + 1)/(B + 1)
cat ( " Valor p : " , p )

# Tras aplicar bootstrapping para la prueba de hipótesis, obtienen un valor p de p = 0.1074,
# superior al nivel de significación, por lo que se falla al rechazar la hipótesis nula. En consecuencia, se concluye
# con 95% de confianza que la diferencia de medias de la cantidad de hijos nacidos vivos en mujeres casadas y mujeres solteras es 0.

#=====================================MÉTODOS ROBUSTOS================================
largo.hijos.soltera <- length(cant.hijos.solteras)
largo.hijos.casadas <- length(cant.hijos.casadas)

mujer.ecivil <- c(rep("Mujeres Solteras", largo.hijos.soltera), rep("Mujeres Casadas", largo.hijos.casadas))

cantidad.hijos <- c(cant.hijos.solteras, cant.hijos.casadas)

datos <- data.frame(mujer.ecivil, cantidad.hijos)

alfa <- 0.01

# Aplicar prueba de yuen para muestras independientes
# aplicar poda al 20%
gamma  <- 0.2

poda.hijos.solteras <- largo.hijos.soltera * gamma
poda.hijos.casadas <- largo.hijos.casadas * gamma

# cant.hijos.solteras.ordenado <- sort.int(cant.hijos.solteras)
# 
# cant.hijos.casadas.ordenado <- sort.int(cant.hijos.casadas)
                             

hijos.solteras.truncada <- cant.hijos.solteras[poda.hijos.solteras:(largo.hijos.soltera - poda.hijos.solteras)]
hijos.casadas.truncada <- cant.hijos.casadas[poda.hijos.casadas:(largo.hijos.casadas - poda.hijos.casadas)]



# Se construye el dataframe

mujer.ecivil <- c(rep("Mujeres Solteras", length(hijos.solteras.truncada)), rep("Mujeres Casadas", length(hijos.casadas.truncada)))

cantidad.hijos <- c(hijos.solteras.truncada, hijos.casadas.truncada)

datos.truncados <- data.frame(mujer.ecivil, cantidad.hijos)

graficoQQ <- ggqqplot(datos.truncados,
                      x = "cantidad.hijos",
                      facet.by = "mujer.ecivil",
                      palette = c("blue", "red"),
                      color = "mujer.ecivil")
print(graficoQQ)

# Se aplica la prueba de Yuen

prueba.yuen <- yuen(cantidad.hijos ~ mujer.ecivil,
                    data = datos,
                    tr = gamma)
print(prueba.yuen)
# Luego de realizar la prueba de Yuen, se obtuvo un p valor de 0.0319, por lo que se puede decir con un 
# 99% de confianza que se falla al rechazar la hipótesis nula, por lo que la diferencia de medias de la cantidad 
# de hijos nacidos vivos en mujeres casadas y mujeres solteras es 0

#===========================================PREGUNTA 2===========================================

# El numero de personas en el hogar (numper) es similar para viviendas en 
# situación ocupa (V13) "Propia pagada", "Arrendada con contrato" y "Cedida por familiar u otro"
#, en la región metropolitana.

personas.situacion1 <- filter(muestra, muestra$v13 %in% c("Propia pagada"), muestra$region %in% c("Región Metropolitana de Santiago"))

num.personas.situacion1 <- personas.situacion1$numper

personas.situacion2 <- filter(muestra, muestra$v13 %in% c("Arrendada con contrato"), muestra$region %in% c("Región Metropolitana de Santiago"))

num.personas.situacion2 <- personas.situacion2$numper

personas.situacion3 <- filter(muestra, muestra$v13 %in% c("Cedida por familiar u otro"), muestra$region %in% c("Región Metropolitana de Santiago"))

num.personas.situacion3 <- personas.situacion3$numper

# Se realiza el Shapiro test para comprobar normalidad

shapiro.situacion1 <- shapiro.test(num.personas.situacion1)

print(shapiro.situacion1)

shapiro.situacion2 <- shapiro.test(num.personas.situacion2)

print(shapiro.situacion2)

shapiro.situacion3 <- shapiro.test(num.personas.situacion3)

print(shapiro.situacion3)

# Shapiro situación 1
# Shapiro-Wilk normality test
# 
# data:  num.personas.situacion1
# W = 0.62978, p-value = 0.001241
# 
#  Shapiro situación 2
# 
# Shapiro-Wilk normality test
# 
# data:  num.personas.situacion2
# W = 0.95563, p-value = 0.7773
# 
#
# Shapiro situación 3
# 
# Shapiro-Wilk normality test
# 
# data:  num.personas.situacion3
# W = 0.86337, p-value = 0.2725


# En base al test de shapiro se puede concluir con un 95% de confianza que para la situación 1 no 
# se distribuye normalmente y para las situaciones 2 y 3 si se distribuyen normalmente 

#=====================================REMUESTREO================================

# Se calcula la diferencia observada entre las medias muestrales

media.situacion1 <- mean(num.personas.situacion1)
media.situacion2 <- mean(num.personas.situacion2)
media.situacion3 <- mean(num.personas.situacion3)

diferencia.medias1.2 <- media.situacion1 - media.situacion2
diferencia.medias1.3 <- media.situacion1 - media.situacion3
diferencia.medias2.3 <- media.situacion2 - media.situacion3

# H0: La diferencia de medias del numero de personas en el hogar es 0 para viviendas en 
# situación ocupa "Propia pagada", "Arrendada con contrato" y "Cedida por familiar u otro"
# HA: La diferencia de medias del numero de personas en el hogar es distinto de 0 para viviendas en 
# situación ocupa "Propia pagada", "Arrendada con contrato" y "Cedida por familiar u otro"

# Se construye la distribución bootstrap


B <- 9999

valor.nulo <- 0

distribucion.bootstrap1 <- two.boot(num.personas.situacion1, 
                                    num.personas.situacion2,
                                    FUN = mean,
                                    R = B)

desplazamiento1 <- mean(distribucion.bootstrap1[["t"]]) - valor.nulo
distribucion_nula1 <- distribucion.bootstrap1[["t"]] - desplazamiento1

p1 <- (sum(abs(distribucion_nula1) > abs(diferencia.medias1.2)) + 1)/(B + 1)
cat ( "\nValor p 1-2 : " , p1 )

distribucion.bootstrap2 <- two.boot(num.personas.situacion1, 
                                    num.personas.situacion3,
                                    FUN = mean,
                                    R = B)

desplazamiento2 <- mean(distribucion.bootstrap2[["t"]]) - valor.nulo
distribucion_nula2 <- distribucion.bootstrap2[["t"]] - desplazamiento2

p2 <- (sum(abs(distribucion_nula2) > abs(diferencia.medias1.3)) + 1)/(B + 1)
cat ( "\nValor p 1-3 : " , p2 )

distribucion.bootstrap3 <- two.boot(num.personas.situacion2, 
                                    num.personas.situacion3,
                                    FUN = mean,
                                    R = B)

desplazamiento3 <- mean(distribucion.bootstrap3[["t"]]) - valor.nulo
distribucion_nula3 <- distribucion.bootstrap3[["t"]] - desplazamiento3

p3 <- (sum(abs(distribucion_nula3) > abs(diferencia.medias2.3)) + 1)/(B + 1)
cat ( "\nValor p 2-3 : " , p3 )

cat("\n Distancias entre valor p1 y p2", abs(p1-p2))
cat("\n Distancias entre valor p1 y p3", abs(p1-p3))
cat("\n Distancias entre valor p2 y p3", abs(p2-p3))


# Tras aplicar bootstrapping para la prueba de hipótesis, obtienen un valor p para la situación 1-2 de p = 0.3364,
# para 2-3 un valor p de 0.4646 superiores al nivel de significación y para 1-3 un valor p de 0.0371. Además las distancias
# de los valores p son menores a 0,6, por lo que se falla al rechazar la hipótesis nula. En consecuencia, se concluye
# con 95% de confianza que La diferencia de medias del numero de personas en el hogar es 0 para viviendas en 
# situación ocupa "Propia pagada", "Arrendada con contrato" y "Cedida por familiar u otro"

#=====================================MÉTODOS ROBUSTOS================================

situacion.ocupa <- c(rep("Propia pagada", length(num.personas.situacion1)),
                     rep("Arrendada con contrato", length(num.personas.situacion2)),
                     rep("Cedida por familiar u otro", length(num.personas.situacion3)))

personas.hogar <- c(num.personas.situacion1, num.personas.situacion2, num.personas.situacion3)

datos2 <- data.frame(situacion.ocupa,
                     personas.hogar)


# H0: La diferencia de medias del numero de personas en el hogar es 0 para viviendas en 
# situación ocupa "Propia pagada", "Arrendada con contrato" y "Cedida por familiar u otro"
# HA: La diferencia de medias del numero de personas en el hogar es distinto de 0 para viviendas en 
# situación ocupa "Propia pagada", "Arrendada con contrato" y "Cedida por familiar u otro"

alfa <- 0.05
# Comparar los diferentes situaciones de ocupación usando medias truncadas .
cat ( " Comparación entre grupos usando bootstrap \n\n" )

muestras <- 999

gamma <- 0.2

set.seed (300)

bootstrap <- t1waybt(personas.hogar ~ situacion.ocupa,
                            data = datos ,
                            tr = gamma ,
                            nboot = muestras)

print(bootstrap)
 
# Al realizar la prueba t1waybt se obtiene un valor p = 0.10271 > alfa por lo que no
# se necesita realizar un análisis post-hoc. Por consiguiente se falla al rechazar la 
# hipótesis nula, por lo que se puede concluir con un 95% de confianza que La diferencia 
# de medias del numero de personas en el hogar es 0 para viviendas en 
# situación ocupa "Propia pagada", "Arrendada con contrato" y "Cedida por familiar u otro"




