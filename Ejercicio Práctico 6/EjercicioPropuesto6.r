# Integrantes:
#   Hern�n Pinochet
#   Dennis Urrutia

# Una de las primeras preguntas a responder por el �ltimo estudio nacional de obesidad infantil fue si exist�an
# diferencias en la prevalencia de la obesidad entre ni�os y ni�as o si, por el contrario, el porcentaje de obesos no
# var�a entre sexos. Se les solicita responder esta pregunta, contando con las primeras observaciones obtenidas en
# el estudio sobre una muestra de 14 menores:

#H_0: no existen diferencias en la prevalencia de la obesidad entre ni�os y ni�as
#H_A: existen diferencias en la prevalencia de la obesidad entre ni�os y ni�as

#Se define el nivel de significancia
alfa <- 0.05
#Se crea una columna con la repetici�n de ni�os 9 veces y ni�a 5 veces
ni�os <- c(rep("ni�o", 9), rep("ni�a", 5))
#Se crea una columna con la repetici�n de si o no dependiendo de la tabla dada en el enunciado
obesos <- c(rep("si", 7), rep("no", 2), rep("si", 1), rep("no", 4)) 
#se crea el dataframe
datos_A <- data.frame(ni�os, obesos)
#Se hace una tabla con los datos del dataframe reducidos
tabla_A <- xtabs (~., datos_A)
#La cantidad de datos de la muestra son pocos, por lo que quedan dos opciones por utilizar, fisher y McNemar
#Se ejecuta la prueba de fisher porque se quiere determinar si la obesidad entre ni�os y ni�as es independiente entre si
#y no pareadas.
prueba_fisher <- fisher.test(tabla_A, 1-alfa)

print("======================================================================")
print("=======================Soluci�n Problema A============================")
print("======================================================================")

# Se estudia la variable p con respecto alfa
if(prueba_fisher$p.value > alfa){
  print("como el valor p es mayor al nivel de significaci�n, se falla al rechazar H_0")
  print("Los datos sugieren la conclusi�n dada en la pregunta, puesto que como no se rechaza H_0, se puede asegurar con un 95% de confianza de que no existen diferencias en la prevalencia de la obesidad entre ni�os y ni�as")
} else {
  print("No hay evidencia suficiente para rechazar la hip�tesis H_0")
  print("Se necesita mayor informaci�n para poder fallar a favor de H_0")
}

# En un art�culo de Garc�a y colaboradores (2010) se describe un estudio en que se compararon diferentes versiones
# de algoritmos evolutivos para resolver variadas instancias de problemas de clasificaci�n tomadas desde el
# repositorio UCI Machine Learning. Suponga que la siguiente tabla muestra los resultados de la clasificaci�n hecha
# por dos versiones de un algoritmo gen�tico evaluado en el estudio para el problema Breast Cancer. �Consigue uno
# de los algoritmos mejor desempe�o?

#La cantidad de datos de la muestra son pocos, por lo que quedan dos opciones por utilizar, fisher y McNemar
# Al ser una muestra pareada (puesto que se utiliza dos algortitmos en un mismo problema (Breast Cancer)), se utilizar�
# la prueba de McNemar 

#H_0: los dos algoritmos tienen el mismo desempe�o
#H_A: los dos algoritmos tienen distinto desempe�o

#Se define el nivel de significancia
alfa <- 0.05

#Se genera una secuancia para enumerar las pruebas
prueba <- seq(1, 12, 1)

#Se generan las columnas corriespondientes a la tabla
AG_v1 <- c(rep("Incorrecta", 1), 
           rep("Correcta", 1), 
           rep("Incorrecta", 1), 
           rep("Correcta", 1), 
           rep("Incorrecta", 2), 
           rep("Correcta", 3),
           rep("Incorrecta", 3))

AG_v2 <- c(rep("Correcta", 4), 
           rep("Incorrecta", 1), 
           rep("Correcta", 2), 
           rep("Incorrecta", 2), 
           rep("Correcta", 3))

#Se colocan las columnas en un dataframe
datos_B <- data.frame(prueba, AG_v1, AG_v2)

#Se pasa el dataframe a tabla para poder utilizar la prueba
tabla_B  <- table(AG_v2, AG_v1)

#Se agrega la suma de las columnas
addmargins(tabla_B)

#Se aplica la prueba de mcnemar
prueba_McNemar <- mcnemar.test(tabla_B)

print("======================================================================")
print("=======================Soluci�n Problema B============================")
print("======================================================================")

# Se estudia la variable p con respecto alfa
if(prueba_McNemar$p.value > alfa){
  print("como el valor p es mayor al nivel de significaci�n, se falla al rechazar H_0")
  print("Los datos sugieren la conclusi�n dada en la pregunta, puesto que como no se rechaza H_0, se puede asegurar con un 95% de confianza de que los dos algoritmos tienen el mismo desempe�o")
} else {
  print("No hay evidencia suficiente para rechazar la hip�tesis H_0")
  print("Se necesita mayor informaci�n para poder fallar a favor de H_0")
}

# La siguiente tabla resume los resultados en matem�ticas de la evaluaci�n PISA 2015 en t�rminos de la cantidad
# porcentual de estudiantes que alcanzaron los diferentes niveles de desempe�o de la prueba. Los estudiantes que
# se encuentran bajo el nivel 2, no alcanzan las competencias m�nimas requeridas para participar completamente
# en una sociedad moderna (m�s detalles en https://www.agenciaeducacion.cl/estudios-internacionales/pisa/).
# Pareciera ser claro que Chile aventaja a Latinoam�rica pero que est� atr�s con respecto al promedio de la OCDE.
# Menos clara est� la comparaci�n con los pa�ses que tienen un PIB similar. �Ser�n distintos los resultados entre
# estas regiones? Nota: 7.053 adolescentes chilenos participaron en la prueba, mientras que los resultados para
# pa�ses con PIB similar se basan en 46.994 estudiantes.

#Se generan las filas con los datos de la tabla
Chile <- c(1.4, 23.8, 25.5, 49.3)
LATAM <- c(0.7, 13.8, 21.3, 64.3)
PIB_similar <- c(6.8, 37, 24.5, 64.3)
Prom_OCDE <- c(10.7, 43.4, 22.5, 23.4)

# H_0: son iguales los resultados para Chile y LATAM
# H_A: son distintos los resultados para Chile y LATAM

#Se genera una tabla para aplicar el test a Chile y LATAM
tabla_C_L <- as.table(rbind(Chile, LATAM))

#Se colocan los nombres de las columnas y filas
dimnames(tabla_C_L) <- list(regi�n = c("Chile", "LATAM"), Niveles = c("5-6", "3-4", "2", "Menor a 2"))

#Se realiza el test de chi cuadrado para comprobar independencia entre variables
prueba_chisq_C_L <- chisq.test(tabla_C_L, correct = FALSE)

print("======================================================================")
print("=======================Soluci�n Problema E============================")
print("======================================================================")

# Se estudia la variable p con respecto alfa
if(prueba_chisq_C_L$p.value > alfa){
  print("como el valor p es mayor al nivel de significaci�n, se falla al rechazar H_0")
  print("Los datos sugieren la conclusi�n dada en la pregunta, puesto que como no se rechaza H_0, se puede asegurar con un 95% de confianza de que son iguales los resultados para Chile y LATAM")
} else {
  print("No hay evidencia suficiente para rechazar la hip�tesis H_0")
  print("Se necesita mayor informaci�n para poder fallar a favor de H_0")
}

# H_0: son iguales los resultados para Chile y los que tienen PIB similar
# H_A: son distintos los resultados para Chile y los que tienen PIB similar

#Se genera una tabla para aplicar el test a Chile y las regiones con PIB similar
tabla_C_PIB <- as.table(rbind(Chile, PIB_similar))
#Se colocan los nombres de las columnas y filas
dimnames(tabla_C_PIB) <- list(regi�n = c("Chile", "PIB_similar"), Niveles = c("5-6", "3-4", "2", "Menor a 2"))
#Se realiza el test de chi cuadrado para comprobar independencia entre variables
prueba_chisq_C_PIB <- chisq.test(tabla_C_PIB, correct = FALSE)

# Se estudia la variable p con respecto alfa
if(prueba_chisq_C_PIB$p.value > alfa){
  print("como el valor p es mayor al nivel de significaci�n, se falla al rechazar H_0")
  print("Los datos sugieren la conclusi�n dada en la pregunta, puesto que como no se rechaza H_0, se puede asegurar con un 95% de confianza de que son iguales los resultados para Chile y los que tienen PIB similar")
} else {
  print("No hay evidencia suficiente para rechazar la hip�tesis H_0")
  print("Se necesita mayor informaci�n para poder fallar a favor de H_0")
}

# H_0: son iguales los resultados para Chile y el promedio OCDE
# H_A: son distintos los resultados para Chile y el promedio OCDE

#Se genera una tabla para aplicar el test a Chile y el promedio de OCDE
tabla_C_OCDE <- as.table(rbind(Chile, Prom_OCDE))
#Se colocan los nombres de las columnas y filas
dimnames(tabla_C_OCDE) <- list(regi�n = c("Chile", "Prom_OCDE"), Niveles = c("5-6", "3-4", "2", "Menor a 2"))
#Se realiza el test de chi cuadrado para comprobar independencia entre variables
prueba_chisq_C_OCDE <- chisq.test(tabla_C_OCDE, correct = FALSE)

# Se estudia la variable p con respecto alfa
if(prueba_chisq_C_OCDE$p.value > alfa){
  print("como el valor p es mayor al nivel de significaci�n, se falla al rechazar H_0")
  print("Los datos sugieren la conclusi�n dada en la pregunta, puesto que como no se rechaza H_0, se puede asegurar con un 95% de confianza de que son iguales los resultados para Chile y el promedio OCDE")
} else {
  print("No hay evidencia suficiente para rechazar la hip�tesis H_0")
  print("Se necesita mayor informaci�n para poder fallar a favor de H_0")
}

print("======================================================================")
print("=========================Respuesta Pregunta===========================")
print("======================================================================")

print("�por qu� las pruebas de la familia chi� son no param�tricas?")

print("Esto es debido a que no menciona ni utiliza algun parametro, incluso, no hace ninguna suposicion sobre la distribucion de la poblacion donde proviene la muestra
analizada, a las no parametricas tambien se les llama libres de distribucion. La ventaja de que esta no sea parametrica es que son menos restrictivas, lo que significa
que no imponen supuestos a las poblaciones para poder trabajarlas.")




