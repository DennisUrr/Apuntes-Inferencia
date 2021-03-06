# Integrantes:
# Hernan Pinochet
# Dennis Urrutia

library(ggplot2)
library(ggpubr)
library(dplyr)
# Indicar directorio
dir <- "C:\\Users\\Dennis\\Documents\\ACLASES\\1-2021\\INFERENCIA\\Ejercicio Pr�ctico 1"

# Una selecci�n de columnas (hecha por el profesor) de los resultados
# p�blicos de la encuesta Casen 2017 en la Regi�n Metropolitana.
# Las siguientes son las columnas que se eligieron:
#  - id.reg: n� secuencial del registro
#  - folio: identificador del hogar (comp: comuna �rea seg viv hogar)
#  - o: n�mero de orden de la persona dentro del hogar
#  - id.vivienda: identificador de la vivienda (comp: comuna �rea seg viv)
#  - hogar: identificaci�n del hogar en la vivienda
#  - region: regi�n
#  - provincia: provincia
#  - comuna: comuna
#  - ing.comuna: posici�n en el ranking hist�rico del ingreso de la
#                comuna (de menor a mayor ingreso)
#  - zona: �rea geogr�fica (Urbano, Rural)
#  - sexo: sexo de la persona registrada
#  - edad: edad de la persona registrada
#  - ecivil: estado civil de la persona registrada
#  - ch1: situaci�n ocupacional de la persona registrada
#  - ytot: ingreso total

basename <- "Casen 2017.csv"
file <- file.path(dir, basename)

# Esta funci�n lee un CSV en ingl�s: campos separados por comas y
# n�meros con "punto decimal".
# Existen otras funciones para leer otros formatos de texto. Ver help.
# Si se quiere leer una planilla MS Excel, se puede usar las funciones
# del paquete "xlsx".
poblaci�n <- read.csv(file = file, encoding = "UTF-8")

# Se devuelve un "marco de datos" o data.frame
print(class(poblaci�n))
print(str(poblaci�n))

# Podemos obtener el n�mero de filas (o de columnas) que corresponde al n�mero de
# "casos" en la muestra/poblaci�n (o de variables).
tama�o.poblaci�n <- nrow(poblaci�n)

# Primero se elige una muestra de forma aleatoria, pero fijando una
# semilla para el generador de secuencias pseudoaleatorias para poder
# repetir los resultados. Luego se fija el tama�o de la muestra y se 
# escoge una muestra al azar.
semilla <- 113
tama�o.muestra <- 100

set.seed(semilla)
muestra <- poblaci�n[sample(1:tama�o.poblaci�n, tama�o.muestra), ]

# Se calculan los quintiles para la muestra seleccionada, y luego se crean grupos
# con las observaciones en cada quintil.
q <- quantile(muestra$ytot, seq(.2, 1, .2))

quintil <- ifelse(muestra$ytot <= q[1], 1, 0)
quintil[muestra$ytot > q[1] & muestra$ytot <= q[2]] <- 2
quintil[muestra$ytot > q[2] & muestra$ytot <= q[3]] <- 3
quintil[muestra$ytot > q[3] & muestra$ytot <= q[4]] <- 4
quintil[muestra$ytot > q[4]] <- 5

# Se crea una nueva matriz de datos con la muestra, a la que se a�aden dos
# variables: el quintil y (por simplicidad) el ingreso en miles de pesos.
datos <- muestra
datos$quintil <- quintil
datos$ingreso <- datos$ytot / 1000

# Se construye un gr�fico de cajas para comparar los diferentes grupos.
p <- ggboxplot(
  datos,
  x = "quintil", y = "ingreso",
  add = "mean", add.params = list(color = "#FC4E07"),
  color = "quintil", fill = "quintil",
  title = "Muestra del ingreso en la Regi�n Metropolitana",
  ylab = "Ingreso total (miles de pesos)"
)
print(p)

# Se muestran los resultados por pantalla.
cat("\n")
cat("L�mites de los quintiles\n")
cat("------------------------\n")
print(c("0%" = 0, q))

cat("\n")
cat("Ingreso medio de cada quintil\n")
cat("-----------------------------\n")
print(by(datos[["ytot"]], datos[["quintil"]], mean))



#===============================================================================#
#f: �Son similares los ingresos registrados en las diferentes provincias de la RM?

# Para la realizaci�n de este problema lo que se har� ser� crear un gr�fico de barra el cual muestre
# el promedio de ingresos de todas las proviencias.
# Se hara un gr�fico para un estudio de tipo muestra y otra de poblacion.

#================================ MUESTRA ======================================#


cat("\n")
cat("Ingreso promedio por provincia de una muestra aleatoria: \n")

# Se usar� la misma muestra describida anteriormente

# En la linea de c�digo a continuaci�n lo que se har� ser� guardar una variable un dataframe
# el cual almacenar� el promedio de ingresos de todas las provincias.
# 1-  Se indicara el dataframe con el cual se trabajara.
# 2-  Se agrupara por provincia por medio del comando group_by() 
#     debido a que la comparaci�n de ingresos se basa tal parametro.
# 3-  Se crear� la tabla final indicando la provincia y su promedio de ingresos,
#     esto se hara por medio del summarise el cual le pasaremos como parametro que indica
#     como nombre de columna "promedioIngresoProvMuestra" y que calcule el promedio de
#     ingresos de las provincias dividido en 1000 (para poder presenciar de mejor manera
#     el valor en los graficos).
             

ing_provincias_muestra <- muestra %>% group_by(provincia) %>% 
  summarise(promedioIngresoProvMuestra = mean(ytot)/1000)

print(ing_provincias_muestra)

# 4-  Como �ltimo paso se realizar� la ejecuci�n del gr�fico por medio de la funci�n ggbarplot, este
#     se rellenara con los siguientes par�metros:
#     a- Indicaremos el dataframe a graficar el cual es: ing_provincias_muestra
#     b- Indicaremos las variables a graficar, como eje X tendremos las provincias y como eje Y
#        tendremos el ingreso promedio de estas.
#     c- En fill indicaremos respectivamente por orden los colores de las barras del grafico.
#     d- Y por ultimo ingresamos las etiquetas las cules son title para el titulo, xlab para la
#        etiqueta del eje X y ylab para la etiqueta del eje Y.

ggbarplot( ing_provincias_muestra ,
           x = "provincia",
           y = "promedioIngresoProvMuestra",
           fill = c(" brown ", " purple ", " orange ", " blue ", " red ", " black "),
           title = " Ingresos por provincias ",
           xlab = " Provincias ",
           ylab = " Ingresos por Provincias (en miles de pesos) de una muestra aleatoria")   

#======================== POBLACI�N ==================================#

# Para el estudio de tipo poblacion se realizaran los mismos pasos y c�digos de la secci�n anterior
# solo que se tienen que cambiar lo par�metros necesarios.

cat("\n")
cat("Ingreso promedio por provincia: \n")

ing_provincias <- poblaci�n %>% group_by(provincia) %>% 
  summarise(promedioIngresoProv = mean(ytot)/1000)

print(ing_provincias)

ggbarplot( ing_provincias ,
           x = "provincia",
           y = "promedioIngresoProv",
           fill = c(" brown ", " purple ", " orange ", " blue ", " red ", " black "),
           title = " Ingresos por provincias ",
           xlab = " Provincias ",
           ylab = " Ingresos por Provincias (en miles de pesos) de la poblaci�n")

# Entonces respondiendo la pregunta �Son similares los ingresos registrados en las diferentes 
# provincias de la RM? 
#
# Muestra: 
#
# En la muestra se nos entrega los siguientes resultados:
# - Chacabuco:  94.1225
# - Cordillera: 376.88400
# - Maipo:      381.49186
# - Melipilla:  168.333
# - Santiago:   610.07785
# - Talagante:  325.995
# Por lo que claramente los ingresos promedio de las provincias son totalmente diferentes en este caso, se tiene como Santiago
# la provincia con mayor promedio de ingresos y Chacabuco la peor, estas separadas por un ingreso de
# alrededor de 515.95535 lo cual es sumamente alto, por otro lado, las provincias de Cordillera, Maipo y Talagante
# se puede decir que si son similares.

# Poblacion:
# 
# Con los datos de la totalidad de la poblacion se nos entrega una realidad muy distinta a los resultados anteriores
# los cuales son los siguientes:
# - Chacabuco:  436.1320
# - Cordillera: 410.7052
# - Maipo:      337.0546
# - Melipilla:  295.9476
# - Santiago:   692.5065
# - Talagante:  378.5773
# Al igual que en el estudio con la muestra, en este tambien se entregan datos los cuales indican los 
# ingresos promedios de las provincias son distintos, pero no del todo, teniendo a Santiago nuevamente 
# como el mayor promedio y Melipilla como el peor, pero, como lo dicho, si se puede logar ver algo de similitud
# en las provincias de Chacabuco, Cordillera, Maipo, Melipilla y Talagante, siendo Santiago la provincia
# en conflicto la cual hace que los promedios no sean del todo similares.


#====== �se observa en la poblaci�n un comportamiento similar al visto en la muestra? =====#

Diferencia <- abs(ing_provincias_muestra$promedioIngresoProvMuestra - ing_provincias$promedioIngresoProv)
Promedio_diferencia <- mean(Diferencia)

print(Diferencia)
print(Promedio_diferencia)

# Como observacion visible a primera vista se tiene que Chacabuco 
# tuvo un cambio muy dr�stico en su promedio de ingresos, en la muestra se indicaba que este tenia un 
# promedio alrededor de 94.1225, en cambio en la poblacion tuvo un incremento muy alto de 342.0095,
# dejandose con un promedio de ingresos de 436.1320.
# Los cambios en bruto de muestra a poblaci�n ser�an alrededor de los siguientes:
# - Chacabuco:  342,00671  
# - Cordillera: 33.82116
# - Maipo:      44.43730
# - Melipilla:  127.61464
# - Santiago:   82.42864
# - Talagante:  52.58226
# Con los datos anteriores, se puede inferir que en todas las provincias hubieron diferencias de
# ingresos promedios, teniendo un promedio de diferencia de 113.8151, por lo que se puede concluir que 
# el comportamiento entre ambos tipos de estudio, muestra y poblaci�n son muy distintos.

#===========================================================================================#
#�Van los ingresos de los chilenos increment�ndose con la experiencia y de forma similar entre hombres y mujeres?
#===========================================================================================#

#=================================  Muestra  ====================================================#
# Se guarda en experiencia un filtro por edad a la muestra, luego se guardan las variables
# experiencia, sexo y el ingreso divido en 1000 para posteriormente tener una mejor apreciaci�n
# de los datos
experiencia <- muestra %>% filter(edad >= 18) %>%
  summarise(experiencia = edad-18,
            sexo = sexo,
            ingreso = ytot/1000)

#De la variable creada anteriormente, se vuelve a filtrar para ver los datos s�lo de hombres
experiencia_hombre <- filter(experiencia, sexo == "Hombre")


#Se grafican los datos hablados anteriormente en un gr�fico de dispersi�n
#para poder ver si la variable experiencia tiene relaci�n con la variable de
#ingresos
ggscatter( experiencia_hombre,
           x = "experiencia",
           y = "ingreso",
           color = "red",
           title = " Ingresos por experiencia hombres ",
           xlab = " experiencia",
           ylab = " ingreso (en miles de pesos)")

#De la misma forma, se filtra por sexo mujer los resultados
experiencia_mujer <- filter(experiencia, sexo == "Mujer")
#De manera an�loga, se grafican los datos
ggscatter( experiencia_mujer,
           x = "experiencia",
           y = "ingreso",
           title = " Ingresos por experiencia mujeres ",
           xlab = " experiencia",
           ylab = " ingreso (en miles de pesos)")

#=================================  POBLACI�N ====================================================#

#B�sicamente es lo mismo que se realiz� con la muestra, solo que en vez de utilizar la variable muestra,
#se utiliza la variable poblaci�n 


# Se guarda en experiencia un filtro por edad a la poblaci�n, luego se guardan las variables
# experiencia, sexo y el ingreso divido en 1000 para posteriormente tener una mejor apreciaci�n
# de los datos
experiencia <- poblaci�n %>% filter(edad >= 18) %>%
  summarise(experiencia = edad-18,
            sexo = sexo,
            ingreso = ytot/1000)

experiencia_hombre_poblaci�n <- filter(experiencia, sexo == "Hombre")


ggscatter( experiencia_hombre_poblaci�n,
           x = "experiencia",
           y = "ingreso",
           title = " Ingresos por experiencia hombres ",
           xlab = " experiencia",
           ylab = " ingreso (en miles de pesos)")

experiencia_mujer_poblaci�n <- filter(experiencia, sexo == "Mujer")

ggscatter( experiencia_mujer_poblaci�n,
           x = "experiencia",
           y = "ingreso",
           title = " Ingresos por experiencia mujeres ",
           xlab = " experiencia",
           ylab = " ingreso (en miles de pesos)")


#===================================================================================================================#
# g. �Van los ingresos de los chilenos increment�ndose con la experiencia y de forma similar entre hombres y mujeres?
#===================================================================================================================#
# Como se puede apreciar en los gr�ficos, la experiencia no tiene una relaci�n con los ingresos, puesto que 
# los puntos est�n muy dispersos en el gr�fico y no siguen ning�n patr�n, por lo tanto se dice que la experiencia
# no influye en el ingreso total de la persona. No se dir�a que se hace de forma similar, ya que los datos no tienen 
# un patr�n definido. 





