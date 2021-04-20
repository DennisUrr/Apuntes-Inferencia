library(ggplot2)
library(ggpubr)
library(dplyr)
# Indicar directorio
dir <- "C:\\Users\\Dennis\\Documents\\ACLASES\\1-2021\\INFERENCIA\\Ejercicio Práctico 1"

# Una selección de columnas (hecha por el profesor) de los resultados
# públicos de la encuesta Casen 2017 en la Región Metropolitana.
# Las siguientes son las columnas que se eligieron:
#  - id.reg: nº secuencial del registro
#  - folio: identificador del hogar (comp: comuna área seg viv hogar)
#  - o: número de orden de la persona dentro del hogar
#  - id.vivienda: identificador de la vivienda (comp: comuna área seg viv)
#  - hogar: identificación del hogar en la vivienda
#  - region: región
#  - provincia: provincia
#  - comuna: comuna
#  - ing.comuna: posición en el ranking histórico del ingreso de la
#                comuna (de menor a mayor ingreso)
#  - zona: área geográfica (Urbano, Rural)
#  - sexo: sexo de la persona registrada
#  - edad: edad de la persona registrada
#  - ecivil: estado civil de la persona registrada
#  - ch1: situación ocupacional de la persona registrada
#  - ytot: ingreso total

basename <- "Casen 2017.csv"
file <- file.path(dir, basename)

# Esta función lee un CSV en inglés: campos separados por comas y
# números con "punto decimal".
# Existen otras funciones para leer otros formatos de texto. Ver help.
# Si se quiere leer una planilla MS Excel, se puede usar las funciones
# del paquete "xlsx".
población <- read.csv(file = file, encoding = "UTF-8")

# Se devuelve un "marco de datos" o data.frame
print(class(población))
print(str(población))

# Podemos obtener el número de filas (o de columnas) que corresponde al número de
# "casos" en la muestra/población (o de variables).
tamaño.población <- nrow(población)

# Primero se elige una muestra de forma aleatoria, pero fijando una
# semilla para el generador de secuencias pseudoaleatorias para poder
# repetir los resultados. Luego se fija el tamaño de la muestra y se 
# escoge una muestra al azar.
semilla <- 113
tamaño.muestra <- 100

set.seed(semilla)
muestra <- población[sample(1:tamaño.población, tamaño.muestra), ]

# Se calculan los quintiles para la muestra seleccionada, y luego se crean grupos
# con las observaciones en cada quintil.
q <- quantile(muestra$ytot, seq(.2, 1, .2))

quintil <- ifelse(muestra$ytot <= q[1], 1, 0)
quintil[muestra$ytot > q[1] & muestra$ytot <= q[2]] <- 2
quintil[muestra$ytot > q[2] & muestra$ytot <= q[3]] <- 3
quintil[muestra$ytot > q[3] & muestra$ytot <= q[4]] <- 4
quintil[muestra$ytot > q[4]] <- 5

# Se crea una nueva matriz de datos con la muestra, a la que se añaden dos
# variables: el quintil y (por simplicidad) el ingreso en miles de pesos.
datos <- muestra
datos$quintil <- quintil
datos$ingreso <- datos$ytot / 1000

# Se construye un gráfico de cajas para comparar los diferentes grupos.
p <- ggboxplot(
  datos,
  x = "quintil", y = "ingreso",
  add = "mean", add.params = list(color = "#FC4E07"),
  color = "quintil", fill = "quintil",
  title = "Muestra del ingreso en la Región Metropolitana",
  ylab = "Ingreso total (miles de pesos)"
)
print(p)

# Se muestran los resultados por pantalla.
cat("\n")
cat("Límites de los quintiles\n")
cat("------------------------\n")
print(c("0%" = 0, q))

cat("\n")
cat("Ingreso medio de cada quintil\n")
cat("-----------------------------\n")
print(by(datos[["ytot"]], datos[["quintil"]], mean))



#===============================================================================#
#f: ¿Son similares los ingresos registrados en las diferentes provincias de la RM?
#================================ MUESTRA ======================================#


cat("\n")
cat("Ingreso promedio por provincia de una muestra aleatoria: \n")

# Se usará la misma muestra describida anteriormente
ing_provincias_muestra <- muestra %>% group_by(provincia) %>% 
  summarise(promedioIngresoProvMuestra = mean(ytot)/1000)

print(ing_provincias_muestra)

ggbarplot( ing_provincias_muestra ,
           x = "provincia",
           y = "promedioIngresoProvMuestra",
           fill = c(" brown ", " purple ", " orange ", " blue ", " red ", " black "),
           title = " Ingresos por provincias ",
           xlab = " Provincias ",
           ylab = " Ingresos por Provincias (en miles de pesos) de una muestra aleatoria")   

#======================== POBLACIÓN ==================================#

cat("\n")
cat("Ingreso promedio por provincia: \n")

ing_provincias <- población %>% group_by(provincia) %>% 
  summarise(promedioIngresoProv = mean(ytot)/1000)

print(ing_provincias)

ggbarplot( ing_provincias ,
           x = "provincia",
           y = "promedioIngresoProv",
           fill = c(" brown ", " purple ", " orange ", " blue ", " red ", " black "),
           title = " Ingresos por provincias ",
           xlab = " Provincias ",
           ylab = " Ingresos por Provincias (en miles de pesos) de la población")

#======= ¿se observa en la población un comportamiento similar al visto en la muestra? =====#
# Sólo en algunas provincias, se puede apreciar que cuando se realiza este estudio con la muestra, chacabuco
# tiene un ingreso promedio de 100 mil pesos aproximadamente, en cambio cuando se realiza con la población, se puede ver que
# tiene un ingreso promedio de poco mas de 400 mil pesos.


#===========================================================================================#
#¿Van los ingresos de los chilenos incrementándose con la experiencia y de forma similar entre hombres y mujeres?
#===========================================================================================#

#=================================  Muestra  ====================================================#

promedio_ingresos <- mean(muestra$ytot)

experiencia <- muestra %>% filter(edad >= 18) %>%
  summarise(experiencia = edad-18,
            sexo = sexo,
            ingreso = ytot/1000)

experiencia_hombre <- filter(experiencia, sexo == "Hombre")




ggscatter( experiencia_hombre,
           x = "experiencia",
           y = "ingreso",
           color = "red",
           title = " Ingresos por experiencia hombres ",
           xlab = " experiencia",
           ylab = " ingreso (en miles de pesos)")

experiencia_mujer <- filter(experiencia, sexo == "Mujer")

ggscatter( experiencia_mujer,
           x = "experiencia",
           y = "ingreso",
           title = " Ingresos por experiencia mujeres ",
           xlab = " experiencia",
           ylab = " ingreso (en miles de pesos)")


#=================================  POBLACIÓN ====================================================#

promedio_ingresos_población <- mean(población$ytot)

experiencia <- población %>% filter(edad >= 18) %>%
  summarise(experiencia = edad-18,
            sexo = sexo,
            ingreso = ytot/1000)

experiencia_hombre_población <- filter(experiencia, sexo == "Hombre")




ggscatter( experiencia_hombre_población,
           x = "experiencia",
           y = "ingreso",
           title = " Ingresos por experiencia hombres ",
           xlab = " experiencia",
           ylab = " ingreso (en miles de pesos)")

experiencia_mujer_población <- filter(experiencia, sexo == "Mujer")

ggscatter( experiencia_mujer_población,
           x = "experiencia",
           y = "ingreso",
           title = " Ingresos por experiencia mujeres ",
           xlab = " experiencia",
           ylab = " ingreso (en miles de pesos)")








