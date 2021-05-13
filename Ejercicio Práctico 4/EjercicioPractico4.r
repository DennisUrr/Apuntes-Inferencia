# Nombres:
# Hernán Pinochet
# Dennis Urrutia

# ------------------- Enunciado -----------

# Se sabe que el proceso de fabricación de barras de acero para concreto reforzado producen barras con medidas
# de dureza que siguen una distribución normal con desviación estándar de 10 kilogramos de fuerza por milímetro
# cuadrado. Usando una muestra aleatoria de tamaño 25, un ingeniero quiere averiguar si una línea de producción
# está generando barras con dureza media de 170 [kgf mm-2].

library(ggplot2)
library(ggpubr)

sd = 10
n = 25
media = 170
SE <- sd/sqrt(n)

# ------------------- Problema D --------------------------------

# ------------------- Pregunta 1 --------------------------------

# Si el ingeniero piensa rechazar la hipótesis nula si la muestra presenta una media menor a 168 [kgf mm-2] o 
# mayor a 172 [kgf mm-2], ¿cuál es la probabilidad de que cometa un error de tipo 1?

#Se genera una secuencia para poder mostrar un gráfico de la distribución normal
x <- seq(media - 5.2*SE, media + 5.2*SE, 0.01)
#Se genera la distribución por medio de la función dnorm
y <- dnorm(x, media, sd=SE)

#Se grafican los datos antes creados para poder realizar un mejor análisis
#Esto para saber cuanto sería la probabilidad (gráficamente)
dist_norm <- plot(x, y, type="l", lty=1, xlab="Z value", ylab="Probability", main="Normal Distribution")

#Se agregan lineas verticales que limitan el intervalo de confianza
abline(v = 168 )
abline(v = 172 )

#Se calcula la probabilidad de cometer un error de tipo I, se multiplica por 2 puesto que son 2 colas
alfa_izq <- pnorm(168, media, sd=SE, lower.tail = TRUE)
alfa_der <- pnorm(172, media, sd=SE, lower.tail = FALSE)

# Se calcula el alfa final
alfa <- alfa_izq + alfa_der

# Al ser una probabilidad muy alta y, viendo que en el gráfico el área que está antes
# del corte del valor mínimo del intervalo de confianza es grande, se puede decir que el valor de alfa es correcto
cat("La probabilidad de cometer un error de tipo I es un:", alfa)

# Y por que nuestra respuesta final es alfa, esto debido a que alfa es el valor que nos representa la
# probabilidad de cometer un error tipo I.

#-------------------------------------------- Problema 2 ------------------------------------------------------------------------

# Si la verdadera dureza media de la línea de producción fuera 173 [kgf mm-2], ¿cuál sería la probabilidad de
# que el ingeniero, que obviamente no conoce este dato, cometa un error de tipo 2?

# Se guarda en una variable la media Real
media_real <- 173

# Se calcula el "delta" como la diferencia de medias real
diferencia_medias <- (media_real - media)

# Se calcula el poder utilizando la función power.t.test para poder obtener beta
poder <- power.t.test(n, diferencia_medias, sd, alfa, alternative = "two.sided")

# Otra forma
# Bsup <- pnorm(172, mean = media_real, sd = SE, lower.tail =  TRUE)
# Binf <- pnorm(168, mean = media_real, sd = SE, lower.tail =  TRUE)
# 
# Bsup <- pnorm(173, mean = media_real, sd = SE, lower.tail =  TRUE)
# 
# 
# B <- Bsup - Binf 
  
#esta región calculada no es la "real", puesto que faltaría eliminar un área que no sería parte del error

#Se calcula beta (probabilidad de cometer un error de tipo II) como 1 - poder (poder = 1 - beta)
Beta <- 1 - poder$power

# Se entrega la probabilidad de cometer un error tipo II, mostrando el valor de B el cual lo indica.
cat("La probabilidad de cometer un error de tipo II es un:", Beta)

#----------------------------------- Problema 3 -----------------------------

# Como no se conoce la verdadera dureza media, genere un gráfico del poder estadístico con las condiciones
# anteriores, pero suponiendo que las verdaderas durezas medias podrían variar de 162 a 178 [kgf mm-2].

#Se calcula el z crítico para posteriormente mostrarlo en un gráfico
#para poder mostrar el área del error de tipo II

# Dureza Media
media = 170

Z_critico <- 172

# Del enunciado indicamos las medias que represntan los q criticos.
q_critico_inferior <- 162
q_critico_superior <- 178

# Se procede a crear el dominio para la realizacion del dnorm en base a la media y al sd
# y se almacenan en un dataframe

x <- seq(100, 250, 0.01)
y <- dnorm(x, mean = media , sd)
df <- data.frame(x, y)

# AL tener los datos listos se procede a graficar.

# Primero se procede a graficar la curva rojo el cual representa la hipótesis nula con sus respectivas
# áreas de restricción

#Se genera la curva
g <- ggplot ( data = df , aes(x))

#Se le agrega una distribución normal
g <- g + stat_function (
  fun = dnorm ,
  args = list ( mean = media , sd),
  colour = "red ", size = 1)

#Se deja el apartado y sin nombre
g <- g + ylab ("")

#Se colorea la parte donde x < q crítico inferior (límite inferior del intervalo de confianza)
g <- g + geom_area( data = subset (df , x < q_critico_inferior),
                    aes (y = y),
                    colour = "red",
                    fill = "red",
                    alpha = 0.5)
#Se colorea la parte donde x > q crítico inferior (límite superior del intervalo de confianza)
g <- g + geom_area( data = subset (df , x > q_critico_superior),
                    aes (y = y),
                    colour = "red",
                    fill = "red",
                    alpha = 0.5)


# Y por ultimo se procede a graficar el gráfico azul el cual representa la hipótesis alternativa con sus respectivas
# áreas de restricción

media_efecto <- 173

x1 <- seq(100, 250, 0.01)
y1 <- dnorm (x, mean = media_efecto , sd )
df1 <- data.frame(x1, y1)


#Se le agrega una distribución normal
g <- g + stat_function (
  fun = dnorm ,
  args = list ( mean = media_efecto , sd ),
  colour = "blue", size = 1)

#Se deja el apartado y sin nombre
g <- g + ylab ("")

#Se colorea la parte donde x < q crítico inferior (límite inferior del intervalo de confianza)
g <- g + geom_area(data = subset (df1, x < q_critico_inferior),
                   aes (x = x1, y = y1),
                   colour = "blue",
                   fill = "blue",
                   alpha = 0.5)

#Se colorea la parte donde x > q crítico inferior (límite superior del intervalo de confianza)

g <- g + geom_area(data = subset (df1, x > q_critico_superior),
                   aes (x = x1, y = y1),
                   colour = " blue ",
                   fill = " blue ",
                   alpha = 0.5)

# Una vez ambos gráficos juntos se puede apreciar el poder debido a los cortes en las áreas de restrición

print (g)

# Y por ultimo con otro gráfico
# Se procede a graficar el poder de manera estático.

# Se crea función para calcular las distribuciones con pnorm
poder <- function(media, SE, lim_inf = NULL, lim_sup = NULL){
  poder_inf <-pnorm(lim_inf, media, SE, lower.tail = TRUE)
  poder_sup <- pnorm(lim_sup, media, SE, lower.tail = FALSE)
  poder_inf + poder_sup
}

# Se crea el dominio para para aplicar la función, con los datos obtenidos de la distribución se procede
# a almacenar en un dataframe.
x2 <- seq(162, 178, 0.1)
y2 <- sapply(x2, poder, SE, lim_inf = 168, lim_sup = 172)
d2 <- data.frame(x=x2,y=y2)

# Por ultimo se procede a graficar.
p10 <- ggplot(d2, aes(x,y))
p10 <- p10 + geom_line(colour = " blue ")
p10 <- p10 + xlab("Dureza media verdadera [kgf/mm²], Tamaño del Efecto")
p10 <- p10 + ylab("Poder estadístico")
print(p10)

