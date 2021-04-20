library(ggplot2)
library(ggpubr)
library(tidyr)

#Se indica el archivo base
dir <- "C:\\Users\\Dennis\\Documents\\ACLASES\\1-2021\\INFERENCIA\\Ejercicio Práctico 2"
basename <- "Casen 2017.csv"
file <- file.path(dir, basename)

#Se asigna a población el dataframe completo del archivo
población <- read.csv(file = file)
tamaño <- nrow(población)

#Se cre la variable ingreso con el ingreso total de cada dato de la población
ingreso <- as.numeric(población[["ytot"]])

#probabilidad
poda <- 0.3
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)

#Se calcula la media del ingreso
media.ingreso <- mean(ingreso.podado)

#se calcula la desviación estandar del ingreso
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )

set.seed(133)
#Se calcula la normal de la población
ingreso.normal <- rnorm(1000, mean = media.ingreso, sd = sd.ingreso)

ingreso.z <- data.frame(
  Ingresos = c((ingreso.normal-media.ingreso)/sd.ingreso)
)

p <- gghistogram(
  data = ingreso.z,
  x = "Ingresos",
  y = "..density..", # Esto porque el conteo es muy distinto
  bins = 31
)


set.seed(176)
ingreso.chi <- sample((ingreso.normal - media.ingreso) / sd.ingreso, 30)

secuencias6 <- combn(ingreso.chi, 6)
chisq6 <- apply(secuencias6, 2, function(x) sum(x^2))

ingreso.chisq6 <- data.frame(
  Sumas = chisq6
)
p.chisq6 <- ggplot(ingreso.chisq6, aes(x = Sumas))
p.chisq6 <- p.chisq6 + geom_histogram(
  aes(y = ..density..),
  binwidth = 1,
  col = "black", fill = "#000099"
)
p.chisq6 <- p.chisq6 + xlab("Sumas cuadradas de 6 valores z")
p.chisq6 <- p.chisq6 + ylab("Densidad")


secuencias9 <- combn(ingreso.chi, 9)

chisq9 <- apply(secuencias9, 2, function(x) sum(x^2))

ingreso.chisq9 <- data.frame(
  Sumas = chisq9
)
p.chisq9 <- ggplot(ingreso.chisq9, aes(x = Sumas))
p.chisq9 <- p.chisq9 + geom_histogram(
  aes(y = ..density..),
  binwidth = 1,
  col = "black", fill = "#000099"
)
p.chisq9 <- p.chisq9 + xlab("Sumas cuadradas de 9 valores z")
p.chisq9 <- p.chisq9 + ylab("Densidad")










