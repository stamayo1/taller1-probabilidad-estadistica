#------------------------------------------------------#
#-- Universidad del Valle: Escuela de Estadistica    --#
#-- Asignatura: Probabilidad y Estadística           --#
#-- Estudiantes: J.J MAFLA 2126990 - S.Tamayo 2110331--#   
#------------------------------------------------------#

##### Taller Análisis Exploratorio del Datos #####

#### 1. Lectura de datos ####

datos <- read.table("data_embutidos.txt", header=TRUE, dec=".")

names(datos) #nombres de las variables en la BD.
str(datos)  #Indica el tipo de variable
head(datos) #Muestra las primeras lineas de la BD.


#### 2. Analisis general de los pesos ####

summary(datos$peso)

#### 2.1 Histograma general de los pesos para los embutidos ####
hist(
  datos$peso,
  main = "Histograma del peso de los embutidos",
  xlab="Peso (gr)",
  ylab="Frecuencia", 
  xlim = c(205, 230),
  col = "#ffe65d"
)

abline(v=c(212, 220, 228), lty=c(2, 1, 2), lwd=2, col="blue")


#### 2.2 Histograma de peso por maquina ####

#### 2.2.1 Maquina 1 #####
maquina1 <- datos %>% filter(maquina != 2)
maquina1

hist(
  maquina1$peso,
  main = "Histograma de peso para la maquina 1",
  xlab="Peso (gr)",
  ylab="Frecuencia", 
  xlim = c(205, 230),
  col = "#ed8df8"
)

abline(v=c(212, 220, 228), lty=c(2, 1, 2), lwd=2, col="blue")

#### 2.2.2 Maquina 2 #####
maquina2 <- datos %>% filter(maquina != 1)
maquina2

hist(
  maquina2$peso,
  main = "Histograma de peso para la maquina 2",
  xlab="Peso (gr)",
  ylab="Frecuencia", 
  xlim = c(205, 230),
  col = "#ffe65d"
)

abline(v=c(212, 220, 228), lty=c(2, 1, 2), lwd=2, col="blue")
