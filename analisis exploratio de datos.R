#------------------------------------------------------#
#-- Universidad del Valle: Escuela de Estadistica    --#
#-- Asignatura: Probabilidad y Estadística           --#
#-- Estudiantes: J.J MAFLA 2126990 - S.Tamayo 2110331--#   
#------------------------------------------------------#
# Alternatively, install just dplyr:
install.packages("dplyr")

##### Taller Análisis Exploratorio del Datos #####

#### 1. Lectura de datos ####

datos <- read.table("data_embutidos.txt", header=TRUE, dec=".")

names(datos) #nombres de las variables en la BD.
str(datos)  #Indica el tipo de variable
head(datos) #Muestra las primeras lineas de la BD.


#### 2. Analisis general de los pesos ####

# Resumen de las medidas de tendencia central de los pesos de los embutidos
summary(datos$peso)

#### 2.1 Histograma general de los pesos para los embutidos ####
hist(
  datos$peso,
  main = "Histograma de pesos de todos los embutidos",
  xlab="Peso (gr)",
  ylab="Frecuencia", 
  xlim = c(205, 230),
  col = "#ffe65d"
)

abline(v=c(212, 220, 228), lty=c(2, 1, 2), lwd=2, col="blue")

# Porcentaje de pesos que estan por fuera de los limites especificos 220 ± 8gr
sum(datos$peso < 212)
P.menores.LEI = sum(datos$peso < 212) / length(datos$peso)
P.mayores.LES = sum(datos$peso > 228) / length(datos$peso)

#### 2.2 Histograma de pesos por maquina ####

#### 2.2.1 Maquina 1 #####
maquina1 <- datos %>% filter(maquina != 2)
maquina1

summary(maquina1$peso)

hist(
  maquina1$peso,
  main = "Histograma de pesos para la maquina 1",
  xlab="Peso (gr)",
  ylab="Frecuencia", 
  xlim = c(205, 230),
  col = "#ed8df8"
)

abline(v=c(212, 220, 228), lty=c(2, 1, 2), lwd=2, col="blue")

#### 2.2.2 Maquina 2 #####
maquina2 <- datos %>% filter(maquina != 1)
maquina2

summary(maquina2$peso)

hist(
  maquina2$peso,
  main = "Histograma de pesos para la maquina 2",
  xlab="Peso (gr)",
  ylab="Frecuencia", 
  xlim = c(206, 230),
  col = "#CDC8B1"
)

abline(v=c(212, 220, 228), lty=c(2, 1, 2), lwd=2, col="blue")

par(mfrow = c(1, 2)) # Particionar ventana 
