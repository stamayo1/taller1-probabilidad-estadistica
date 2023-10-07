#------------------------------------------------------#
#-- Universidad del Valle: Escuela de Estadistica    --#
#-- Asignatura: Probabilidad y Estadística           --#
#-- Estudiantes: J.J MAFLA 2126990 - S.Tamayo 2110331--#   
#------------------------------------------------------#
# Alternatively, install just dplyr:
install.packages("dplyr")
library(dplyr) #Cargando la libreria dplyr

##### Taller Análisis Exploratorio del Datos #####

#### 1. Lectura de datos ####

datos <- read.table("data_embutidos.txt", header=TRUE, dec=".")

names(datos) #nombres de las variables en la BD.
head(datos) #Muestra las primeras lineas de la BD.

#### 2. Analisis general de los pesos ####

# Resumen de las medidas de tendencia central de todos los pesos

summary(datos$peso)
sd(datos$peso)

#### 2.1 Histograma general de los pesos para los embutidos ####
hist(
  datos$peso,
  main = "Histograma de pesos de todos los embutidos",
  xlab="Peso (gr)",
  ylab="Frecuencia acumulada", 
  xlim = c(205, 230),
  col = "#ffe65d"
)

#Las especificaciones del peso son 220 ± 8 gr
abline(v=c(212, 220, 228), lty=c(2, 1, 2), lwd=2, col="blue")

# Porcentaje de pesos que estan por fuera de los limites especificos 220 ± 8gr
peso.menores.LEI = sum(datos$peso < 212) / length(datos$peso)
peso.mayores.LES = sum(datos$peso > 228) / length(datos$peso)

#### 3. Analisis Maquina 1 ####

peso.maquina1 <- filter(datos, maquina == 1);
peso.maquina1.mean <- tapply(peso.maquina1$peso, peso.maquina1[,1] , mean)

summary(peso.maquina1$peso) #resumen de tendencias 
sd(peso.maquina1$peso) #desviación 

#### 3.1 Histograma Maquina 1 #####

hist(
  peso.maquina1$peso,
  main = "Histograma de pesos maquina 1",
  xlab="Peso (gr)",
  ylab="Frecuencia", 
  xlim = c(205, 230),
  col = "#ed8df8"
)

abline(v=c(212, 220, 228), lty=c(2, 1, 2), lwd=2, col="blue")

#### 3.2 Dispersión  Maquina 1 #####
dias  <- c(1:20)
plot(dias, peso.maquina1.mean, pch=1,  xlab="Dia", ylab="Peso promedio (gr)", main="Peso promedio maquina 1", xlim = c(0, 20), ylim=c(205,230))
abline(h=c(212, 220, 228), lty=c(1, 2, 3), lwd=2, col="blue")

#### 4 Analisis Maquina 2 ####

peso.maquina2 <- filter(datos, maquina == 2);
peso.maquina2.mean <- tapply(peso.maquina2$peso, peso.maquina2[,1] , mean)

summary(peso.maquina2$peso) #resumen de tendencias 
sd(peso.maquina2$peso) #desviación 

#### 4.1 Histograma Maquina 2 #####

hist(
  peso.maquina2$peso,
  main = "Histograma de pesos maquina 2",
  xlab="Peso (gr)",
  ylab="Frecuencia", 
  xlim = c(205, 230),
  col = "#98FB98"
)

abline(v=c(212, 220, 228), lty=c(2, 1, 2), lwd=2, col="blue")

#### 4.2 Dispersión  Maquina 2 #####
dias  <- c(1:20)
plot(dias, peso.maquina2.mean, pch=1,  xlab="Dia", ylab="Peso promedio (gr)", main="Peso promedio maquina 2", xlim = c(0, 20), ylim=c(205,230))
abline(h=c(212, 220, 228), lty=c(1, 2, 3), lwd=2, col="blue")

par(mfrow = c(1, 2)) # Particionar ventana

#### 5 Analisis general Operario A ####

peso.operarioA <- filter(datos, operario == 'A');
peso.operarioA.mean <- tapply(peso.operarioA$peso, peso.operarioA[,1], mean)

median(peso.operarioA.mean) # Promedio general de los días laborados

summary(peso.operarioA$peso) #resumen de tendencias 
sd(peso.operarioA$peso) #desviación

#### 5.1 Dispersión  operario A #####
dias  <- c(1,5,6,7,10,14,15,18,19,20)

plot(dias, peso.operarioA.mean, pch=1,  xlab="Dia laborado", ylab="Peso promedio (gr)", main="Peso promedio operario a", ylim=c(205,230), xlim=c(1,20))
abline(h=c(212, 220, 228), lty=c(1, 2, 3), lwd=2, col="blue")


#### 6. Analisis general Operario B ####

peso.operarioB <- filter(datos, operario == 'B');
peso.operarioB.mean <- tapply(peso.operarioB$peso, peso.operarioB[,1], mean)

median(peso.operarioB.mean) # Promedio general de los días laborados

summary(peso.operarioB$peso) #resumen de tendencias 
sd(peso.operarioB$peso) #desviación

#### 6.1 Dispersión  operario B #####
dias  <- c(2,3,4,8,9,11,12,13,16,17)

plot(dias, peso.operarioB.mean, pch=1,  xlab="Dia laborado", ylab="Peso promedio (gr)", main="Peso promedio operario b", ylim=c(205,230), xlim=c(2,17))
abline(h=c(212, 220, 228), lty=c(1, 2, 3), lwd=2, col="blue")



#### 7. Analisis Operario A vs Maquina 1 ####

peso.operarioA.maquina1 <- filter(peso.maquina1, operario == 'A');
peso.operarioA.maquina1.mean <- tapply(peso.operarioA.maquina1$peso, peso.operarioA.maquina1[,1], mean)

summary(peso.operarioA.maquina1$peso) #resumen de tendencias 
sd(peso.operarioA.maquina1$peso) #desviación

#### 7.1 Dispersión  Operario A vs Maquina 1 #####
dias  <- c(1,5,6,7,10,14,15,18,19,20)
plot(dias, peso.operarioA.maquina1.mean, pch=1,  xlab="Dia laborado", ylab="Peso promedio (gr)", main="Peso promedio operario a con maquina 1", ylim=c(205,230), xlim=c(2,17))
abline(h=c(212, 220, 228), lty=c(1, 2, 3), lwd=2, col="blue")



#### 8. Analisis Operario A vs Maquina 2 ####

peso.operarioA.maquina2 <- filter(peso.maquina2, operario == 'A');
peso.operarioA.maquina2.mean <- tapply(peso.operarioA.maquina2$peso, peso.operarioA.maquina2[,1], mean)

summary(peso.operarioA.maquina2$peso) #resumen de tendencias 
sd(peso.operarioA.maquina2$peso) #desviación

#### 8.1 Dispersión  Operario A vs Maquina 1 #####
dias  <- c(1,5,6,7,10,14,15,18,19,20)
plot(dias, peso.operarioA.maquina2.mean, pch=1,  xlab="Dia laborado", ylab="Peso promedio (gr)", main="Peso promedio operario a con maquina 2", ylim=c(205,230), xlim=c(2,17))
abline(h=c(212, 220, 228), lty=c(1, 2, 3), lwd=2, col="blue")


#### 9. Analisis Operario B vs Maquina 1 ####

peso.operarioB.maquina1 <- filter(peso.maquina1, operario == 'B');
peso.operarioB.maquina1.mean <- tapply(peso.operarioB.maquina1$peso, peso.operarioB.maquina1[,1], mean)

summary(peso.operarioB.maquina1$peso) #resumen de tendencias 
sd(peso.operarioB.maquina1$peso) #desviación

#### 9.1 Dispersión  Operario b vs Maquina 1 #####
dias  <- c(2,3,4,8,9,11,12,13,16,17)
plot(dias, peso.operarioB.maquina1.mean, pch=1, xlab="Dia laborado", ylab="Peso promedio (gr)", main="Peso promedio operario B con maquina 1", ylim=c(205,230), xlim=c(2,17))
abline(h=c(212, 220, 228), lty=c(1, 2, 3), lwd=2, col="blue")


#### 10. Analisis Operario B vs Maquina 2 ####

peso.operarioB.maquina2 <- filter(peso.maquina2, operario == 'B');
peso.operarioB.maquina2.mean <- tapply(peso.operarioB.maquina2$peso, peso.operarioB.maquina2[,1], mean)

summary(peso.operarioB.maquina2$peso) #resumen de tendencias 
sd(peso.operarioB.maquina2$peso) #desviación

#### 10.1 Dispersión  Operario b vs Maquina 2 #####
dias  <- c(2,3,4,8,9,11,12,13,16,17)
plot(dias, peso.operarioB.maquina1.mean, pch=1, xlab="Dia laborado", ylab="Peso promedio (gr)", main="Peso promedio operario B con maquina 2", ylim=c(205,230), xlim=c(2,17))
abline(h=c(212, 220, 228), lty=c(1, 2, 3), lwd=2, col="blue")
abline(h=216.5, lty=2, lwd=2, col="red")

