options (scipen = 999, digits = 5)
#ABRIR RSTUDIO E INDICAR LA CARPETA DE TRABAJO
maiz1 <- read.csv("BTesDBCAmaiz1.csv", header = T)
maiz1
maiz <- read.csv("BTesDBCAmaiz.csv", header = T)
maiz

TRAT <- factor(maiz1$TRAT);BLOQUE <-factor(maiz1$BLOQUE) ; CTRAS1<- factor(maiz1$CTRAS1) ; CTRAS2 <- factor(maiz1$CTRAS2)
aovma<- aov(maiz1$ALT~TRAT+BLOQUE)
aovma1<- aov(maiz1$ALT~CTRAS1+CTRAS2)
summary(aovma)
summary(aovma1)
plot(maiz1$TRAT,maiz1$ALT)

BLOQUE <-factor(maiz$BLOQUE) ; FUENTE<- factor(maiz$FUENTE) ; DOSIS <- factor(maiz$DOSIS)
aovma2<- aov(maiz$ALT~BLOQUE+FUENTE*DOSIS)
summary(aovma2)
plot(maiz$FUENTE,maiz$ALT, mean=TRUE)
 
getAnywhere(fat2.ad.crd)#código fuente del paquete completamente al azar mas uno
getAnywhere(fat2.ad.rbd)#código fuente del paquete bloques completos al azar más uno


tidy(aovma)
tidy(aovma1)
tidy(aovma2)
ana<- tidy(aovma)
summary(ana)
ana1<- summary(ana)
capture.output(ana1, file="prueba")
write.csv(ana, file="Prueba1.csv")


#falta juntar datos de estas tablas




augment(aovma)
augment(aovma1)
augment(aovma2)
glance(aovma)

library(broom)
library(dplyr)    # Manipulación de data.frames
library(tidyr)    # Datos ordenados
library(readxl)   # Leer ficheros excel
library(lattice)  # Gráficos
#https://cran.r-project.org/doc/contrib/rdebuts_es.pdf
#https://diarium.usal.es/jaortega/2016/02/06/introduccion-a-tidyr-datos-ordenados-en-r/
  

uno<-aovma
dos<-aovma1
tres<-aovma2
TK<-aovma
TK_data<-as.data.frame(TK[1:1]) # the [1:1] locates the part of the output to be exported as a dataframe
write.csv(TK_data, 'TK_data.csv')
TK_data
