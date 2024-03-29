library(ExpDes)
options (scipen = 999, digits = 5)
#ABRIR RSTUDIO E INDICAR LA CARPETA DE TRABAJO
pisolytus <- read.csv("BTesDCApisolytus.csv", header = T)
attach(pisolytus)
pisolytus
ttaltp <- c(16.5,15.9,16.0)
ttlonr <- c(14.10,13.9,13.9)
ttdiap <- c(0.23,0.24,0.21)
ttpsp <- c(0.42,0.45,0.42)
#EN EL CASO DE UN DISE�O COMPLETAMENTE ALEATORIZADO (Cambiar ALTP y ttaltp para analizar otra variable)
fat2.ad.crd(CC, IDA, REP, ALTP, ttaltp, quali = c(TRUE,TRUE), mcomp = "tukey", 
            fac.names = c("Concentraci�n", "Intervalo de aplicaci�n"), sigT = 0.05, sigF = 0.05)
#EN EL CASO DE UN DISE�O DE BLOQUES COMPLETOS AL AZAR(Cambiar ALTP y ttaltp para analizar otra variable)
fat2.ad.rbd(CC, IDA, REP, ALTP, ttaltp, quali=c(TRUE, TRUE), mcomp = "tukey",
            fac.names = c("Concentraci�n", "Intervalo de aplicaci�n"), sigT = 0.05, sigF = 0.05)
detach(pisolytus)



getAnywhere(fat2.ad.crd)#c�digo fuente del paquete completamente al azar mas uno
getAnywhere(fat2.ad.rbd)#c�digo fuente del paquete bloques completos al azar m�s uno




