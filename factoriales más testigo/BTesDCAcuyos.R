library(ExpDes)
options (scipen = 999, digits = 5)
#ABRIR RSTUDIO E INDICAR LA CARPETA DE TRABAJO
cuyos <- read.csv("BTesDCAcuyos.csv", header = T)
cuyos
attach(cuyos)
testi <- read.csv("VecPasto.csv", header = F)
testi
testigo<-as.numeric(testi$V1)
testigo
fat2.ad.crd(APRO, AENE, REP, IP, testigo, quali = c(TRUE,TRUE), mcomp = "tukey", 
            fac.names = c("Alimento protéico", "Alimento energético"), sigT = 0.05, sigF = 0.05)

#DE OTRA FORMA
library(ExpDes)
options (scipen = 999, digits = 5)
#ABRIR RSTUDIO E INDICAR LA CARPETA DE TRABAJO
cuyos <- read.csv("BTesDCAcuyos.csv", header = T)
cuyos
testigo <- c(19.4,17.6,18.6,17.6,18.6,19.4)
testigo
#EN EL CASO DE UN DISEÑO COMPLETAMENTE ALEATORIZADO
fat2.ad.crd(APRO, AENE, REP, IP, testigo, quali = c(TRUE,TRUE), mcomp = "tukey", 
            fac.names = c("Alimento protéico", "Alimento energético"), sigT = 0.05, sigF = 0.05)
#EN EL CASO DE UN DISEÑO DE BLOQUES COMPLETOS AL AZAR
fat2.ad.rbd(APRO, AENE, REP, IP, testigo, quali=c(TRUE, TRUE), mcomp = "tukey",
            fac.names = c("Alimento protéico", "Alimento energético"), sigT = 0.05, sigF = 0.05)
detach(cuyos)





