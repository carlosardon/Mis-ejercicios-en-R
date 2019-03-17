#INSTALAR PREVIAMENTE LOS SIGUIENTES PAQUETES
library(agricolae)
library(car) 
library(nortest)
library(normtest)
library(outliers)
library(MASS)
library(geoR) #para Box-Cox con ceros
library(foreign) #datos externos
library(multcomp)# Prueba de medias
library(ScottKnott)#Prueba múltiple de medias
library(lsr)#eta cuadrado
library(ggplot2)#diagrama de caja
options (scipen = 999, digits = 5)
#ABRIR RSTUDIO E INDICAR LA CARPETA DE TRABAJO
pinusFP <- read.csv("BCAfor.csv", header = T)
attach(pinusFP)
names(pinusFP)
pinusFP

#DEFINIR LAS VARIABLES INDEPENDIENTES (FACTORES)
TRA<- factor(TRAT)#No es necesario: "TRAT$pinusFP" debido al attach
BLO<- factor(BLO)
CTRAS1<-factor(CTRAS1)
CTRAS2<-factor(CTRAS2)
CTRAS3<-factor(CTRAS3)
CTRAS4<-factor(CTRAS4)
CTRAS5<-factor(CTRAS5)
#CREANDO VECTORES DE DATOS PARA LAS VARIABLES
FP<-as.vector(FP)
#LOS VECTORES SE CONVIERTEN A TIPO NUMÉRICO
FP1<-as.numeric(FP)

#INDICANDO EL MODELO ESTADÍSTICO PARA EL ANOVA DE LA VARIABLE FACTOR DE PRODUCTIVIDAD
aovFP<-lm(FP1~TRA+BLO)
anova(aovFP)
Anova(aovFP)
aovFP1<- aov(FP1~TRA+BLO)
summary(aovFP1)#Resultados del ANOVA
cv.model(aovFP1)
etaSquared(aovFP1) 

#CONTRASTES ORTOGONALES
contras<-aov(FP1~CTRAS1+CTRAS2+CTRAS3+CTRAS4+CTRAS5)
lista<-list(contras,CTRAS="UNO CONTRA TODOS")
summary(contras)
#CTRAS1.Sin fertilización=Con fertilización
#CTRAS2.No enriquecido=Enriquecidos
#CTRAS3.Adición de boro=Adición de fósforo
#CTRAS4.25g de fósforo=50 y 100g de fósforo
#CTRAS4.50g de fosforo=100g de fósforo

#DIAGRAMA DE CAJA PARA TRATAMIENTOS
boxplot(FP1~TRA,col = "lightsalmon",horizontal=FALSE, pch=8)#col="bisque"
title("Comparación de tratamientos con diagramas de caja", xlab="Tratamientos", ylab="Factor de productividad")
media <- tapply(FP1, TRA, mean)
points(media, col = "blue", pch = 19)

ggplot(data=pinusFP, aes(TRAT,  FP))+
  geom_boxplot(outlier.color ="green",outlier.shape =8,outlier.size = 3) + 
  geom_jitter()+
  stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="red")

#PARA OBTENER RESIDUOS DEL MODELO
resFP1<-residuals(aovFP1)#Residuos ordinarios 
summary(resFP1)#Residuos ordinarios
resFP1_estan<- rstandard(aovFP1)#Residuos estandarizados (estudentizado en InfoStat)
summary(resFP1_estan)#Residuos estandarizados (estudentizado en InfoStat)
resFP1_estud<- rstudent(aovFP1)#Residuos estudentizados (externamente estudentizado en Infostat)
summary(resFP1_estud)#Residuos estudentizados (externamente estudentizado en Infostat)
predFP1<-fitted.values(aovFP1)#Predichos 
summary(predFP1)#Predichos
Residuos <- data.frame(resFP1,resFP1_estan,resFP1_estud, predFP1)
Residuos

#DIAGRAMA DE CAJA PARA RESIDUOS ORDINARIOS
boxplot(resFP1,col = "bisque",horizontal=FALSE, pch=8)#col="lightsalmon"
title("Diagrama de caja para residos ordinarios", xlab="", ylab="Residuo ordinario")
media <- tapply(resFP1,TRA, mean)
points(media, col = "blue", pch = 19)

#TEST PARA RESIDUOS ORDINARIOS ATÍPICOS
grubbs.test(resFP1,type=10)
#Type 10 uno atípico en cualquiera de las colas
#Type 11 dos atípicos en colas opuestas
#Type 20 dos atípicos en cualquiera de las colas

#ANÁLISIS GRÁFICO DEL SUPUESTO DE NORMALIDAD
qqPlot(resFP1,pch=19,col="red",cex=0.9,main="QQ-plot variable factor de productividad",xlab="Cuantiles teóricos normal",ylab="Cuantiles observados (Rduo factor de productividad)")
plotdensd<-density(resFP1)
plot(plotdensd,main="Densidad observada",xlab="residuos",ylab="Densidad",col="red")
polygon(plotdensd,col="red",border="red")
hisFP1 <- hist(resFP1, ylim=c(0,10), col="violet", main="Distribución de residuos para factor de productividad",border="black")#Cambiar 10 de ser necesario
multiplier <- hisFP1$counts/hisFP1$density 
hFP1den <- density(resFP1) 
hFP1den$y <- hFP1den$y * multiplier[1] 
lines(hFP1den, col= "red",lwd=3,lty=4) 
normal.freq(hisFP1,col="blue",lwd=3, lty=1)
legend("topleft",col=c("blue","red"),lwd= c(3,3),lty=c(1,4),legend =c("Densidad normal estimada","Densidad observada"), bty = "n")

# TEST DE NORMALIDAD
shapiro.test(resFP1)# Modificada por Royston (1995)
ad.test(resFP1)#Anderson-Darling (1954)
lillie.test(resFP1)#Kolmogorov-Smirnov con modificación de Lillefors (1967)
jb.norm.test(resFP1)# Jarque-Bera (1987)
ajb.norm.test(resFP1)# Jarque-Bera ajustada (1996)

#PRUEBA DE HOMOCEDASTICIDAD
bartlett.test(aovFP1$res,TRA)
leveneTest(resFP1~TRA, center="median")
leveneTest(resFP1~TRA, center="mean", trim=0)#trim aplicada a una media % recortada

#GRÁFICO DE PREDICHOS Y RESIDUOS ESTUDENTIZADOS
plot(predFP1,resFP1_estan,xlab="Valores predichos",ylab="Resíduos estudentizados",ylim=c(-3,3),abline(h=c(0, 2.5, -2.50), lty=c(1,2,2), lwd=c(2,2,2), col=c("blue","red","red")), main="Diagrama de dispersión de residuos estudentizados",col="blue",pch=19,cex=0.9)

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN JOHN W. TUKEY (1915)
TukeyHSD(aovFP1, "TRA", console=TRUE, ordered = FALSE, conf.level = 0.95)
plot(TukeyHSD(aovFP1,"TRA"))
HSD.test(aovFP1, "TRA", alpha=0.05, group=TRUE, console=TRUE, main="Factor de productividad")
plot(HSD.test(aovFP1,"TRA"),main="Grupos y rango", xlab="Tratamientos", ylab="Factor de productividad")
compFP1<-HSD.test(aovFP1,"TRA", alpha=0.05,group=TRUE)
par(cex=1.5)#define tamaño de títulos y símbolos
bar.group(compFP1$groups,horiz=TRUE,density=8,col="blue",border="red",xlim=c(0,1010),las=1)#Cambiar 1010 de ser necesario
title(cex.main=0.8,main="Comparación de medias entre tratamientos",xlab="Factor de productividad",ylab="Tratamientos")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Scott Knott (1974)
sktest <- SK(aovFP1, sig.level=0.05)
summary(sktest)
plot(sktest,col=rainbow(max(sktest$groups)), mm.lty=3, id.las=2,rl=FALSE,xlab= "Tratamientos/planes de fertilización", ylab="Factor de productividad promedio",title=" ")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Duncan (1955)
duncan.test(aovFP1, "TRA", alpha=0.05, group=TRUE, console=TRUE, main="Factor de productividad según plan de fertilización")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN FisherLSD (1935)
LSD.test(aovFP1, "TRA", alpha=0.05, group=TRUE, console=TRUE, main="Factor de productividad según medio de cultivo")
LSD.test(aovFP1, "TRA", p.adj= "bon",console=TRUE)

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Scheffe (1959)
scheffe.test(aovFP1, "TRA", alpha=0.05, group=TRUE, console=TRUE, main="Factor de productividad")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Studen Newman & Keuls, SNK (1959)
SNK.test(aovFP1, "TRA", alpha=0.05, group=TRUE, console=TRUE, main="Factor de productividad según plan de fertilización")

#TRANFORMACIÓN BOX-COX (para valores estrictamente positivos): Sólo cuando se requiera
lambdaFP1<- boxcoxfit (FP1, lambda2 = NULL)
lambdaFP1
lbda =lambdaFP1$lambda
if (lbda==0) {FP.t=log(FP1)}
if (lbda!=0) {FP.t=(FP1^lbda-1)/lbda}
hist (FP.t, col="gray")
FP.t
FP.T<-as.vector(FP.t)
FP.T1<-as.numeric(FP.T)
aovFP.T1<- aov(FP.T1~TRA+BLO)
summary(aovFP.T1)#Resultados del ANOVA


detach(pinusFP)

