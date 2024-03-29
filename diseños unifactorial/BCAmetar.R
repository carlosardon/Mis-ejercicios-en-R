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
library(ScottKnott)#Prueba m�ltiple de medias
library(lsr)#eta cuadrado
options (scipen = 999, digits = 5)
#ABRIR RSTUDIO E INDICAR LA CARPETA DE TRABAJO
metaEC <- read.csv("BCAmetar.csv", header = T)
attach(metaEC)
names(metaEC)
metaEC

#DEFINIR LAS VARIABLES INDEPENDIENTES (FACTORES)
TRA<- factor(TRAT)#No es necesario: "TRAT$metaEC" debido al attach
BLO<- factor(BLO)
CTRAS1<-factor(CTRAS1)
CTRAS2<-factor(CTRAS2)
CTRAS3<-factor(CTRAS3)
CTRAS4<-factor(CTRAS4)
#CREANDO VECTORES DE DATOS PARA LAS VARIABLES
EC<-as.vector(EDC)
#LOS VECTORES SE CONVIERTEN A TIPO NUM�RICO
EC1<-as.numeric(EC)

#INDICANDO EL MODELO ESTAD�STICO PARA EL ANOVA DE LA VARIABLE EFICIENCIA DE CONTROL
aovEC<-lm(EC1~TRA+BLO)
anova(aovEC)
Anova(aovEC)
aovEC1<- aov(EC1~TRA+BLO)
summary(aovEC1)#Resultados del ANOVA
cv.model(aovEC1)
etaSquared(aovEC1) 

#CONTRASTES ORTOGONALES
contras<-aov(EC1~CTRAS1+CTRAS2+CTRAS3+CTRAS4)
lista<-list(contras,CTRAS="UNO CONTRA TODOS")
summary(contras)
#CTRAS1.Qu�mico=biol�gico
#CTRAS2.Met15,20=Met25,30
#CTRAS3.met15=Met20
#CTRAS4.met25=Met30

#DIAGRAMA DE CAJA PARA TRATAMIENTOS
boxplot(EC1~TRA,col = "lightsalmon",horizontal=FALSE, pch=8)#col="bisque"
title("Comparaci�n de tratamientos con diagramas de caja", xlab="Tratamientos", ylab="Eficiencia de control")
media <- tapply(EC1, TRA, mean)
points(media, col = "blue", pch = 19)

ggplot(data=metaEC, aes(TRAT,  EC1))+
  geom_boxplot(outlier.color ="green",outlier.shape =8,outlier.size = 3) + 
  geom_jitter(width = 0.2)+
  stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="red")

#PARA OBTENER RESIDUOS DEL MODELO
resEC1<-residuals(aovEC1)#Residuos ordinarios 
summary(resEC1)#Residuos ordinarios
resEC1_estan<- rstandard(aovEC1)#Residuos estandarizados (estudentizado en InfoStat)
summary(resEC1_estan)#Residuos estandarizados (estudentizado en InfoStat)
resEC1_estud<- rstudent(aovEC1)#Residuos estudentizados (externamente estudentizado en Infostat)
summary(resEC1_estud)#Residuos estudentizados (externamente estudentizado en Infostat)
predEC1<-fitted.values(aovEC1)#Predichos 
summary(predEC1)#Predichos
Residuos <- data.frame(resEC1,resEC1_estan,resEC1_estud, predEC1)
Residuos

#DIAGRAMA DE CAJA PARA RESIDUOS ORDINARIOS
boxplot(resEC1,col = "bisque",horizontal=FALSE, pch=8)#col="lightsalmon"
title("Diagrama de caja para residos ordinarios", xlab="", ylab="Residuo ordinario")
media <- tapply(resEC1,TRA, mean)
points(media, col = "blue", pch = 19)

#TEST PARA RESIDUOS ORDINARIOS AT�PICOS
grubbs.test(resEC1,type=10)
#Type 10 uno at�pico en cualquiera de las colas
#Type 11 dos at�picos en colas opuestas
#Type 20 dos at�picos en cualquiera de las colas

#AN�LISIS GR�FICO DEL SUPUESTO DE NORMALIDAD
qqPlot(resEC1,pch=19,col="red",cex=0.9,main="QQ-plot variable eficacia de control",xlab="Cuantiles te�ricos normal",ylab="Cuantiles observados (Rduo eficacia de control)")
plotdensd<-density(resEC1)
plot(plotdensd,main="Densidad observada",xlab="residuos",ylab="Densidad",col="red")
polygon(plotdensd,col="red",border="red")
hisEC1 <- hist(resEC1, ylim=c(0,10), col="violet", main="Distribuci�n de residuos para eficacia de control",border="black")#Cambiar 10 de ser necesario
multiplier <- hisEC1$counts/hisEC1$density 
hEC1den <- density(resEC1) 
hEC1den$y <- hEC1den$y * multiplier[1] 
lines(hEC1den, col= "red",lwd=3,lty=4) 
normal.freq(hisEC1,col="blue",lwd=3, lty=1)
legend("topleft",col=c("blue","red"),lwd= c(3,3),lty=c(1,4),legend =c("Densidad normal estimada","Densidad observada"), bty = "n")

# TEST DE NORMALIDAD
shapiro.test(resEC1)# Modificada por Royston (1995)
ad.test(resEC1)#Anderson-Darling (1954)
lillie.test(resEC1)#Kolmogorov-Smirnov con modificaci�n de Lillefors (1967)
jb.norm.test(resEC1)# Jarque-Bera (1987)
ajb.norm.test(resEC1)# Jarque-Bera ajustada (1996)

#PRUEBA DE HOMOCEDASTICIDAD
bartlett.test(aovEC1$res,TRA)
leveneTest(resEC1~TRA, center="median")
leveneTest(resEC1~TRA, center="mean", trim=0)#trim aplicada a una media % recortada

#GR�FICO DE PREDICHOS Y RESIDUOS ESTUDENTIZADOS
plot(predEC1,resEC1_estan,xlab="Valores predichos",ylab="Res�duos estudentizados",ylim=c(-3,3),abline(h=c(0, 2.5, -2.50), lty=c(1,2,2), lwd=c(2,2,2), col=c("blue","red","red")), main="Diagrama de dispersi�n de residuos estudentizados",col="blue",pch=19,cex=0.9)

#PRUEBA M�LTIPLE DE MEDIAS SEG�N JOHN W. TUKEY (1915)
TukeyHSD(aovEC1, "TRA", console=TRUE, ordered = FALSE, conf.level = 0.95)
plot(TukeyHSD(aovEC1,"TRA"))
HSD.test(aovEC1, "TRA", alpha=0.05, group=TRUE, console=TRUE, main="Eficacia de control seg�n tratamiento")
plot(HSD.test(aovEC1,"TRA"),main="Grupos y rango", xlab="Tratamientos", ylab="Eficacia de control")
compEC1<-HSD.test(aovEC1,"TRA", alpha=0.05,group=TRUE)
par(cex=1.5)#define tama�o de t�tulos y s�mbolos
bar.group(compEC1$groups,horiz=TRUE,density=8,col="blue",border="red",xlim=c(0,100),las=1)#Cambiar 100 de ser necesario
title(cex.main=0.8,main="Comparaci�n de medias entre tratamientos",xlab="Eficacia de control",ylab="Tratamientos")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scott Knott (1974)
sktest <- SK(aovEC1, sig.level=0.05)
summary(sktest)
plot(sktest,col=rainbow(max(sktest$groups)), mm.lty=3, id.las=2,rl=FALSE,xlab= "Tratamientos/medios de cultivo", ylab="Porcentaje de germinaci�n promedio",title=" ")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Duncan (1955)
duncan.test(aovEC1, "TRA", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de germinaci�n seg�n medio de cultivo")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N FisherLSD (1935)
LSD.test(aovEC1, "TRA", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de germinaci�n seg�n medio de cultivo")
LSD.test(aovEC1, "TRA", p.adj= "bon",console=TRUE)

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scheffe (1959)
scheffe.test(aovEC1, "TRA", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de germinaci�n seg�n medio de cultivo")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Studen Newman & Keuls, SNK (1959)
SNK.test(aovEC1, "TRA", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de germinaci�n seg�n medio de cultivo")

#TRANFORMACI�N BOX-COX (cuando incluye ceros): S�lo cuando se requiera
lambdaEC1<- boxcoxfit (EC1, lambda2 = T)
lambdaEC1
lbda =lambdaEC1$lambda[1]
lbda2 = lambdaEC1$lambda[2]
if (lbda==0) {EC.t=log(EC1+lbda2)}
if (lbda!=0) {EC.t=((EC1+lbda2)^lbda-1)/lbda}
hist (EC.t, col="gray")
EC.t
EC.T<-as.vector(EC.t)
EC.T1<-as.numeric(EC.T)
aovEC.T1<- aov(EC.T1~TRA+BLO)
summary(aovEC.T1)#Resultados del ANOVA

#TRANSFORMACI�N ARSENO de la ra�z cuadrada de ...
YT<- ((180/pi)*(asin(sqrt((EC1+0)/100))))#Puede cambiar "0" por "0.5" si hay observaciones con valor cero
hist (YT, col="gray")
YT
YT.T<-as.vector(YT)
YT.T1<-as.numeric(YT.T)
aovYT.T1<- aov(YT.T1~TRA+BLO)
summary(aovYT.T1)#Resultados del ANOVA

detach(metaEC)

