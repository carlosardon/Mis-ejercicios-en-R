#INSTALAR PREVIAMENTE LOS SIGUIENTES PAQUETES
library(agricolae)
library(car) 
library(nortest)
library(normtest)
library(outliers)
library(MASS)
library(geoR) #para Box-Cox con ceros
library(foreign) #Prueba de medias
library(multcomp)# Prueba de medias
library(phia)
library(emmeans)
library(ScottKnott)#Prueba m�ltiple de medias
library(lsr)#eta cuadrado
options (scipen = 999, digits = 5)

#ABRIR RSTUDIO E INDICAR LA CARPETA DE TRABAJO
Pi�aIF <- read.csv("pdivpinha.csv", header = T)
attach(Pi�aIF)
names(Pi�aIF)
Pi�aIF

#DEFINIR LAS VARIABLES INDEPENDIENTES (FACTORES)
TRA<- factor(TRAT)#No es necesario: "Pi�aIF$TRAT" debido al attach
MEZ<- factor(MEZCLA)
DOS<- factor(DOSISN)
BLO<- factor(BLOQUE)

#CREANDO VECTORES DE DATOS PARA LAS VARIABLES
FLO<-as.vector(FLOR)
PES<-as.vector(PESO)

#LOS VECTORES SE CONVIERTEN A TIPO NUM�RICO
FLO1<-as.numeric(FLO)
PES1<-as.numeric(PES)

#INDICANDO EL MODELO ESTAD�STICO PARA EL ANOVA DE LA VARIABLE FLORACI�N
aovFLO1<- aov(FLO1~BLO+MEZ*DOS+Error(BLO+MEZ%in%BLO))
summary(aovFLO1)#Resultados del ANOVA
#OTRA FORMA DE REALIZAR EL ANOVA DE LA VARIABLE FLORACI�N 
anovaFLO<-sp.plot(BLO, MEZ, DOS, FLO1)
cv.model(aovFLO1$Within)

#DIAGRAMAS DE CAJA
boxplot(FLO1~TRAT, col="lightsalmon", horizontal=FALSE, pch=8)#col="bisque"
title("Comparaci�n de tratamientos", xlab="Tratamientos", ylab="Porcentaje promedio de floraci�n")
media <- tapply(FLO1, TRAT, mean)
points(media, col = "blue", pch = 19)

boxplot(FLO1~MEZ, col="lightsalmon", horizontal=FALSE, pch=8)#col="bisque"
title("Comparaci�n de niveles del factor mezcla", xlab="Mezcla fuentes de nitr�geno", ylab="Porcentaje promedio de floraci�n")
mediaM <- tapply(FLO1, MEZ, mean)
points(mediaM, col = "blue", pch = 19)

boxplot(FLO1~DOS, col="lightsalmon", horizontal=FALSE, pch=8)#col="bisque"
title("Comparaci�n de niveles del factor dosis", xlab="Dosis de nitr�geno", ylab="Porcentaje promedio de floraci�n")
mediaD <- tapply(FLO1, DOS, mean)
points(mediaD, col = "blue", pch = 19)

#PARA OBTENER RESIDUOS DEL MODELO
aovFLO<- aov(FLO1~BLO+MEZ*DOS+BLO/MEZ)#Para obtener un solo tipo de residuo para el modelo
resFLO<-residuals(aovFLO)#Residuos ordinarios 
summary(resFLO)#Residuos ordinarios
resFLO_estan<- rstandard(aovFLO)#Residuos estandarizados (estudentizado en InfoStat)
summary(resFLO_estan)#Residuos estandarizados (estudentizado en InfoStat)
resFlO_estud<- rstudent(aovFLO)#Residuos estudentizados (externamente estudentizado en Infostat)
summary(resFlO_estud)#Residuos estudentizados (externamente estudentizado en Infostat)
predFLO<-fitted.values(aovFLO)#Predichos 
summary(predFLO)#Predichos
Residuos <- data.frame(resFLO,resFLO_estan,resFlO_estud, predFLO)
Residuos

#TEST DE VALORES AT�PICOS: RESIDUOS ORDINARIOS
grubbs.test(resFLO,type=10)
#Type 10 uno at�pico en cualquiera de las colas
#Type 11 dos at�picos en colas opuestas
#Type 20 dos at�picos en cualquiera de las colas

#DIAGRAMA DE CAJA PARA RESIDUOS ORDINARIOS
boxplot(resFLO,col = "bisque",horizontal=FALSE, pch=8)#col="lightsalmon"
title("Diagrama de caja para residos ordinarios", xlab="", ylab="Residuo ordinario")
media <- tapply(resFLO,TRA, mean)
points(media, col = "blue", pch = 19)

#VERIFICACI�N GR�FICA DEL SUPUESTO DE NORMALIDAD
qqPlot(resFLO,pch=19,col="red",cex=0.9,main="QQ-plot variable porcentaje de floraci�n",xlab="Cuantiles te�ricos normal",ylab="Cuantiles observados (Rduo floraci�n)")
plotdensd<-density(resFLO)
plot(plotdensd,main="Densidad observada",xlab="residuos",ylab="Densidad",col="red")
polygon(plotdensd,col="red",border="red")
hisflo <- hist(resFLO, ylim=c(0,12), col="violet", main="Distribuci�n de residuos para porcentaje de floraci�n",border="black")#Cambiar 12 de ser necesario
multiplier <- hisflo$counts/hisflo$density 
hfloden <- density(resFLO) 
hfloden$y <- hfloden$y * multiplier[1] 
lines(hfloden, col= "red",lwd=3,lty=4) 
normal.freq(hisflo,col="blue",lwd=3, lty=1)
legend("topleft",col=c("blue","red"),lwd= c(3,3),lty=c(1,4),legend =c("Densidad normal estimada","Densidad observada"), bty = "n")

# TEST DE NORMALIDAD
shapiro.test(resFLO)#Modificada por Royston (1995)
ad.test(resFLO)#Anderson-Darling (1954)
lillie.test(resFLO)#Kolmogorov-Smirnov con modificaci�n de Lillefors (1967)
jb.norm.test(resFLO)# Jarque-Bera (1987)
ajb.norm.test(resFLO)# Jarque-Bera (1996)

#PRUEBA DE HOMOCEDASTICIDAD
bartlett.test(aovFLO$res,TRAT)
leveneTest(resFLO~TRA, center="median")
leveneTest(resFLO~TRA, center="mean", trim=0)#trim aplicada a una media % recortada

#GR�FICO DE PREDICHOS Y RESIDUOS ESTUDENTIZADOS
plot(predFLO,resFLO_estan,xlab="Valores predichos",ylim=c(-3,3),ylab="Res�duos estudentizados",abline(h=c(0, 2.5, -2.50), lty=c(1,2,2), lwd=c(2,2,2), col=c("blue","red","red")), main="Diagrama de dispersi�n de residuos estudentizados",col="blue",pch=19,cex=0.9)

#PRUEBA M�LTIPLE DE MEDIAS SEG�N JOHN W. TUKEY (1915)
# FACTOR DOSIS
TukeyHSD(aovFLO, "DOS", console=TRUE)
plot(TukeyHSD(aovFLO,"DOS"))
HSD.test(aovFLO, "DOS", group=TRUE,console=TRUE)
plot(HSD.test(aovFLO,"DOS"),main="Grupos y rango", xlab="Dosis de nitr�geno", ylab="Porcentaje de floraci�n promedio")
compflo<-HSD.test(aovFLO,"DOS", alpha=0.05,group=TRUE)
par(cex=1.5)#define tama�o de t�tulos y s�mbolos
bar.group(compflo$groups,horiz=TRUE,density=8,col="blue",border="red",xlim=c(0,100),las=1)#Cambiar 100 de ser necesario
title(cex.main=0.8,main="Comparaci�n de medias dosis de nitr�geno",xlab="Porcentaje de floraci�n",ylab="")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scott Knott (1974)
sktest <- SK(aovFLO, "DOS", sig.level=0.05)
summary(sktest)
plot(sktest,col=rainbow(max(sktest$groups)), mm.lty=3, id.las=2,rl=FALSE,xlab= "Dosis de nitr�geno", ylab="Porcentaje promedio de floraci�n",title=" ")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Duncan (1955)
duncan.test(aovFLO, "DOS", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de floraci�n seg�n medio de cultivo")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N FisherLSD (1635)
LSD.test(aovFLO, "DOS", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de floraci�n seg�n medio de culivo")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scheffe (1959)
scheffe.test(aovFLO, "DOS", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de floraci�n seg�n medio de culivo")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Studen Newman & Keuls, SNK (1959)
SNK.test(aovFLO, "DOS", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de floraci�n seg�n medio de culivo")

#FACTOR MEZCLA
TukeyHSD(aovFLO, "MEZ", console=TRUE)
plot(TukeyHSD(aovFLO,"MEZ"))
HSD.test(aovFLO, "MEZ", group=TRUE,console=TRUE)
plot(HSD.test(aovFLO,"MEZ"),main="Grupos y rango", xlab="Fuentes de nitr�geno", ylab="Porcentaje floraci�n seg�n fuente de nitr�geno")
compflo1<-HSD.test(aovFLO,"MEZ", alpha=0.05,group=TRUE)
par(cex=1.5)#define tama�o de t�tulos y s�mbolos
bar.group(compflo1$groups,horiz=TRUE,density=8,col="blue",border="red",xlim=c(0,100),las=1)#Cambiar 100 de ser necesario
title(cex.main=0.8,main="Comparaci�n medias fuentes de nitr�geno",xlab="Porcentaje de floraci�n",ylab="")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scott Knott (1974)
sktest1 <- SK(aovFLO, "MEZ", sig.level=0.05)
summary(sktest1)
plot(sktest1,col=rainbow(max(sktest1$groups)), mm.lty=3, id.las=2,rl=FALSE,xlab= "Fuentes de nitr�geno", ylab="Porcentaje floraci�n seg�n fuente de nitr�geno",title=" ")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Duncan
duncan.test(aovFLO, "MEZ", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje floraci�n seg�n fuente de nitr�geno")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N FisherLSD
LSD.test(aovFLO, "MEZ", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje floraci�n seg�n fuente de nitr�geno")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scheffe
scheffe.test(aovFLO, "MEZ", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje floraci�n seg�n fuente de nitr�geno")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N SNK
SNK.test(aovFLO, "MEZ", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje floraci�n seg�n fuente de nitr�geno")

# INTERACCI�N DE FACTORES
TukeyHSD(aovFLO, "MEZ:DOS", console=TRUE)
plot(TukeyHSD(aovFLO,"MEZ:DOS"))
trat <- with(Pi�aIF, interaction(MEZ,DOS))
amod <- aov(FLO1~trat, data=Pi�aIF)
HSD.test(amod, "trat", group=TRUE, console=TRUE)
plot(HSD.test(amod,"trat"),main="Grupos y rango", xlab="Tratamiento/interacci�n", ylab="Porcentaje de floraci�n con interacci�n")
compflo2<-HSD.test(amod,"trat", alpha=0.05,group=TRUE)
par(cex=1.2)#define tama�o de t�tulos y s�mbolos
bar.group(compflo2$groups,horiz=TRUE,density=15,col="blue",border="red",xlim=c(0,120),las=1, cex.axis =0.7, cex.names=0.7)#Cambiar 120 de ser necesario
title(cex.main=1.0,main="Comparaci�n de medias de tratamientos",xlab="Porcentaje de floraci�n",ylab="")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scott Knott (1974)
sktest2 <- SK(amod, "trat", sig.level=0.05)
summary(sktest2)
plot(sktest2,col=rainbow(max(sktest2$groups)), mm.lty=3, id.las=2,rl=FALSE,xlab= "Tratamientos/interacci�n", ylab="Porcentaje de floraci�n con interacci�n",title=" ")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Duncan
duncan.test(amod, "trat", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de floraci�n con interacci�n")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N FisherLSD
LSD.test(amod, "trat", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de floraci�n con interacci�n")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scheffe
scheffe.test(amod, "trat", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de floraci�n con interacci�n")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N SNK
SNK.test(amod, "trat", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de floraci�n con interacci�n")

#AN�LISIS DE INTERACCI�N
#DIAGRAMA DE INTERACCI�N DE EFECTOS ENTRE ESPECIE Y RECIPIENTE)
ANAint<-lm(FLO1~MEZ*DOS)
(pinha.means<- interactionMeans(ANAint))#medias ajustadas de tratamientos
plot(pinha.means, errorbar="ci0")#Todos los gr�ficos de interacci�n
interaction.plot(MEZ,DOS,FLO1,type="b",col=c(2:3),leg.bty="b",leg.bg="beige",lwd=2,pch=c(18,20,17,24,1),xlab="Ethrel m�s fuente de nitr�geno",ylab="Porcentaje de floraci�n",trace.label="Dosis N",main="gr�fico de intereacci�n")
interaction.plot(DOS,MEZ,FLO1,type="b",col=c(2:3),leg.bty="b",leg.bg="beige",lwd=2,pch=c(18,20,17,24,1),xlab="Ethrel m�s fuente de nitr�geno",ylab="Porcentaje de floraci�n",trace.label="Dosis N",main="gr�fico de intereacci�n")
emmip(ANAint, DOS~MEZ)#Gr�fico de interacci�n: dosis a trav�s de mezclas (Dosis dado mezclas)con emeams 
emmip(ANAint, MEZ~DOS)#Grafico de interacci�n: mezclas a trav�s de dosis (mezclas dado dosis)con emeams
testInteractions(ANAint, fixed="DOS", across="MEZ", digits=5)#Efectos simples:Dosis a trav�s de mezclas
testInteractions(ANAint, fixed="MEZ", across="DOS", digits=5)#Efectos simples:Mezclas a trav�s de dosis
testInteractions(ANAint, pairwise="DOS", across="MEZ")#Interacci�n:Diferencias de dosis a trav�s de mezclas
testInteractions(ANAint, pairwise="MEZ", across="DOS", digits=5)#Interacci�n:diferencias de mezcla a trav�s de dosis
testInteractions(ANAint)#interacci�n:diferencia de efectos simples

#TRANFORMACI�N BOX-COX (cuando incluye ceros)
lambdaFLO<- boxcoxfit (FLO1, lambda2 = T)
lambdaFLO
lbda =lambdaFLO$lambda[1]
lbda2 = lambdaFLO$lambda[2]
if (lbda==0) {flo.t=log(FLO1+lbda2)}
if (lbda!=0) {flo.t=((FLO1+lbda2)^lbda-1)/lbda}
par(mfcol=c(2,2))
hist (flo.t, col="gray")
flo.t
FLO.T<-as.vector(flo.t)
FLO.T1<-as.numeric(FLO.T)
aovFLO1T<- aov(FLO.T1~BLO+MEZ*DOS+Error(BLO+MEZ%in%BLO))
summary(aovFLO1T)#Resultados del ANOVA

#TRANSFORMACI�N ARSENO de la ra�z cuadrada de ...
YT<- ((180/pi)*(asin(sqrt((FLO1+0)/100))))#Puede cambiar "0" por "0.5" si hay observaciones con valor cero
hist (YT, col="gray")
YT
YT.T<-as.vector(YT)
YT.T1<-as.numeric(YT.T)
aovYT.T1<- aov(YT.T1~BLO+MEZ*DOS+Error(BLO+MEZ%in%BLO))
summary(aovYT.T1)#Resultados del ANOVA
old_par<- par()
detach(Pi�aIF)