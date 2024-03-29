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
Eucalt<- read.csv("Fdcareci.csv", header = T)
attach(Eucalt)
names(Eucalt)
Eucalt

#DEFINIR LAS VARIABLES INDEPENDIENTES (FACTORES)
TRA<- factor(TRAT)#No es necesario: "Eucalt$TRAT" debido al attach
ESPE<- factor(ESP)
RECI<- factor(REC)

#CREANDO VECTORES DE DATOS PARA LAS VARIABLES
ALTU<-as.vector(ALT)

#LOS VECTORES SE CONVIERTEN A TIPO NUM�RICO
ALTU1<-as.numeric(ALTU)

#INDICANDO EL MODELO ESTAD�STICO PARA EL ANOVA DE LA VARIABLE ALTURA DE PLANTA
aovALTU1<- aov(ALTU1~ESPE*RECI)
summary(aovALTU1)#Resultados del ANOVA
cv.model(aovALTU1)
etaSquared(aovALTU1) 

#DIAGRAMAS DE CAJA
#PARA TRATAMIENTOS
boxplot(ALTU1~TRA, col="lightsalmon", horizontal=FALSE, pch=8)#col="bisque"
title("Comparaci�n de tratamientos", xlab="Tratamientos", ylab="Altura de planta")
media <- tapply(ALTU1, TRA, mean)
points(media, col = "blue", pch = 19)

#PARA ESPECIES
boxplot(ALTU1~ESPE, col="lightsalmon", horizontal=FALSE, pch=8)#col="bisque"
title("Comparaci�n de niveles del factor especie", xlab="Especies", ylab="Altura de planta")
mediaM <- tapply(ALTU1, ESPE, mean)
points(mediaM, col = "blue", pch = 19)

#PARA RECIPIENTES
boxplot(ALTU1~RECI, col="lightsalmon", horizontal=FALSE, pch=8)#col="bisque"
title("Comparaci�n de niveles del factor recipientes", xlab="Recipientes", ylab="Altura de planta")
mediaD <- tapply(ALTU1, RECI, mean)
points(mediaD, col = "blue", pch = 19)

#PARA OBTENER RESIDUOS DEL MODELO
aovALTU<- aov(ALTU1~ESPE*RECI)#Para obtener un solo tipo de residuo para el modelo
resALTU<-residuals(aovALTU)#Residuos ordinarios 
summary(resALTU)#Residuos ordinarios
resALTU_estan<- rstandard(aovALTU)#Residuos estandarizados (estudentizado en InfoStat)
summary(resALTU_estan)#Residuos estandarizados (estudentizado en InfoStat)
resALTU_estud<- rstudent(aovALTU)#Residuos estudentizados (externamente estudentizado en Infostat)
summary(resALTU_estud)#Residuos estudentizados (externamente estudentizado en Infostat)
predALTU<-fitted.values(aovALTU)#Predichos 
summary(predALTU)#Predichos
Residuos <- data.frame(resALTU,resALTU_estan,resALTU_estud, predALTU)
Residuos

#TEST DE VALORES AT�PICOS:: RESIDUOS ORDINARIOS
grubbs.test(resALTU,type=10)
#Type 10 uno at�pico en cualquiera de las colas
#Type 11 dos at�picos en colas opuestas
#Type 20 dos at�picos en cualquiera de las colas

#DIAGRAMA DE CAJA PARA RESIDUOS ORDINARIOS
boxplot(resALTU,col = "bisque",horizontal=FALSE, pch=8)#col="lightsalmon"
title("Diagrama de caja para residos ordinarios", xlab="", ylab="Residuo ordinario")
media <- tapply(resALTU,TRA, mean)
points(media, col = "blue", pch = 19)

#VERIFICACI�N GR�FICA DEL SUPUESTO DE NORMALIDAD
qqPlot(resALTU,pch=19,col="red",cex=0.9,main="QQ-plot variable altura de planta",xlab="Cuantiles te�ricos normal",ylab="Cuantiles observados (Rduo altura de planta)")
plotdensd<-density(resALTU)
plot(plotdensd,main="Densidad observada",xlab="Residuos",ylab="Densidad",col="red")
polygon(plotdensd,col="red",border="red")
hisALTU <- hist(resALTU, ylim=c(0,15), col="violet", main="Distribuci�n de residuos para altura de planta",border="black")#Cambiar 15 de ser necesario
multiplier <- hisALTU$counts/hisALTU$density 
hALTUden <- density(resALTU) 
hALTUden$y <- hALTUden$y * multiplier[1] 
lines(hALTUden, col= "red",lwd=3,lty=4) 
normal.freq(hisALTU,col="blue",lwd=3, lty=1)
legend("topleft",col=c("blue","red"),lwd= c(3,3),lty=c(1,4),legend =c("Densidad normal estimada","Densidad observada"), bty = "n")

# TEST DE NORMALIDAD
shapiro.test(resALTU)#Modificada por Royston (1995)
ad.test(resALTU)#Anderson-Darling (1954)
lillie.test(resALTU)#Kolmogorov-Smirnov con modificaci�n de Lillefors (1967)
jb.norm.test(resALTU)# Jarque-Bera (1987)
ajb.norm.test(resALTU)# Jarque-Bera (1996)

#PRUEBA DE HOMOCEDASTICIDAD
bartlett.test(aovALTU$res,TRA)
leveneTest(resALTU~TRA, center="median")
leveneTest(resALTU~TRA, center="mean", trim=0)#trim aplicada a una media % recortada

#GR�FICO DE PREDICHOS Y RESIDUOS ESTUDENTIZADOS
plot(predALTU,resALTU_estan,ylim=c(-3,3),xlab="Valores predichos",ylab="Res�duos estudentizados",abline(h=c(0, 2.5, -2.50), lty=c(1,2,2), lwd=c(2,2,2), col=c("blue","red","red")), main="Diagrama de dispersi�n de residuos estudentizados",col="blue",pch=19,cex=0.9)

#PRUEBA M�LTIPLE DE MEDIAS SEG�N JOHN W. TUKEY (1915)
#FACTOR ESPECIE
TukeyHSD(aovALTU, "ESPE", console=TRUE)
plot(TukeyHSD(aovALTU,"ESPE"))
HSD.test(aovALTU, "ESPE", group=TRUE,console=TRUE)
plot(HSD.test(aovALTU,"ESPE"),main="Grupos y rango", xlab="Especies", ylab="Altura de planta seg�n especies")
compALTU1<-HSD.test(aovALTU,"ESPE", alpha=0.05,group=TRUE)
par(cex=1.5)#define tama�o de t�tulos y s�mbolos
bar.group(compALTU1$groups,horiz=TRUE,density=15,col="blue",border="red",xlim=c(0,33),las=1)#Cambiar 33 de ser necesario
title(cex.main=0.8,main="Comparaci�n medias medias de especies",xlab="Altura de planta",ylab="")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scott Knott (1974)
sktest1 <- SK(aovALTU, "ESPE", sig.level=0.05)
summary(sktest1)
plot(sktest1,col=rainbow(max(sktest1$groups)), mm.lty=3, id.las=2,rl=FALSE,xlab= "Especies", ylab="altura de planta seg�n especies",title="Comparaci�n de medias ")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Duncan (1955)
duncan.test(aovALTU, "ESPE", alpha=0.05, group=TRUE, console=TRUE, main="Altura de planta especie")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N FisherLSD (1935)
LSD.test(aovALTU, "ESPE", alpha=0.05, group=TRUE, console=TRUE, main="Altura de planta especie")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scheffe (1959)
scheffe.test(aovALTU, "ESPE", alpha=0.05, group=TRUE, console=TRUE, main="Altura de planta especie")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Studen Newman & Keuls, SNK (1959)
SNK.test(aovALTU, "ESPE", alpha=0.05, group=TRUE, console=TRUE, main="Altura de planta especie")

# PRUEBA M�LTIPLE DE MEDIAS:FACTOR RECIPIENTE
#SEG�N JOHN W. TUKEY (1915)
TukeyHSD(aovALTU, "RECI", console=TRUE)
plot(TukeyHSD(aovALTU,"RECI"))
HSD.test(aovALTU, "RECI", group=TRUE,console=TRUE)
plot(HSD.test(aovALTU,"RECI"),main="Grupos y rango", xlab="Recipientes", ylab="Altura de planta")
compALTU<-HSD.test(aovALTU,"RECI", alpha=0.05,group=TRUE)
par(cex=1.5)#define tama�o de t�tulos y s�mbolos
bar.group(compALTU$groups,horiz=TRUE,density=15,col="blue",border="red",xlim=c(0,33),las=1)#Cambiar 100 de ser necesario
title(cex.main=0.8,main="Comparaci�n de medias recipientes",xlab="Altura de planta",ylab="")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scott Knott (1974)
sktest <- SK(aovALTU, "RECI", sig.level=0.05)
summary(sktest)
plot(sktest,col=rainbow(max(sktest$groups)), mm.lty=3, id.las=2,rl=FALSE,xlab= "Recipientes", ylab="Altura de planta",title=" ")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Duncan
duncan.test(aovALTU, "RECI", alpha=0.05, group=TRUE, console=TRUE, main="Altura de planta seg�n recipiente")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N FisherLSD
LSD.test(aovALTU, "RECI", alpha=0.05, group=TRUE, console=TRUE, main="Altura de planta seg�n recipiente")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scheffe
scheffe.test(aovALTU, "RECI", alpha=0.05, group=TRUE, console=TRUE, main="Altura de planta seg�n recipiente")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N SNK
SNK.test(aovALTU, "RECI", alpha=0.05, group=TRUE, console=TRUE, main="Altura de planta seg�n recipiente")

#PRUEBA M�LTIPLE DE MEDIAS:INTERACCI�N DE FACTORES
#SEG�N JOHN W. TUKEY (1915)
TukeyHSD(aovALTU, "ESPE:RECI", console=TRUE)
plot(TukeyHSD(aovALTU,"ESPE:RECI"))
trat <- with(Eucalt, interaction(ESPE,RECI))
amod <- aov(ALTU1~trat, data=Eucalt)
HSD.test(amod, "trat", group=TRUE, console=TRUE)
plot(HSD.test(amod,"trat"),main="Grupos y rango", xlab="Tratamiento/interacci�n", ylab="Altura de planta")
compALTU2<-HSD.test(amod,"trat", alpha=0.05,group=TRUE)
par(cex=1.2)#define tama�o de t�tulos y s�mbolos
bar.group(compALTU2$groups,horiz=TRUE,density=20,col="blue",border="red",xlim=c(0,33),las=1, cex.axis =0.7, cex.names=0.7)#Cambiar 120 de ser necesario
title(cex.main=1.0,main="Comparaci�n de medias de tratamientos",xlab="altura de planta",ylab="")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scott Knott (1974)
sktest2 <- SK(amod, "trat", sig.level=0.05)
summary(sktest2)
plot(sktest2,col=rainbow(max(sktest2$groups)), mm.lty=3, id.las=2,rl=FALSE,xlab= "Tratamientos/interacci�n", ylab="altura de planta",title="Comparaci�n de medias")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Duncan
duncan.test(amod, "trat", alpha=0.05, group=TRUE, console=TRUE, main="Altura de planta")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N FisherLSD
LSD.test(amod, "trat", alpha=0.05, group=TRUE, console=TRUE, main="Altura de planta")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scheffe
scheffe.test(amod, "trat", alpha=0.05, group=TRUE, console=TRUE, main="Altura de planta")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N SNK
SNK.test(amod, "trat", alpha=0.05, group=TRUE, console=TRUE, main="Altura de planta")

#AN�LISIS DE INTERACCI�N
#DIAGRAMA DE INTERACCI�N DE EFECTOS ENTRE ESPECIE Y RECIPIENTE)
ANAint<-lm(ALTU1~ESPE*RECI)
(ANAint.means<- interactionMeans(ANAint))#medias ajustadas de tratamientos
plot(ANAint.means, errorbar="ci0")#Todos los gr�ficos de interacci�n
interaction.plot(ESPE,RECI,ALTU1,type="b",col=c(2:3),leg.bty="b",leg.bg="beige",lwd=2,pch=c(18,20,17,24,1),xlab="Especies",ylab="Altura de planta",trace.label="Recipientes",main="Gr�fico de intereacci�n: recipientes dado especies")
interaction.plot(RECI,ESPE,ALTU1,type="b",col=c(2:3),leg.bty="b",leg.bg="beige",lwd=2,pch=c(18,20,17,24,1),xlab="Especies",ylab="Altura de planta",trace.label="Recipientes",main="Gr�fico de intereacci�n: recipientes dado especies")
emmip(ANAint, RECI~ESPE)#Gr�fico de interacci�n: recipientes a trav�s de especies (recipientes dado especies)con emeams
emmip(ANAint, ESPE~RECI)#Grafico de interacci�n: especies a trav�s de recipientes (especies dado recipientes)con emeams
testInteractions(ANAint, fixed="RECI", across="ESPE", digits=5)#Efectos simples:Recipientes a trav�s de especies
testInteractions(ANAint, fixed="ESPE", across="RECI", digits=5)#Efectos simples:especies a trav�s de recipientes
testInteractions(ANAint, pairwise="RECI", across="ESPE")#Interacci�n:Diferencias de recipientes a trav�s de especies
testInteractions(ANAint, pairwise="ESPE", across="RECI", digits=5)#Interacci�n:diferencias de especies a trav�s de recipientes
testInteractions(ANAint)#interacci�n:diferencia de efectos simples

#TRANFORMACI�N BOX-COX (para valores estrictamente positivos): S�lo cuando se requiera
lambdaALTU<- boxcoxfit (ALTU1, lambda2 = NULL)
lambdaALTU
lbda =lambdaALTU$lambda
if (lbda==0) {ALTU.t=log(ALTU1)}
if (lbda!=0) {ALTU.t=(ALTU1^lbda-1)/lbda}
hist (ALTU.t, col="gray")
ALTU.t
ALTU.T<-as.vector(ALTU.t)
ALTU.T1<-as.numeric(ALTU.T)
aovALTU.T1<- aov(ALTU.T1~ESPE*RECI)
summary(aovALTU.T1)#Resultados del ANOVA

#TRANSFORMACI�N ARSENO de la ra�z cuadrada de ...
YT<- ((180/pi)*(asin(sqrt((ALTU1+0)/100))))#Puede cambiar "0" por "0.5" si hay observaciones con valor cero
hist (YT, col="gray")
YT
YT.T<-as.vector(YT)
YT.T1<-as.numeric(YT.T)
aovYT.T1<- aov(ALTU.T1~ESPE*RECI)
summary(aovYT.T1)#Resultados del ANOVA

detach (Eucalt)


