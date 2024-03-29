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
platano<- read.csv("Fdcariego.csv", header = T)
attach(platano)
names(platano)
platano

#DEFINIR LAS VARIABLES INDEPENDIENTES (FACTORES)
TRA<- factor(TRAT)#No es necesario: "platano$TRAT" debido al attach
CLON<- factor(CLON)
LAMR<- factor(LAMR)

#CREANDO VECTORES DE DATOS PARA LAS VARIABLES
PR<-as.vector(PR)

#LOS VECTORES SE CONVIERTEN A TIPO NUM�RICO
PR1<-as.numeric(PR)

#INDICANDO EL MODELO ESTAD�STICO PARA EL ANOVA DE LA VARIABLE PESO DE RACIMO
aovPR1<- aov(PR1~CLON*LAMR)
summary(aovPR1)#Resultados del ANOVA
cv.model(aovPR1)
etaSquared(aovPR1) 

#DIAGRAMAS DE CAJA
#PARA TRATAMIENTOS
boxplot(PR1~TRA, col="lightsalmon", horizontal=FALSE, pch=8)#col="bisque"
title("Comparaci�n de tratamientos", xlab="Tratamientos", ylab="Peso de racimo")
media <- tapply(PR1, TRA, mean)
points(media, col = "blue", pch = 19)

#PARA FACTOR CLON
boxplot(PR1~CLON, col="lightsalmon", horizontal=FALSE, pch=8)#col="bisque"
title("Comparaci�n de niveles del factor Clon", xlab="Clones", ylab="Peso de racimo")
mediaM <- tapply(PR1, CLON, mean)
points(mediaM, col = "blue", pch = 19)

#PARA FACTOR L�MINA DE RIEGO
boxplot(PR1~LAMR, col="lightsalmon", horizontal=FALSE, pch=8)#col="bisque"
title("Comparaci�n de niveles del factor l�mina de riego", xlab="L�mina de riego", ylab="Peso de racimo")
mediaD <- tapply(PR1, LAMR, mean)
points(mediaD, col = "blue", pch = 19)

#PARA OBTENER RESIDUOS DEL MODELO
aovPR<- aov(PR1~CLON*LAMR)
resPR<-residuals(aovPR)#Residuos ordinarios 
summary(resPR)#Residuos ordinarios
resPR_estan<- rstandard(aovPR)#Residuos estandarizados (estudentizado en InfoStat)
summary(resPR_estan)#Residuos estandarizados (estudentizado en InfoStat)
resPR_estud<- rstudent(aovPR)#Residuos estudentizados (externamente estudentizado en Infostat)
summary(resPR_estud)#Residuos estudentizados (externamente estudentizado en Infostat)
predPR<-fitted.values(aovPR)#Predichos 
summary(predPR)#Predichos
Residuos <- data.frame(resPR,resPR_estan,resPR_estud, predPR)
Residuos

#DIAGRAMA DE CAJA PARA RESIDUOS ORDINARIOS
boxplot(resPR,col = "bisque",horizontal=FALSE, pch=8)#col="lightsalmon"
title("Diagrama de caja para residos ordinarios", xlab="", ylab="Residuo ordinario")
media <- tapply(resPR,TRA, mean)
points(media, col = "blue", pch = 19)

#TEST DE VALORES AT�PICOS:: RESIDUOS ORDINARIOS
grubbs.test(resPR,type=10)
#Type 10 uno at�pico en cualquiera de las colas
#Type 11 dos at�picos en colas opuestas
#Type 20 dos at�picos en cualquiera de las colas

#VERIFICACI�N GR�FICA DEL SUPUESTO DE NORMALIDAD
qqPlot(resPR,pch=19,col="red",cex=0.9,main="QQ-plot variable peso de racimo",xlab="Cuantiles te�ricos normal",ylab="Cuantiles observados (Rduo peso de racimo)")
plotdensd<-density(resPR)
plot(plotdensd,main="Densidad observada",xlab="Residuos",ylab="Densidad",col="red")
polygon(plotdensd,col="red",border="red")
hisPR <- hist(resPR, ylim=c(0,22), col="violet", xlab="Residuo peso de racimo", main="Distribuci�n de residuos para peso de racimo",border="black")#Cambiar 22 de ser necesario
multiplier <- hisPR$counts/hisPR$density 
hPRden <- density(resPR) 
hPRden$y <- hPRden$y * multiplier[1] 
lines(hPRden, col= "red",lwd=3,lty=4) 
normal.freq(hisPR,col="blue",lwd=3, lty=1)
legend("topleft",col=c("blue","red"),lwd= c(3,3),lty=c(1,4),legend =c("Densidad normal estimada","Densidad observada"), bty = "n")

# TEST DE NORMALIDAD
shapiro.test(resPR)#Modificada por Royston (1995)
ad.test(resPR)#Anderson-Darling (1954)
lillie.test(resPR)#Kolmogorov-Smirnov con modificaci�n de Lillefors (1967)
jb.norm.test(resPR)# Jarque-Bera (1987)
ajb.norm.test(resPR)# Jarque-Bera (1996)

#PRUEBA DE HOMOCEDASTICIDAD
bartlett.test(aovPR$res,TRA)
leveneTest(resPR~TRA, center="median")
leveneTest(resPR~TRA, center="mean", trim=0)#trim aplicada a una media % recortada

#GR�FICO DE PREDICHOS Y RESIDUOS ESTUDENTIZADOS
plot(predPR,resPR_estan,ylim=c(-3,3),xlab="Valores predichos",ylab="Res�duos estudentizados",abline(h=c(0, 2.5, -2.50), lty=c(1,2,2), lwd=c(2,2,2), col=c("blue","red","red")), main="Diagrama de dispersi�n de residuos estudentizados",col="blue",pch=19,cex=0.9)

#PRUEBA M�LTIPLE DE MEDIAS SEG�N JOHN W. TUKEY (1915)
#FACTOR CLON
TukeyHSD(aovPR, "CLON", console=TRUE)
plot(TukeyHSD(aovPR,"CLON"))
HSD.test(aovPR, "CLON", group=TRUE,console=TRUE)
plot(HSD.test(aovPR,"CLON"),main="Grupos y rango", xlab="Clones", ylab="Peso de racimo")
compPR1<-HSD.test(aovPR,"CLON", alpha=0.05,group=TRUE)
par(cex=1.5)#define tama�o de t�tulos y s�mbolos
bar.group(compPR1$groups,horiz=TRUE,density=15,col="blue",border="red",xlim=c(0,40),las=1)#Cambiar 33 de ser necesario
title(cex.main=0.8,main="Comparaci�n medias medias de clones",xlab="Peso de racimo",ylab="")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scott Knott (1974)
sktest1 <- SK(aovPR, "CLON", sig.level=0.05)
summary(sktest1)
plot(sktest1,col=rainbow(max(sktest1$groups)), mm.lty=3, id.las=2,rl=FALSE,xlab= "Clones", ylab="Peso de racimo",title="Comparaci�n de medias ")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Duncan (1955)
duncan.test(aovPR, "CLON", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N FisherLSD (1935)
LSD.test(aovPR, "CLON", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scheffe (1959)
scheffe.test(aovPR, "CLON", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Studen Newman & Keuls, SNK (1959)
SNK.test(aovPR, "CLON", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

# PRUEBA M�LTIPLE DE MEDIAS:FACTOR L�MINA DE RIEGO
#SEG�N JOHN W. TUKEY (1915)
TukeyHSD(aovPR, "LAMR", console=TRUE)
plot(TukeyHSD(aovPR,"LAMR"))
HSD.test(aovPR, "LAMR", group=TRUE,console=TRUE)
plot(HSD.test(aovPR,"LAMR"),main="Grupos y rango", xlab="L�mina de riego", ylab="Peso de racimo")
compPR<-HSD.test(aovPR,"LAMR", alpha=0.05,group=TRUE)
par(cex=1.5)#define tama�o de t�tulos y s�mbolos
bar.group(compPR$groups,horiz=TRUE,density=15,col="blue",border="red",xlim=c(0,46),las=1)#Cambiar 46 de ser necesario
title(cex.main=0.8,main="Comparaci�n de medias l�mina de riego",xlab="Peso de racimo",ylab="")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scott Knott (1974)
sktest <- SK(aovPR, "LAMR", sig.level=0.05)
summary(sktest)
plot(sktest,col=rainbow(max(sktest$groups)), mm.lty=3, id.las=2,rl=FALSE,xlab= "L�mina de riego", ylab="Peso de racimo",title=" ")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Duncan
duncan.test(aovPR, "LAMR", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N FisherLSD
LSD.test(aovPR, "LAMR", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scheffe
scheffe.test(aovPR, "LAMR", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N SNK
SNK.test(aovPR, "LAMR", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA M�LTIPLE DE MEDIAS:INTERACCI�N DE FACTORES
#SEG�N JOHN W. TUKEY (1915)
TukeyHSD(aovPR, "CLON:LAMR", console=TRUE)
plot(TukeyHSD(aovPR,"CLON:LAMR"))
trat <- with(platano, interaction(CLON,LAMR))
amod <- aov(PR1~trat, data=platano)
HSD.test(amod, "trat", group=TRUE, console=TRUE)
plot(HSD.test(amod,"trat"),main="Grupos y rango", xlab="Tratamiento/interacci�n", ylab="Peso de racimo")
compPR2<-HSD.test(amod,"trat", alpha=0.05,group=TRUE)
par(cex=1.2)#define tama�o de t�tulos y s�mbolos
bar.group(compPR2$groups,horiz=TRUE,density=20,col="blue",border="red",xlim=c(0,53),las=1, cex.axis =0.7, cex.names=0.7)#Cambiar 48 de ser necesario
title(cex.main=1.0,main="Comparaci�n de medias de tratamientos",xlab="Peso de racimo",ylab="")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scott Knott (1974)
sktest2 <- SK(amod, "trat", sig.level=0.05)
summary(sktest2)
plot(sktest2,col=rainbow(max(sktest2$groups)), mm.lty=3, id.las=2,rl=FALSE,xlab= "Tratamientos/interacci�n", ylab="Peso de racimo",title="Comparaci�n de medias")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Duncan
duncan.test(amod, "trat", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N FisherLSD
LSD.test(amod, "trat", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N Scheffe
scheffe.test(amod, "trat", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA M�LTIPLE DE MEDIAS SEG�N SNK
SNK.test(amod, "trat", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#AN�LISIS DE INTERACCI�N
#DIAGRAMA DE INTERACCI�N DE EFECTOS ENTRE CLON Y L�MINA DE RIEGO
interaction.plot(CLON,LAMR,PR1,type="b",col=c(2:3),leg.bty="b",leg.bg="beige",lwd=2,pch=c(18,20,17,24,1),xlab="Clones",ylab="Peso de racimo",trace.label="L�minas de riego",main="Gr�fico de intereacci�n: L�mina de riego dado Clones")
interaction.plot(LAMR,CLON,PR1,type="b",col=c(2:3),leg.bty="b",leg.bg="beige",lwd=2,pch=c(18,20,17,24,1),xlab="L�minas de riego",ylab="Peso de racimo",trace.label="L�minas de riego",main="Gr�fico de intereacci�n: Clones dado l�minas de riego")
ANAint<-lm(PR1~CLON*LAMR)
(ANAint.means<- interactionMeans(ANAint))#medias ajustadas de tratamientos
plot(ANAint.means, errorbar="ci0")#Todos los gr�ficos de interacci�n
emmip(ANAint, LAMR~CLON)#Gr�fico de interacci�n: L�minas de riego a trav�s de clones (L�minas de riego dado Clones)con emeams
emmip(ANAint, CLON~LAMR)#Grafico de interacci�n: Clones a trav�s de l�minas de riego (Clones dado l�mina de riego)con emeams
testInteractions(ANAint, fixed="LAMR", across="CLON", digits=5)#Efectos simples:L�minas de riego a trav�s de clones
testInteractions(ANAint, fixed="CLON", across="LAMR", digits=5)#Efectos simples:Clones a trav�s de l�minas de riego
testInteractions(ANAint, pairwise="LAMR", across="CLON")#Interacci�n:Diferencias de L�minas de riego a trav�s de clones
testInteractions(ANAint, pairwise="CLON", across="LAMR", digits=5)#Interacci�n:diferencias de Clones a trav�s de l�minas de riego
testInteractions(ANAint)#interacci�n:diferencias de efectos simples

#TRANFORMACI�N BOX-COX (para valores estrictamente positivos): S�lo cuando se requiera
lambdaPR<- boxcoxfit (PR1, lambda2 = NULL)
lambdaPR
lbda =lambdaPR$lambda
if (lbda==0) {PR.t=log(PR1)}
if (lbda!=0) {PR.t=(PR1^lbda-1)/lbda}
par(mfcol=c(2,2))#inicia panel seccionado
hist (PR.t, col="gray")
PR.t
PR.T<-as.vector(PR.t)
PR.T1<-as.numeric(PR.T)
aovPR.T1<- aov(PR.T1~CLON*LAMR)
summary(aovPR.T1)#Resultados del ANOVA

#TRANSFORMACI�N ARSENO de la ra�z cuadrada de ...
YT<- ((180/pi)*(asin(sqrt((PR1+0)/100))))#Puede cambiar "0" por "0.5" si hay observaciones con valor cero
hist (YT, col="gray")
YT
YT.T<-as.vector(YT)
YT.T1<-as.numeric(YT.T)
aovYT.T1<- aov(PR.T1~CLON*LAMR)
summary(aovYT.T1)#Resultados del ANOVA
old_par<-par()#fin de panel seccionado
detach (platano)


