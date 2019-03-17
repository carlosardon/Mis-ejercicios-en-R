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
library(ScottKnott)#Prueba múltiple de medias
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

#LOS VECTORES SE CONVIERTEN A TIPO NUMÉRICO
PR1<-as.numeric(PR)

#INDICANDO EL MODELO ESTADÍSTICO PARA EL ANOVA DE LA VARIABLE PESO DE RACIMO
aovPR1<- aov(PR1~CLON*LAMR)
summary(aovPR1)#Resultados del ANOVA
cv.model(aovPR1)
etaSquared(aovPR1) 

#DIAGRAMAS DE CAJA
#PARA TRATAMIENTOS
boxplot(PR1~TRA, col="lightsalmon", horizontal=FALSE, pch=8)#col="bisque"
title("Comparación de tratamientos", xlab="Tratamientos", ylab="Peso de racimo")
media <- tapply(PR1, TRA, mean)
points(media, col = "blue", pch = 19)

#PARA FACTOR CLON
boxplot(PR1~CLON, col="lightsalmon", horizontal=FALSE, pch=8)#col="bisque"
title("Comparación de niveles del factor Clon", xlab="Clones", ylab="Peso de racimo")
mediaM <- tapply(PR1, CLON, mean)
points(mediaM, col = "blue", pch = 19)

#PARA FACTOR LÁMINA DE RIEGO
boxplot(PR1~LAMR, col="lightsalmon", horizontal=FALSE, pch=8)#col="bisque"
title("Comparación de niveles del factor lámina de riego", xlab="Lámina de riego", ylab="Peso de racimo")
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

#TEST DE VALORES ATÍPICOS:: RESIDUOS ORDINARIOS
grubbs.test(resPR,type=10)
#Type 10 uno atípico en cualquiera de las colas
#Type 11 dos atípicos en colas opuestas
#Type 20 dos atípicos en cualquiera de las colas

#VERIFICACIÓN GRÁFICA DEL SUPUESTO DE NORMALIDAD
qqPlot(resPR,pch=19,col="red",cex=0.9,main="QQ-plot variable peso de racimo",xlab="Cuantiles teóricos normal",ylab="Cuantiles observados (Rduo peso de racimo)")
plotdensd<-density(resPR)
plot(plotdensd,main="Densidad observada",xlab="Residuos",ylab="Densidad",col="red")
polygon(plotdensd,col="red",border="red")
hisPR <- hist(resPR, ylim=c(0,22), col="violet", xlab="Residuo peso de racimo", main="Distribución de residuos para peso de racimo",border="black")#Cambiar 22 de ser necesario
multiplier <- hisPR$counts/hisPR$density 
hPRden <- density(resPR) 
hPRden$y <- hPRden$y * multiplier[1] 
lines(hPRden, col= "red",lwd=3,lty=4) 
normal.freq(hisPR,col="blue",lwd=3, lty=1)
legend("topleft",col=c("blue","red"),lwd= c(3,3),lty=c(1,4),legend =c("Densidad normal estimada","Densidad observada"), bty = "n")

# TEST DE NORMALIDAD
shapiro.test(resPR)#Modificada por Royston (1995)
ad.test(resPR)#Anderson-Darling (1954)
lillie.test(resPR)#Kolmogorov-Smirnov con modificación de Lillefors (1967)
jb.norm.test(resPR)# Jarque-Bera (1987)
ajb.norm.test(resPR)# Jarque-Bera (1996)

#PRUEBA DE HOMOCEDASTICIDAD
bartlett.test(aovPR$res,TRA)
leveneTest(resPR~TRA, center="median")
leveneTest(resPR~TRA, center="mean", trim=0)#trim aplicada a una media % recortada

#GRÁFICO DE PREDICHOS Y RESIDUOS ESTUDENTIZADOS
plot(predPR,resPR_estan,ylim=c(-3,3),xlab="Valores predichos",ylab="Resíduos estudentizados",abline(h=c(0, 2.5, -2.50), lty=c(1,2,2), lwd=c(2,2,2), col=c("blue","red","red")), main="Diagrama de dispersión de residuos estudentizados",col="blue",pch=19,cex=0.9)

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN JOHN W. TUKEY (1915)
#FACTOR CLON
TukeyHSD(aovPR, "CLON", console=TRUE)
plot(TukeyHSD(aovPR,"CLON"))
HSD.test(aovPR, "CLON", group=TRUE,console=TRUE)
plot(HSD.test(aovPR,"CLON"),main="Grupos y rango", xlab="Clones", ylab="Peso de racimo")
compPR1<-HSD.test(aovPR,"CLON", alpha=0.05,group=TRUE)
par(cex=1.5)#define tamaño de títulos y símbolos
bar.group(compPR1$groups,horiz=TRUE,density=15,col="blue",border="red",xlim=c(0,40),las=1)#Cambiar 33 de ser necesario
title(cex.main=0.8,main="Comparación medias medias de clones",xlab="Peso de racimo",ylab="")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Scott Knott (1974)
sktest1 <- SK(aovPR, "CLON", sig.level=0.05)
summary(sktest1)
plot(sktest1,col=rainbow(max(sktest1$groups)), mm.lty=3, id.las=2,rl=FALSE,xlab= "Clones", ylab="Peso de racimo",title="Comparación de medias ")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Duncan (1955)
duncan.test(aovPR, "CLON", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN FisherLSD (1935)
LSD.test(aovPR, "CLON", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Scheffe (1959)
scheffe.test(aovPR, "CLON", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Studen Newman & Keuls, SNK (1959)
SNK.test(aovPR, "CLON", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

# PRUEBA MÚLTIPLE DE MEDIAS:FACTOR LÁMINA DE RIEGO
#SEGÚN JOHN W. TUKEY (1915)
TukeyHSD(aovPR, "LAMR", console=TRUE)
plot(TukeyHSD(aovPR,"LAMR"))
HSD.test(aovPR, "LAMR", group=TRUE,console=TRUE)
plot(HSD.test(aovPR,"LAMR"),main="Grupos y rango", xlab="Lámina de riego", ylab="Peso de racimo")
compPR<-HSD.test(aovPR,"LAMR", alpha=0.05,group=TRUE)
par(cex=1.5)#define tamaño de títulos y símbolos
bar.group(compPR$groups,horiz=TRUE,density=15,col="blue",border="red",xlim=c(0,46),las=1)#Cambiar 46 de ser necesario
title(cex.main=0.8,main="Comparación de medias lámina de riego",xlab="Peso de racimo",ylab="")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Scott Knott (1974)
sktest <- SK(aovPR, "LAMR", sig.level=0.05)
summary(sktest)
plot(sktest,col=rainbow(max(sktest$groups)), mm.lty=3, id.las=2,rl=FALSE,xlab= "Lámina de riego", ylab="Peso de racimo",title=" ")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Duncan
duncan.test(aovPR, "LAMR", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN FisherLSD
LSD.test(aovPR, "LAMR", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Scheffe
scheffe.test(aovPR, "LAMR", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN SNK
SNK.test(aovPR, "LAMR", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA MÚLTIPLE DE MEDIAS:INTERACCIÓN DE FACTORES
#SEGÚN JOHN W. TUKEY (1915)
TukeyHSD(aovPR, "CLON:LAMR", console=TRUE)
plot(TukeyHSD(aovPR,"CLON:LAMR"))
trat <- with(platano, interaction(CLON,LAMR))
amod <- aov(PR1~trat, data=platano)
HSD.test(amod, "trat", group=TRUE, console=TRUE)
plot(HSD.test(amod,"trat"),main="Grupos y rango", xlab="Tratamiento/interacción", ylab="Peso de racimo")
compPR2<-HSD.test(amod,"trat", alpha=0.05,group=TRUE)
par(cex=1.2)#define tamaño de títulos y símbolos
bar.group(compPR2$groups,horiz=TRUE,density=20,col="blue",border="red",xlim=c(0,53),las=1, cex.axis =0.7, cex.names=0.7)#Cambiar 48 de ser necesario
title(cex.main=1.0,main="Comparación de medias de tratamientos",xlab="Peso de racimo",ylab="")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Scott Knott (1974)
sktest2 <- SK(amod, "trat", sig.level=0.05)
summary(sktest2)
plot(sktest2,col=rainbow(max(sktest2$groups)), mm.lty=3, id.las=2,rl=FALSE,xlab= "Tratamientos/interacción", ylab="Peso de racimo",title="Comparación de medias")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Duncan
duncan.test(amod, "trat", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN FisherLSD
LSD.test(amod, "trat", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Scheffe
scheffe.test(amod, "trat", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN SNK
SNK.test(amod, "trat", alpha=0.05, group=TRUE, console=TRUE, main="Peso de racimo")

#ANÁLISIS DE INTERACCIÓN
#DIAGRAMA DE INTERACCIÓN DE EFECTOS ENTRE CLON Y LÁMINA DE RIEGO
interaction.plot(CLON,LAMR,PR1,type="b",col=c(2:3),leg.bty="b",leg.bg="beige",lwd=2,pch=c(18,20,17,24,1),xlab="Clones",ylab="Peso de racimo",trace.label="Láminas de riego",main="Gráfico de intereacción: Lámina de riego dado Clones")
interaction.plot(LAMR,CLON,PR1,type="b",col=c(2:3),leg.bty="b",leg.bg="beige",lwd=2,pch=c(18,20,17,24,1),xlab="Láminas de riego",ylab="Peso de racimo",trace.label="Láminas de riego",main="Gráfico de intereacción: Clones dado láminas de riego")
ANAint<-lm(PR1~CLON*LAMR)
(ANAint.means<- interactionMeans(ANAint))#medias ajustadas de tratamientos
plot(ANAint.means, errorbar="ci0")#Todos los gráficos de interacción
emmip(ANAint, LAMR~CLON)#Gráfico de interacción: Láminas de riego a través de clones (Láminas de riego dado Clones)con emeams
emmip(ANAint, CLON~LAMR)#Grafico de interacción: Clones a través de láminas de riego (Clones dado lámina de riego)con emeams
testInteractions(ANAint, fixed="LAMR", across="CLON", digits=5)#Efectos simples:Láminas de riego a través de clones
testInteractions(ANAint, fixed="CLON", across="LAMR", digits=5)#Efectos simples:Clones a través de láminas de riego
testInteractions(ANAint, pairwise="LAMR", across="CLON")#Interacción:Diferencias de Láminas de riego a través de clones
testInteractions(ANAint, pairwise="CLON", across="LAMR", digits=5)#Interacción:diferencias de Clones a través de láminas de riego
testInteractions(ANAint)#interacción:diferencias de efectos simples

#TRANFORMACIÓN BOX-COX (para valores estrictamente positivos): Sólo cuando se requiera
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

#TRANSFORMACIÓN ARSENO de la raíz cuadrada de ...
YT<- ((180/pi)*(asin(sqrt((PR1+0)/100))))#Puede cambiar "0" por "0.5" si hay observaciones con valor cero
hist (YT, col="gray")
YT
YT.T<-as.vector(YT)
YT.T1<-as.numeric(YT.T)
aovYT.T1<- aov(PR.T1~CLON*LAMR)
summary(aovYT.T1)#Resultados del ANOVA
old_par<-par()#fin de panel seccionado
detach (platano)


