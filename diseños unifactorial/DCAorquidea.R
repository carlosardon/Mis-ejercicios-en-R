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
options (scipen = 999, digits = 5)
#ABRIR RSTUDIO E INDICAR LA CARPETA DE TRABAJO
orquiPG <- read.csv("DCAorquidea.csv", header = T)
attach(orquiPG)
names(orquiPG)
orquiPG

#DEFINIR LAS VARIABLES INDEPENDIENTES (FACTORES)
TRA<- factor(MDC)#No es necesario: "MCD$orqui" debido al attach
#CREANDO VECTORES DE DATOS PARA LAS VARIABLES
PG<-as.vector(GER)
#LOS VECTORES SE CONVIERTEN A TIPO NUMÉRICO
PG1<-as.numeric(GER)
id<-as.numeric(id)

#INDICANDO EL MODELO ESTADÍSTICO PARA EL ANOVA DE LA VARIABLE PORCENTAJE DE GERMINACIÓN
aovPG<-lm(PG1~TRA)
anova(aovPG)
Anova(aovPG)
aovPG1<- aov(PG1~TRA)
summary(aovPG1)#Resultados del ANOVA
cv.model(aovPG1)
etaSquared(aovPG1) 

#DIAGRAMA DE CAJA PARA TRATAMIENTOS
boxplot(PG1~TRA,col = "lightsalmon",horizontal=FALSE, pch=8)#col="bisque"
title("Comparación de tratamientos con diagramas de caja", xlab="Tratamientos/medios de cultivo", ylab="Porcentaje promedio de germinación")
media <- tapply(PG1, TRA, mean)
points(media, col = "blue", pch = 19)

ggplot(data=orquiPG, aes(MDC,  PG1))+
  geom_boxplot(outlier.color ="green",outlier.shape =8,outlier.size = 3) + 
  geom_jitter()+
  stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="red")

#PARA OBTENER RESIDUOS DEL MODELO
resPG1<-residuals(aovPG1)#Residuos ordinarios 
summary(resPG1)#Residuos ordinarios
resPG1_estan<- rstandard(aovPG1)#Residuos estandarizados (estudentizado en InfoStat)
summary(resPG1_estan)#Residuos estandarizados (estudentizado en InfoStat)
resPG1_estud<- rstudent(aovPG1)#Residuos estudentizados (externamente estudentizado en Infostat)
summary(resPG1_estud)#Residuos estudentizados (externamente estudentizado en Infostat)
predPG1<-fitted.values(aovPG1)#Predichos 
summary(predPG1)#Predichos
Residuos <- data.frame(resPG1,resPG1_estan,resPG1_estud, predPG1)
Residuos

#DIAGRAMA DE CAJA PARA RESIDUOS ORDINARIOS
boxplot(resPG1,col = "bisque",horizontal=FALSE, pch=8)#col="lightsalmon"
title("Diagrama de caja para residos ordinarios", xlab="", ylab="Residuo ordinario")
media <- tapply(resPG1,TRA, mean)
points(media, col = "blue", pch = 19)

#TEST PARA RESIDUOS ORDINARIOS ATÍPICOS
grubbs.test(resPG1,type=10)
#Type 10 uno atípico en cualquiera de las colas
#Type 11 dos atípicos en colas opuestas
#Type 20 dos atípicos en cualquiera de las colas

#ANÁLISIS GRÁFICO DEL SUPUESTO DE NORMALIDAD
qqPlot(resPG1,pch=19,col="red",cex=0.9,main="QQ-plot variable porcentaje de germinación",xlab="Cuantiles teóricos normal",ylab="Cuantiles observados (Rduo germinación)")
plotdensd<-density(resPG1)
plot(plotdensd,main="Densidad observada",xlab="residuos",ylab="Densidad",col="red")
polygon(plotdensd,col="red",border="red")
hisPG1 <- hist(resPG1, ylim=c(0,8), col="violet", main="Distribución de residuos para porcentaje de germinación",border="black")#Cambiar 8 de ser necesario
multiplier <- hisPG1$counts/hisPG1$density 
hPG1den <- density(resPG1) 
hPG1den$y <- hPG1den$y * multiplier[1] 
lines(hPG1den, col= "red",lwd=3,lty=4) 
normal.freq(hisPG1,col="blue",lwd=3, lty=1)
legend("topleft",col=c("blue","red"),lwd= c(3,3),lty=c(1,4),legend =c("Densidad normal estimada","Densidad observada"), bty = "n")

# TEST DE NORMALIDAD
shapiro.test(resPG1)#Royston (1995)
ad.test(resPG1)#Anderson-Darling (1954)
lillie.test(resPG1)#Kolmogorov-Smirnov con modificación de Lillefors (1967)
jb.norm.test(resPG1)# Jarque-Bera (1987)
ajb.norm.test(resPG1)# Jarque-Bera (1996)

#PRUEBA DE HOMOCEDASTICIDAD
bartlett.test(aovPG1$res,TRA)
leveneTest(resPG1~TRA, center="median")
leveneTest(resPG1~TRA, center="mean", trim=0)#trim aplicada a una media % recortada

#GRÁFICO DE PREDICHOS Y RESIDUOS ESTUDENTIZADOS
plot(predPG1,resPG1_estan,xlab="Valores predichos",ylim=c(-3,3), ylab="Resíduos estudentizados",abline(h=c(0, 2.5, -2.50), lty=c(1,2,2), lwd=c(2,2,2), col=c("blue","red","red")), main="Diagrama de dispersión de residuos estudentizados",col="blue",pch=19,cex=0.9)

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN JOHN W. TUKEY (1915)
TukeyHSD(aovPG1, "TRA", console=TRUE, ordered = FALSE, conf.level = 0.95)
plot(TukeyHSD(aovPG1,"TRA"))
HSD.test(aovPG1, "TRA", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de germinación según medio de cultivo")
plot(HSD.test(aovPG1,"TRA"),main="Grupos y rango", xlab="Medios de cultivo", ylab="Porcentaje de germinación promedio")
comppg1<-HSD.test(aovPG1,"TRA", alpha=0.05,group=TRUE)
par(cex=1.5)#define tamaño de títulos y símbolos
bar.group(comppg1$groups,horiz=TRUE,density=8,col="blue",border="red",xlim=c(0,85),las=1)#Cambiar 85 de ser necesario
title(cex.main=0.8,main="Comparación de medias entre tratamientos",xlab="Porcentaje de germinación",ylab="Medios de cultivo")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Scott Knott (1974)
sktest <- SK(aovPG1, sig.level=0.05)
summary(sktest)
plot(sktest,col=rainbow(max(sktest$groups)), mm.lty=3, id.las=2,rl=FALSE,xlab= "Tratamientos/medios de cultivo", ylab="Porcentaje de germinación promedio",title=" ")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Duncan
duncan.test(aovPG1, "TRA", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de germinación según medio de cultivo")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN FisherLSD
LSD.test(aovPG1, "TRA", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de germinación según medio de cultivo")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Scheffe
scheffe.test(aovPG1, "TRA", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de germinación según medio de cultivo")

#PRUEBA MÚLTIPLE DE MEDIAS SEGÚN Scheffe
SNK.test(aovPG1, "TRA", alpha=0.05, group=TRUE, console=TRUE, main="Porcentaje de germinación según medio de cultivo")

#TRANFORMACIÓN BOX-COX (para valores estrictamente positivos): Sólo cuando se requiera
lambdaPG1<- boxcoxfit (PG1, lambda2 = NULL)
lambdaPG1
lbda =lambdaPG1$lambda
if (lbda==0) {PG.t=log(PG1)}
if (lbda!=0) {PG.t=(PG1^lbda-1)/lbda}
hist (PG.t, col="gray")
PG.t
PG.T<-as.vector(PG.t)
PG.T1<-as.numeric(PG.T)
aovPG.T1<- aov(PG.T1~TRA)
summary(aovPG.T1)#Resultados del ANOVA

#TRANSFORMACIÓN ARSENO de la raíz cuadrada de ...
YT<- ((180/pi)*(asin(sqrt((PG1+0)/100))))# Puede cambiar "0" por "0.5" si hay observaciones con valor cero
hist (YT, col="gray")
YT
YT.T<-as.vector(YT)
YT.T1<-as.numeric(YT.T)
aovYT.T1<- aov(YT.T1~TRA)
summary(aovYT.T1)#Resultados del ANOVA

detach(orquiPG)

