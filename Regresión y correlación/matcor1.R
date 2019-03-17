#Script R para correlación lineal de Pearson y correlación de distancias
#Primero instalar los siguientes paquetes
library(energy) 
library(ggplot2) 
library(magrittr) 
library(ggpubr) 
library(dplyr) 
library(Hmisc) 
library(corrplot)
library("PerformanceAnalytics")
library(psych)
library(ppcor)
#Luego debe de activarlos

#Importar la base de datos
matrizcor1 <- read.csv("matcor1.csv", header = T)
print(head(matrizcor1)) #Para que muestre las primeras 6 filas
attach(matrizcor1) #Para adjuntar las variables
names(matrizcor1) #Nombre de las columnas
str(matrizcor1) #Tipo de objeto(numerico, entero, caracter,etc)

#Pruebas de normalidad para cada variable
shapiro.test(TF)
shapiro.test(AP)
shapiro.test(CS)
shapiro.test(PF)
shapiro.test(RE)
multi.hist(matrizcor1,ncol =NULL,nrow =NULL,  breaks="sturges", bcol="lightblue", dcol = c("blue", "red"), dlty = c("dotted", "solid"),  lwd=2, main = "")#Usar uno de "sturges", "freedman-diaconis"("fd"), "scott"

#Gráficos cuantil cuantil: Verificación gráfica de normalidad
ggqqplot(TF, xlab ="Cuantiles teóricos",ylab="TF(cantidad de frutos)")
ggqqplot(AP, xlab ="Cuantiles teóricos", ylab="AP(altura de planta)")
ggqqplot(CS, xlab ="Cuantiles teóricos", ylab="CS(concentración de sólidos)")
ggqqplot(PF, xlab ="Cuantiles teóricos", ylab="PF(peso del fruto)")
ggqqplot(RE, xlab ="Cuantiles teóricos", ylab="RE(rendimiento)")

#matriz de varianzas y covarianzas
cov(matrizcor1)

#coeficientes de correlación y significancia
cor.test(TF, RE, method="pearson") 
cor.test(RE, CS, method="pearson") 
cor.test(PF, CS, method="pearson") 
cor.test(TF, PF, method="pearson") 
cor.test(AP, TF, method="pearson") 

#correlación parcial
pcor.test(x = TF, y = RE, z =CS, method = "pearson")# Corr. Entre TF y RE controlando CS
pcor.test(x = TF, y = CS, z =RE, method = "pearson")# Corr. Entre TF y CS controlando RE
pcor.test(x = TF, y = PF, z = RE, method = "pearson")# Corr. Entre TF y PF controlando RE

#Coeficiente de correlación múltiple
(R <- cor(RE, fitted(lm(RE ~ TF +PF))))#Rendimiento en función del total de frutos y peso del fruto
R^2#coeficiente de determinación múltiple                 

#Diagramas de correlación o correlogramas
round(cor(matrizcor1),2)
rcorr(as.matrix(matrizcor1))
corrplot(cor(matrizcor1),method="circle")
corrplot(cor(matrizcor1),method="ellipse")
corrplot(cor(matrizcor1),method="pie")
corrplot(cor(matrizcor1),method="number")
corrplot(cor(matrizcor1),type="upper")
corrplot(cor(matrizcor1),type="lower")
corrplot(cor(matrizcor1),order="hclust")
corrplot(cor(matrizcor1),type="upper", order="hclust")
corrplot(cor(matrizcor1),type="upper", order="hclust", method = "number", number.digits=3)
chart.Correlation(matrizcor1)
pairs.panels(matrizcor1, pch=21,main="matriz de correlaciones")

#Recta que mejor ajusta
ggscatter(matrizcor1,x = "TF", y = "RE", add = "loess", conf.int = TRUE, add.params = list(color = "blue", fill = "lightgray"),color="red",shape =10, size = 3,  cor.coef = TRUE, cor.coeff.args = list(method = "pearson", label.x.npc = "left", label.y.npc = "top"), xlab = "Total de frutos", ylab = "Rendimiento (t)")
ggscatter(matrizcor1,x = "RE", y = "CS", add = "reg.line", conf.int = TRUE, add.params = list(color = "blue", fill = "lightgray"),color="red",shape =10, size = 3,  cor.coef = TRUE, cor.coeff.args = list(method = "pearson", label.x.npc = "left", label.y.npc = "top"), xlab = "Rendimiento (t)", ylab = "Concentración de sólidos (Brix)")
ggscatter(matrizcor1,x = "PF", y = "CS", add = "loess", conf.int = TRUE, add.params = list(color = "blue", fill = "lightgray"),color="red",shape =10, size = 3,  cor.coef = TRUE, cor.coeff.args = list(method = "pearson", label.x.npc = "left", label.y.npc = "top"), xlab = "Peso del fruto (g)", ylab = "Concentración de sólidos (Brix)")
ggscatter(matrizcor1,x = "TF", y = "PF", add = "reg.line", conf.int = TRUE,add.params = list(color = "blue", fill = "lightgray"),color="red",shape =10, size = 3,  cor.coef = TRUE, cor.coeff.args = list(method = "pearson", label.x.npc = "left", label.y.npc = "top"), xlab = "Total de frutos", ylab = "Peso del fruto (g)")

#Correlación de distancias: relación lineal o no lineal
#Entre el total de frutos y el rendimiento
dcor(TF,RE) #Coeficiente de correlación de distancias
unlist(DCOR(TF,RE))#covarianza, dcor y varianzas de variables
bcdcor(TF,RE)#Con corrección del sesgo que incrementa con la dimensión
dcor.test(TF, RE, R=61)#prueba de significancia sin corrección (R=2n-1)
dcor.ttest(TF, RE, distance=FALSE)#Prueba de significancia con corrección
#Entre rendimiento y concentración de sólidos
dcor(RE,CS) #Coeficiente de correlación de distancias
unlist(DCOR(RE,CS))#covarianza, dcor y varianzas de variables
bcdcor(RE,CS)#Con corrección del sesgo que incrementa con la dimensión
dcor.test(RE,CS, R=61)#prueba de significancia sin corrección (R=2n-1)
dcor.ttest(RE,CS, distance=FALSE)#Prueba de significancia con corrección
#Entre el peso del fruto y concentración de sólidos
dcor(PF,CS) #Coeficiente de correlación de distancias
unlist(DCOR(PF,CS))#covarianza, dcor y varianzas de variables
bcdcor(PF,CS)#Con corrección del sesgo que incrementa con la dimensión
dcor.test(PF,CS, R=61)#prueba de significancia sin corrección (R=2n-1)
dcor.ttest(PF,CS, distance=FALSE)#Prueba de significancia con corrección
#Total del frutos y peso del fruto
dcor(TF,PF) #Coeficiente de correlación de distancias
unlist(DCOR(TF,PF))#covarianza, dcor y varianzas de variables
bcdcor(TF,PF)#Con corrección del sesgo que incrementa con la dimensión
dcor.test(TF,PF, R=61)#prueba de significancia sin corrección (R=2n-1)
dcor.ttest(TF,PF, distance=FALSE)#Prueba de significancia con corrección
