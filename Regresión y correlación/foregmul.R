library(dplyr)#atípicos e influenciales
library(psych)
library(GGally)
library(ggplot2)
library(gridExtra)
library(lmtest)
library(corrplot)
library(car)#valores influenciales
library(leaps)
library(ggplot2)
library(MASS)
library(sandwich)#correccion de heterocedasticidad de White
library(nortest)#pruebas de normalidad
library(normtest)#pruebas de normalidad
library(e1071)
library(caret) # for box-cox transformation
library(gvlma) # global validation of linear model assumptions
options (scipen = 999, digits = 5)

#cargar el archivo de datos
datos <- read.csv("foregmul.csv")
datos#Se muestran los datos
attach(datos) #Para adjuntar las variables
names(datos) #Nombre de las columnas
str(datos) #Mostrar estructura del objeto

#Correlación entre variables
round(cor(x = datos, method = "pearson"), 3)
ggpairs(datos, lower = list(continuous = "smooth"),diag = list(continuous = "barDiag"), axisLabels = "none")

#Generar el modelo
modelo <- lm(ctf~ dfu + alt + dco + dma + btf + bts, data = datos )
summary(modelo)
avPlots(modelo, pch=16, main ="correlaciones parciales de Pearson")#Added-Variable Plots en reg.múltiple

#Selección de mejores predictores
stepAIC(object = modelo, direction = "both", trace = 1)#Puede ser "backward","forward" o ambos "both"
#mejor modelo resultante del proceso de selección es:
modeloa <- (lm(formula = ctf~ dfu + dco + dma + btf + bts, data = datos))
summary(modeloa)
str(summary(modeloa))#estructura del objeto que contiene el resumen del modelo
summary(modeloa)$r.squared

#intervalos de confianza
confint(modeloa)

#Gráficos para chequeo de supuestos
predicts <- modeloa$fitted.values
predicts
rest<-rstandard(modeloa)
rest
plot1 <- ggplot(data = datos, aes(dfu, modeloa$residuals)) + geom_point() + geom_smooth(color = "firebrick",method = loess,formula=y~x) + geom_hline(yintercept = 0) + theme_bw()
plot2 <- ggplot(data = datos, aes(dco, modeloa$residuals)) + geom_point() + geom_smooth(color = "firebrick",method = loess,formula=y~x) + geom_hline(yintercept = 0) + theme_bw()
plot3 <- ggplot(data = datos, aes(dma, modeloa$residuals)) + geom_point() + geom_smooth(color = "firebrick",method = loess,formula=y~x) + geom_hline(yintercept = 0) + theme_bw()
plot4 <- ggplot(data = datos, aes(btf, modeloa$residuals)) + geom_point() + geom_smooth(color = "firebrick",method = loess,formula=y~x) + geom_hline(yintercept = 0) + theme_bw()
plot5 <- ggplot(data = datos, aes(bts, modeloa$residuals)) + geom_point() + geom_smooth(color = "firebrick",method = loess,formula=y~x) + geom_hline(yintercept = 0) + theme_bw()
grid.arrange(plot1, plot2, plot3, plot4, plot5)
multi.hist(x = modeloa$residual, dcol = c("blue", "red"), dlty = c("dotted", "solid"), lwd=c(3,2), bcol="cyan", main = "Distribución de residuos")
qqPlot(modeloa, id.n = 3, pch=16)#usando t
qqPlot(rest, pch=16)#usando z
ggplot(data = datos, aes(modeloa$fitted.values, rest)) + geom_point() + geom_smooth(color = "firebrick",method = "loess" , formula = y ~ x, se = FALSE) + geom_hline(yintercept = 0) + theme_bw()
res<-modeloa$residuals
rest<-rstandard(modeloa)

#Valores atípicos
outlierTest(modeloa)
boxplot(modeloa$residuals, col = rgb(0,0,1,0.5), main = "Boxplot of y[,2]", varwidth=FALSE)
ggplot(data = datos, aes(modeloa$fitted.values, modeloa$residuals)) + geom_boxplot(outlier.colour = "green", outlier.color = "blue", outlier.shape = 19)+ #dibujamos el diagrama de cajas
stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="red")
leveragePlots(modeloa)

#identificación de posibles valores atípicos influyentes
datos$studentized_residual <- rstudent(modeloa)
ggplot(data = datos, aes(x = predict(modeloa), y = abs(studentized_residual))) +
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed") +
  # se identifican en rojo observaciones con residuos estandarizados absolutos > 3
  geom_point(aes(color = ifelse(abs(studentized_residual) > 3, 'red', 'black'))) +
  scale_color_identity() +
  labs(title = "Distribución de los residuos studentized", x = "predicción modelo") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
which(abs(datos$studentized_residual) > 3)
summary(influence.measures(modelo1))
influencePlot(modeloa)

#autocorrelación Durbin Watson
dwt(modeloa, alternative = "two.sided")

#pruebas de normalidad
shapiro.test(modeloa$residuals)#Royston (1995)
ad.test(modeloa$residuals)#Anderson-Darling
lillie.test(modeloa$residuals)#Kolmogorov-Smirnov con modificación de Lillefors
jb.norm.test(modeloa$residuals)# Jarque-Bera 1987
ajb.norm.test(modeloa$residuals)# Jarque-Bera 1996

#homocedasticidad
bptest(modeloa)#Breusch-Pagan test
gqtest(modeloa)#Goldfeld-Quandt test
ncvTest(modeloa)#Non-Constant Error Variance

# Prueba de White
resids <- modeloa$residuals
resids2 <- resids^2
predicts <- modeloa$fitted.values
predicts2 <- predicts^2
lmhet <- lm(resids2 ~ predicts + predicts2)
summary(lmhet)

# Cuando no se encuentra evidencia estadística
#de que la varianza de los residuos cambia con los predichos o
#el cuadrado de los predichos, se dice que no hay evidencia de heterocedasticidad.

#Corrección de heterocedasticidad de White
vv <- vcovHC(modeloa, type="HC1")
vv
coeftest(modeloa, vcov = vv)
summary(modeloa)#Comparar este con el anterior

#no colinealidad
corrplot(cor(dplyr::select(datos, dfu, dco,dma,btf, bts)), method = "number", tl.col = "black")
vif(modeloa)# si es menor o igual que 10 no hay problema de multicolinealidad
#Regla de Klein, si ninguna de la regresiones auxiliares presenta R2 múltiple mayor que el global, no hay problema de multicolinealidad. 
modelo1 <- (lm(formula = dfu ~ dco+dma+btf+bts,data=datos))
modelo2 <- (lm(formula = dco ~ dfu+dma+btf+bts,data=datos))
modelo3 <- (lm(formula = dma ~ dco+dfu+btf+bts,data=datos))               
modelo4 <- (lm(formula = btf ~ dfu+dco+dma+bts,data=datos))
modelo5 <- (lm(formula = bts ~ dfu+dco+dma+btf,data=datos))
summary(modeloa)$ r.squared#R2 múltiple del modelo de regresión global
#Coeficientes de determinación múltiple de regresiones auxiliares entre variables regresoras
summary(modelo1)$ r.squared
summary(modelo2)$ r.squared
summary(modelo3)$ r.squared
summary(modelo4)$ r.squared
summary(modelo5)$ r.squared

#chequea si las suposiciones de regresión lineal son verdaderas.
gvlma(modeloa)
#Gráficos de asociados a la situación anterior
par(mfrow=c(2,2)) # 4 charts in 1 panel
plot(modeloa)
#transformación Box-cox variable independiente
trans <- BoxCoxTrans(datos$ctf)
print(trans)
#Agregando la nueva variable transformada a la base de datos.
datos <- cbind(datos, ctftrans=predict(trans, datos$ctf))
datos
#Construye el nuevo modelo de regresión y prueba de heterocedasticidad
modelo2 <- lm(ctftrans~ dfu + dco + dma + btf + bts, data = datos)
gvlma(modelo2) # checking assumptions
plot(modelo2)
