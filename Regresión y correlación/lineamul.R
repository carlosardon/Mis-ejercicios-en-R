# El data set empleado es el state.x77
# Para facilitar su interpretación se renombra y se modifica
#instalar los siguientes programas
library(dplyr)#atípicos e influyentes
library(psych)
library(GGally)
library(ggplot2)
library(gridExtra)
library(lmtest)
library(corrplot)
library(car)
library(leaps)
library(ggplot2)
library(MASS)
library(sandwich)#correccion de heterocedasticidad de White
library(nortest)#pruebas de normalidad
library(normtest)#pruebas de normalidad
options (scipen = 999, digits = 5)
#cargar el archivo de datos
datos <- as.data.frame(state.x77)
datos <- rename(habitantes = Population, analfabetismo = Illiteracy,ingresos = Income, esp_vida = `Life Exp`, asesinatos = Murder,universitarios = `HS Grad`, heladas = Frost, area = Area,.data = datos)
datos <- mutate(.data = datos, densidad_pobl = habitantes * 1000 / area)

#analizar la relación entre las variables
round(cor(x = datos, method = "pearson"), 3)
multi.hist(x = datos, dcol = c("blue", "red"), dlty = c("dotted", "solid"), main = "")
ggpairs(datos, lower = list(continuous = "smooth"),diag = list(continuous = "bar"), axisLabels = "none")

#Generar el modelo
modelo <- lm(esp_vida ~ habitantes + ingresos + analfabetismo + asesinatos + universitarios + heladas + area + densidad_pobl, data = datos )
summary(modelo)
avPlots(modelo, id.n = 2, id.cex = 0.7)#Added-Variable Plots en reg.múltiple


#Primera forma de seleccionar el mejor modelo
regfit.fwd<-regsubsets(esp_vida~.,data=datos,method="forward")#forward, backward, seqrep,exhaustive  
plot(regfit.fwd,scale="bic")#fila es el modelo y columna las variables
summary(regfit.fwd)
coef(regfit.fwd,8)#estimaciones MCO de los parámetros del modelo 8

regfit.bwd<-regsubsets(esp_vida~.,data=datos,method="backward") 
plot(regfit.bwd,scale="r2")
summary(regfit.bwd)
coef(regfit.bwd,8)#estimaciones MCO de los parámetros del modelo 8

regfit.sqp<-regsubsets(esp_vida~.,data=datos,method="seqrep") 
plot(regfit.sqp,scale="Cp")
summary(regfit.sqp)
coef(regfit.sqp,8)#estimaciones MCO de los parámetros del modelo 8

regfit.ete<-regsubsets(esp_vida~.,data=datos,method="exhaustive", really.big=F, nbest=3, nvmax=8) 
plot(regfit.ete,scale="adjr2")
summary(regfit.ete)
coef(regfit.ete,8)#estimaciones MCO de los parámetros del modelo 8
coef(regfit.ete, 1:3)
vcov(regfit.ete, 3)

regfit.ete<-regsubsets(esp_vida~.,data=datos,method=c("exhaustive","backward","forward","seqrep"), really.big=F, nbest=3, nvmax=8) 
plot(regfit.ete,scale="r2")#fila es el modelo y columna las variables
summary(regfit.ete, matrix.logical=TRUE)
coef(regfit.ete,8)#estimaciones MCO de los parámetros del modelo 8
coef(regfit.ete, 1:3)
vcov(regfit.ete, 3)
matrix.logical=TRUE
#force.in=2 indica que la 2da regresora se incluirá en todos los modelos


#Selección de mejores predictores
step(object = modelo, direction = "both", trace = 1)#Puede ser "backward","forward" o ambos "both"
stepAIC(object = modelo, direction = "both", trace = 1)#Puede ser "backward","forward" o ambos "both"
#mejor modelo resultante del proceso de selección es:
modelo <- (lm(formula = esp_vida ~ habitantes + asesinatos + universitarios + heladas, data = datos))
summary(modelo)
str(summary(modelo))#estructura del objeto que contiene el resumen del modelo
summary(modelo)$ r.squared

#intervalos de confianza
confint(modelo)

#validación de condiciones para el modelo de regresión múltiple
rest<-rstandard(modelo)
plot1 <- ggplot(data = datos, aes(habitantes, modelo$residuals)) + geom_point() + geom_smooth(color = "firebrick",method = loess,formula=y~x) + geom_hline(yintercept = 0) + theme_bw()
plot2 <- ggplot(data = datos, aes(asesinatos, modelo$residuals)) + geom_point() + geom_smooth(color = "firebrick",method = loess,formula=y~x) + geom_hline(yintercept = 0) + theme_bw()
plot3 <- ggplot(data = datos, aes(universitarios, modelo$residuals)) + geom_point() + geom_smooth(color = "firebrick",method = loess,formula=y~x) + geom_hline(yintercept = 0) + theme_bw()
plot4 <- ggplot(data = datos, aes(heladas, modelo$residuals)) + geom_point() + geom_smooth(color = "firebrick",method = loess,formula=y~x) + geom_hline(yintercept = 0) + theme_bw()
grid.arrange(plot1, plot2, plot3, plot4)
qqPlot(modelo, id.n = 3)#usando t
qqPlot(rest)#usando z
ggplot(data = datos, aes(modelo$fitted.values, rest)) + geom_point() + geom_smooth(color = "firebrick",method = "loess" , formula = y ~ x, se = FALSE) + geom_hline(yintercept = 0) + theme_bw()

#Valores atípicos
outlierTest(modelo)
boxplot(modelo$residuals, col = rgb(0,0,1,0.5), main = "Boxplot of y[,2]", varwidth=FALSE)
ggplot(data = datos, aes(modelo$fitted.values, modelo$residuals)) + geom_boxplot() + # dibujamos el diagrama de cajas
stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="red")
leveragePlots(modelo)

#pruebas de normalidad
shapiro.test(modelo$residuals)#Royston (1995)
ad.test(modelo$residuals)#Anderson-Darling
lillie.test(modelo$residuals)#Kolmogorov-Smirnov con modificación de Lillefors
jb.norm.test(modelo$residuals)# Jarque-Bera 1987
ajb.norm.test(modelo$residuals)# Jarque-Bera 1996

#homocedasticidad
bptest(modelo)#Breusch-Pagan test
gqtest(modelo)#Goldfeld-Quandt test
ncvTest(modelo)#Non-Constant Error Variance

# Prueba de White
resids <- modelo$residuals
resids2 <- resids^2
predicts <- modelo$fitted.values
predicts2 <- predicts^2
lmhet <- lm(resids2 ~ predicts + predicts2)
summary(lmhet)

# Cuando no se encuentra evidencia estadística
#de que la varianza de los residuos cambia con los predichos o
#el cuadrado de los predichos, se dice que no hay evidencia de heterocedasticidad.

#Corrección de heterocedasticidad de White
vv <- vcovHC(modelo, type="HC1")
vv
coeftest(modelo, vcov = vv)
summary(modelo)#Comparar este con el anterior
plot(y=rest^2, x=predicts)
predicts <- modelo$fitted.values
predicts

vcov(modelo)
res<-modelo$residuals
rest<-rstandard(modelo)
hist(rest)
boxplot(rest)
qqnorm(rest)


#no colinealidad
corrplot(cor(dplyr::select(datos, habitantes, asesinatos,universitarios,heladas)), method = "number", tl.col = "black")
vif(modelo)# si es menor o igual que 10 no hay problema de multicolinealidad
#Regla de Klein, si ninguna de la regresiones auxiliares presenta R2 múltiple mayor que el global, no hay problema de multicolinealidad. 
modelo1 <- (lm(formula = habitantes ~ asesinatos + universitarios + heladas, data = datos))
modelo2 <- (lm(formula = asesinatos ~ habitantes + universitarios + heladas, data = datos))
modelo3 <- (lm(formula = universitarios ~ asesinatos + habitantes + heladas, data = datos))               
modelo4 <- (lm(formula = heladas ~ asesinatos + universitarios + habitantes, data = datos))
summary(modelo)$ r.squared#R2 múltiple del modelo de regresión global
#Coeficientes de determinación múltiple de regresiones auxiliares entre variables regresoras
summary(modelo1)$ r.squared
summary(modelo2)$ r.squared
summary(modelo3)$ r.squared
summary(modelo4)$ r.squared


dwt(modelo, alternative = "two.sided")#autocorrelación
#identificación de posibles valores atípicos o influyentes
library(dplyr)
datos$studentized_residual <- rstudent(modelo)
ggplot(data = datos, aes(x = predict(modelo), y = abs(studentized_residual))) +
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed") +
  # se identifican en rojo observaciones con residuos estandarizados absolutos > 3
  geom_point(aes(color = ifelse(abs(studentized_residual) > 3, 'red', 'black'))) +
  scale_color_identity() +
  labs(title = "Distribución de los residuos studentized",
       x = "predicción modelo") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
which(abs(datos$studentized_residual) > 3)
summary(influence.measures(modelo))
influencePlot(modelo)




#https://ggplot2.tidyverse.org/reference/geom_boxplot.html
#http://rstatistics.net/how-to-test-a-regression-model-for-heteroscedasticity-and-if-present-how-to-correct-it/
# initialize the packages)
library(e1071)
library(caret) # for box-cox transformation
library(lmtest) # bptest for testing heteroscedasticity
library(gvlma) # global validation of linear model assumptions
#Construye el modelo inicial y prueba de heterocedasticidad.
lmMod <- lm(ctf~ dfu + dco + dma + btf + bts, data = datos) # initial model
bptest(lmMod)  # Breusch-Pagan test
#Un valor de p> 0.05 indica que la hipótesis nula (la varianza es invariable en el residual) se puede rechazar y, por lo tanto, existe heterestesia. Esto se puede confirmar ejecutando una validación global de los supuestos del modelo lineal (gvlma) en el objeto lm .
gvlma(lmMod) # validate if assumptions of linear regression holds true.
#Interpretación grafica
#Como se sospechaba, la condición de heteroscedasticidad NO se cumple. Ahora vamos a trazar y ver cómo se ve también.
par(mfrow=c(2,2)) # 4 charts in 1 panel
plot(lmMod)
#Usando la transformación caja-cox
distBCMod <- BoxCoxTrans(datos$ctf)
print(distBCMod)
#Ahora agreguemos esta nueva variable transformada a los datos y reconstruyamos el modelo.
datos <- cbind(datos, ctfn=predict(distBCMod, datos$ctf)) # append the transformed variable to cars
datos # view the top 6 rows
#Construye el nuevo modelo de regresión y prueba de heterocedasticidad
lmMod_bc <- lm(ctfn~ dfu + dco + dma + btf + bts, data = datos)
bptest(lmMod_bc)
gvlma(lmMod_bc) # checking assumptions
plot(lmMod_bc)


