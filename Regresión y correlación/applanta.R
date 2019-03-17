#instalar los siguientes programas
library(ggplot2)#gráficos
library(lmtest)#pruebas de heterocedasticidad
library(sandwich)#correccion de heterocedasticidad de White
library(car)##Non-Constant Error Variance
options (scipen = 999, digits = 5)

#Importar la base de datos
datos <- read.csv("applanta.csv")
datos#Se muestran los datos
attach(datos) #Para adjuntar las variables
names(datos) #Nombre de las columnas
str(datos) #Tipo de objeto(numerico, entero, caracter,etc)

#Generar el modelo
modelo <- lm(Ren ~ APP, data = datos )
summary(modelo)

#modelo de residuos:si presentan algún patrón
rest<-rstandard(modelo)
ggplot(data = datos, aes(modelo$fitted.values, modelo$residuals)) + geom_point() + geom_smooth(color = "firebrick",method = "loess" , formula = y ~ x, se = FALSE) + geom_hline(yintercept = 0) + theme_bw()
ggplot(data = datos, aes(modelo$fitted.values, rest)) + geom_point() + geom_smooth(color = "firebrick",method = "loess" , formula = y ~ x, se = FALSE) + geom_hline(yintercept = 0) + theme_bw()
#homocedasticidad
bptest(modelo)#Breusch-Pagan test
ncvTest(modelo)#Non-Constant Error Variance
gqtest(modelo)#Goldfeld-Quandt test
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






