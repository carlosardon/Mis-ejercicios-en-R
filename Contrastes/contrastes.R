#http://www.southampton.ac.uk/~cpd/anovas/datasets/Orthogonal%20contrasts.htm
#1.1 Modelo de un factor Y = A + ??
aovdata <- read.table("http://www.southampton.ac.uk/~cpd/anovas/datasets/R/Model1_1.txt", header = T) 
attach(aovdata)
A <- factor(A)
model1_1i <- aov(Y ~ A)         # Create an object (here called 'model1_1i') that gets an analysis of variance with the model: numeric vector Y is explained by factor A
summary(model1_1i)              # report the results of the analysis
anova(glm(Y ~ A, family = gaussian(link = identity)), test = "F") # deviances equal SS
Commands for plotting Y at each level of factor A
plot(A, Y, las = 1, xlab = "A", ylab = "Y")# box and whisker plot of the response Y to factor A
round(tapply(Y, A, mean),2)     # report the mean of Y at each level of factor A, rounded to 2 decimal places
A <- as.numeric(A)
model1_1i <- aov(Y ~ A)         # Create an object (here called 'model1_1i') that gets an analysis of variance with the model: numeric vector Y is explained by numeric vector A
summary(model1_1i)              # report the results of the regression
A <- as.numeric(A)
model1_1i <- lm(Y ~ A)
summary(model1_1i)
Or by glm with the error distribution belonging to a named family
A <- as.numeric(A)
anova(glm(Y ~ A, family = gaussian(link = identity)), test = "F")
plot(A, Y, las = 1, xlab = "A", ylab = "Y")   # scatter plot of the response Y to numeric covariate A
abline(coef(model1_1i))         # add regression line to scatter plot
coef(model1_1i)                 # report the intercept and slope of the linear regression

#1.1 modelo Y = A + ?? con contrastes en 3 niveles A
aovcont <- read.table("http://www.southampton.ac.uk/~cpd/anovas/datasets/R/Model1_1contrasts3.txt", header = T)
attach(aovcont)
aovcont
A <- factor(A) ; B <- factor(B) ; C <- factor(C)
model1_1c <- aov(Y ~ B + C)
summary(model1_1c)
contrasts(A) <- cbind(c(2,-1,-1), c(0,1,-1))
model1_1i <- aov(Y ~ A)
summary.lm(model1_1i)
options(contrasts = c("contr.helmert", "contr.helmert"))
model1_1i <- lm(Y ~ A)
summary(model1_1i)
anova(glm(Y ~ B + C, family = gaussian(link = identity)), test = "F") 
contrasts(A) <- cbind(c(2,-1,-1), c(0,1,-1))
model1_1i <- glm(Y ~ A, family = gaussian(link = identity), contrasts = A)
summary(model1_1i) 

library(lattice)
xyplot(Y ~ A|B,
       panel = function(x, y)
       {
         panel.xyplot(x, y)             # plot Y against A at each level of B
         panel.xyplot(x, y, type = "a") # join the means of consecutive levels of A at each level of B
       })
Yn <- as.numeric(Y) ; A <- as.factor(A) # convert vector Y to numeric and vector A to a factor
Y1 <- log(Y)                    # create a vector Y1 that is the natural log of the vector Y
Y2 <- log10(Y)                  # create a vector Y2 that is the log to base 10 of the vector Y
(Y3 <- asin(sqrt(Yn)))            # create a vector Y3 that is the arcsin-root transformation of the vector Y