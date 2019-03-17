pain <- c(4, 5, 4, 3, 2, 4, 3, 4, 4, 6, 8, 4, 5, 4, 6, 5, 8, 6, 6, 7, 6, 6, 7, 5, 6, 5, 5)
drug <- c(rep("A", 9), rep("B", 9), rep("C", 9))
migraine <- data.frame(pain, drug)
AOV <- aov(pain ~ drug, data=migraine)
summary(AOV)
LM <- lm(pain ~ drug, data=migraine)
anova(LM)
summary(LM)

# We use the tidy function from the broom package to extract values

library(broom)
library(tibble)

tidy_aov <- tidy(AOV)
tidy_aov

##        term df    sumsq    meansq statistic      p.value
## 1      drug  2 28.22222 14.111111  11.90625 0.0002558807
## 2 Residuals 24 28.44444  1.185185        NA           NA

# The values we need are in the sumsq column of this data frame

sum_squares_regression <- tidy_aov$sumsq[1]
sum_squares_residuals <- tidy_aov$sumsq[2]

R_squared <- sum_squares_regression /
  (sum_squares_regression + sum_squares_residuals)

R_squared

## 0.4980392



#otros ejemplos:
# para data framme utilizando tibble
as_tibble(migraine)
#para resultados de análisis utilizando broom
tidy(AOV)
augment(AOV)
glance(AOV)



# a parte del broon, estos son otros paquetes para extraer y ordendar información de objetos de R existentes

install.packages("tidyverse")
install.packages("tibble")
install.packages("devtools")
devtools::install_github("tidyverse/tibble")


library(broom)
library(tidyverse)
library(tibble)
library(devtools)
library(dplyr)# para integrar información y hacer análisis conjunto.
devtools::install_github("tidyverse/tibble")
as_tibble(AOV)
tibble(AOV)

#http://varianceexplained.org/r/broom-intro/
#https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
