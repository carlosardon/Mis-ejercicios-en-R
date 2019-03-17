library(phia)

# Tests of the interactions described in Boik (1979)
# See ?Boik for a description of the data set

mod.boik <- lm(edr ~ therapy * medication, data=Boik)
Boik
Anova(mod.boik)
cntrl.vs.T1 <- list(therapy = c(1, -1, 0))
cntrl.vs.T2 <- list(therapy = c(1, 0, -1))
plcb.vs.doses <- list(medication = c(1, -1/3, -1/3, -1/3))
testInteractions(mod.boik, pairwise="therapy", adjustment="none")
testInteractions(mod.boik, custom=plcb.vs.doses, adjustment="none")
testInteractions(mod.boik, custom=cntrl.vs.T1, across="medication", adjustment="none")
testInteractions(mod.boik, custom=c(cntrl.vs.T1, plcb.vs.doses), adjustment="none")
testInteractions(mod.boik, custom=cntrl.vs.T2, across="medication", adjustment="none")
testInteractions(mod.boik, custom=plcb.vs.doses, across="therapy", adjustment="none")

library(phia)

# Tests of the interactions described in Boik (1979)
# See ?Boik for a description of the data set

mod.boik <- lm(edr ~ therapy * medication, data=Boik)
Anova(mod.boik)
cntrl.vs.T1 <- list(therapy = c(1, -1, 0))
cntrl.vs.T2 <- list(therapy = c(1, 0, -1))
plcb.vs.doses <- list(medication = c(1, -1/3, -1/3, -1/3))
testInteractions(mod.boik)

#ESTE ES UN BUEN ANÁLISIS
library(phia)
library(emmeans)
library (lsmeans)
library(car)#Anova
library(stats)#anova
library(agricolae)#aov
ANA<-lm(edr~therapy*medication, data=Boik)
print (Anova(ANA), digits=7)#usando Anova de library car
ANA1<-Anova(ANA)#Genera un tabla
print(anova(ANA),digits=7)#usando anova de library stats
ANA2<-(anova(ANA))#Genera un tabla
ana<-aov(edr~therapy*medication, data=Boik)
summary(ana, digits=7)#Resultados del ANOVA
(boik.means <- interactionMeans(mod.boik))
plot(boik.means, errorbar="ci0")
with(Boik, interaction.plot(medication,therapy , edr, type="b"))# con lsmeams
with(Boik, interaction.plot(therapy,medication, edr, type="b"))# con lsmeams
emmip(mod.boik, therapy~medication)# con emeams
emmip(mod.boik, medication~therapy)# con emeams
#Estos son los buenos
testInteractions(mod.boik, fixed="therapy", across="medication", digits=5)# terapia a traves de medicación
testInteractions(mod.boik, fixed="medication", across="therapy", digits=5)# terapia a traves de medicación
testInteractions(mod.boik, pairwise="therapy", across="medication")
lsmeans(mod.boik, pairwise~therapy|medication)# análogo al anterior (con lsmeans)
testInteractions(mod.boik, pairwise="medication", across="therapy", digits=5)# terapia a traves de medicación
lsmeans(mod.boik, pairwise~medication|therapy)#análogo al anterior(con lsmeans)
testInteractions(mod.boik)
lsmeans(mod.boik, pairwise~"therapy")
#Final de buenos
(custom.contr <- contrastCoefficients(
therapy ~ control - (T1 + T2)/2, # Control vs. ambas terapias
therapy ~ T1 - T2, # terapia T1 vs. T2
medication ~ placebo - (D1 + D2 + D3)/3, # Placebo vs. todas las dosis
medication ~ D1 - D3, # dosis mínima vs. dosis máxima
medication ~ D2 - (D1 + D2 + D3)/3, # dosis media vs. el promedio de todas las dosis
data=Boik, normalize=TRUE)) # Normalize to homogeinize the scale
names(custom.contr$therapy) <- c("cntrl.vs.all", "T1.vs.T2")
names(custom.contr$medication) <- c("plcb.vs.all", "D1.vs.D3", "D2.vs.avg")
testInteractions(mod.boik,custom=custom.contr)







library(emmeans)
noise.lm <- lm(noise ~ size * type * side, data = auto.noise)
anova(noise.lm)
aov(noise.lm)
emmeans(noise.lm, pairwise ~ size)
emmip(noise.lm, type ~ size | side)
emm_s.t <- emmeans(noise.lm, pairwise ~ size | type)
## NOTE: Results may be misleading due to involvement in interactions
emm_s.t
noise.emm <- emmeans(noise.lm, ~ size * side * type)
contrast(noise.emm, "consec", simple = "each", combine = TRUE, adjust = "mvt")
contrast(emm_s.t[[1]], "poly")   ## 'by = "type"' already in previous result 
IC_st <- contrast(emm_s.t[[1]], interaction = c("poly", "consec"))
IC_st
coef(IC_st)
test(IC_st, joint = TRUE)
joint_tests(noise.lm)
joint_tests(noise.lm, by = "side")








library (lsmeans)
model1 <- lm(breaks ~ tension*wool, data = warpbreaks)
# get the source table
car::Anova(model1, type=3)
# means of tension collapsing across wool
means.tension <- lsmeans(model1, specs = "tension")
means.tension
# means of wool collapsing across tension
means.wool  <- lsmeans(model1, specs = "wool")
means.wool
# cell means
# contrasts on the main effect of tension
# compare Low to Medium, and the combination of Low and Medium to High
levels(warpbreaks$tension)
contrast(means.tension, list(LvM = c(-1,1,0), LMvH=c(.5,.5, -1)))
# contrast on the main effect of wool (not actually necessary since there's only 2 levels)
contrast(means.wool, list(AvB = c(-1,1)))
# contrasts on simple effect of tension at wool=A
contrast(means.int, list(LvMforA = c(1,-1,0,0,0,0), LMvHforA=c(.5,.5,-1,0,0,0)))
# contrasts on simple effect of tension at wool=B
contrast(means.int, list(LvMforB = c(0,0,0,1,-1,0), LMvHforB=c(0,0,0,.5,.5,-1)))
# you can apply contrasts for simple effects quickly by using the "by" argument
contrast(means.int, list(LvM = c(1,-1,0), LMvH=c(.5,.5,-1)), by="wool")
# use built-in polynomial contrasts on tension (linear and quadratic)
contrast(means.tension, method = "poly")
# wanna see the contrast weights it uses?
Poly.lsmc(1:3)
poly.lsmc(1:4) 
poly.lsmc(1:5)
### Setting up a custom contrast function (Helmert contrasts, in this case)
helmert.lsmc <- function(levs, ...) {
  M <- as.data.frame(contr.helmert(levs))
  names(M) <- paste(levs[-1],"vs earlier")
  attr(M, "desc") <- "Helmert contrasts"
  M
}
contrast(means.tension, method = "helmert")
# you can apply contrasts for simple effects quickly by using the "by" argument
contrast(means.int, method = "poly", by="wool")
# Here are the weights used for polynomial contrasts in base R:
contr.poly(3)
# And here are the weights for the same contrats in lsmeans:
poly.lsmc(1:3)
### Setting up a custom contrast function (the stats polynomial contrasts, in this case)
polyalt.lsmc <- function(levs, ...) {
  M <- as.data.frame(contr.poly(levs))
  M
}
contrast(means.tension, method = "polyalt") # the stats weights
contrast(means.tension, method = "poly") # the lsmeans weights
means.tension
# linear contrast: H vs. L
21.66667 - 36.38889
# quadratic contrast: The mean of H and L vs. M
mean(c(21.66667, 36.38889)) - 26.38889
# post-hoc comparisons of all means (using Tukey's HSD)
pairs(means.int, adjust = "tukey")
# bonferroni adjustment
pairs(means.int, adjust = "bon")


#--- Three-factor example
noise.lm = lm(noise ~ size * type * side, data = auto.noise)
# Separate interaction plots of size by type, for each side
emmip(noise.lm, type ~ size | side)
# One interaction plot, using combinations of size and side as the x factor
# ... with added confidence intervals
emmip(noise.lm, type ~ side * size, CIs = TRUE)
# One interaction plot using combinations of type and side as the trace factor
emmip(noise.lm, type * side ~ size)
# Individual traces in panels
emmip(noise.lm, ~ size | type * side)






