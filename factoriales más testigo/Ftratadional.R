library(ExpDes)

#bifactorial DCA con testigo adicional
data(ex8)
ex8
attach(ex8)
data(secaAd)

fat2.ad.crd(inoculante, biodiesel, vaso, seca, secaAd, quali = c(TRUE,FALSE), mcomp = "tukey", 
            fac.names = c("Inoculant", "Biodiesel"), sigT = 0.05, sigF = 0.05)
detach(ex8)

#Bifactorial BCA con tratamiento adicional
data(ex7)
ex7
attach(ex7)
data(est21Ad)
fat2.ad.rbd(periodo, nivel, bloco, est21, est21Ad, quali=c(TRUE, FALSE), mcomp = "tukey",
            fac.names = c("Period", "Level"), sigT = 0.05, sigF = 0.05)
detach(ex7)



#trifactorial BCA con tratamiento adicional
data(ex6)
attach(ex6)
data(respAd)
fat3.ad.rbd(fatorA, fatorB, fatorC, rep, resp, respAd, quali = c(TRUE, TRUE, TRUE),
            mcomp = "snk", fac.names = c("Factor A", "Factor B", "Factor C"), sigT = 0.05, sigF = 0.05)

detach(ex6)

getAnywhere(fat2.ad.crd)
getAnywhere(fat2.ad.rbd)
