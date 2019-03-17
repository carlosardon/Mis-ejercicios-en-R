# NOT RUN {
# Some of the examples may run some time.

# gait data (the first feature)
library(fda)
library(fdANOVA)
gait.data.frame <- as.data.frame(gait)
x.gait <- as.matrix(gait.data.frame[, 1:39])

# vector of group labels
group.label.gait <- rep(1:3, each = 13)
# }
# NOT RUN {
# all FANOVA tests with default parameters
set.seed(123)
(fanova1 <- fanova.tests(x = x.gait, group.label = group.label.gait))
summary(fanova1)
# data projections generated in the test based on random projections
fanova1$TRP$data.projections

# only three tests with non-default parameters
set.seed(123)
fanova2 <- fanova.tests(x.gait, group.label.gait,
                        test = c("FP", "GPF", "Fmaxb"),
                        params = list(paramFP = list(int = c(0.025, 0.975),
                                                     B.FP = 1000, basis = "b-spline",
                                                     criterion = "eBIC",
                                                     commonK = "mean",
                                                     minK = 5, maxK = 20,
                                                     norder = 4, gamma.eBIC = 0.7),
                                      paramFmaxb = 1000))
summary(fanova2)

# the FP test with predefined basis function representation
library(fda)
fbasis <- create.bspline.basis(rangeval = c(0.025, 0.975), 19, norder = 4)
own.basis <- Data2fd(seq(0.025, 0.975, length = 20), x.gait, fbasis)$coefs
own.cross.prod.mat <- inprod(fbasis, fbasis)
set.seed(123)
fanova3 <- fanova.tests(group.label = group.label.gait, test = "FP",
                        params = list(paramFP = list(B.FP = 1000, basis = "own",
                                                     own.basis = own.basis,
                                                     own.cross.prod.mat = own.cross.prod.mat)))
summary(fanova3)

# the tests based on random projections with the Gaussian white noise generated for projections
set.seed(123)
fanova4 <- fanova.tests(x.gait, group.label.gait, test = "TRP",
                        parallel = TRUE, nslaves = 2,
                        params = list(paramTRP = list(k = c(10, 20, 30), B.TRP = 1000)))
summary(fanova4)
set.seed(123)
fanova5 <- fanova.tests(x.gait, group.label.gait, test = "TRP",
                        parallel = TRUE, nslaves = 2,
                        params = list(paramTRP = list(k = c(10, 20, 30),
                                                      permutation = TRUE, B.TRP = 1000)))
summary(fanova5)

# the tests based on random projections with the Brownian motion generated for projections
set.seed(123)
fanova6 <- fanova.tests(x.gait, group.label.gait, test = "TRP",
                        parallel = TRUE, nslaves = 2,
                        params = list(paramTRP = list(k = c(10, 20, 30), projection = "BM",
                                                      B.TRP = 1000)))
summary(fanova6)
set.seed(123)
fanova7 <- fanova.tests(x.gait, group.label.gait, test = "TRP",
                        parallel = TRUE, nslaves = 2,
                        params = list(paramTRP = list(k = c(10, 20, 30), projection = "BM",
                                                      permutation = TRUE, B.TRP = 1000)))
summary(fanova7)
# }
