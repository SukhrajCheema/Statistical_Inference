library(fitdistrplus)
install.packages("pastecs")

Data <- read_csv("~/Coding/R/R Assessments/Statistical Inference/Assessment 1/R_assessment1_data.csv")

e2 <- fitdist(Data$x2, distr = "lnorm")
plot(e2)

e3 <- fitdist(Data$x3, distr = "beta")
plot(e3)

e4 <- fitdist(Data$x4, distr = "unif")
plot(e4)

e5 <- fitdist(Data$x5, distr = "exp")
plot(e5)

e6 <- fitdist(Data$x6, distr = "pois")
plot(e6)