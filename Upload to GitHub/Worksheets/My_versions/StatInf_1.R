# R worksheet #1

library(fitdistrplus)
#library(pastecs)
install.packages("pastecs")
####################################
# Question 1 : Match datasets and distributions #
####################################

# Read the data file
Data <- read.csv("R_assessment1_data.csv")

# The identification is based on visualization graphics like box-plot, qq-plots, histograms, empirical cdfs...

# Visualization of the results using fitdistrplus
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

# The binomial distribution requires to estimate both n and p. We have an analytical estimator for p given n.
# Of course, n must be higher than max(Data$x1) since this value is observed.
# One way to distinguish different couples (n, p) is of course, compute the associated likelihood (the higher, the better).

k <- sum(Data$x1)
m <- max(Data$x1)
Like <- rep(0, 10)
for (i in (5:15))
{
  size <- 100 * i
  p <- k / size
  Like[i-4] <- dbinom(k, prob = p, size = size)
}
plot(5:15, Like, main="Likelihood of (n,p)", xlab="n", ylab="L((n,p),X)")
# The best couple (n, p) seems to be (5, 0.504)

####################################
# Simple test for normal law #
####################################
library("readxl")
data <- read_excel("sample_gauss0_final.xlsx")

sample_0 <- data$x

N <- 100
mu0 <- 10
mu1 <- 15
std <- 10
x_avg <- mean(sample_0)
#Likelihood
lambda <- exp(-1/(2*std**2) *(N*(mu0**2-mu1**2) -2*(mu0-mu1)*N*x_avg))

k <- qnorm(0.95,mean=10,sd=1)  #\kappa normal(10,1)
k_lambda <- exp(N/std**2 *(mu0-mu1)*k - N/(2*std**2)*(mu0**2-mu1**2))
#Prints 'TRUE' if we accept H_0
print(lambda > k_lambda)

p_test_value <- std**2/(N*(mu0-mu1))*log(lambda) + (mu0+mu1)/2
#p_value
p_value <- 1-pnorm(p_test_value,mean=mu0,sd=std/sqrt(N))
#type 2 error
beta_test <- std**2/(N*(mu0-mu1))*log(k_lambda) + (mu0+mu1)/2
#Power (= 1-beta)
1-pnorm(beta_test,mean=mu1,sd=1)
