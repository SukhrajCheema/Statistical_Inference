#Question 1, faulty fibers

x <- seq(0.0001,1.5,0.01) #evaluation of all the pdfs on this grid

#Posterio distribution parameters
shape <- 10  
rate <- 22
#density of the points on the grin
y <- dgamma(x, shape, rate, log = FALSE)

#posterior probability that lambda < 0.646 -> pgamma
z<- pgamma(0.646, shape, rate, log = FALSE)
# find the high probability density region at the \alpha=0.95 level

# k_a - this is the minimum probability to be achieved 
k_a<-seq(0.01,2.0,0.1)
x_l<-k_a
x_r<-k_a
integral_value <- k_a
#This is one way to achieve it, you can also use hdi funcion
for(i in 1:length(k_a)) {
  
  # find the point to the left to the mean st p(point)>=k_a
  for (j in 1:length(x)){
    if (y[j]>k_a[i]){
      x_l[i]<-x[j]
      break
    }
  }
  
  # find the point to the right of ther mean s.t. p(point_r)>=k_a 
  for (j in length(x):1){
    if (y[j]>k_a[i]){
      x_r[i]<-x[j]
      break 
    }
  }
  
  integral_value[i]<-(pgamma(x_r[i], shape, rate, log = FALSE)-pgamma(x_l[i], shape, rate, log = FALSE))
  
}
#Plots 
plot(x,y)
abline(v=x_l[7], col="blue")
abline(v=x_r[7], col="blue")
abline(h=k_a[7], col="red")

M_1<- choose(10, 3)*beta(4,8)
M_2<- choose(10, 3)*beta(3.5,7.5)/beta(1/2,1/2)
M_3<- choose(10, 3)*beta(103,107)/beta(100,100)
PM1<-(M_1)/(M_1+M_2+M_3);
PM2<-(M_2)/(M_1+M_2+M_3);
PM3<-(M_3)/(M_1+M_2+M_3);
x_beta<-seq(0.0001,1.0,0.01)
y_beta<- dbeta(x_beta, 99, 99, ncp = 0, log = FALSE)
plot(x_beta,y_beta)



#Problem 2
data <- scan("datapoints.csv")

x <- seq(from=0.01, to=20, by=0.1)


#First prior: Gamma(15,2)
shape <- 5
rate <- 0.5 

### --- for level 7, part 6 --- ###
#shape <- 12
#rate <- 0.1 


#First model: Poisson distribution

prior_one <- dgamma(x, shape = shape, rate = rate)
plot(x,prior_one,col=1,type="l", xlab='lambda',ylim=c(0,0.4),ylab='p_lambda', main='Gamma - Pois')
#First posterior: Gamma distribution (Conjugate prior)
post_one <- dgamma(x,shape = shape+sum(data), rate = rate + length(data))

lines(x,post_one,type="l", col=3)
legend("topright", legend=c("Prior", "Posterior"),
       col=c(2, 3), lty=c(1,1))

#Second model: Geometric distribution
p <- seq(from=0, to=1, by=0.001)
alpha <- 5
beta  <-  10
prior_two <- dbeta(p,shape1=alpha, shape2=beta)

#Second posterior: Beta distribution (Conjugate prior)
post_two <- dbeta(p,shape1=alpha+length(data), shape2=beta + sum(data))

plot(p,prior_two,col=1,type="l", xlab='lambda',ylim=c(0,9),ylab='p', main='Beta - Geom')
lines(p,post_two,type="l", col=3)
legend("topright", legend=c("Prior", "Posterior"),
       col=c(2, 3), lty=c(1,1))

#Map
map_one <- x[which.max(post_one)]
map_two <- p[which.max(post_two)]

#Cond_mean
cm_one <- sum(x*post_one*0.1)
cm_two <- sum(p*post_two*0.001)


#Bayes Factor
marg_one <- function(lambda){
  return(lambda**(shape+sum(data)-1)*exp(-lambda*(rate+length(data)))*rate**shape/gamma(shape) 
         /(prod(gamma(data))) )
}
marg_two <- function(p){
  return(p**(alpha+length(data)-1)*(1-p)**(beta+sum(data)-1)/((gamma(alpha)*gamma(beta)/gamma(alpha+beta))))
}
#I should integrate the first marginal up to + infinity, but stopping at 30 is fine
#(See also the plot)
max_L <- 30
#Using function integrate to get the marginals
numerator <- integrate(marg_one,0,max_L)
denominator <- integrate(marg_two,0,1)

#Bayes factor
B01 <- numerator$value/denominator$value







