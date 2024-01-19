num.trt <- 4
num.rep <- 60 #sample size per group

# factor effects model this is tau_i=mu_i-mu_. for i=1,2,3,4
# we will look at 3 different cases which are meant to illustrate power as we move away from the null
# like we do if we considered H0:mu=0 and alternatives mu1=2, mu2=2.5, mu3=3

# Using sum to zero constraint
tau.1 <- c(-2.5,0,0,2.5) #Example 1, group 1 is 2.5 units below grand mean and group 4 is 2.5 units above grand mean
tau.2 <- c(-5,0,0,5) #Example 2
tau.3 <- c(-10,0,0,10) #Example 3
sigma <- 16 #estimate of sigma

df.trt <- num.trt-1
df.err <- num.trt*(num.rep-1)
nc.0 <- 0
(nc.1 <- num.rep*sum((tau.1-mean(tau.1))^2)/(sigma^2))
(nc.2 <- num.rep*sum((tau.2-mean(tau.2))^2)/(sigma^2))
(nc.3 <- num.rep*sum((tau.3-mean(tau.3))^2)/(sigma^2))

y <- seq(0,15,.01)
fy.0 <- df(y,df.trt,df.err,nc.0)
fy.1 <- df(y,df.trt,df.err,nc.1)
fy.2 <- df(y,df.trt,df.err,nc.2)
fy.3 <- df(y,df.trt,df.err,nc.3)
(f.crit <- qf(.95,df.trt,df.err))

plot(y,fy.0,type="l",main="Central and Non-central F-distributions")
lines(y,fy.1,type="l",lty=2)
lines(y,fy.2,type="l",lty=3)
lines(y,fy.3,type="l",lty=4)
abline(v=f.crit)
legend(x=5,y=0.6,c("F(nc.0)","F(nc.1)","F(nc.2)","F(nc.3)"),lty=1:4)

(power.0 <- 1-pf(f.crit,df.trt,df.err,nc.0))
(power.1 <- 1-pf(f.crit,df.trt,df.err,nc.1))
(power.2 <- 1-pf(f.crit,df.trt,df.err,nc.2))
(power.3 <- 1-pf(f.crit,df.trt,df.err,nc.3))

# To find the sample size required for a specific power a lot of trial and error
# is required where you change num.rep until you get the power you want...OR

# There are some other functions
power.anova.test(groups = length(tau.1),
                 between.var = var(tau.1),
                 within.var = 16^2, power = .90)

# if you actually know group means
groupmeans <- c(120, 130, 140, 150) #Delta=30
power.anova.test(groups = length(groupmeans),
                 between.var = var(groupmeans),
                 within.var = 16^2, power = .90)
