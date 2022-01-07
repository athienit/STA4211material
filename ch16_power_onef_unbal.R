num.trt <- 4
num.rep <- c(15, 30, 45, 60)
tau <- c(-5,0,0,5)
sigma <- 16

power=function(num.trt,num.rep,tau,sigma,alpha=0.05){
if(length(num.rep)==1){num.rep=rep(num.rep,num.trt)}
if(length(num.rep)!=num.trt){stop("number of group sample sizes not the same as num.trt")}
df.trt <- num.trt-1
df.err <- sum(num.rep)-num.trt
nc.0 <- 0
nc.1 <- sum(num.rep*(tau-mean(tau))^2)/(sigma^2)
f.crit <- qf(1-alpha,df.trt,df.err)
power.1 <- 1-pf(f.crit,df.trt,df.err,nc.1)


zv=f.crit
cord.x <- c(zv,seq(zv,10,0.01),10) 
cord.y <- c(0,df(seq(zv,10,0.01),df.trt,df.err,nc.1),0) 
curve(df(x,df.trt,df.err),xlim=c(0,10),xaxp=c(0,0,1),yaxt="n",xlab='',ylab='',main="Central and Non-central F-distributions") 
curve(df(x,df.trt,df.err,nc.1),lty=2,xlim=c(0,10),xaxp=c(0,0,1),yaxt="n",xlab='',ylab='',main="Central and Non-central F-distributions",add=TRUE) 

axis(1,zv,col='red',labels=expression(paste(F[1-alpha/2])),cex.axis=1,font=3)
polygon(cord.x,cord.y,density=20,col='blue')
legend(3,0.5,legend=paste("power",round(power.1,3)),col=c("blue"),fill=c('blue'),density=20,cex=0.8)
legend(6,0.5,legend=c("F(0)",paste("F(",round(nc.1,2),")")),col=c("black"),lty=c(1,2),cex=0.8)

return(power.1)
}

power(4,num.rep,tau,sigma)
