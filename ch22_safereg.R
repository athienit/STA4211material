dat=read.csv("https://raw.githubusercontent.com/athienit/STA4210material/main/safe_reg.csv",header=TRUE)
library(car)

dat$cx=dat$x1-mean(dat$x1)
colnames(dat)[2]=c("z")

scatterplot(y~cx|z,xlab="centered x",smooth=FALSE,regLine=FALSE,col=c("black","red"),data=dat)
reg1=lm(y~cx+z,data=dat)
summary(reg1)
coef1=coef(reg1)
curve(coef1[1]+coef1[3]+coef1[2]*x,from=-4225,to=3740,add=T,col="red",lwd=2)
curve(coef1[1]+coef1[2]*x,from=-4225,to=3740,add=T,lwd=2)


reg2=lm(y~cx*z,data=dat) #same as lm(y~cx+z+cx:z)
summary(reg2)
scatterplot(y~cx|z,xlab="centered x",smooth=FALSE,regLine=TRUE,col=c("black","red"),data=dat)

# To create a CI around gamma_2+gamma_4
vc=vcov(reg2);round(vc,5)
sum(reg2$coefficients[c(2,4)])+c(1,-1)*qt(0.025,reg2$df.residual)*sqrt(vc[2,2]+vc[4,4]+2*vc[2,4])

cor.trans=function(y){
 n=length(y)
 1/sqrt(n-1)*(y-mean(y))/sd(y)
}
