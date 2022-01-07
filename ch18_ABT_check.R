ABT=read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2018%20Data%20Sets/CH18TA04.txt",header=FALSE)
ABT=ABT[,1:3]
colnames(ABT)=c("Group","Rep","Y")
ABT$Group=factor(ABT$Group)

stripchart(Y~Group, data=ABT, method="stack", vertical=TRUE,
           pch=1, cex=1.5, xlab="Factor", ylab="Y", main="Dotplots by Condition")
title(sub="pre-analysis plot", adj=0, cex=5/6)
#mtext("Example")
points(1:5,tapply(ABT$Y,ABT$Group,mean),col=2,pch=8)
legend(4,19, c("Observations", " Trt Mean"), col = c(1,2), text.col= "black",
       lty=c(0,0),pch=c(1,8),bg='gray90')

mod=aov(Y~Group,data=ABT)

### Check assumptions, everything okay-ish except constant variance
library(car)
library(lawstat)

#mod=mod2
re=rstudent(mod) #standardized residuals
ylimits=c(-3,3)
ylimits[1]=ifelse(min(re)<(-3),min(re),-3)
ylimits[2]=ifelse(max(re)>(3),max(re),3)

par(mfrow=c(2,2))

# Homogeneity of variance
plot(re~fitted.values(mod),xlab=expression(hat(y)),ylab="StD res.",main="Homogeneity of var.",ylim=ylimits)
abline(h=0)

leveneTest(mod)
ncvTest(lm(Y~Group,data=ABT))

# Independence
plot(re,type="o",pch=22,xlab="Order",ylab="StD res.",main="Independence",ylim=ylimits)
abline(h=0)

runs.test(re)
durbinWatsonTest(mod)

# Normality
hist(re,main="Studentized Deleted residuals",xlab="StD res.")
qqnorm(re,datax=TRUE)
qqline(re,datax=TRUE)

shapiro.test(re)

#Outliers via Cook's Distance
cd=cooks.distance(mod)
cd[cd>4/35] #guideline D>4/dfe.  Note they are all in group 3

### WLS
#tapply(ABT$Y,ABT$Group,var)
library(plyr)
library(MASS)
data.sum=ddply(ABT,~Group,summarise,mean=mean(Y),var=var(Y),weight=1/var,size=length(Y));data.sum
wts=rep(data.sum$weight,times=data.sum$size)
ABTwls=lm(Y~Group,data=ABT,weights=wts)
summary(ABTwls)

#Supplemental
ABTiwls=rlm(Y~Group,data=ABT,wt.method="inv.var",method="M") #Iterative WLS via ML
summary(ABTiwls)
anova(ABTiwls)

### Box-Cox
bc2=powerTransform(ABT$Y~ABT$Group)
summary(bc2)
ABT$YT=bcPower(ABT$Y,0)
mod2=aov(ABT$YT~Group,data=ABT)
# check assumptions again

### Now Box-Cox AND WLS???
ABTwls2=lm(YT~Group,data=ABT,weights=wts)
summary(ABTwls2)
anova(ABTwls2)

#Nonparametric method
kruskal.test(Y~Group,data=ABT)

source("http://www.stat.ufl.edu/~athienit/rankTukey.R")

rank.Tukey(Y~Group,data=ABT)
