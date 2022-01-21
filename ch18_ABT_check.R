ABT=read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2018%20Data%20Sets/CH18TA04.txt",header=FALSE)
ABT=ABT[,1:3]
colnames(ABT)=c("Group","Rep","Y")
ABT$Group=factor(ABT$Group)

### OLD STRIPCHART

#stripchart(Y~Group, data=ABT, method="stack", vertical=TRUE,
#           pch=1, cex=1.5, xlab="Factor", ylab="Y", main="Dotplots by Condition")
#title(sub="pre-analysis plot", adj=0, cex=5/6)
##mtext("Example")
#points(1:5,tapply(ABT$Y,ABT$Group,mean),col=2,pch=8)
#legend(4,19, c("Observations", " Trt Mean"), col = c(1,2), text.col= "black",
#       lty=c(0,0),pch=c(1,8),bg='gray90')

library(ggdist)
library(ggplot2)
library(gghalves)

ggplot(ABT, aes(x = Group, y = Y,fill=Group)) + 
  ggdist::stat_halfeye(
    adjust = .5, #custom bandwidth
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  coord_flip()

########## MODEL FIT AND DIAGNOSTICS ##################

mod=aov(Y~Group,data=ABT)
source("https://raw.githubusercontent.com/athienit/STA4210material/main/check.R")
check(mod,tests=TRUE)
plot(mod)

### Checking outliers
sdr=rstudent(mod)
sdr[which(abs(sdr)>=abs(qt(0.05/length(sdr),mod$df.residual)))]

### Observations with x-values with the potential to "pull" the regression line
hat=hatvalues(mod)
hat[which(hat>2*5/length(sdr))]

### DFFITS
dftrt=length(levels(ABT$Group))-1
dffits(mod)[which(dffits(mod)>2*sqrt(dftrt/length(sdr)))]

### Cooks D
cd=cooks.distance(mod)
cd[which(cd>1)] # criterion >1
cd[which(cd>qf(0.5,dftrt,df.residual(mod)))] # more conservative

###### REMEDIAL ###########

### Perform Welch's ANOVA
oneway.test(Y ~ Group, data = ABT, var.equal = FALSE)
# If significant perform all pairwise t-tests using bonferroni adjustment

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

source("https://raw.githubusercontent.com/athienit/STA4211material/main/rankTukey.R")
rank.Tukey(Y~Group,data=ABT)
