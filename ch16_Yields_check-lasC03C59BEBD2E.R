#Yields of Amoebae

yields=read.table("https://raw.githubusercontent.com/athienit/STA4211material/main/entozamoeba.txt",header=FALSE)
colnames(yields)=c("Condition","Yields")
yields$Condition=factor(yields$Condition)

means=tapply(yields$Yields,yields$Condition,mean) # obtain means by trt
means
tapply(yields$Yields,yields$Condition,sd) # obtain st.dev. by trt

#windows(6.5,5.5)
# Dot plot 
stripchart(Yields~Condition, data=yields, method="stack", vertical=TRUE,
           pch=1, cex=1.5, xlab="Factor", ylab="Yields", main="Dotplots by Condition")
title(sub="pre-analysis plot", adj=0, cex=5/6)
#mtext("Example")
points(1:5,tapply(yields$Yields,yields$Condition,mean),col=2,pch=8)
legend(2,290, c("Observations", " Trt Mean"), col = c(1,2), text.col= "black",
       lty=c(0,0),pch=c(1,8),bg='gray90')

library(ggdist)
library(ggplot2)
library(gghalves)
ggplot(yields, aes(x = Condition, y = Yields,fill=Condition)) + 
  ggdist::stat_halfeye(
    adjust = .9, #custom bandwidth
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
  coord_cartesian(xlim = c(1.2, NA), clip = "off")

# + coord_flip()

yaov1=aov(Yields~Condition,data=yields)
re=rstudent(yaov1)

# Time Series Plot
plot(re,type="o",pch=22,xlab="Order",ylab="studentized res.",main="Time Series")
abline(h=0)

### DIAGNOSTICS
plot(yaov1)
source("https://raw.githubusercontent.com/athienit/STA4210material/main/check.R")
check(yaov1,tests=TRUE)
# Why is Durban Watson not valid?
ncvTest(lm(Yields~Condition,data=yields)) #Breush-Pagan test of non constant variance

### Checking outliers
sdr=rstudent(yaov1)
sdr[which(abs(sdr)>=abs(qt(0.05/length(sdr),yaov1$df.residual)))]

### Observations with x-values with the potential to "pull" the regression line
hat=hatvalues(yaov1)
hat[which(hat>2*5/length(sdr))]

### DFFITS
dftrt=length(levels(yields$Condition))-1
dffits(yaov1)[which(dffits(yaov1)>2*sqrt(dftrt/length(sdr)))]

### Cooks D
cd=cooks.distance(yaov1)
cd[which(cd>1)] # criterion >1
cd[which(cd>qf(0.5,dftrt,df.residual(yaov1)))] # more conservative
