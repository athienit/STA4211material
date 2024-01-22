rust <- data.frame(y=c(43.9, 39.0, 46.7, 43.8, 44.2, 47.7, 43.6, 38.9, 43.6, 40.0,
                       89.8, 87.1, 92.7, 90.6, 87.7, 92.4, 86.1, 88.1, 90.8, 89.1,
                       68.4, 69.3, 68.5, 66.4, 70.0, 68.1, 70.6, 65.2, 63.8, 69.2,
                       36.2, 45.2, 40.7, 40.5, 39.3, 40.3, 43.2, 38.7, 40.9, 39.7),
                   brand=rep(LETTERS[1:4],c(10,10,10,10)), replication=rep(1:10,4))

head(rust)
# Dot plot 
stripchart(y~brand, data=rust, method="stack", vertical=TRUE,
           pch=1, cex=1.5, xlab="Brand", ylab="rust quantity", main="Dotplots by Treatments")
title(sub="pre-analysis plot", adj=0, cex=5/6)
mtext("Rust inhibitor")
points(c(1,2,3,4),tapply(rust$y,rust$brand,mean),col=2,pch=8)
abline(h=mean(rust$y),col=3)
legend(3.5,80, c("Observations", " Trt Mean","Grand Mean"), col = c(1,2,3), text.col= "black",       lty=c(0,0,1),pch=c(1,8,NA),bg='gray90')

# ANOVA table
rust.m=aov(y~brand,data=rust)
anova(rust.m)

### Obsolete --- Code developed to do all pairwise CI
source("https://raw.githubusercontent.com/athienit/STA4211material/main/allpairCI.R")
allpairCI(y~brand,data=rust,level=0.95,method="Bonf")
allpairCI(y~brand,data=rust,level=0.95,method="Scheffe")
Tu=allpairCI(y~brand,data=rust,level=0.95,method="Tukey");Tu
plot(Tu,sub="Tukey Honest Significant Differences",las=1)

### All pairwise CI's

library(DescTools)
PostHocTest(rust.m,method="bonferroni")
PostHocTest(rust.m,method="scheffe")
PostHocTest(rust.m,method="hsd") # Tukey's

# Equivalent regression model
rust.lm=lm(y~brand, data=rust)
anova(rust.lm)
summary(rust.lm)

# Convert 'brand' to a factor
rust$brand <- factor(rust$brand)
# Create the 'rust_base_c' data frame with re-leveled 'brand'
rust_base_c <- transform(rust, brand = relevel(brand, ref = "C"))

rust.lm2=lm(y~brand, data=rust_base_c)
summary(rust.lm2)
#or alternatively
#rust.lm2=lm(y~brand,data=rust,contrasts=list(brand=contr.treatment(4,base=3,contrasts=TRUE)))


### Constructing contrasts 3 ways
c1=c(-0.5,0.5,0.5,-0.5)  # Named vs Generic (BC brand, AD generic)
c2=c(1,0,0,-1)           # A vs D
c3=c(0,1,-1,0)           # B vc C
sizes=tapply(rust$y,rust$brand,length)
# or run this code instead of tapply
library(plyr)
sizes=ddply(rust,c("brand"),summarise,sizes=length(y))[,2]

sum(c1*c2/sizes)
sum(c1*c3/sizes)
sum(c2*c3/sizes)

# Option 1 - Manually
SSc=function(c){
  sum(tapply(y,brand,mean)*c)^2/sum(c^2/tapply(y,brand,length))
}
SSc(c1)+SSc(c2)+SSc(c3)

ci=function(c,means,model,method=c("Bonf","Scheffe","Tukey"),g=1,conf.level=0.95){
  dfE=anova(rust.m)[2,1]
  MSE=anova(rust.m)[2,3]
  if(method[1]=="Bonf"){
    cv=qt((1-conf.level)/(2*g),dfE)
  }else{if(method[1]=="Scheffe"){
    cv=sqrt(anova(rust.m)[1,1]*qf(conf.level,anova(rust.m)[1,1],dfE))
  }else{if(method[1]=="Tukey"){
    cv=qtukey(conf.level,anova(rust.m)[1,1]+1,dfE)
  }}}
  print(paste(method[1]," at ",conf.level*100,"%"))
  list(estimate=sum(c*means),interval=sum(c*means)+c(1,-1)*cv*sqrt(MSE*sum(c^2/sizes)))
}
ci(c1,tapply(y,brand,mean),rust.m,method="Bonf")
ci(c1,tapply(y,brand,mean),rust.m,method="Scheffe")
ci(c1,tapply(y,brand,mean),rust.m,method="Tukey")
ci(c2,tapply(y,brand,mean),rust.m)
ci(c3,tapply(y,brand,mean),rust.m)

# Option 2 R comes with some commonly used contrast methods.
rust$Named.vs.Generic <- c(A=-.5,B=.5,C=.5,D=-.5)[rust$brand]
rust$A.vs.D <- c(A=1,B=0,C=0,D=-1)[rust$brand]
rust$B.vs.C <- c(A=0,B=1,C=-1,D=0)[rust$brand]

# Check orthogonality
crossprod(data.matrix(rust[,4:6]))

rust.oc=lm(y~Named.vs.Generic+A.vs.D+B.vs.C, data=rust)
anova(rust.oc)
summary(rust.oc)
# Note that the contrasts are orthogonal. The 2nd and 3rd
# contrasts have squared length of 2 ? n, where
# n is the common sample size, 10. Hence, in the output
# below, the coefficients to these two contrasts give half
# of their respective estimates. (Hint: think about the
# coefficient as rise per unit change while these contrasts
# measure twice the rise (from -1 to 1: 2 units change).

# Option 3 - Use gmodels package
library(gmodels)
cm=rbind(
  'B+C vs A+D'=c1,
  'A vs D'=c2,
  'B vs C'=c3)
rust.lm3=lm(y~brand+0,data=rust)
estimable(rust.lm3,cm)

# Option 4 
contrasts(brand)
contrasts(brand)=cbind(c1,c2,c3)
rust.lm4=lm(y~brand,data=rust)
summary(rust.lm4)

# so comparing 
# B+C vs A+D = (beta1 + beta2)/2 - (0 + beta3)/2 = (46.3+24.81)/2 - (-2.67)/2=36.89
# A vs D = 0 - beta3 = 0 - -2.67 = 2.67
# B vs C = beta1 - beta2 = 46.3-24.81 = 21.49