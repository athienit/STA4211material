Input = ("
 Day  Snake  Openings
         1    D1        85 
         1    D3       107
         1    D5        61
         1    D8        22
         1    D11       40
         1    D12       65
         2    D1        58
         2    D3        51
         2    D5        60
         2    D8        41
         2    D11       45
         2    D12       27
         3    D1        15
         3    D3        30
         3    D5        68
         3    D8        63
         3    D11       28
         3    D12        3
         4    D1        57
         4    D3        12
         4    D5        36
         4    D8        21
         4    D11       10
         4    D12       16
         ")

Data = read.table(textConnection(Input),header=TRUE)
Data$Day=factor(Data$Day)

library(asbio)
with(Data,tukey.add.test(Openings,Snake,Day))
#so no interaction and no snake

mod=aov(Openings~Snake+Day,data=Data) #could remove snake since not significant
anova(mod)
TukeyHSD(mod,which="Day")

library(dae)
with(Data,tukey.1df(mod,Data))

#######################################################################
### Now to calculate D in 3 ways
library(plyr)
meanDay=unname(ddply(Data,~Day,summarise,mean=mean(Openings))[,2])
meanSnake=unname(ddply(Data,~Snake,summarise,mean=mean(Openings))[,2])
gmean=mean(Data$Openings) #grandmean

newData=as.matrix(xtabs(Openings~Snake+Day,data=Data))
# Way 1, formula page 886 book
tmp=rep(0,6)
for(i in 1:6){
    tmp[i]=sum((meanDay-gmean)*newData[i,])
}
num=sum((meanSnake-gmean)*tmp)
denom=sum((meanSnake-gmean)^2)*sum((meanDay-gmean)^2)
D=num/denom

# Way 2, regression through origin
Ystar=newData
Xstar=newData
for(i in 1:6){
  for(j in 1:4){
    Ystar[i,j]=newData[i,j]-meanSnake[i]-meanDay[j]+gmean
    Xstar[i,j]=(meanSnake-gmean)[i]*(meanDay-gmean)[j]
  }
}
Ystar=melt(Ystar)[,"value"]
Xstar=melt(Xstar)[,"value"]
lm_mod=lm(Ystar~0+Xstar)
summary(lm_mod)

# Way 3, change the function tukey.add.test
tukey.add.test2=function (y, A, B) 
{
  dname <- paste(deparse(substitute(A)), "and", deparse(substitute(B)), 
                 "on ", deparse(substitute(y)))
  A <- factor(A)
  B <- factor(B)
  ybar.. <- mean(y)
  ybari. <- tapply(y, A, mean)
  ybar.j <- tapply(y, B, mean)
  len.means <- c(length(levels(A)), length(levels(B)))
  SSAB <- sum(rep(ybari. - ybar.., len.means[2]) * rep(ybar.j - 
                                                         ybar.., rep(len.means[1], len.means[2])) * tapply(y, 
                                                                                                           interaction(A, B), mean))^2/(sum((ybari. - ybar..)^2) * 
                                                                                                                                          sum((ybar.j - ybar..)^2))
  aovm <- anova(lm(y ~ A + B))
  SSrem <- aovm[3, 2] - SSAB
  dfdenom <- aovm[3, 1] - 1
  STATISTIC <- SSAB/SSrem * dfdenom
  names(STATISTIC) <- "F"
  DEGREES.OF.FREEDOM <- c(1, dfdenom)
  names(DEGREES.OF.FREEDOM) <- c("num.df", "denom.df")
  D <- sqrt(SSAB/(sum((ybari. - ybar..)^2) * sum((ybar.j - 
                                                    ybar..)^2)))
  names(D) = "D"
  RVAL <- list(statistic = STATISTIC, df = DEGREES.OF.FREEDOM, 
               p.value = 1 - pf(STATISTIC, 1, dfdenom), head = "Tukey's one df test for additivity", 
               data.name = dname)
  attr(RVAL, "class") <- "addtest"
  list(RVAL,D)
}
with(Data,tukey.add.test2(Openings,Snake,Day))
