#Yields of Amoebae

yields=read.table("http://www.stat.ufl.edu/~winner/data/entozamoeba.dat",header=FALSE)
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


### Useful link: http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm

options(contrasts=c('contr.treatment', 'contr.treatment'))
contrasts(yields$Condition)
contr.treatment(5)
help(contrasts)
contr.sum(5)

yaov1=aov(Yields~Condition,data=yields)
summary(yaov1)
summary.lm(yaov1)

yields_base3=transform(yields,Condition=relevel(Condition,"3"))
contrasts(yields_base3$Condition)
yaov_base3=aov(Yields~Condition,data=yields_base3)
summary(yaov_base3)
summary.lm(yaov_base3)

options(contrasts=c('contr.sum', 'contr.sum'))
contrasts(yields$Condition)
yaov2=aov(Yields~Condition,data=yields)
summary(yaov2)
summary.lm(yaov2)

source("http://www.stat.ufl.edu/~athienit/check.R")
check(yaov1,tests=TRUE)
# Why is Durban Watson not valid?
ncvTest(lm(Yields~Condition,data=yields))
