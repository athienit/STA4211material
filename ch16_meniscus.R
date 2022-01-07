meniscus <- read.table("http://www.stat.ufl.edu/~winner/data/meniscus.dat",header=F,
                      col.names=c("method","loadfail","displace","stiff"))

attach(meniscus)

library(plyr)
mv=ddply(meniscus,~method,summarise,mean=mean(loadfail),v=var(loadfail));mv

anvtable=anova(aov(loadfail~factor(method)));anovtable

library(lmPerm)
summary(aovp(loadfail~factor(method)))  #This calculates Type III SS

#Other alternatives exist too
library(perm)
help("permKS")
permKS(loadfail,factor(method))

# To manually create a permutation test see
# http://www.stat.ufl.edu/~winner/computing/r/meniscus_rout.txt
