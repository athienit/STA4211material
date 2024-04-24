# Introduction
# Analysis of a split plot design originally presented by Yates (1935). 
# The experiment was conducted to assess the effects on yield of three oat varieties 
# (Golden Rain, Marvellous and Victory) with four levels of nitrogen application 
# (0, 0.2, 0.4 and 0.6 cwt/acre). The field layout consisted of six blocks/replications 
# (labelled I, II, III, IV, V and VI) with three whole-plots per block, 
# each split into four sub-plots. The three varieties were randomly allocated to the 
# three whole-plots while the four levels of nitrogen application were randomly 
# assigned to the four sub-plots within each whole-plot. 

#nitrogen
#block  variety  0.0cwt  0.2cwt  0.4cwt  0.6cwt
#        GR      111     130     157     174
#I       M       117     114     161     141
#        V       105     140     118     156
#
#        GR       61      91      97     100
#II      M        70     108     126     149
#        V        96     124     121     144
#
#        GR       68      64     112      86
#III     M        60     102      89      96
#        V        89     129     132     124
#
#        GR       74      89      81     122
#IV      M        64     103     132     133
#        V        70      89     104     117
#
#        GR       62      90     100     116
#V       M        80      82      94     126
#        V        63      70     109      99
#
#        GR       53      74     118     113
#VI      M        89      82      86     104
#        V        97      99     119     121

sp.oats=read.csv("https://raw.githubusercontent.com/athienit/STA4211material/main/oats.csv")
sp.oats <- within(sp.oats, nitroF <- factor(nitro))

library(lattice)  
library(car)
with(sp.oats, xyplot(yield ~ nitroF | variety, groups = replicate))

# We assume that variety and nitrogen are fixed.  Why?

##########################
### CRD on A, no block ###
##########################

res.good <- aov(yield ~ variety * nitroF + replicate:variety, data = sp.oats)
anova(res.good)[,1:3]

# if both factors are fixed then can simply use
res.good2 <- aov(yield ~ variety * nitroF + Error(replicate:variety), data = sp.oats)
summary(res.good2)

# check assumptions and/or Box-Cox but you will see everything seems okay

# Only nitrogen is significant
# We implement the formula in the notes
library(plyr)
ddply(sp.oats,~nitroF,summarise,means=mean(yield))

b=length(levels(sp.oats$nitroF))
g=b*(b-1)/2
lett=combn(levels(sp.oats$nitroF),2)
means=tapply(sp.oats$yield,sp.oats$nitroF,mean)
d=unname(-combn(t(means),2,diff)) #minus is to do A-B not B-A

alpha=0.05; level=0.95
MSE=177
df=45
sizes=combn(table(sp.oats$nitroF),2)
mat=matrix(NA,g,4)
colnames(mat)=c("Diff.","Lower","Upper","Differ?")
mat[,1]=round(d,3)
rnames=rep(NA,g)

for(i in 1:g){
  rnames[i]=paste(lett[1,i],"-",lett[2,i])
  ME=qt(1-alpha/(2*g),df)*sqrt(MSE*sum(1/sizes[,i]))
  mat[i,4]=abs(d[i])>=ME
  mat[i,2:3]=round(d[i]+c(-1,1)*ME,3)
}
rownames(mat)=rnames
print(paste(level*100,"%"," Pairwise CIs"))
mat

############################
### RCBD on A with block ###
############################

wblock <- aov(yield ~ replicate+ variety*nitroF + replicate:variety, data = sp.oats)
anova(wblock)[,1:3]