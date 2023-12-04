# Yates (1935) as an example of a split-plot design.  These data have been introduced by Yates (1935) as an example of a split-plot design. The treatment structure used in the experiment was a 3$times$4 full factorial, with three varieties of oats and four concentrations of nitrogen. The experimental units were arranged into six blocks, each with three whole-plots subdivided into four subplots. The varieties of oats were assigned randomly to the whole-plots and the concentrations of nitrogen to the subplots. All four concentrations of nitrogen were used on each whole-plot.

sp.oats=read.csv("https://raw.githubusercontent.com/athienit/STA4211material/main/oats.csv")
sp.oats <- within(sp.oats, nitroF <- factor(nitro))

library(lattice)  
library(car)
with(sp.oats, xyplot(yield ~ nitroF | variety, groups = replicate))

# We assume that variety and nitrogen are fixed.  Why?

res.good <- aov(yield ~ variety * nitroF + Error(replicate:variety), data = sp.oats)
summary(res.good)

# You could run it like this and take care which error term to use
res.good2 <- aov(yield ~ variety * nitroF + replicate:variety, data = sp.oats)
summary(res.good2)

# check assumptions and/or Box-Cox but you will see everything seems okay

# Only nitrogen is significant
# We implement the formula in the notes
library(plyr)

b=length(levels(sp.oats$nitroF))
g=b*(b-1)/2
lett=combn(levels(sp.oats$nitroF),2)
means=tapply(sp.oats$yield,sp.oats$nitroF,mean)
d=unname(-combn(t(means),2,diff)) #minus is to do A-B not B-A

alpha=0.05; level=0.95
MSE=177 #website makes a mistake and uses 117
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

library(agricolae)
print(with(sp.oats, HSD.test(yield, nitroF, DFerror = 45, MSerror = 177)))

