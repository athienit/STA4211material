# function that creates a Latin Square Design of dimesion t
latin=function (t,nrand=20){
  x=t(matrix(LETTERS[1:t],t,t))
  for(i in 2:t)x[i,]=x[i,c(i:t,1:(i-1))]
  if(nrand>0){
    for(i in 1:nrand){
      x=x[sample(t),]
      x=x[,sample(t)]
    }
  }
  print(x)
}

latin(4) # run this a few times to see the different designs
latin(8)

### ------------------- Data Example ------------------------- ###
#Data on crop yield were collected for 5 types of fertilizer (treatment) using 5 watering schemes
#(row), over 5 growing seasons (column).

millet=read.table("http://www.stat.ufl.edu/~athienit/STA4211/Examples/millet.txt",header=TRUE)
attach(millet)
millet

row=factor(row)
column=factor(column)
millet.rbd2=aov(yield~treatment+row+column)
summary(millet.rbd2)

# Note that treatment is not significant, so we stop here.
# However if it was significant then do all pairwise comparisons

millet.Tukey=TukeyHSD(millet.rbd2,"treatment",conf.level=0.95)
print(millet.Tukey)
#windows(width=5,height=5,pointsize=10)
plot(millet.Tukey, sub="Ex6D3 Data", adj=0,cex.lab=1.5)
mtext("Tukey Honest Significant Differences",side=3,line=0.5)

# Let's calculate the Relative Efficiency
df=anova(millet.rbd2)[,"Df"]
MS=anova(millet.rbd2)[,"Mean Sq"]
(MS[2]+MS[3]+df[1]*MS[4])/((df[1]+2)*MS[4])