# Florida Swamps Classified in 3 Size Categories: Small, Medium, Large (Fixed Factor)
# 3 Swamps sampled from each size type (Random Factor)
# 27 locations sampled and water levels measured, in cm.

swamp = read.table("https://raw.githubusercontent.com/athienit/STA4211material/main/swamp.txt",
col.names=c("size", "swampid", "watlev"))
swamp$size=factor(swamp$size, levels=1:3,labels=c("Small", "Medium", "Large"))
swamp$swampid=factor(swamp$swampid)

# look at nested structure
xtabs(~swampid+size,data=swamp)
xtabs(watlev~swampid+size,aggregate(watlev~swampid+size,swamp,mean))

### Plot data
# Plot old way
boxplot(watlev~size/swampid, pch=1, cex=1.5, xaxt="n",xlab="Swamps",
   ylab="Water Level", main="Swamp Water Level Means",data=swamp)
axis(1,at=c(1,4,7,11,14,17,21,24,27),labels=c("S1","S2","S3","S4","S5","S6","S7","S8","S9"))
title(sub="pre-analysis plot", adj=0, cex=5/6)
mtext("Swamp example")
abline(h=mean(swamp$watlev),col=3)

# Plot new way
library(ggplot2)
ggplot(swamp, aes(x=factor(rep(rep(1:3,27),3)),y=watlev,fill=size)) +
  geom_boxplot() +
  geom_jitter(width=0.1,alpha=0.2) +
  xlab("SwampID")+ 
  ylab("Water Level")+
  guides(fill=FALSE)+ #hide fill legend
  facet_wrap(~size,ncol = 3,labeller=label_both) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Test whether each swampid, i.e. beta has equal variance
library(car)
leveneTest(watlev~swampid,data=swamp)

### Find the Factor A: size means
size.m=with(swamp,tapply(watlev, size, mean));size.m
# or 
library(plyr)
ddply(swamp,~size,summarise,mean=mean(watlev))


### INFERENCE
# This provides ANOVA, not appropriate F-test for fsize
swamp.aov1 = aov(watlev ~ size + size:swampid,data=swamp)
anv1=anova(swamp.aov1)[,1:3];anv1

# Test for B(A), that is: sigma_b(a)=0
anv1[2,3]/anv1[3,3];1-pf(anv1[2,3]/anv1[3,3],anv1[2,1],anv1[3,1])

# Test for A, that is alpha_1=...=alpha_3=0
anv1[1,3]/anv1[2,3]
1-pf(anv1[1,3]/anv1[2,3],anv1[1,1],anv1[2,1])

## This provides appropriate F-test for fsize
#swamp.aov2 = aov(watlev ~ size + Error(swampid))
#summary(swamp.aov2)

## Alternatives
#library(lme4)
#alt1=lmer(watlev~size+(size|swampid))
##summary(alt1)
#anova(alt1)

#library(nlme)
#alt2=lme(watlev~size,random=~size/swampid)
##summary(alt2)
#anova(alt2)

###----------------- Tukey Pairwise Comparisons for Factor A-----------------###
a=3;b=9;n=27
size.m=with(swamp,tapply(watlev, size, mean))
d=apply(combn(size.m,2),2,diff) # pairwise differences
d=as.array(d);dimnames(d)=list(c("Medium-Small","Large-Small","Large-Medium")) # just labeling the names of the dimensions
for(i in 1:3){
 print(dimnames(d)[[1]][i])
 print(d[i]+c(-1,1)*qtukey(0.95,nmeans=a,df=b-a)*sqrt(anv1[,3][2]/(3*n)))
}

###----------------- CI for sigma_beta and sigma-----------------###
#sigma_beta using W-S lemma
L_sb=1/n*(anv1[2,3]-anv1[3,3]);L_sb #estimate
df_sb=L_sb^2/(((1/n)*anv1[2,3])^2/6+((1/n)*anv1[3,3])^2/234);df_sb

df_sb*L_sb/qchisq(c(0.975,0.025),df_sb) #95 CI

#sigma
anv1["Residuals","Mean Sq"] #estimate
anv1["Residuals","Sum Sq"]/qchisq(c(0.975,0.025),anv1["Residuals","Df"])
