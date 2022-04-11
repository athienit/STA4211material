# Subjects (Random): 8 Women with Androgenetic Alopecia

# Treatments (Fixed): Minoxidil (Rogaine) and Placebo   (4 Women per treatment)

# Time Periods (Fixed): Measurements made Pre-treatment 8, 16, 24, and 32 weeks (will only consider post-treatment measures)
#--------------------------------------------------------------------------------------------------------------------
rogaine=read.table("https://raw.githubusercontent.com/athienit/STA4211material/main/hair.txt",header=TRUE)

# Make sure we read factors as categorical not continuous
rogaine1=data.frame(trt=factor(rogaine$trt,labels=c("Placebo","Rogaine")), subj=factor(rogaine$subj),
                    time=factor(rogaine$time), hair=rogaine$hair)
attach(rogaine1)
### Check conditions
# Equal variances
library(car)
leveneTest(hair,subj)

# Sphericity (old defunct way)
mat=xtabs(hair~subj+time)
mod=lm(mat~1)
mauchly.test(mod,X=~1)

des=factor(c("8","16","24","36"))
aov=Anova(mod,idata=data.frame(des),idesign=~des,type="III")
summary(aov)

# Sphericity (new way)
# https://www.datanovia.com/en/lessons/mauchlys-test-of-sphericity-in-r/
library(rstatix)
res <- anova_test(data = rogaine1, dv = hair, wid = subj, within = time)
res

# Time is actually a continuous factor so lets look at the trt*time interaction
interaction.plot(x.factor=time, trace.factor=trt, response=hair,
                 fun=mean, type="b", legend=T, xlab="Week",ylab="Hair", main="Interaction Plot of Trt*Time",
                 col=1:3,pch=c(1,19),xaxt="n")
axis(1,at=1:4,labels=des)

library(ggplot2)
ggplot(data=rogaine1) +
  aes(x = time, color = trt, group = trt, y = hair) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")+
  scale_x_discrete(name="Time (weeks)",labels=c("1"="8", "2"="16", "3"="24", "4"="36"))+
  labs(y="Hair",color="Treatment")
  
# work in progress to add error bars, create hm=hair.mean and hsd=hair.sd
# geom_errorbar(aes(ymin=hm-hsd, ymax=hm+hsd), width=.2, position=position_dodge(0.05))

# Obtain ANOVA and manualy test terms
rogaine1.mod1 = aov(hair ~ trt + trt:subj + time + trt:time)
anv=anova(rogaine1.mod1)[,1:3];anv

# Test interaction AC
1-pf(anv[,3][4]/anv[,3][5],anv[,1][4],anv[,1][5])

# Test Factor A
1-pf(anv[,3][1]/anv[,3][3],anv[,1][1],anv[,1][3])

# Test Factor C
1-pf(anv[,3][2]/anv[,3][5],anv[,1][2],anv[,1][5])

#------------
# Can tell R to choose the correct error to test trt
rogaine1.mod2 <- aov(hair ~ trt*time + Error(subj))
summary(rogaine1.mod2)
#------------
# Multiple Comparison NONE since no effect is significant

# if trt*time was significant then bar(y)_{i.k}-bar(y)_{i'.k}
# comparisons would be made
library(plyr)
ddply(rogaine1,~trt*time,summarise,mean=mean(hair))
