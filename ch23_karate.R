karate <- read.table("http://www.stat.ufl.edu/~winner/sta4211/karate_board1.dat",header=T)

karate$Wood=factor(karate$Wood)
karate$Board=factor(karate$Board)
attach(karate)
require(plyr)
xtabs(n~Wood+Board,data=ddply(karate,~Wood+Board,summarise,n=length(Y))) #cell sample sizes
xtabs(Mean~Wood+Board,data=ddply(karate,~Wood+Board,summarise,Mean=mean(Y)))

#Interaction Plot
interaction.plot(Wood,Board,Y,type="b")

### Base group restriction alpha_1=beta_1=alphabeta_1j=alphabeta_i1=0 for all i,j
#options(contrasts=c('contr.treatment', 'contr.treatment'))
reg_bg=lm(Y ~ Wood*Board,data=karate)
summary(reg_bg)
model.matrix(reg_bg)  #notice that wood1 is base group

# For cell {1,1} the model estimates mu_{11.} as gamma_1=mu_{...}
predict.lm(reg_bg,newdata=data.frame(Wood="1",Board="1"))

# For cell {3,2} the model estimates mu_{32.} as gamma_1+gamma_3+gamma_5+gamma_7=mu_{...}+alpha_3+beta_2+alphabeta_32
predict.lm(reg_bg,newdata=data.frame(Wood="3",Board="2"))

### SUM TO ZERO CONSTRAINTS

#options(contrasts=c('contr.sum', 'contr.sum'))
#reg_so=lm(Y ~ Wood*Board,data=karate)
#summary(reg_so)
#model.matrix(reg_so) #This is same as the model matrix manually constructed in the dataset 

## For cell {1,1} the model estimates mu_{11.} as 
## b_0+b_1+b_4+b_5=mu_{...}+alpha_1+beta_1+alphabeta_11
#predict.lm(reg_so,newdata=data.frame(Wood="1",Board="1"))

## For cell {3,2} the model estimates mu_{32.} as
## mu_{...}+alpha_3+beta_2+alphabeta_32 = mu_{...}+alpha_3-beta_1-alphabeta_31                                               
#predict.lm(reg_bg,newdata=data.frame(Wood="3",Board="2"))

## Same as reg_so model where the design matrix is spelled out
#kb.mod1 <- lm(Y ~ X1 + X2 + X3 + X4 + X1X4 + X2X4 + X3X4,data=karate)
#summary(kb.mod1)
#kb.mod2<-lm(Y~X4+X1X4+ X2X4 + X3X4)
#anova(kb.mod2,kb.mod1)

# Test for interaction already done but lets do it with a regression model reg_bg
reg_bg2=update(reg_bg,.~.-Board:Wood)
anova(reg_bg2,reg_bg) #full vs reduced model, one with and one without interactions
summary(reg_bg2)

### Now to test main effects using Type II SS approach
# Test Wood
reg_bg3=update(reg_bg2,.~.-Wood)
anova(reg_bg3,reg_bg2)

# or all in one go
# note that book does things via Type III SSS
mod=aov(Y~Wood+Board+Wood:Board,data=karate)
library(car)
Anova(mod,type="II") # For unbalanced data use Type II SS
# SS(A|B)
# SS(B|A)
# SS(AB|A,B)

### Multiple comparisons

aov_mod=aov(formula(reg_bg2),data=karate)
# Caution: TukeyHSD function incorporates an adjustment for sample size that produces sensible intervals for mildly unbalanced designs.
# Best done manually
TukeyHSD(aov_mod)
