# Source: G. Meyer, M. von Meduna, T. Brosowski, T. Hayer (2012). "Is poker
# a Game of Skill or Chance? A Quasi-Experimental Study," Journal of Gambling 
# Studies, Online First DOI 10.1007/s10899-012-9327-8

# Description: Results from 3-Factor ANOVA to investigate effects of
# poker skill (average/expert), poker hand (bad/neutral/good), and
# bet limit (fixed/none) on winnings. 25 individuals in each of 12 conditions
# (each individual in only one treatment). Data simulated to match
# means and SDs by condition.

# Variables/Columns
# Skill   8    /* 1=Expert, 2=Average  */
#   Hand   16    /* 1=Bad, 2=Neutral, 3=Good   */
#   Limit  24    /* 1=Fixed, 2=None  */
#   Final Cash Balance (euros)  27-32

poker <- read.table("http://www.stat.ufl.edu/~winner/data/poker_skill.dat",
                    header=F,col.names=c("skill","hand","limit","cash"))

attach(poker)

skill <- factor(skill); levels(skill) <- c("Expert", "Novice")
hand <- factor(hand); levels(hand) <- c("Bad","Neutral","Good")
limit <- factor(limit); levels(limit) <- c("Fixed","None")


(tapply(cash,list(skill,hand,limit),mean)); (tapply(cash,list(skill,hand),mean))
(tapply(cash,list(skill,limit),mean)); (tapply(cash,list(hand,limit),mean))
(tapply(cash,skill,mean)); (tapply(cash,hand,mean)); (tapply(cash,limit,mean)) 

par(mfrow=c(2,2))

interaction.plot(hand[skill=="Expert"],limit[skill=="Expert"],cash[skill=="Expert"],
                 ylim=c(0,18),xlab="hand", ylab="Cash Balance",main="Experts")

interaction.plot(hand[skill=="Novice"],limit[skill=="Novice"],cash[skill=="Novice"],
                 ylim=c(0,18),xlab="hand", ylab="Cash Balance",main="Novices")

interaction.plot(hand[limit=="Fixed"],skill[limit=="Fixed"],cash[limit=="Fixed"],
                 ylim=c(0,18),xlab="hand", ylab="Cash Balance",main="Fixed Limit")

interaction.plot(hand[limit=="None"],skill[limit=="None"],cash[limit=="None"],
                 ylim=c(0,18),xlab="hand", ylab="Cash Balance",main="No Limit")

poker.aov1 <- aov(cash ~ skill*hand*limit)
anova(poker.aov1) #note 3 way and 2way hand"limit not significant.
# all pairwise comparisons
TukeyHSD(poker.aov1,which=c("skill:hand","skill:limit"))
         