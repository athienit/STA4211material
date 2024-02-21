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
library(plyr)
library(ggplot2)
library(PupillometryR)
library(Rmisc)
library(ggpubr)

poker <- read.table("http://www.stat.ufl.edu/~winner/data/poker_skill.dat",
                    header=F,col.names=c("skill","hand","limit","cash"))
poker$skill <- factor(poker$skill, labels=c("Expert", "Novice"))
poker$hand <- factor(poker$hand, labels=c("Bad","Neutral","Good"))
poker$limit <- factor(poker$limit,labels=c("Fixed","None"))

ddply(poker,~skill+hand+limit,summarise,mean=mean(cash))

sumdat <- summarySE(poker, measurevar = "cash",
                    groupvars=c("skill", "hand","limit"))
sumdat

ggplot(poker, aes(x = hand, y = cash, fill = limit)) +
  geom_flat_violin(position = position_nudge(x = .1, y = 0), adjust = 0.75, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(hand)-.1, y = cash, colour = limit),position = position_jitter(width = .05), size = 1, shape = 1)+
  geom_boxplot(outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  geom_line(data = sumdat, aes(x = as.numeric(hand)+.1, y = cash, group = limit, colour = limit), linetype = 2)+
  geom_point(data = sumdat, aes(x = as.numeric(hand)+.1, y = cash, group = limit, colour = limit), shape = 18) +
  geom_errorbar(data = sumdat, aes(x = as.numeric(hand)+.1, y = cash, group = limit, colour = limit, ymin = cash-se, ymax = cash+se), width = .05)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Aligned and Interaction Plot") +
  facet_wrap(~skill)

poker.aov1 <- aov(cash ~ skill*hand*limit)
anova(poker.aov1) #note 3 way and 2way hand"limit not significant.
ddply(poker,~skill+hand,summarise,mean=mean(cash))
ddply(poker,~skill+limit,summarise,mean=mean(cash))
# all pairwise comparisons
TukeyHSD(poker.aov1,which=c("skill:hand","skill:limit"))
         