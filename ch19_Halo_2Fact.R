halo1 <- read.table("http://www.stat.ufl.edu/~winner/data/halo1.dat",header=F,
                    col.names=c("essay","attract","score"))
halo1$essay=factor(halo1$essay)
halo1$attract=factor(halo1$attract)
library(plyr)
ddply(halo1,~essay,summarise,mean=mean(score))
ddply(halo1,~attract,summarise,mean=mean(score))

halo.mod1 <- aov(score ~ factor(essay) + factor(attract),data=halo1)
summary(halo.mod1)
summary.lm(halo.mod1)

halo.mod2 <- aov(score ~ essay*attract,data=halo1)
summary(halo.mod2)
summary.lm(halo.mod2)
ddply(halo1,~essay*attract,summarise,mean=mean(score))

anova(halo.mod1, halo.mod2)

interaction.plot(essay,attract,score)
interaction.plot(attract,essay,score)

### Multiple comparisons ###
TukeyHSD(halo.mod2,which=c("essay","attract"))
