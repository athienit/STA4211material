halo1 <- read.table("http://www.stat.ufl.edu/~winner/data/halo1.dat",header=F,
                    col.names=c("essay","attract","score"))
halo1$essay=factor(halo1$essay)
halo1$attract=factor(halo1$attract)

##############################
### Descriptive Statistics ###
##############################
library(plyr)
library(dplyr)

# newer way to summarise using dplyr and pipe operator
halo1 %>%
  group_by(essay,attract) %>%
  dplyr::summarise(mean=mean(score),n=n())

# older way using plyr
ddply(halo1,~essay*attract,summarise,mean=mean(score))
ddply(halo1,~essay,summarise,mean=mean(score))
ddply(halo1,~attract,summarise,mean=mean(score))

#############
### PLOTS ###
#############
with(halo1,interaction.plot(essay,attract,score))
with(halo1,interaction.plot(attract,essay,score))

### Fancier plot ###
library(ggplot2)
library(PupillometryR)
library(Rmisc)
sumdat <- summarySE(halo1, measurevar = "score",
                    groupvars=c("attract", "essay"))
sumdat

# use "adjust" in violin to adjust density/historgram bandwidth
ggplot(halo1, aes(x = attract, y = score, fill = essay)) +
  geom_flat_violin(position = position_nudge(x = .1, y = 0), adjust = 0.75, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(attract)-.1, y = score, colour = essay),position = position_jitter(width = .05), size = 1, shape = 1)+
  geom_boxplot(outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  geom_line(data = sumdat, aes(x = as.numeric(attract)+.1, y = score, group = essay, colour = essay), linetype = 2)+
  geom_point(data = sumdat, aes(x = as.numeric(attract)+.1, y = score, group = essay, colour = essay), shape = 18) +
  geom_errorbar(data = sumdat, aes(x = as.numeric(attract)+.1, y = score, group = essay, colour = essay, ymin = score-se, ymax = score+se), width = .05)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Aligned and Inreaction Plot")

#################
### Inference ###
#################
halo.mod1 <- aov(score ~ essay + attract,data=halo1)
summary(halo.mod1)
summary.lm(halo.mod1)

halo.mod2 <- aov(score ~ essay*attract,data=halo1)
summary(halo.mod2)
summary.lm(halo.mod2)

anova(halo.mod1, halo.mod2)

### Multiple comparisons ###
TukeyHSD(halo.mod2,which=c("essay","attract"))
