clrnc=read.table("http://www.stat.ufl.edu/~winner/data/biostat/ex0603.dat",header=FALSE,col.names=c("subj", "intagnt", "thcl"))
head(clrnc)

# Create qualitative factor variable for intagnt, and assign names to levels
fintagnt=factor(clrnc$intagnt, levels=1:3)
levels(fintagnt)=c("Placebo", "Famotidine", "Cimetidine")

# We have to assign subj (Subject id) as a factor level or the linear model will treat
# it as a numeric (continuous) variable and fit a regression
clrnc.df=data.frame(thcl=clrnc$thcl, fintagnt, subj=factor(clrnc$subj))
head(clrnc.df)

# create easy to view table
table=xtabs(thcl~subj+fintagnt,data=clrnc.df);table
round(addmargins(table,c(1,2),FUN=mean),2)

### Fancier plot ###
library(ggplot2)
library(PupillometryR)
library(Rmisc)
sumdat <- summarySE(clrnc.df, measurevar = "thcl",
                    groupvars=c("fintagnt"))
sumdat

# use "adjust" in violin to adjust density/historgram bandwidth
ggplot(clrnc.df, aes(x = fintagnt, y = thcl, fill=fintagnt)) +
  geom_flat_violin(position = position_nudge(x = .1, y = 0), adjust = 0.75, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(fintagnt)-.1, y = thcl, colour = fintagnt),position = position_jitter(width = .05), size = 1, shape = 1)+
  geom_boxplot(outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  geom_point(data = sumdat, aes(x = as.numeric(fintagnt)+.1, y = thcl, group = fintagnt, colour = fintagnt), shape = 18) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Aligned Plot")

# Fit the ANOVA for the RBD with subject and interacting agent as independent variables
rcbd=aov(thcl~fintagnt+subj,data=clrnc.df)
anova(rcbd)

rcbd.Tukey=TukeyHSD(rcbd,"fintagnt",level=0.95)
print(rcbd.Tukey)
plot(rcbd.Tukey, sub="Theophylline Data", adj=0)
mtext("Tukey Honest Significant Differences",side=3,line=0.5)

# Or use
library(DescTools)
PostHocTest(rcbd,which="fintagnt",method="bonferroni")
#PostHocTest(rust.m,method="scheffe")
PostHocTest(rcbd,which="fintagnt",method="hsd") # Tukey's

# Let's calculate the Relative Efficiency
df=anova(rcbd)[,"Df"]
MS=anova(rcbd)[,"Mean Sq"]
(df[2]*MS[2]+(df[2]+1)*df[1]*MS[3])/(sum(df)*MS[3])
