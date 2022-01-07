ex0603dat=read.table("http://www.stat.ufl.edu/~winner/data/biostat/ex0603.dat",header=FALSE,col.names=c("subj", "intagnt", "thcl"))
head(ex0603dat)

# Create qualitative factor variable for intagnt, and assign names to levels
fintagnt=factor(ex0603dat$intagnt, levels=1:3)
levels(fintagnt)=c("Placebo", "Famotidine", "Cimetidine")

# We have to assign subj (Subject id) as a factor level or the linear model will treat
# it as a numeric (continuous) variable and fit a regression
ex0603=data.frame(thcl=ex0603dat$thcl, fintagnt, subj=factor(ex0603dat$subj))
attach(ex0603)
head(ex0603)

# create easy to view table
table=xtabs(thcl~subj+fintagnt);table
round(addmargins(table,c(1,2),FUN=mean),2)

# Fit the ANOVA for the RBD with subject and interacting agent as independent variables
ex0603.rbd=aov(thcl~fintagnt+subj)
anova(ex0603.rbd)

ex0603.Tukey=TukeyHSD(ex0603.rbd,"fintagnt",level=0.95)
print(ex0603.Tukey)
#windows(width=5,height=5,pointsize=10)
plot(ex0603.Tukey, sub="Theophylline Data", adj=0)
mtext("Tukey Honest Significant Differences",side=3,line=0.5)

# Do Bonferoni using our own made function
source("http://www.stat.ufl.edu/~athienit/Bonf.R")
Bonf(thcl~fintagnt+subj,data=ex0603,level=0.95)

# Let's calculate the Relative Efficiency
df=anova(ex0603.rbd)[,"Df"]
MS=anova(ex0603.rbd)[,"Mean Sq"]
(df[2]*MS[2]+(df[2]+1)*df[1]*MS[3])/(sum(df)*MS[3])
