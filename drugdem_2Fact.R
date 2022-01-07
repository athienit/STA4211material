drugdem_dat=read.table("http://www.stat.ufl.edu/~winner/data/biostat/ex0605.dat",col.names=c("ethnic", "gender", "clrnc"))

# Create qualitative factor variable for ethnic, and assign names to levels
fethnic=factor(drugdem_dat$ethnic, levels=1:2)
levels(fethnic)=c("hispanic", "anglo")

# Create qualitative factor variable for gender, and assign names to levels
fgender=factor(drugdem_dat$gender, levels=1:2)
levels(fgender)=c("female", "male")

# Create new data set with "factorized" predictors
drugdem=data.frame(clrnc=drugdem_dat$clrnc, fethnic, fgender)
head(drugdem)
attach(drugdem)

# Factor1 by Factor2 Means
tapply(clrnc,list(fgender,fethnic),mean)

# Plot...for large datasets use boxplot instead of stripchart
stripchart(clrnc ~ fethnic*fgender,method="stack", vertical=TRUE,
           pch=1, cex=1.5, xlab="Factors", ylab="Total Clearence", main="Dotplots by Factors")
title(sub="pre-analysis plot", adj=0, cex=5/6)
points(c(1,2,3,4),as.vector(t(tapply(clrnc,list(fgender,fethnic),mean))),col=2,pch=8)
abline(h=mean(clrnc),col=3)
legend("topright", c("Observations", " Trt Means","Grand Mean"), col = c(1,2,3), text.col= "black",
       lty=c(0,0,1),pch=c(1,8,NA),bg="gray90")

#windows(width=5,height=3,pointsize=10) # creates new graphic window
interaction.plot(x.factor=fethnic, trace.factor=fgender, response=clrnc,
                 fun=mean, type="b", legend=T, ylab="Total Clearence", main="Interaction Plot", pch=c(1,19))

# Although a=b=2 in this example what model do you think we will have?

# Run 2 way ANOVA with main effects for fethnic and fgender, and interaction
# by typing fethnic*fgender as "independent variable", R includes main effects and interactions
drugdem.aov=aov(clrnc ~ fethnic*fgender)
anv=anova(drugdem.aov);anv
summary.lm(drugdem.aov)

# only gender is significant so
drugg.aov=aov(clrnc ~ fgender)
anvg=anova(drugg.aov);anvg
summary.lm(drugg.aov)

#95% CI for mu_female-mu_male
-2.0200+c(-1,1)*qt(0.975,18)*0.9906

###---------------------- Multiple Comparisons --------------------###
# Since interaction is not significant we will use the comparison method
# for NO interaction.  Note that here there is no "need" to refit.
# Simpler model is just a CRD with one factor (gender) with two levels

# Refitting does have an advantage here.  What is it? Hint: Sample size

# performing Tukey for Male-Female trt means
# Can obtain it via built in function together for the one with gender
TukeyHSD(drugdem.aov,which=c("fgender")) # If we don't specify "which" it will do all of them
TukeyHSD(drugg.aov,which=c("fgender"))

# Can obtain all comparisons that would include the interaction IF it were significant
TukeyHSD(drugdem.aov)

### For practice try doing some contrasts
### assuming interaction is significant, such as (anglo:male+hispanic:male)/2-(anglo:female+hispanic:female)/2
