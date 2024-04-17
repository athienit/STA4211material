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

# Sphericity 
# https://www.datanovia.com/en/lessons/mauchlys-test-of-sphericity-in-r/
library(rstatix)
res <- anova_test(data = rogaine1, dv = hair, wid = subj, within = time)
res

# Time is actually a continuous factor so lets look at the trt*time interaction
library(ggplot2)

# Plot with boxplots and mean points
ggplot(data=rogaine1, aes(x=time, y=hair, color=trt, group=trt)) +
  geom_boxplot(aes(group=interaction(time, trt)), alpha=0.5) +  # Add boxplots
  stat_summary(fun=mean, geom="point", size=3, shape=2) +      # Add mean points
  stat_summary(fun=mean, geom="line", aes(group=trt)) +         # Add lines connecting the mean points
  scale_x_discrete(name="Time (weeks)",labels=c("1"="8", "2"="16", "3"="24", "4"="32")) +
  labs(y="Hair Count", color="Treatment") +
  theme_minimal()  # Optional: clean minimalistic theme
  
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
# encourage reader to perfom multiple comparisons, may see this question in exam