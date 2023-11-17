Input=("
Rating Officer Candidate
76  1  1
65  1  2
85  1  3
74  1  4
59  2  1
75  2  2
81  2  3
67  2  4
49  3  1
63  3  2
61  3  3
46  3  4
74  4  1
71  4  2
85  4  3
89  4  4
66  5  1
84  5  2
80  5  3
79  5  4
")

off=read.table(textConnection(Input),header=TRUE)
off$Officer=factor(off$Officer)

attach(off)
library(reshape2)
acast(off,Officer~Candidate,value.var ="Rating")

# Means vary but we are not modeling the differences in mean, just accounting for the fact they vary
library(plyr)
ddply(off,~Officer,summarise,mean=mean(Rating))

model_off=aov(Rating~Officer,data=off)

# Test sigma^2_alpha=0
anv=anova(model_off);anv

# 95% CI on grand mean
mean(Rating)+c(-1,1)*qt(0.975,4)*sqrt(anv["Officer","Mean Sq"]/(4*5))

# 95% CI on sigma^2
anv["Residuals","Mean Sq"]/qchisq(c(0.975,0.025),15)

# 95% CI on sigma^2_alpha
Lhat=1/4*anv["Officer","Mean Sq"]+(-1)*1/4*anv["Residuals","Mean Sq"]
df=Lhat^2/(((1/4)*anv["Officer","Mean Sq"])^2/anv["Officer","Df"]+
             ((-1/4)*anv["Residuals","Mean Sq"])^2/anv["Residuals","Mean Sq"])

df*Lhat/qchisq(c(0.975,0.025),df)
