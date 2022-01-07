qc=read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2022%20Data%20Sets/CH22PR09.txt")
colnames(qc)=c("y","color","replication","x")
qc$color=factor(qc$color,levels=1:3,labels=c("blue","green","orange"))

#use (correlation) transformation/standardization 
qc$y=with(qc,1/sqrt(14)*(y-mean(y)/sd(y))) #standardize y
qc$x=with(qc,1/sqrt(14)*14*(x-mean(x)/sd(x))) #standardize x

reduced.model=lm(y~color+x,data=qc) #additive model
full.model=lm(y~color*x, data=qc) #model with interaction which is
#E(Y)=gamma_0 + gamma_1 I_green + gamma_2 I_orange
#       +gamma_3 x
#       +gamma_4 (I_green x) +gamma_5 (I_orange x)

#Test whether interaction, gamma 4=gamma=5=0
anova(reduced.model,full.model)

# Plots using ggplot2 rather "scatteplot" function
library(ggplot2)
library(moderndive)
# Plot model with interaction
ggplot(qc, aes(x = x, y = y, color = color) ) +
  geom_point() +
  geom_smooth(method = "lm", formula=y~x,se = FALSE)+
  scale_color_manual(values = c("blue", "green", "orange"))

# Plot model without interaction
ggplot(qc, aes(x = x, y = y, color = color) ) +
  geom_point() +
  geom_parallel_slopes(formula=y~x,se = FALSE)+
  scale_color_manual(values = c("blue", "green", "orange"))

# Assume we decided on parallel model
summary(reduced.model)

# Test whether color is significant via full vs reduced again
anova(lm(y~x,data=qc),lm(y~color+x,data=qc))

# CI interval
predict.lm(reduced.model,newdata=data.frame(color="blue",x=280),interval="confidence",level=0.90)

# Make Bonferroni, Scheffe (or even Tukey) for CI on
# (1) gamma 1 -> Green vs Blue
# (2) gamma 2 -> Orange vs Blue
# (3) gamma 1 - gamma 2 ->Green vs Orange
# since only g=3 use bonferroni
# use vcov(reduced.model) and do it MANUALLY, no built ini method exists
vc=vcov(reduced.model);vc
# (1) gamma 1
coefficients(reduced.model)[2]+c(-1,1)*qt(1-0.10/(2*3),11)*sqrt(vc[2,2])
# (2) gamma 2
coefficients(reduced.model)[3]+c(-1,1)*qt(1-0.10/(2*3),11)*sqrt(vc[3,3])
# (3) gamma 1- gamma 2
diff(coefficients(reduced.model)[2:3])+c(-1,1)*qt(1-0.10/(2*3),11)*sqrt(vc[2,2]+vc[3,3]-2*vc[2,3])

