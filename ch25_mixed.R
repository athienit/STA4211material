# Three machines are introduced in an effort to improve productivity.
# Each machine was operated by 6 randomly selected workers.  Each worker had 3 runs
# on each machine on which they were scored.

Machines=read.table("https://raw.githubusercontent.com/athienit/STA4211material/main/Machines.txt",header=TRUE)
Machines$Worker=factor(Machines$Worker)
Machines$Machine=factor(Machines$Machine)
head(Machines)
attach(Machines)

round(tapply(score,list(Worker,Machine),mean),2)
Machine.m=tapply(score,Machine,mean);
round(Machine.m,2)

m1=aov(score ~ Machine*Worker)
anv=anova(m1)[,1:3];anv

# Test for main effect Machine.
# Could test Worker and Machine:Worker but not that interested in them
TS=anv["Machine","Mean Sq"]/anv["Machine:Worker","Mean Sq"];TS
1-pf(TS,anv["Machine","Df"],anv["Machine:Worker","Df"])


# Estimate sigma, sigma_ab and sigma_b
# Exercise create 95% CI around each one
a=3;b=6;n=3
s2=anv["Residual","Mean Sq"]; s2
s2.ab=(anv[3,3]-anv[4,3])/n;  s2.ab
s2.b=(anv[2,3]-anv[3,3])/(a*n);  s2.b


d=apply(combn(Machine.m,2),2,diff) # pairwise differences
d=as.array(d);dimnames(d)=list(c("B-A","C-A","C-B")) # just labeling the names of the dimensions
for(i in 1:3){
 print(dimnames(d)[[1]][i])
 print(d[i]+c(-1,1)*qtukey(0.95,nmeans=3,df=anv[3,1])*sqrt(anv[3,3]/(b*n)))
}

