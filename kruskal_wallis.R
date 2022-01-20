### Kruskal Wallis Test
# Study of the effect of glucose concentration on insulin release in 12 pancreatic specimens 
insulin=data.frame(value=c(1.59,1.73,3.64,1.97,3.36,4.01,3.49,2.89,3.92,4.82,3.87,5.39),
	treat=factor(rep(1:3,each=4),labels=c("Low","Medium","High")))
#print(insulin)

# Dot plot 
stripchart(value~treat, data=insulin, method="stack", vertical=TRUE,
  pch=1, cex=1.5, xlab="Glucose Concentration", ylab="Insulin", main="Dotplots by Treatments")
  
title(sub="pre-analysis plot", adj=0, cex=5/6)
mtext("Example")
points(c(1,2,3),tapply(insulin$value,insulin$treat,mean),col=2,pch=8)
abline(h=mean(insulin$value),col=3)
legend(2.5,2.5, c("Observations", " Trt Mean","Grand Mean"), col = c(1,2,3), text.col= "black",
       lty=c(0,0,1),pch=c(1,8,NA),bg='gray90')

### Obtain T.kw manually (you can skip this)
r.insulin=data.frame(cbind(rank(insulin$value),insulin))
colnames(r.insulin)[1]=c("ranks")
rsums=tapply(r.insulin$ranks,r.insulin$treat,sum) # obtain rank means by trt
rsums

n_i=c(4,4,4);n=sum(n_i)
T.kw=12/(n*(n+1))*sum(rsums^2/n_i)-3*(n+1);T.kw
1-pchisq(T.kw,2) # p-value

### Obtain T.kw via built in function
kruskal.test(value~treat,data=insulin)

### Rank analogue to Tukey's HSD
source("http://users.stat.ufl.edu/~athienit/rankTukey.R")
rank.Tukey(formula=value~treat,data=insulin,level=0.95)

