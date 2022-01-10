#Yields of Amoebae part 2

yields=read.table("https://raw.githubusercontent.com/athienit/STA4211material/main/entozamoeba.txt",header=FALSE)
colnames(yields)=c("Condition","Yields")
yields$Condition=factor(yields$Condition)

means=tapply(yields$Yields,yields$Condition,mean) # obtain means by trt
sizes=tapply(yields$Yields,yields$Condition,length)

plot(means,type="o",xlab="Condition",ylab="Yield")

yaov1=aov(Yields~Condition,data=yields)
MSE=anova(yaov1)["Residuals","Mean Sq"]
dfe=anova(yaov1)["Residuals","Df"]

L=means-qt(1-0.025,dfe)*sqrt(MSE/sizes)
U=means+qt(1-0.025,dfe)*sqrt(MSE/sizes)

require(plotrix)
x=1:5
plotCI(x, means, ui=U, li=L,xlab="Condition",ylab="Yields")
