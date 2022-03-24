### ----------------Part I ----------------###

Dose=rep(c(0.2,0.4,0.8,1.6),each=3)
Product=rep(c("A","B","C"),4)
y=c(2.0,1.8,1.3,4.3,4.1,2.0,6.5,4.9,2.8,8.9,5.7,3.4)
ds=data.frame(Dose=(Dose),Product=factor(Product),Response=y)
xtabs(Response~Dose+Product,data=ds)
ds

library(car)
scatterplot(Response~Dose|Product,smooth=FALSE,reg.line=lm,data=ds)

# Notice that a log transformation on Dose may help fit a more linear relationship (try Box Cox)
summary(powerTransform(ds$Dose))

ds$ClDose=log(Dose)-mean(log(Dose)) # center quantitative predictor
###########################################################################
######### Probably best to standardize all quantitative variables #########
###########################################################################
cor.trans=function(y){
  n=length(y)
  1/sqrt(n-1)*(y-mean(y))/sd(y)
}

ds$ClDose=cor.trans(log(ds$Dose))
ds$Response=cor.trans(ds$Response)
###########################################################################

# Full model
modelfull=lm(Response~ClDose*Product,data=ds)
summary(modelfull)
anova(modelfull)

scatterplot(Response~ClDose|Product,smooth=FALSE,reg.line=lm,data=ds)
# The log transform really helps reduce s and increases R^2 adj

### Things to note:

# The coefficients for the interactions are both significant and negative
# so slope for Dose is:
# 3.3038 for A
# 3.3038-1.5004 for B
# 3.3038-2.2795 for C

# Is slope for Dose the same for B and C?
# Do a CI on beta_4 - beta_5
vmat=vcov(modelfull);round(vmat,3)
d=diff(modelfull$coefficients[6:5]);names(d)=NULL;d
d+c(1,-1)*qt(0.025,6)*sqrt(vmat[5,5]+vmat[6,6]-2*vmat[5,6])

# We can always choose which is the base group.  Let's choose C to be the base group
ds_base_c=transform(ds,Product=relevel(Product,"C"))
model_bc=lm(Response~Dose*Product,data=ds_base_c)
summary(model_bc)

###-----------Part II -----------------------------###
# Simultaneous 95% CI on all 5 betas except beta0
summary(modelfull)
modelfull$coefficients[2:4]+qt(0.025/5,6)*sqrt(diag(vcov(modelfull))[2:4]) #Lower limits
modelfull$coefficients[2:4]-qt(0.025/5,6)*sqrt(diag(vcov(modelfull))[2:4]) #Upper limits

# CI on mean response when log dose=-0.6931475 and product=B
predict(modelfull,newdata=data.frame(Dose=-0.6931475,Product="B"),se.fit=TRUE,interval="confidence")
W=sqrt(6*qf(0.95,6,12-6)) #for confidence region

# PI when log dose=-0.6931475 and product=B
predict(modelfull,newdata=data.frame(Dose=-0.6931475,Product="B"),interval="prediction")
xh=c(1,-0.6931475,1,0,-0.6931475,0)
s.pred=anova(modelfull)["Residuals","Mean Sq"]*t(xh)%*%solve(t(model.matrix(modelfull))%*%model.matrix(modelfull))%*%xh
# Need "s.pred" for Scheffe intervals and Bonferroni