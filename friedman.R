capdat=read.table("https://raw.githubusercontent.com/athienit/STA6166material/master/capsule.dat",
		col.names=c("subj", "formu", "tmax"))

# Create qualitative factor variable for formu, and assign names to levels
fformu=factor(capdat$formu, levels=1:3)
levels(fformu)=c("cap_f", "cap_nf", "entct_f")

# We have to assign subj (Subject id) as a factor level or the linear model will treat
# it as a numeric (continuous) variable and fit a regression
cap=data.frame(tmax=capdat$tmax, fformu, subj=factor(capdat$subj))
attach(cap)
head(cap)

xtabs(tmax~subj+fformu)
#tapply(tmax,fformu,mean) # trt means
temp=tapply(tmax,subj,rank)
rtable=matrix(0,11,3,dimnames=list(c(),c("cap_f", "cap_nf", "entct_f")))
for(i in 1:11){
 rtable[i,]=temp[[i]]
}
rtable

# Run Friedman's test for the RBD with subject as block and formulation as treatment
friedman.test(tmax ~ fformu|subj,data=cap)
# value slightly different from notes due to the way it treats ties in ranks

# Using our own function
source("http://www.stat.ufl.edu/~athienit/friedman.test2.R")
friedman.test2(tmax~fformu|subj,mc=TRUE,data=cap,level=0.95)

