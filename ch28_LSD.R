# function that creates a Latin Square Design of dimesion t
latin=function (t,nrand=20){
  x=t(matrix(LETTERS[1:t],t,t))
  for(i in 2:t)x[i,]=x[i,c(i:t,1:(i-1))]
  if(nrand>0){
    for(i in 1:nrand){
      x=x[sample(t),]
      x=x[,sample(t)]
    }
  }
  print(x)
}

latin(4) # run this a few times to see the different designs
latin(8)

##################################################################
### ------------------- Data Example ------------------------- ###
##################################################################

#Data on crop yield were collected for 5 types of fertilizer (treatment) using 5 watering schemes
#(row), over 5 growing seasons (column).

millet=read.table("https://raw.githubusercontent.com/athienit/STA4211material/main/millet.txt",header=TRUE)
attach(millet)
millet

row=factor(row)
column=factor(column)
millet.rbd2=aov(yield~treatment+row+column)
summary(millet.rbd2)

# Note that treatment is not significant, so we stop here.
# However if it was significant then do all pairwise comparisons

millet.Tukey=TukeyHSD(millet.rbd2,"treatment",conf.level=0.95)
print(millet.Tukey)
#windows(width=5,height=5,pointsize=10)
plot(millet.Tukey, sub="Ex6D3 Data", adj=0,cex.lab=1.5)
mtext("Tukey Honest Significant Differences",side=3,line=0.5)

# Let's calculate the Relative Efficiency
df=anova(millet.rbd2)[,"Df"]
MS=anova(millet.rbd2)[,"Mean Sq"]
(MS[2]+MS[3]+df[1]*MS[4])/((df[1]+2)*MS[4])

##################################################################
### ------------------- Data Example ------------------------- ###
### --------------Replicated Latin Squares-------------------- ###
##################################################################
# Blood concentration of a drug is being tested for 3 delivery methods:
#. A Solution,
#. B Tablet,
#. C Capsule.
# It is commonly thought that there will be variability due to subject and the time (period)
# administered (due to changes in metabolism), so these factors need to be accounted for. The
# response value is area under the time-concentration curve.

# Area under the time/concentration curve in a bioequivalence study.
# Columns are period, subject, treatment, and response (area).  
# Treatments are 1 solution, 2 tablet, 3 capsule. Full data set.
Input="
period      subject          trt         area
1            1            1         1799
2            1            3         1846
3            1            2         2147
1            2            3         2075
2            2            2         1156
3            2            1         1777
1            3            2         1396
2            3            1          868
3            3            3         2291
1            4            2         3100
2            4            1         3065
3            4            3         4077
1            5            3         1451
2            5            2         1217
3            5            1         1288
1            6            1         3174
2            6            3         1714
3            6            2         2919
1            7            3         1430
2            7            1          836
3            7            2         1063
1            8            1         1186
2            8            2          642
3            8            3         1183
1            9            2         1135
2            9            3         1305
3            9            1          984
1           10            3          873
2           10            1         1426
3           10            2         1540
1           11            1         2061
2           11            2         2433
3           11            3         1337
1           12            2         1053
2           12            3         1534
3           12            1         1583
"
blood=read.table(textConnection(Input),header=T)
blood$period=factor(blood$period)
blood$subject=factor(blood$subject)
blood$trt=factor(blood$trt,levels=1:3,labels=LETTERS[1:3])

xtabs(as.numeric(trt)~subject+period,data=blood)
blood.LSD=aov(area=trt+period+subject,data=blood)
summary(blood.LSD)
