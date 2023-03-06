library(plyr)
##################################################################
### ------------------- Data Example ------------------------- ###
### --------------Replicated Latin Squares-------------------- ###
##################################################################

### Replications within Cells ###
# Three incentive methods given to workers to test impact on productivity

Input="
IQ Age Method Prod
High Young B 19
High Young B 16
High Middle A 20
High Middle A 24
High Old C 25
High Old C 21
Normal Young C 24
Normal Young C 22
Normal Middle B 14
Normal Middle B 15
Normal Old A 14
Normal Old A 14
Low Young A 10
Low Young A 14
Low Middle C 12
Low Middle C 13
Low Old B 7
Low Old B 4
"

worker=read.table(textConnection(Input),header=T)
ddply(worker, ~Method, summarize, means=mean(Prod))
worker.LSD=aov(Prod~Method+IQ+Age,data=worker)
summary(worker.LSD)
TukeyHSD(worker.LSD, which="Method")


### Crossover ###
# 3 different displays on sale of apple.
# 6 stores used ***NOTICE TWO WAYS DATA ENTERED FOR STORE ***

Input="
Sales Pattern Order Display StoreW1 StoreW2
   9  1  1  2  1  1
   4  1  1  2  2  2
  12  2  1  1  3  1
  13  2  1  1  4  2
   7  3  1  3  5  1 
   5  3  1  3  6  2
  12  1  2  3  1  1
  12  1  2  3  2  2
  14  2  2  2  3  1
  14  2  2  2  4  2
  18  3  2  1  5  1
  20  3  2  1  6  2
  15  1  3  1  1  1
   9  1  3  1  2  2
   3  2  3  3  3  1
   3  2  3  3  4  2
   6  3  3  2  5  1
   4  3  3  2  6  2
"
apples=read.table(textConnection(Input),header=T)
apples$Pattern=as.factor(apples$Pattern)
apples$Order=as.factor(apples$Order)
apples$Display=as.factor(apples$Display)
apples$StoreW1=as.factor(apples$StoreW1)
apples$StoreW2=as.factor(apples$StoreW2)

ddply(apples, ~Display, summarize, means=mean(Sales))
apples.LSDW1=aov(Sales~Display+Pattern+Order+StoreW1,data=apples)
apples.LSDW2=aov(Sales~Display+Pattern+Order+StoreW2:Pattern,data=apples)
summary(apples.LSDW1)
summary(apples.LSDW2)
TukeyHSD(apples.LSDW1, which="Display")
