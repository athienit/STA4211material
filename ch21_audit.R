Input=c("
Proficiency Block Method
73.0      1      1
81.0      1      2
92.0      1      3
76.0      2      1
78.0      2      2
89.0      2      3
75.0      3      1
76.0      3      2
87.0      3      3
74.0      4      1
77.0      4      2
90.0      4      3
76.0      5      1
71.0      5      2
88.0      5      3
73.0      6      1
75.0      6      2
86.0      6      3
68.0      7      1
72.0      7      2
88.0      7      3
64.0      8      1
74.0      8      2
82.0      8      3
65.0      9      1
73.0      9      2
81.0      9      3
62.0     10      1
69.0     10      2
78.0     10      3
")

audit=read.table(textConnection(Input),header=TRUE)
audit$Block=factor(audit$Block)
audit$Method=factor(audit$Method)

xtabs(Proficiency~Method+Block,data=audit)

model.audit=aov(Proficiency~Method+Block,data=audit)
summary(model.audit)
TukeyHSD(model.audit,which="Method")

RE=(9*48.2+10*2*6.2)/((3*10-1)*6.2);RE
REp=((2*9+1)*(3*9+3))/((2*9+3)*(3*9+2))*RE;REp
