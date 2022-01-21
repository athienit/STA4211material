allpairCI=function(formula,fixed=NULL,data,method=c("Bonf","Scheffe","Tukey"),level=0.95){
  m1=aov(formula,data=data)
  anova.table=anova(m1)
  if(is.null(fixed)){
    pred=all.vars(formula)[2]
  }else{
    pred=fixed
  }
  treat=m1$model[,pred]
  MSE=anova.table["Residuals","Mean Sq"]
  df=df.residual(m1)
  resp=all.vars(formula)[1]
  
  t=length(levels(treat))
  g=t*(t-1)/2
  lett=combn(levels(treat),2)
  means=tapply(data[,resp],data[,pred],mean)
  d=unname(-combn(means,2,diff)) #minus is to do A-B not B-A
  
  alpha=1-level
  sizes=combn(table(treat),2)
  
  if(method[1]=="Tukey"){
    TukeyHSD(m1,conf.level=level)
  }else{
  mat=matrix(NA,g,4)
  colnames(mat)=c("Diff.","Lower","Upper","Differ?")
  mat[,1]=round(d,3)
  rnames=rep(NA,g)
  
  for(i in 1:g){
    rnames[i]=paste(lett[1,i],"-",lett[2,i])
    if(method[1]=="Bonf"){
      ME=qt(1-alpha/(2*g),df)*sqrt(MSE*sum(1/sizes[,i]))
    }else{if(method[1]=="Scheffe"){
      ME=sqrt((t-1)*qf(1-alpha,t-1,df))*sqrt(MSE*sum(1/sizes[,i]))
    }
    }#close else  
    mat[i,4]=abs(d[i])>=ME
    mat[i,2:3]=round(d[i]+c(-1,1)*ME,3)
  }
  rownames(mat)=rnames
  print(paste(level*100,"%"," Pairwise CIs"))
  mat
  }
}