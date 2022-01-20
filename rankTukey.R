rank.Tukey=function(formula,data=data,level=0.95){
  resp=all.vars(formula)[1]
  pred=all.vars(formula)[2]
  ranked_data=data.frame(cbind(rank(data[,resp]),data))
  colnames(ranked_data)[1]=c("ranks")
  rmeans=tapply(ranked_data$ranks,ranked_data[,pred],mean)
  d=apply(-combn(rmeans,2),2,diff) # pairwise differences
  
  t=length(levels(data[,pred]))
  n=dim(data)[1]
  g=t*(t-1)/2
  lett=combn(levels(data[,pred]),2)
  
  sizes=combn(table(data[,pred]),2)
  mat=matrix(NA,g,4)
  colnames(mat)=c("Diff.","Lower","Upper","Differ?")
  mat[,1]=round(d,3)
  rnames=rep(NA,g)
  
  for(i in 1:g){
    rnames[i]=paste(lett[1,i],"-",lett[2,i])
    ME=qtukey(level,nmeans=t,df=Inf)*sqrt(n*(n+1)/24*(1/sizes[1,i]+1/sizes[2,i]))
    mat[i,4]=abs(d[i])>=ME
    mat[i,2:3]=round(d[i]+c(-1,1)*ME,3)
  }
  rownames(mat)=rnames
  print(paste(level*100,"%"," Pairwise CIs"))
  mat
}