# Load necessary libraries
library(boot)

# Generate example data
set.seed(123)
group1 <- rt(30, mean = 10, sd = 1.5)
group2 <- rt(30, mean = 12, sd = 2.5)
group3 <- rt(30, mean = 11, sd = 2)

# Combine the groups
data <- c(group1, group2, group3)

# Create a grouping variable
group <- rep(c("Group1", "Group2", "Group3"), each = 20)

# Make dataframe and cleanup other variables
mydata=data.frame(value=data,group=factor(group))
remove(group1,group2,group3,data,group)


# Define a function to calculate the pairwise mean differences
pairwise_mean_diff <- function(formula, data=NULL,indices){
  dat=data[indices,] 
  if (inherits(formula, "formula") == FALSE) {
    stop("The input model must be a formula.\n")
  }
  m1=aov(formula,data=dat)
  resp=all.vars(formula)[1]
  pred=all.vars(formula)[2]
  
  treat=as.factor(m1$model[,pred])
  r=length(levels(treat)) # number of levels
  g=r*(r-1)/2  #number of all pairwise comparisons
  labs=combn(levels(treat),2)
  means=tapply(dat[,resp],dat[,pred],mean)
  diffs=unname(-combn(means,2,diff)) #minus is to do A-B not B-A
  rnames=rep(0,g)
  for(i in 1:g){
    rnames[i]=paste(labs[1,i],"-",labs[2,i])
  }
  diffs=setNames(diffs,rnames)
  return(diffs)
}

# Perform bootstrap
bootstrap_results <- boot(data=mydata,statistic=pairwise_mean_diff, sim="ordinary",R = 1000,formula=value~group)
# use index=1 for 1st parameter (difference), 2 for 2nd etc
generate_results <- function(results,conf.level=0.90,bonf.adj=T) {
  #Create labels
  m1=aov(formula,data=dat)
  resp=all.vars(formula)[1]
  pred=all.vars(formula)[2]
  treat=as.factor(m1$model[,pred])
  r=length(levels(treat)) # number of levels
  g=r*(r-1)/2  #number of all pairwise comparisons
  if(bonf.adj==T){conf=round(1-(1-conf.level)/g,3)} #bonferroni adjustment
  labs=combn(levels(treat),2)
  rnames=rep(0,g)
  for(i in 1:g){
    rnames[i]=paste(labs[1,i],"-",labs[2,i])
  }
  
  # Create a loop to generate plots
  num_plots=dim(results$t0)
  for (i in 1:num_plots) {
    # Create a new plot
    plot(results,index=i)
    title(paste("  |  Plot:",rnames[i]))
  }
  # Print the results
  cat("Bootstrap results for differences:\n")
  cat(rnames,sep ="\n")
  print(bootstrap_results)
  for (i in 1:num_plots) {
    cat("\n")
    cat(paste("CI for Pairwise Mean Differences:",rnames[i],"\n"))
    print(boot.ci(boot.out=bootstrap_results, type="all",index=i,conf=conf)) # BCa preferred
  }
  cat(paste("\n Experimentwise confidence level at least",conf.level*100,"%","\n"))
}

# Get confidence intervals for pairwise mean differences
generate_results(bootstrap_results) # plot histogram for difference, do for 1:g, scroll through plots

