# Load necessary libraries
library(boot)
library(ggdist)
library(ggplot2)
library(gghalves)
library(plyr)

# Generate example data, note not normal and non-constant variance
set.seed(812347)
n=30
group1 <- rt(n,df=23)*1+10
group2 <- rt(n,df=23)*2.5+12
group3 <- rt(n,df=23)*2+11

# Combine the groups
data <- c(group1, group2, group3)

# Create a grouping variable
group <- rep(c("Group1", "Group2", "Group3"), each = n)

# Make dataframe and cleanup other variables
mydata=data.frame(value=data,group=factor(group))
remove(group1,group2,group3,data,group)

# Plot data
ggplot(mydata, aes(x = group, y = value, fill=group)) + 
  ggdist::stat_halfeye(
    adjust = .5, #custom bandwidth
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  coord_flip()

# get group means and variances
ddply(mydata,~group,summarise,mean=mean(value),var=var(value))

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
generate_results <- function(results,formula,dat,conf.level=0.90,bonf.adj=T) {
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
generate_results(bootstrap_results,formula=value~group,dat=mydata) # plot histogram for difference, do for 1:g, scroll through plots

