# Load necessary libraries
library(boot)

# Generate example data
set.seed(123)
group1 <- rnorm(20, mean = 10, sd = 2)
group2 <- rnorm(20, mean = 12, sd = 2)
group3 <- rnorm(20, mean = 11, sd = 2)

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
  anova.table=anova(m1)
  resp=all.vars(formula)[1]
  pred=all.vars(formula)[2]
  
  treat=as.factor(m1$model[,pred])
  r=length(levels(treat)) # number of levels
  g=r*(r-1)/2  #number of all pairwise comparisons
  lett=combn(levels(treat),2)
  means=tapply(dat[,resp],dat[,pred],mean)
  diffs=unname(-combn(means,2,diff)) #minus is to do A-B not B-A
  rnames=rep(0,g)
  for(i in 1:g){
    rnames[i]=paste(lett[1,i],"-",lett[2,i])
  }
  setNames(diffs,rnames)
  return(diffs)
}


# Perform bootstrap
bootstrap_results <- boot(data=mydata,statistic=pairwise_mean_diff, sim="ordinary",R = 1000,formula=value~group)
# use index=1 for 1st parameter (difference), 2 for 2nd etc
generate_results <- function(results) {
  # Create a loop to generate plots
  num_plots=dim(results$t0)
  for (i in 1:num_plots) {
    # Create a new plot
    plot(results,index=i)
    title(paste("Plot",i))
  }
  # Print the results
  cat("Bootstrap results:\n")
  print(bootstrap_results)
  for (i in 1:num_plots) {
    cat("\n")
    print(paste("95% Confidence Intervals for Pairwise Mean Differences:",i))
    print(boot.ci(boot.out=bootstrap_results, type="all",index=i)) # BCa preferred
  }
}

# Get confidence intervals for pairwise mean differences
generate_results(bootstrap_results) # plot histogram for difference, do for 1:g, scroll through plots

