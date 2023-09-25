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

########################
### WORK IN PROGRESS ###
########################
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
  means=tapply(data[,resp],data[,pred],mean)
  diffs=unname(-combn(means,2,diff)) #minus is to do A-B not B-A
  return(diffs)
}

#################################
### ONCE FUNCTION IS COMPLETE ###
#################################

# Perform bootstrap
bootstrap_results <- boot(data=mydata,statistic=pairwise_mean_diff, sim="ordinary",R = 10,formula=value~group)
# use index=1 for 1st parameter (difference), 2 for 2nd etc
plot(bootstrap_results, index=2) # plot histogram for slope coefficient
summary(bootstarp_results$t[,2])

# Get confidence intervals for pairwise mean differences
boot.ci(boot.out=bootstrap_results, type="all",index=2) # BCa preferred




# Print the results
cat("Bootstrap results:\n")
print(bootstrap_results)

cat("\n95% Confidence Intervals for Pairwise Mean Differences:\n")