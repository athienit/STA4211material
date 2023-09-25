# Load necessary libraries
library(boot)
library(agricolae)

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
pairwise_mean_diff <- function(ff, data){
  # ff is the formula statement, e.g.  ff=value~group
  #1 - Calculate sample group means
  #2 - Take all pairwise differences 
  #3 - Save in list/vector with labels
  }

pairwise_mean_diff(value~group,data)

  ff=value~group
  pred=as.formula(paste("~",as.character(all.vars(ff)[2])))
  pred=noquote(all.vars(ff)[2])
  resp=noquote(as.character(all.vars(ff)[1]))
  
  ddply(data,pred,summarise,mean=mean(ff[[2]]))
 
  
   means=means[,2]
  d=unname(-combn(means,2,diff)) #minus is to do A-B not B-A
  lett=combn(levels(treat),2)
  combn(means$mean, 2,diff)
  
  # Clean up output
  diff_data <- tidy(diff_data)
  
  return(diff_data)
}

#################################
### ONCE FUNCTION IS COMPLETE ###
#################################

# Apply the function to see if it works
result <- pairwise_mean_diff(value ~ group, mydata)
print(result)

# Perform bootstrap
bootstrap_results <- boot(data=mydata,statistic=pairwise_mean_diff, sim="ordinary",R = 2000,formula=value~group)

# Get confidence intervals for pairwise mean differences
boot.ci(boot.out=bootstrap_results, type="all",index=2) # BCa preferred

# use index=1 for 1st parameter (difference), 2 for 2nd etc
plot(results, index=2) # plot histogram for slope coefficient
summary(results$t[,2])
boot.ci(boot.out=results, type="all",index=2) # BCa preferred


# Print the results
cat("Bootstrap results:\n")
print(bootstrap_results)

cat("\n95% Confidence Intervals for Pairwise Mean Differences:\n")
