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
pairwise_mean_diff <- function(formula, data=NULL){
  # formula is the formula statement, e.g.  formula=value~group
  #1 - Calculate sample group means
  #2 - Take all pairwise differences 
  #3 - Save in list/vector with labels
}

### Testing internals of function

# set arguments
formula=as.formula(value~group)
data=mydata

if (inherits(formula, "formula") == FALSE) {
  stop("The input model must be a formula.\n")
}
full.model.frame <- model.frame(formula, data = data, na.action = na.pass)
resp <- model.response(full.model.frame)
p <- dim(full.model.frame)[2] - 1
n <- length(resp)
if (is.matrix(resp) != TRUE && is.vector(resp) != TRUE) {
  stop("Response must be a vector or matrix.\n")
}
else if ((dim(resp)[1] == 0 || dim(resp)[2] == 0) && length(resp) == 
         0) {
  stop("Response must have entries.\n")
}
else if (mode(resp) != "numeric") {
  stop("Response must be of type numeric.\n")
}
else if (anyNA(resp) == TRUE) {
  stop("Response must not have any missing values.\n")
}
if (p > 2) {
  warning("This function has only been fully tested for one-way and two-way ANOVA.\n")
}
else if (p == 0) {
  stop("The model must have predictor variables. \n")
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
