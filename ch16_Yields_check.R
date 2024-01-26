#Yields of Amoebae

yields=read.table("https://raw.githubusercontent.com/athienit/STA4211material/main/entozamoeba.txt",header=FALSE)
colnames(yields)=c("Condition","Yields")
yields$Condition=factor(yields$Condition)

library(plyr)
ddply(yields,.(Condition),summarise,means=mean(Yields),sds=sd(Yields))

#windows(6.5,5.5)
# Dot plot 
stripchart(Yields~Condition, data=yields, method="stack", vertical=TRUE,
           pch=1, cex=1.5, xlab="Factor", ylab="Yields", main="Dotplots by Condition")
title(sub="pre-analysis plot", adj=0, cex=5/6)
#mtext("Example")
points(1:5,tapply(yields$Yields,yields$Condition,mean),col=2,pch=8)
legend(2,290, c("Observations", " Trt Mean"), col = c(1,2), text.col= "black",
       lty=c(0,0),pch=c(1,8),bg='gray90')

library(ggdist)
library(ggplot2)
library(gghalves)
ggplot(yields, aes(x = Condition, y = Yields,fill=Condition)) + 
  ggdist::stat_halfeye(
    adjust = .9, #custom bandwidth
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
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 17,  # Use a triangle as the point shape
    size = 3,
    color = "red",  # Set the color to red
    alpha=0.5, #opacity
    position = position_dodge(width = 0.75)  # Adjust the dodge width as needed
  ) +
  coord_cartesian(xlim = c(1.2, NA), clip = "off")

# + coord_flip()

yaov1=aov(Yields~Condition,data=yields)
re=rstudent(yaov1)

# Time Series Plot
plot(re,type="o",pch=22,xlab="Order",ylab="studentized res.",main="Time Series")
abline(h=0)

### DIAGNOSTICS
#Built in R option
plot(yaov1)

#Use custom code
source("https://raw.githubusercontent.com/athienit/STA4210material/main/check.R")
check(yaov1,tests=TRUE)
# Why is Durban Watson not valid?
ncvTest(lm(Yields~Condition,data=yields)) #Breush-Pagan test of non constant variance

### Checking outliers
sdr=rstudent(yaov1)
sdr[which(abs(sdr)>=abs(qt(0.05/length(sdr),yaov1$df.residual)))]

### Observations with x-values with the potential to "pull" the regression line
hat=hatvalues(yaov1)
hat[which(hat>2*5/length(sdr))]

### DFFITS
dftrt=length(levels(yields$Condition))-1
dffits(yaov1)[which(dffits(yaov1)>2*sqrt(dftrt/length(sdr)))]

### Cooks D
cd=cooks.distance(yaov1)
cd[which(cd>1)] # criterion >1
cd[which(cd>qf(0.5,dftrt,df.residual(yaov1)))] # more conservative

### Some cross-validation
# Load necessary libraries
library(dplyr) # For data manipulation
library(caret) # For cross-validation

# Define the number of folds
num_folds <- 10

# Create indices for stratified 10-fold cross-validation
folds <- createDataPartition(yields$Yields, times = num_folds, p=0.2, list = TRUE)

# Initialize a vector to store cross-validation results
cv_results <- numeric(num_folds)

# Perform 10-fold cross-validation
for (i in 1:num_folds) {
  # Split the data into training and validation sets
  train_data <- yields[-folds[[i]], ]
  valid_data <- yields[folds[[i]], ]
  
  # Fit the model on the training data using aov
  model <- aov(Yields ~Condition, data = train_data)
  
  # Make predictions on the validation set
  predictions <- predict(model, newdata = valid_data)
  
  # Calculate the mean squared error (you may choose a different metric)
  mse <- mean((valid_data$Yields - predictions)^2)
  
  # Store the result
  cv_results[i] <- mse
}

# Calculate the mean and standard deviation of the cross-validation results
mean_mse <- mean(cv_results)
std_mse <- sd(cv_results)

# Print the results
cat("Mean MSE:", mean_mse, "\n")
cat("Standard Deviation of MSE:", std_mse, "\n")

