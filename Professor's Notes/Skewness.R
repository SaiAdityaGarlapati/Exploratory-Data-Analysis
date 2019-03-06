## Skewness in Data
library(readxl)
library(ggplot2)
library(e1071)

# Lets import ODI data set

odi = read_excel("E://Training data//Amit//Data Set//odi-batting-analysis.xlsx")

# Lets plot a histogram on Runs

hist(odi$Runs, xlab = "Runs bins", ylab = "Frequency")

# From histogram it is clear that the data is positively skewed. Lets verify the same by mean and median
# For Positively skewed data mean > median
# For Neg. skewed data mean < median
# For non skewed data mean = median

mean(odi$Runs, na.rm = TRUE)

median(odi$Runs, na.rm = TRUE)

# This the data is positively skewed. Lets check if it is highly skewed, modrate skewed, or less skewed

skewness(odi$Runs, na.rm = TRUE)

# As the value is greater than +1 its highly positive skewed. 
# Lets try to transform the data using log transformation

odi$Runs_Log = log(odi$Runs, na.rm = TRUE)

hist(odi$Runs_Log) # Histogram shows that the data is normal now.

# However, as Runs column contain 0, log(0) = undefine. 
# Thanks the reason mean, meadian and Skewness will not give output

skewness(odi$Runs_Log, na.rm = TRUE)

# To avoid this we will be doing the square root transformation.

odi$Runs_sqrt = sqrt(odi$Runs)

hist(odi$Runs_sqrt) # Histogram shows skewness reduced but still not a normal dist. 

mean(odi$Runs_sqrt, na.rm = TRUE)

median(odi$Runs_sqrt, na.rm = TRUE)

skewness(odi$Runs_sqrt, na.rm = TRUE) # Skewness show its still modrate skewed. 

# We may try one more square root transformation. 

odi$Runs_sqrt = sqrt(odi$Runs_sqrt)

hist(odi$Runs_sqrt) # Histogram shows its almost normal now.

# Mean and median will be very close to each other

mean(odi$Runs_sqrt, na.rm = TRUE)

median(odi$Runs_sqrt, na.rm = TRUE)

# Skewness is very low.
skewness(odi$Runs_sqrt, na.rm = TRUE)
