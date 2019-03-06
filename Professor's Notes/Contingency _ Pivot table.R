# In this section we will look where to start exploring our data set after importing in R

library(rpivotTable)
library(readxl)
library(dplyr)
library(tidyr)

# We will use inbuild data set
My_cars = mtcars

# Lets see if R is understanding our data

str(My_cars)
dim(My_cars)

# I can clearly see that column "am", "gear", "vs" and "carb" are not numerical column. These are factor.
# We will convert these to factor bit later but its always adviable to keep a track how R is treating your columns

# lets see the Descriptive stats of my data
# We also call this as 6 point summary

summary(My_cars)

# From summary I can also see that I do not have any missing values in any or my column. 
# However, if you have missing values in your column its always advisable to identify the count of NA for each column
# We will be doing this in next webinar

##### Presenting your data in cross table

# Cross table always help you to understand your data.
# Lets say I want to see How many of these cars are automatic in various gears

# Inbuild function allow us to do that but not a very good represantation. 

cTable = table(My_cars$am, My_cars$gear)
cTable

# To get the row total

cTable = cbind(cTable, Total = rowSums(cTable))

# To get the column total

cTable = rbind(cTable, Total = colSums(cTable))

cTable

# Lets say I want to also check the Proportion contibution of each gear type in my data set

prop.table(cTable) # This the proportion comparied to whole population

# To check the proportion row - wise or Column - wise

prop.table(cTable, 1) # Row sum will be 1

prop.table(cTable, 2) # Column sum will be 1

#### We can use other packages to make more effective tables

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

# Same table using crosstab
crosstab(My_cars, row.vars = "am", col.vars = "gear", type = "f")

crosstab(My_cars, row.vars = "am", col.vars = "gear", type = "r") # This will give proportion row-wise

crosstab(My_cars, row.vars = "am", col.vars = "gear", type = "c") # This will give proportion column - wise

#### Lets answer few question on titanic data set

ship = as.data.frame(Titanic)
View(Titanic)

# Sex wise number of people died and survied

survived_sex = crosstab(ship, row.vars = "Survived", col.vars = "Sex", type = "f")
survived_sex

# Class wise number of people died and survied

survived_class = crosstab(ship, row.vars = "Survived", col.vars = "Class", type = "f")
survived_class


#### And the best way is to make pivot chart

library(rpivotTable)

rpivotTable(My_cars, rows="gear", cols="am",width="100%", height="400px")

### Wide data vs long data - Data transformation

# Lets create a wide data. Tracking the weight of kids over different weeks

wide = read.table(header=TRUE, text='
                  subject sex   Week1 Week2 Week3
                  Kid1   M     7.9  12.3  10.7
                  Kid2   F     6.3  10.6  11.1
                  Kid3   F     9.5  13.1  13.8
                  Kid4   M    11.5  13.4  12.9
                  ')

View(wide)

str(wide)

# Convert this to a long data 

long = gather(wide, Week, Weight, Week1:Week3)

# Converting your long data to wide

wide1 = spread(long, Week, Weight)




