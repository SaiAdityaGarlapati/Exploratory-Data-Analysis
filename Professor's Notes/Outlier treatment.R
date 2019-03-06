### Outerlier treatement - Option 1

library(ggplot2)
library(dplyr)

# Step 1: Finding if my data have outliers

data2 = mpg


# For a given continuous variable, outliers are those observations that lie outside 1.5 * IQR, 
# where IQR, the 'Inter Quartile Range' is the difference between 75th and 25th quartiles. 
# Look at the points outside the whiskers in below box plot.

boxplot(data2$hwy, data2$cty, names = c("Highway Milage", "City Milage"))

# Box plot shows that I do have outliers in my highway milage and city milage

# Step 2: Treatment of these outlier
# Step 2.1: Removing these values from data set itself

# From box plot I can see that the outlier in highway have the milage greater than 40 and in city have a milage greater than 27

# Thus we can filter the data using dplyr to remove these values

data2_without_outlier = data2 %>% filter(hwy > 40) %>% filter(cty > 27)

# Step 2.2: Replace these outlier values by the mean or median of respective column

data2_with_replacement = data2 %>% mutate(hwy=replace(hwy, hwy > 40, mean(hwy))) %>% 
  mutate(cty=replace(cty, cty > 27, mean(cty)))

# To Check if mutate worked

data2_Check = data2_with_replacement %>% filter(hwy > 40) %>% filter(cty > 27)

#### Option 2: Manual Calculation of outlier and capping

# Find the quatiles of data

q = quantile(data2$hwy)
q

# Calculate the IQR

IQR = 27-18

# Calculate the upper whisker and lower whisker

upper_whisker = 27 + 1.5*IQR
lower_whisker = 18 - 1.5*IQR

# Imputing the column in data set to mark each row an outlier

data2$Outlier = ifelse(data2$hwy > upper_whisker | data2$hwy < lower_whisker, "Yes", "No")


# Now if you want to cap the outlier to the upper whisker value

data3 = data2 %>% mutate(new_hwy = 
                           ifelse(Outlier == "Yes", upper_whisker, hwy))

#### Why is not recomended to change the outlier in EDA

# New beetle is an outlier. Lets check that only in the subcompact segment


sub_comp = filter(data3, class == "subcompact")

boxplot(sub_comp$hwy, sub_comp$cty, names = c("Highway Milage", "City Milage"))

# If I calculate the upper whisker for this class

# Find the quatiles of data

q_sub_comp = quantile(sub_comp$hwy)
q_sub_comp

# Calculate the IQR

IQR_sub_comp = 30.5-24.5

# Calculate the upper whisker and lower whisker

upper_whisker_sub_comp = 30.5 + 1.5*IQR_sub_comp
lower_whisker_sub_comp = 24.5 - 1.5*IQR_sub_comp

# We can see the differenc in upper whisker. Thus capping is not clearly a good option


# DO it your self:

# In this case the 2 models are outlier with-in the class also. 
# Check in case of city milage, if all the 4 models are outlier in respective class also.

# Lets try ploting multiple box plot using ggplot so that we can viz all outlier in a single chart

############ Box plot in ggplot

# Data Set used: mpg

# Problem statement : To create a box plot with city mileage grouped by Class of car for each cylender

chart1= ggplot(data2, aes(class, cty)) + geom_boxplot(varwidth = TRUE, outlier.colour ="Red", 
                                                      outlier.size = 2.5)

chart1

# Now I want to plot the case for each cylender car

chart2 = ggplot(data2, aes(class, cty)) + geom_boxplot(aes(fill = factor(cyl)), 
                                                       varwidth = TRUE, outlier.colour ="Red", 
                                                       outlier.size = 2.5)

chart2
























