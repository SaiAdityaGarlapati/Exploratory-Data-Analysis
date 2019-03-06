########### Stacked bar chart #######################################################

library(ggplot2)
library(purrr)
library(dplyr)
# Data Set used: mpg
# Category: Comparison (Bivariate Analysis)
# No. of dimension presented: 2
# No. of measure (Matric) presented: 1
# Special remark: Mainly used to compare two categories on same scale.
# Problem Statement: manufacturer wise, class wise count of cars

data1 = mpg %>% group_by(manufacturer, class) %>% summarise(Count = n())

stack_bar = ggplot(data1, aes(x = manufacturer, y = Count, fill = class)) + geom_bar(stat = "Identity", width = .6) 

stack_bar + geom_text(data=data1, aes(x=manufacturer, y = Count, label = Count),position = position_stack(vjust=0.5)) +
  
  theme(axis.text.x = element_text(angle = 90))


#####  stacked bar chart with % contribution
# Data Set used: mpg
# Category: Comparison / Composition (Bivariate Analysis)
# No. of dimension presented: 2
# No. of measure (Matric) presented: 1
# Special remark: Mainly used to compare or find the composition of two categories on same scale.
# Problem Statement: manufacturer wise, class wise composition of cars

stack_fill = ggplot(mpg, aes(x = manufacturer)) + 
  geom_bar(aes(fill = class),position = "fill", width = .6) 

stack_fill
stack_fill + theme(axis.text.x = element_text(angle = 90))
