library(ggplot2)
library(dplyr)

#relationship between city mileage and highway mileage

ds=mpg
# scatter plot
s=ggplot(mpg,aes(cty,hwy))+ geom_point(aes(col=class))
s
# jitter plot
j=ggplot(mpg,aes(cty,hwy))+ geom_jitter(aes(col=class))

j

library(gridExtra)

grid.arrange(s,j)

#coloring has to be done on categorical value
ggplot(mpg,aes(cty,hwy))+ geom_jitter(aes(col=class,size=cyl))

#coloring numerical variable or attribute is not advised
ggplot(mpg,aes(cty,hwy))+ geom_jitter(aes(col=cyl,size=class))


m=cor(mtcars)

View(m)

library(corrplot)

corrplot(m,method = "square")


