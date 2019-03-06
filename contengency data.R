library(dplyr)

epl=read.csv("C:/Users/Administrator/Documents/Exploratory Data Analysis/penalty_data.csv")


mt_cars= mtcars

View(mt_cars)


qwerty1=mtcars%>%group_by(gear,cyl)%>%select(gear,cyl)%>%View 


gTable=table(mt_cars$gear,mt_cars$cyl,mt_cars$am,mt_cars$vs)                                              

gTable

p=prop.table(gTable)*100

p
View(p)

swd=read.csv("simple wide data.csv")
library(tidyr)

long=gather(swd,week,weight,Week1,Week2,Week3)

View(long)

wide=spread(long,week,weight)

View(wide)

rpivotTable::rpivotTable(mt_cars)

str(mtcars)

