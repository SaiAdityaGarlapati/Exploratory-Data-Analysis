library(pysch)
library(ggplot2)
library(readxl)

tyre=read.csv("C:/Users/Administrator/Documents/Exploratory Data Analysis/tyre.csv")

describeBy(tyre$Mileage,group = tyre$Brands,digits=2,mat=TRUE)

ggplot(tyre,aes(x=Brands,y=Mileage)) +geom_boxplot(outlier.colour = "RED")

model=aov(Mileage~Brands,data= tyre)

summary(model)

test=TukeyHSD(model,conf.level = 0.95,ordered = TRUE)

test

plot(test,col="red",las=1)


marks=read_excel("C:/Users/Administrator/Documents/Exploratory Data Analysis/Students Marks.xlsx")

marks_long=gather(marks,Assessment_type,Marks_obtain,FA:IA)

marks_long

marks_long$Student=as.factor(marks_long$Student)
marks_long$Assessment_type=as.factor(marks_long$Assessment_type)

marks_model=aov(Marks_obtain~Assessment_type+Student,data=marks_long)

summary(marks_model)

plot(marks_model,col="red",las=1)

View(economics)

library(lubridate)

ymd(economics$date)

chart1=ggplot(economics,aes(x=date,y=unemploy))+geom_line(aes(y=unemploy,color="unemployment"))+
  geom_line(aes(y=pce,color="Price"))

brks<-economics$date[seq(1,length(economics$date),12)]
lbls<-lubridate::year(brks)


odi=read.csv("C:/Users/Administrator/Documents/R programming Class Notes/odi-batting.csv")
