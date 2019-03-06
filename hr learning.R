



library(dplyr)
library(ggplot2)
library(tidyr)

hr = read.csv("C:/Users/Administrator/Documents/Exploratory Data Analysis/HR analytics.csv")

ggplot(hr,aes(x=EducationField)) +geom_bar(width = 0.5,aes(fill=EducationField)) +
  theme(axis.text.x = element_text(angle = -45),legend.position = "bottom") +
  scale_fill_manual(values=c("Red","green","blue","orange","violet","black"),
                    labels=c("HR","LS","MKT","MD","OT","TD"))


data1=mpg
?
data2= data1

d1= hr%>%group_by(JobRole,Attrition)%>%summarise(count_jobrole=n())

stack_bar=ggplot(hr,aes(x=JobRole,fill= Attrition)) +
  geom_bar(width = .6,position = "fill") 

stack_bar
