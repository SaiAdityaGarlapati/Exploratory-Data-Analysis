library(dplyr)
library(ggplot2)
library(tidyr)
library(moments)
library(psych)
hr = read.csv("C:/Users/Administrator/Documents/Exploratory Data Analysis/HR analytics.csv")

#what portion of staff are leaving

ggplot(hr,aes(x=Attrition))+geom_histogram(aes(fill=as.factor(Attrition)))

