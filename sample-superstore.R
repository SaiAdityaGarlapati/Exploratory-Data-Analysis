library(readxl)
library(FinCal)
library(psych)
library(e1071)
library(dplyr)
library(ggplot2)
sss=read_excel("C:/Users/Administrator/Documents/Exploratory Data Analysis/Sample-Superstore-Subset-Excel.xlsx")

str(sss)

#univariant analysis #bivariant analysis #multivariant analysis



#=================================================================
h=hist(odi$Run,xlab="Runs Bin",ylab="Frequency",labels=TRUE)

h$density=h$counts/sum(h$counts)*100

plot(h,freq=FALSE,labels=TRUE)
mean(odi$Runs,na.rm = TRUE)
median(odi$Runs,na.rm = TRUE)
skew(odi$Runs,na.rm = TRUE)

#convert the postiive skewed data to normal distributed data

odi$runs_sqrt= sqrt(odi$Runs)

skew(odi$runs_sqrt,na.rm = TRUE)
mean(odi$runs_sqrt,na.rm = TRUE)
median(odi$runs_sqrt,na.rm = TRUE)

odi$runs_sqrt2=sqrt(odi$runs_sqrt)

skew(odi$runs_sqrt2,na.rm = TRUE)
mean(odi$runs_sqrt2,na.rm = TRUE)
median(odi$runs_sqrt2,na.rm = TRUE)

#outlier 

library(ggplot2)

dataset2=mpg
ds2=boxplot(dataset2$cty)

ds2

dataset2

ds3=mpg

ds4=ds3%>%group_by(class,cyl)%>%filter(cty<=quantile(cty,probs=c(0.75),na.rm = TRUE)+1.5*IQR(cty) & cty>=quantile(cty,probs=c(0.25),na.rm = TRUE)-1.5*IQR(cty) | n()==1)


ds5=ds3%>%group_by(class,cyl)%>%filter(cty<=quantile(cty,probs=c(0.75),na.rm = TRUE)+1.5*IQR(cty) & cty>=quantile(cty,probs=c(0.25),na.rm = TRUE)-1.5*IQR(cty) | n()==1)

ggplot(ds4,aes(class,cty))+geom_boxplot(aes(fill=as.factor(cyl)),outlier.color = 'Red',outlier.size = 2)

odi%>%group_by(MatchDate)%>%summarise(average_runs=sum(Runs)/2)%>%View



ggplot(hr, aes(x=RelationshipSatisfaction, fill = as.factor(Attrition))) + geom_histogram(bins = 3) + theme_bw() +
  scale_fill_manual(breaks = c("1", "2","3", "4"), values=c("green", "red","blue","yellow"))


