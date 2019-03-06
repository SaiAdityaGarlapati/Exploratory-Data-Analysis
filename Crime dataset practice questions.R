library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(tidyr)


setwd("C:/Users/Administrator/Documents/EDA/Assignment/rajanand-crime-in-india")


kidnap=read.csv("C:/Users/Administrator/Documents/EDA/Assignment/rajanand-crime-in-india/39_Specific_purpose_of_kidnapping_and_abduction.csv")

kidnap%>%group_by(ï..Area_Name,Group_Name)%>%summarise(total=sum(as.integer(K_A_Male_Total)))%>%View

str(kidnap)

View(kidnap$ï..Area_Name)

table(kidnap$Group_Name)




