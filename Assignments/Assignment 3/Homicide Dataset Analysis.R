#Homicide Data set USA
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
homicide=read_excel("C:/Users/Administrator/Documents/EDA/Assignment 3/homicide.xlsx")

homicide%>%head(10)
