setwd("C:/Users/Administrator/Downloads")
install.packages('ROracle_1.2-1.zip', repos = NULL)


library(DBI)
library(dplyr)
library(dbplyr)
library(ROracle)
drv <- dbDriver("Oracle")
con <- dbConnect(drv, "hr", "hr")

q1<-dbGetQuery(con,'select sum(noofseats) as Total_Seats,sum(noofseats*seatfare) as Total_fare from bookingdetail 
                join seattype using (seattypeid) where bookingid=60001')
View(q1)
