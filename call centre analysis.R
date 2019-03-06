library(dplyr)
library(moments)
library(psych)
library(ggplot2)
library(readxl)

calling=read_excel("C:/Users/Administrator/Documents/Exploratory Data Analysis/Quiz/Call Centre Analysis- Data set/Calling Data.xlsx")
lead_source_id=read_excel("C:/Users/Administrator/Documents/Exploratory Data Analysis/Quiz/Call Centre Analysis- Data set/ID Table.xlsx",sheet=1)
course_id=read_excel("C:/Users/Administrator/Documents/Exploratory Data Analysis/Quiz/Call Centre Analysis- Data set/ID Table.xlsx",sheet=2)
enrollment=read_excel("C:/Users/Administrator/Documents/Exploratory Data Analysis/Quiz/Call Centre Analysis- Data set/Enrollment Data.xlsx")
leads=read_excel("C:/Users/Administrator/Documents/Exploratory Data Analysis/Quiz/Call Centre Analysis- Data set/Leads.xlsx")



Q1= merge(x=Match[,c("Match_Id","Match_Winner_Id","IS_Result","Toss_Winner_Id")],y=Team[,c("Team_Id","Team_Name")],by.x = "Match_Winner_Id",by.y = "Team_Id",all.x = TRUE)


str(leads)

leads$newcallback=as.Date(leads$`Callback/Followup Date`,"%d-%m-%Y",na.rm=TRUE)

leads$newrecent=as.Date(leads$`Recent Enquired Date`,"%d-%m-%Y",na.rm=TRUE)

q1=leads%>%filter(!is.na(newcallback),!is.na(newrecent))%>%group_by(`Lead ID`)%>%summarise(diffdays=min(newcallback)-min(newrecent))%>%ungroup()%>%filter(diffdays>0)%>%summarise(meandays=mean(diffdays))


q1=leads%>%filter(!is.na(newcallback),!is.na(newrecent))%>%group_by(`Lead ID`)%>%summarise(diffdays=min(newcallback)-min(newrecent))%>%filter(diffdays>0)

min=quantile(q1$diffdays,probs = 0.025,na.rm = TRUE)

min

max=quantile(q1$diffdays,probs = 0.0975,na.rm = TRUE)

max

q2=merge(x=leads,y=calling,by.x = "Lead ID",by.y = "Lead ID",all.x = TRUE)

q2%>%group_by(`Lead ID`,`Assigned To`)%>%summarise(sumofcalls=sum(!is.na(call_date)))%>%group_by(`Assigned To`)%>%summarise(zerocalltimes=sum(sumofcalls==0))%>%View

q2%>%group_by(`Lead ID`)%>%summarise(sumofcalls=sum(!is.na(call_date)))%>%summarise(zerocalltimes=sum(sumofcalls==0))%>%View


View(q2)

q3=merge(x=leads,y=lead_source_id,by.x = "Lead Source ID",by.y = "Source ID",all.x = TRUE)

q3%>%filter(!is.na(`Lead Source ID`))%>%group_by(`Source Name`)%>%summarise(conversion_ratio=sum(Status=='Admission Taken',na.rm = TRUE)/n())%>%View



q5=merge(x=enrollment,y=calling,by.x="Lead ID",by.y="Lead ID",all.x = TRUE)

q5%>%filter(Total>=100000)%>%group_by(`Lead ID`)%>%summarise(sumofcalls=sum(length_in_sec>=0))%>%summarise(meancalltimes=mean(sumofcalls,na.rm=TRUE))%>%View



