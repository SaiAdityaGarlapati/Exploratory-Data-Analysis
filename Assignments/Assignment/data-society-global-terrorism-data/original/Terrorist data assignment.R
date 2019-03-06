#global terrorist data set
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(tidyr)
library(gridExtra)

setwd("C:/Users/Administrator/Documents/EDA/Assignment/data-society-global-terrorism-data/original")
files <- list.files(path = "C:/Users/Administrator/Documents/EDA/Assignment/data-society-global-terrorism-data/original",pattern = ".csv")
temp <- lapply(files, fread, sep=",")
terrorist_data <- rbindlist( temp,fill=TRUE )

View(terrorist_data)


sum(is.na(terrorist_data$attacktype1))

q1_td=terrorist_data%>%group_by(iyear)%>%summarise(No_of_Terrorist_attacks=n())

View(q1_td)

ggplot(q1_td,aes(x=iyear,y=No_of_Terrorist_attacks))+geom_bar(stat = "Identity",aes(fill=as.factor(iyear)))

q1_ggplot=ggplot(q1_td,aes(x=iyear,y=No_of_Terrorist_attacks))+
  geom_area(fill="light blue")+
  geom_point(col="black",size=0.9)+
  geom_text(size=3,fontface='bold',
            aes(label = No_of_Terrorist_attacks),
            position = position_dodge(width = .9), vjust = -0.25)
#===================================================
q2_td=terrorist_data%>%filter(attacktype1_txt=="Bombing/Explosion")%>%group_by(iyear)%>%summarise(no_of_terrorist_bombings=n())

q2_ggplot=ggplot(q2_td,aes(x=iyear,y=no_of_terrorist_bombings))+
  geom_area(fill="light green")+
  geom_point(size=0.9)+
  geom_text(size=3,fontface='bold',
            aes(label = no_of_terrorist_bombings),
            position = position_dodge(width = .9), vjust = -0.25)
 
q2_ggplot

grid.arrange(q1_ggplot,q2_ggplot)

#===================================================

q3_td=terrorist_data%>%filter(region_txt!="Russia & the Newly Independent States (NIS)")%>%group_by(region_txt,iyear)%>%summarise(No_of_attacks_each_year=n())

View(q3_td)

ggplot(q3_td,aes(x=iyear,y=No_of_attacks_each_year))+
  geom_area(aes(fill = as.factor(region_txt)))+
  theme_bw() + facet_wrap(~region_txt, scales="free",ncol=3)+
  geom_point(col="black",size=0.9)
  

q4_td=terrorist_data%>%group_by(region_txt,attacktype1_txt)%>%
  summarise(No_of_attacks=n())%>%
  top_n(5)
View(q4_td)

ggplot(q4_td,aes(x=attacktype1_txt,y=No_of_attacks))+
  geom_bar(stat = "Identity",width = .5, aes(fill = as.factor(attacktype1_txt)))+
  theme_bw() + facet_wrap(~region_txt, scales="free",ncol=3)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

q5_td=terrorist_data%>%
  group_by(targtype1_txt)%>%summarise(maximum_kills=sum(nkill,na.rm = TRUE),maximum_wounded=sum(nwound,na.rm = TRUE))

View(q5_td)

q5_td%>%gather(-targtype1_txt,key="var",value="value")%>%ggplot(aes(x=targtype1_txt,y=value))+
  geom_bar(stat = "Identity",aes(fill=as.factor(targtype1_txt))) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  geom_text(size=3,fontface='bold',
            aes(label = targtype1_txt),
            position = position_dodge(width = .9),angle=90, vjust = -0.4)


q6_td=terrorist_data%>%filter(iyear!=1970)%>%
  filter(country_txt=="India" | country_txt=="Pakistan")%>%
  group_by(iyear,country_txt)%>%summarise(No_of_Attacks=n())

View(q6_td)

ggplot(q6_td,aes(x=iyear,y=No_of_Attacks))+
  geom_area(aes(fill=as.factor(country_txt)))+
  theme_bw() + facet_wrap(~country_txt,scales = "free")

#=================================================================
q2_select=terrorist_data%>%select(iyear,attacktype1_txt,attacktype2_txt,attacktype3_txt)

q2_gather=gather(q2_select,attack_type,attack,attacktype1_txt:attacktype3_txt,factor_key=TRUE)

dim(q2_gather)

q2_td=q2_gather%>%filter(attack=="Bombing/Explosion")%>%group_by(iyear)%>%summarise(no_of_terrorist_bombings=n())

ggplot(q2_td,aes(x=iyear,y=no_of_terrorist_bombings))+geom_bar(stat = "Identity",aes(fill=as.factor(iyear)))

#=================================================================

q4_select=terrorist_data%>%select(region_txt,attacktype1_txt,attacktype2_txt,attacktype3_txt)
q4_gather=gather(q4_select,attack_type,attack,attacktype1_txt:attacktype3_txt,factor_key=TRUE)

q4_td=q4_gather%>%filter(!is.na(attack),attack!=".")%>%group_by(region_txt,attack)%>%
  summarise(No_of_attacks=n())%>%
  top_n(5)

View(q4_td)

ggplot(q4_td,aes(x=attack,y=No_of_attacks))+
  geom_bar(stat = "Identity",width = .5, aes(fill = as.factor(attack)))+
  theme_bw() + facet_wrap(~region_txt, scales="free",ncol=3)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
#=================================================================
# q5_select=terrorist_data%>%select(targtype1_txt,targtype2_txt,targtype3_txt,nkill,nwound)
#q5_gather=gather(q5_select,target_type,target,targtype1_txt:targtype3_txt,factor_key=TRUE)

#q5_td=q5_gather%>%filter(!is.na(target),target!=".")%>%
# group_by(target)%>%summarise(maximum_kills=sum(nkill,na.rm = TRUE),maximum_wounded=sum(nwound,na.rm = TRUE))


#View(q5_td)

#q5_td%>%gather(-target,key="var",value="value")%>%ggplot(aes(x=target,y=value))+
#  geom_bar(stat = "Identity",aes(fill=as.factor(target))) +
#  facet_wrap(~ var, scales = "free") +
#  theme_bw()+
#  theme(axis.title.x=element_blank(),
#       axis.text.x=element_blank(),
#       axis.ticks.x=element_blank())

#==================================================================
q7_td=terrorist_data%>%filter(iyear!=1970)%>%
  filter(country_txt=="Russia"| country_txt=="Soviet Union" | country_txt=="United States")%>%
  group_by(iyear,country_txt)%>%
  summarise(No_of_Attacks=n())

View(q7_td)

ggplot(q7_td,aes(x=iyear,y=No_of_Attacks))+
  geom_bar(stat = "Identity",width = .5,aes(fill=as.factor(country_txt)))+
  theme_bw() + facet_wrap(~country_txt,scales = "free")

ggplot(q7_td,aes(x=iyear,y=No_of_Attacks))+
  geom_bar(stat = "Identity",width = .5,aes(fill=as.factor(country_txt)))+
  theme_bw()

View(terrorist_data)
#===================================================================
q8_select=terrorist_data%>%select(country_txt,nkill)
#q5_gather=gather(q5_select,target_type,target,targtype1_txt:targtype3_txt,factor_key=TRUE)

q8_td=q8_select%>%filter(!is.na(country_txt),country_txt!=".")%>%
  group_by(country_txt)%>%summarise(maximum_kills=sum(nkill,na.rm = TRUE))%>%
  arrange(-maximum_kills)%>%
  head(10)

View(q8_td)

ggplot(q8_td,aes(x=country_txt,y=maximum_kills))+
  geom_bar(stat="Identity",width=0.5,aes(fill=as.factor(country_txt)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#===================================================================
q9_select=terrorist_data%>%
  select(country_txt,iyear,targtype1_txt,targtype2_txt,
         targtype3_txt,attacktype1_txt,attacktype2_txt,attacktype3_txt,nkill)

q9_gather1=gather(q9_select,target_type,target,targtype1_txt:targtype3_txt,factor_key=TRUE)
q9_gather=gather(q9_gather1,attack_type,attack,attacktype1_txt:attacktype3_txt,factor_key=TRUE)

q9_td=terrorist_data%>%
  filter(!is.na(nkill))%>%
  group_by(iyear,attacktype1_txt)%>%
  summarise(no_of_casualities=sum(nkill))

View(q9_td)

dim(q9_gather)

ggplot(q9_td,aes(x=iyear,y=no_of_casualities))+
  geom_bar(stat="identity",aes(fill=as.factor(attacktype1_txt)))


ggplot(q9_td,aes(x=iyear,y=no_of_casualities))+geom_bar(stat="identity",aes(fill=as.factor(attacktype1_txt)),position = "fill")

ggplot(q9_td,aes(x=iyear,y=no_of_casualities))+geom_line(stat="identity",aes(col=as.factor(attacktype1_txt)),size=1)

#===================================================================

q10_select=terrorist_data%>%select(iyear,weaptype1_txt,nkill)

q10_td=q10_select%>%
  group_by(weaptype1_txt)%>%
  summarise(Total_Kills=sum(nkill,na.rm = TRUE))%>%
  arrange(-Total_Kills)

q10_head6=q10_td%>%head(6)

q10_tail6=q10_td%>%tail(6)

a10=ggplot(q10_head6,aes(x=weaptype1_txt,y=Total_Kills))+
  geom_bar(stat="identity",aes(fill=as.factor(weaptype1_txt)))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.position="bottom")

b10=ggplot(q10_tail6,aes(x=weaptype1_txt,y=Total_Kills))+
  geom_bar(stat="identity",aes(fill=as.factor(weaptype1_txt)))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.position="bottom")

grid.arrange(a10,b10)



View(q10_tail6)

View(q10_head6)
#===========================================================
q11_select=terrorist_data%>%select(iyear,natlty1_txt,nkill)



q11_td=q11_select%>%
  group_by(natlty1_txt)%>%
  summarise(Total_Kills=sum(nkill,na.rm = TRUE))%>%
  arrange(-Total_Kills)

View(q11_td)

q11=ggplot(q11_td,aes(x=natlty1_txt,y=Total_Kills))+
  geom_point(stat="identity",aes(fill=as.factor(natlty1_txt)))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.position="none")

ggplotly(q11)
#===============================================================

q12_basic=terrorist_data%>%
  group_by(country_txt)%>%
  filter(doubtterr==0)%>% 
  summarise(safest_countries=sum(nkill==0,na.rm = TRUE))%>%
  arrange(-safest_countries)%>%head(15)

View(q12_basic)

q12=terrorist_data%>%
  group_by(attacktype1_txt,country_txt)%>%
  filter(doubtterr==0)%>% 
  summarise(safest_countries=sum(nkill==0,na.rm = TRUE))%>%
  top_n(5)
  
View(q12)

q12_basic_ggplot=ggplot(q12_basic,aes(x=country_txt,y=safest_countries))+
  geom_bar(stat = "Identity",width = .9, aes(fill = as.factor(country_txt)))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplotly(q12_basic_ggplot)

q12_ggplot=ggplot(q12,aes(x=country_txt,y=safest_countries))+
  geom_bar(stat = "Identity",width = .5, aes(fill = as.factor(country_txt)))+
  theme_bw() + facet_wrap(~attacktype1_txt, scales="free",ncol=3)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplotly(q12_ggplot)
#==============================================================

#causalities according to group type:

q9_td=terrorist_data%>%filter(gname!="Unknown")%>%group_by(gname) %>%
  summarise(total.casualties = sum(nkill+nwound, na.rm = TRUE))%>%
  arrange(-total.casualties)%>%
  head(6)

View(q9_td)

terrorist_data$gname

q9_ggplot=ggplot(q9_td,aes(x=gname,y=total.casualties))+
  geom_bar(stat="identity",aes(fill = as.factor(gname)))+
  labs(subtitle="X-Axis:Year\nY-Axis:Total casualties", 
     y="Total casualties", x="Year", title="Total casualties over the years", 
     caption = "Source: Global Terrorism Database")

q9_ggplot


#===========================================================

table(terrorist_data$alternative_txt)


conflict=terrorist_data%>%filter(!is.na(INT_ANY))%>%group_by(iyear,INT_ANY) %>%
  summarise(total.casualties = sum(nkill+nwound, na.rm = TRUE))
  

View(conflict)

ggplot(conflict,aes(x=iyear,y=total.casualties))+
  geom_line(size=1,stat="identity",aes(col=as.factor(INT_ANY)))+geom_point()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#============================================

q11_select=terrorist_data%>%select(iyear,natlty1_txt,nkill)



q11_td=q11_select%>%
  group_by(natlty1_txt)%>%
  summarise(Total_Kills=sum(nkill,na.rm = TRUE))%>%
  arrange(-Total_Kills)%>%head(10)


q11=ggplot(q11_td,aes(x=natlty1_txt,y=Total_Kills))+
  geom_bar(stat="identity",aes(fill=as.factor(natlty1_txt)))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(q11_td,aes(x=natlty1_txt,y=Total_Kills))+
  geom_bar(stat="identity",aes(fill=as.factor(natlty1_txt)))+
  facet_wrap(~as.factor(INT_MISC), scales="free",ncol=3)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
#==============================================================

q11_select=terrorist_data%>%select(natlty1_txt,nkill,INT_MISC)



q11_td=q11_select%>%filter(INT_MISC!= -9,!is.na(INT_MISC))%>%
  group_by(natlty1_txt,INT_MISC)%>%
  summarise(Total_Kills=sum(nkill,na.rm = TRUE))%>%
  ungroup%>%
  group_by(INT_MISC)%>%
  top_n(10)%>%arrange(-INT_MISC)

View(q11_td)


q11=ggplot(q11_td,aes(x=natlty1_txt,y=Total_Kills))+
  geom_bar(stat="identity",aes(fill=as.factor(natlty1_txt)))+
  facet_wrap(~as.factor(INT_MISC), scales="free",ncol=3)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

q11
