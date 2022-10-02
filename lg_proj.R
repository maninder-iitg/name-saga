library(readr)
Data <- read_csv("Downloads/BDA_Data - BDA.csv")
View(Data)
library(tidyverse)
brand<-Data%>%
  group_by(Brand,Quarter)%>%
  summarise(total_dur=sum(Duration),.groups='drop')

Data<-Data%>%
    Data$Quarter=quarter(Data$Date, type = "year.quarter")

brand1 %>% 
  ggplot(aes(x = Quarter, 
             y = total_dur, 
             fill = Brand)) + 
  labs(fill='Brand') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Share of various brands in TV airings', subtitle = 'Ad Duration vs Brand') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Quarter') + ylab('Ad Duration in hrs')+
  geom_text(aes(label = total_dur, hjust = "left"), position = position_dodge(width = 0.9),size = 3, angle = 90)

pie_chart<-brand%>%
  group_by(Brand,total_dur) %>% # Variable to be transformed
  ungroup() %>%
  mutate(perc = (total_dur / sum(total_dur))) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

pie_chart2<-pie_chart%>%
  group_by(Brand) %>% # Variable to be transformed
  ungroup() %>%
  mutate(perc = sum(pie_chart$perc))

dat_labels%>%
  ggplot(aes(x="", y=total_dur, fill=Brand)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = paste(labels)),position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  labs(fill='Brands') +
  ggtitle(label = 'Share of various brands in TV airings', subtitle = 'Percentage of Ad Duration by Brand') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab(' ') + ylab(' ')+
  facet_wrap(.~ Brand)

 


library(dplyr)
dat_labels <- pie_chart %>% group_by(Brand) %>% 
  summarise(total_dur=sum(perc),.groups='drop')%>%
  mutate(labels = scales::percent(total_dur))


brand2<-Data%>%
  group_by(Brand,Quarter)%>%
  summarise(total_dur=sum(EQ_Units),.groups='drop')


names(Data)[names(Data) == 'EQ Units'] <- 'EQ_Units'


brand1 %>% 
  ggplot(aes(x = Quarter, 
             y = total_dur, 
             fill = Brand)) + 
  labs(fill='Brand') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Share of various brands in TV airings', subtitle = 'EQ Units vs Brand') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Quarter') + ylab('Sum of EQ Units')+
  geom_text(aes(label = paste(round(total_dur,0)), hjust = "middle",vjust=0), position = position_dodge(width = 0.9),size = 2, angle = 0)+
  geom_text(aes(label = paste(Brand), vjust = -1.2), position = position_dodge(width = 0.9),size = 2, angle = 0)


pie_chart1<-brand2%>%
  group_by(Brand,total_dur) %>% # Variable to be transformed
  ungroup() %>%
  mutate(perc = (total_dur / sum(total_dur))) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))


dat_labels2 <- pie_chart1 %>% group_by(Brand) %>% 
  summarise(total_dur=sum(perc),.groups='drop')%>%
  mutate(labels = scales::percent(total_dur))

dat_labels2%>%
  ggplot(aes(x="", y=total_dur, fill=Brand)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label = paste(labels)),position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  labs(fill='Brands') +
  ggtitle(label = 'Share of various brands in TV airings', subtitle = 'Percentage of Ad Duration by Brand') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab(' ') + ylab(' ')



brand3<-Data%>%
  group_by(Brand,Quarter)%>%
  summarise(total_dur=sum(`Spend ($)`
                          ),.groups='drop')
brand3%>%
  ggplot(aes(x=Quarter,
             y=total_dur/1000000,
             fill=Brand))+
  labs(fill="Brand")+
  geom_col(position = "dodge")+
  ggtitle(label='Share of various brands in TV airings',subtitle = 'Expenditure on Ads vs Brand')+
  theme(plot.title = element_text(hjust=0.5) )+
  theme(plot.subtitle = element_text(hjust=0.5))+
  xlab('Quarter')+ylab("Expenditure in US dollars (millions)")+
  geom_text(aes(label=round(total_dur/1000000,1),hjust='middle',vjust=-0.25),position=position_dodge(width=0.9),angle=0,size=3)+
  facet_wrap(.~Brand)




library(ggplot2)

ggplot(brand6, aes(x=Brand, y='Pod Position', fill=total_dur)) +  
  ggtitle(label='Share of various brands in TV airings',subtitle = 'Expenditure on Ads vs Brand')+
  geom_tile() +
  scale_fill_gradient(low="white", high="darkgreen", name="Total Expenditure")


brand4<-Data%>%
  group_by(Brand,`EQ Units`,Dayparts)%>%
  summarise(total_dur=sum(`EQ Units`),.groups='drop')

brand4$Dayparts <- factor(brand4$Dayparts, levels = c("EARLY MORNING","DAYTIME","EARLY FRINGE",
                                                      "PRIME ACCESS","EVENING NEWS","OVERNIGHT",
                                                      "LATE FRINGE","PRIME TIME","WEEKEND"))


brand4 %>% 
  ggplot(aes(x = Brand, 
             y = total_dur, 
             fill = Dayparts)) + 
  labs(fill='Dayparts') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Share of various brands in TV airings', subtitle = 'EQ Units vs Brand') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Brand') + ylab('Sum of EQ Units')+
  geom_text(aes(label = "", hjust = "left"), position = position_dodge(width = 0.9),size = 3, angle = 90)



brand7 %>% 
  ggplot(aes(x = Brand, 
             y = total_dur/1000000, 
             fill = `Pod Position`)) +
  geom_line(aes(color=`Pod Position`))+
  geom_point(aes(color=`Pod Position`))+
  ggtitle(label = 'Share of various Time Periods in TV airings', subtitle = 'Expenditure on Ads for Toyota') + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Quarter') + ylab('Expenditure in US dollars (millions)')+
  geom_text(aes(label='', hjust = "middle"), position = position_dodge(width = 0.9),size = 3, angle = 0)+
  facet_grid(. ~ Brand)


brand7<-brand6%>%
  subset(`Pod Position`<6)

ggplot(brand7, aes(x=Brand, y='Pod Position', fill=total_dur)) +  
  ggtitle(label='Share of various brands in TV airings',subtitle = 'Expenditure on Ads vs Brand')+
  geom_tile() +
  scale_fill_gradient(low="white", high="darkgreen", name="Total Expenditure")

brand7 %>% 
  ggplot(aes(x = `Pod Position`,
             y = total_dur/1000000, 
             fill = Brand)) + 
  labs(fill='Brand') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Share of various brands in TV airings', subtitle = 'Expenditure on Ads') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Pod Position') + ylab('Expenditure in US dollars (millions)')+
  geom_text(aes(label=round(total_dur/1000000,1),vjust=-0.25), position = position_dodge(width = 0.9),size = 3, angle =0 )+
  facet_wrap(. ~ Brand)

names(Data)[names(Data) == 'Day Of Week'] <- 'Day_Of_Week'

Data$Week_Day<-wday(Data$Day_Of_Week, label=TRUE)

library(lubridate)
day<-wday(Data$Day_Of_Week, label=TRUE)


brand8<-Data%>%
  group_by(Brand,Week_Day)%>%
  summarise(total_dur=sum(`Spend ($)`),.groups='drop')


brand8 %>% 
  ggplot(aes(x = Week_Day,
             y = total_dur/1000000, 
             fill = Brand)) + 
  labs(fill='Brand') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Share of various brands in TV airings', subtitle = 'Expenditure on Ads') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Week Day') + ylab('Expenditure in US dollars (millions)')+
  geom_text(aes(label=round(total_dur/1000000,1),vjust=-0.25), position = position_dodge(width = 0.9),size = 3, angle =0 )+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=0.5))+
  facet_wrap(~ Brand)






brand9<-Data%>%
  group_by(Brand,`Broadcast Month`)%>%
  summarise(total_dur=sum(`Spend ($)`),.groups='drop')


brand9$`Broadcast Month` <- factor(brand9$`Broadcast Month`, levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL","AUG","SEP","OCT","NOV","DEC"))


brand9%>% 
  ggplot(aes(x = `Broadcast Month`,
             y = total_dur/1000000, 
             fill = `Broadcast Month`)) + 
  labs(fill='Broadcast Month') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Share of various brands in TV airings', subtitle = 'Expenditure on Ads') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Month') + ylab('Expenditure in US dollars (millions)')+
  geom_text(aes(label=round(total_dur/1000000,1),vjust=-0.25), position = position_dodge(width = 0.9),size = 3, angle =0 )+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=0.5))+
  facet_wrap(~ Brand)




Data%>%
  subset(Brand=="Lexus")%>%
  summarise(total_dur=sum(Duration)/3600)

brand<-Data%>%
  group_by(Brand)%>%
  summarise(total_dur=sum(`EQ Units`),.groups='drop')

brand6<-Data%>%
  group_by(Brand,)%>%
  summarise(total_dur=sum(`Spend ($)`)/1000000,.groups='drop')


brand7<-Data%>%
  group_by(Show)%>%
  summarise(Sum_EQ_Units=sum(`EQ Units`),.groups='drop')




brand8<-brand7%>%
  group_by(Show,Sum_EQ_Units) %>% # Variable to be transformed
  ungroup() %>%
  mutate(perc = (Sum_EQ_Units / sum(Sum_EQ_Units))) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

brand9<-brand8%>%
  arrange(desc(perc))%>%
  top_n(10)

library(ggplot2)
# Basic scatter plot
brand9%>%
  arrange(desc(perc))%>%
  ggplot(aes(x = Show,
                    y = perc*100,fill=Show))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.5))+
  xlab('') + ylab('Percentage of EQ Units')+
  geom_col(position = "dodge")
  
  



final2<-Data%>%
  group_by(`Network`)%>%
  summarise(Sum_EQ_Units=sum(`EQ Units`),.groups='drop')

final3<-final2%>%
  mutate(perc = (Sum_EQ_Units / sum(Sum_EQ_Units))) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

final3<-final3%>%
  arrange(desc(perc))%>%
  top_n(10)

library(ggplot2)
# Basic scatter plot
final3%>%
  ggplot(aes(x = Network, y = perc*100,fill=Network))+
  labs(fill='Broadcast Network') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.5))+
  xlab('') + ylab('Percentage of EQ Units')+
  geom_col(position = "dodge")
