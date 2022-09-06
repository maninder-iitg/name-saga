#--------------------Google Data Analytics Capstone---------------------

Maninder Singh
06/09/2022
#----------load libraries
install.packages("tidyverse")
library(tidyverse)
library(lubridate) #dates
library(readr) #load CSVs
library(ggplot2) #plotting
library(dplyr)

#----------Importing Data from 2021

JAN <- read_csv("data-set")
FEB <- read_csv("data-set")
MAR <- read_csv("data-set")
APR <- read_csv("data-set")
MAY <- read_csv("data-set")
JUN <- read_csv("data-set")
JUL <- read_csv("data-set")
AUG <- read_csv("data-set")
SEP <- read_csv("data-set")
OCT <- read_csv("data-set")
NOV <- read_csv("data-set")
DEC <- read_csv("data-set")

#----------Importing Data from 2021
data <- rbind(jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)

data <- mutate(data, trip_duration=as.numeric(trip_duration))

names(bike_data)[names(bike_data) == 'new_time_length(min)'] <- "trip_duration"

pie_chart<-bike_data%>%
group_by(member_casual) %>% # Variable to be transformed
count() %>%
ungroup() %>%
mutate(perc = (n / sum(n))) %>%
arrange(perc) %>%
mutate(labels = scales::percent(perc))

library(ggplot2) # this pie chart is to display %age of both memberships
pie_chart%>%
ggplot(aes(x="", y=perc, fill=member_casual)) +
geom_bar(stat="identity", width=1) +
coord_polar("y", start=0)+
geom_text(aes(label = labels),position = position_stack(vjust = 0.5)) +
coord_polar(theta = "y")+
labs(fill='Rider Group') +
ggtitle(label = 'Ride Percentage ', subtitle = 'Casual Riders vs Members') +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.subtitle = element_text(hjust = 0.5)) +
xlab(' ') + ylab(' ')

bike_data%>% # bar graph to show total rides
ggplot(aes(x=member_casual,fill=member_casual)) +
geom_bar() +
geom_text(stat='count', aes(label=..count..), vjust=0)+
labs(fill='Rider Group') +
ggtitle(label = 'Total Rides per Year', subtitle = 'Casual Riders vs Members') +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.subtitle = element_text(hjust = 0.5)) +
xlab('Rider Group') + ylab('Number of Rides')

bike_data %>% #Total Rides per Month
mutate(month = factor(month, levels = month.name) ) %>%
arrange(month)%>%
group_by(member_casual, month) %>%
summarise(number_of_rides = n()) %>%
arrange(member_casual, month) %>%
ggplot(aes(x = month,
y = number_of_rides,
fill = member_casual)) +
labs(fill='Rider Group') +
geom_col(position = "dodge") +
ggtitle(label = 'Total Rides per Month', subtitle = 'Casual Riders vs Members') +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.subtitle = element_text(hjust = 0.5)) +
xlab('Month') + ylab('Number of Rides') +
geom_text(aes(label = number_of_rides, hjust = 0),
position = position_dodge(width = 0.9),size = 3, angle = 90)+
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.5))

bike_data%>%#Total Rides per Day
group_by(member_casual,day)%>%
summarise(no_of_rides=n())%>%
arrange(member_casual,day)%>%
ggplot(aes(x=day,y=no_of_rides,fill=member_casual))+
labs(fill='Rider Group') +
geom_col(position = "dodge") +
ggtitle(label = 'Total Rides per Day', subtitle = 'Casual Riders vs Members') +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.subtitle = element_text(hjust = 0.5)) +
xlab('Day of Week') + ylab('Number of Rides') +
geom_text(aes(label = no_of_rides, hjust = "right"), position = position_dodge(width = 0.9),size = 3, angle = 90)+
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.5))

bike_data %>% #Average Trip Duration by Bike Type
group_by(member_casual, rideable_type) %>%
summarise(average_duration = mean(trip_duration/60, na.rm = TRUE)) %>%
arrange(member_casual, rideable_type) %>%
ggplot(aes(x = rideable_type,
y = average_duration,
fill = member_casual)) +
labs(fill='Rider Group') +
geom_col(position = "dodge") +
ggtitle(label = 'Average Trip Duration by Bike Type', subtitle = 'Casual Riders vs Members') +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.subtitle = element_text(hjust = 0.5)) +
xlab('Bike Type') + ylab('Trip Duration in Minutes') +
geom_text(aes(label = round(average_duration, digits = 2)), position = position_dodge(width = 0.9), vjust = 0)

pie_chart1<-bike_data%>%
group_by(member_casual,rideable_type) %>% # Variable to be transformed
count() %>%
ungroup() %>%
mutate(perc = (n / sum(n))) %>%
arrange(perc) %>%
mutate(labels = scales::percent(perc))

library(ggplot2) # this pie chart is to display %age of both memberships
library(scales)
blank_theme <- theme_minimal()+
theme(
axis.title.x = element_blank(),
axis.title.y = element_blank(),
panel.border = element_blank(),
panel.grid=element_blank(),
axis.ticks = element_blank(),
plot.title=element_text(size=14, face="bold")
)
pie_chart1%>%
ggplot(aes(x="", y=n, group=member_casual,fill=rideable_type)) +
scale_fill_brewer("Rider_Type") + blank_theme +
theme(axis.text.x=element_blank())+
geom_bar(stat="identity", width=1) +
geom_label(aes(label = paste(labels,member_casual)),check_overlap = TRUE,position = position_stack(vjust = 0.5)) +
coord_polar("y", start=0) +
labs(fill='Rider Group') +
ggtitle(label = 'Ride Percentage by Bike Type ', subtitle = 'Casual Riders vs Members') +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.subtitle = element_text(hjust = 0.5)) +
xlab(' ') + ylab(' ')

bike_data%>% #Average Trip Duration by Membership Type
group_by(member_casual) %>%
summarise(average_duration = mean(trip_duration/60, na.rm = TRUE)) %>%
arrange(member_casual) %>%
ggplot(aes(x = "",
y = average_duration,
fill = member_casual)) +
labs(fill='Rider Group') +
geom_col(position = "dodge") +
ggtitle(label = 'Average Trip Duration by Membership Type', subtitle = 'Casual Riders vs Members') +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.subtitle = element_text(hjust = 0.5)) +
xlab('Rider Type') + ylab('Trip Duration in Minutes') +
geom_text(aes(label = round(average_duration, digits = 2)), position = position_dodge(width = 0.9), vjust = 0)

bike_data%>%
mutate(month = factor(month, levels = month.name) ) %>%
group_by(member_casual,month) %>%
summarise(average_duration = mean(trip_duration/60, na.rm = TRUE)) %>%
arrange(member_casual,month) %>%
ggplot(aes(x = month ,
y = average_duration,
fill = member_casual)) +
labs(fill='Rider Group') +
geom_col(position = "dodge") +
ggtitle(label = 'Average Trip Duration by Month', subtitle = 'Casual Riders vs Members') +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.subtitle = element_text(hjust = 0.5)) +
xlab('Month') + ylab('Trip Duration in Minutes') +
geom_text(aes(label = round(average_duration, digits = 2), hjust = "right"),
position = position_dodge(width = 0.9),size = 3, angle = 90)+
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.5))

bike_data$day <- factor(bike_data$day, levels = c("MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY", "SUNDAY"))

bike_data%>%
group_by(member_casual,day) %>%
summarise(average_duration = mean(trip_duration/60, na.rm = TRUE)) %>%
arrange(member_casual,day) %>%
ggplot(aes(x = day ,
y = average_duration,
fill = member_casual)) +
labs(fill='Rider Group') +
geom_col(position = "dodge") +
ggtitle(label = 'Average Trip Duration by Weekday', subtitle = 'Casual Riders vs Members') +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.subtitle = element_text(hjust = 0.5)) +
xlab('Weekday') + ylab('Trip Duration in Minutes') +
geom_text(aes(label = round(average_duration, digits = 2), hjust = "right"),
position = position_dodge(width = 0.9),size = 3, angle = 90)+
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.5))

bike_data %>%
group_by(member_casual, rideable_type) %>%
summarise(no_of_rides=n())%>%
arrange(member_casual, rideable_type) %>%
ggplot(aes(x = rideable_type,
y = no_of_rides,
fill = member_casual)) +
labs(fill='Rider Group') +
geom_col(position = "dodge") +
ggtitle(label = 'Total No. of Trips by Bike Type', subtitle = 'Casual Riders vs Members') +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.subtitle = element_text(hjust = 0.5)) +
xlab('Bike Type') + ylab('No. of Trips') +
geom_text(aes(label = round(no_of_rides, digits = 2)), position = position_dodge(width = 0.9), vjust = 0)

bike_data %>%
group_by(member_casual,day) %>%
summarise(average_duration = mean(trip_duration/60, na.rm = TRUE)) %>%
arrange(member_casual,day) %>%
ggplot(aes(x = day ,
y = average_duration,
fill = member_casual)) +
labs(fill='Rider Group') +
geom_col(position = "dodge") +
ggtitle(label = 'Average Trip Duration by Weekday', subtitle = 'Casual Riders vs Members') +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.subtitle = element_text(hjust = 0.5)) +
xlab('Weekday') + ylab('Trip Duration in Minutes') +
geom_text(aes(label = round(average_duration, digits = 2), hjust = "right"),
position = position_dodge(width = 0.9),size = 3, angle = 90)+
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.5))

weekday_data1<- filter(bike_data,day=="MONDAY"| day=="TUESDAY"| day=="WEDNESDAY"
| day=="THURSDAY"| day=="FRIDAY")

weekday_data1%>%
group_by(member_casual) %>%
summarise(average_duration = mean(trip_duration/60, na.rm = TRUE)) %>%
arrange(member_casual) %>%
ggplot(aes(x = "",
y = average_duration,
fill = member_casual)) +
labs(fill='Rider Group') +
geom_col(position = "dodge") +
ggtitle(label = 'Average Trip Duration on Weekdays', subtitle = 'Casual Riders vs Members') +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.subtitle = element_text(hjust = 0.5)) +
xlab('Rider Type') + ylab('Trip Duration in Minutes') +
geom_text(aes(label = round(average_duration, digits = 2)), position = position_dodge(width = 0.9), vjust = 0)

weekend_data1<- filter(bike_data,day=="SATURDAY"| day=="SUNDAY")

weekend_data1%>%
group_by(member_casual) %>%
summarise(average_duration = mean(trip_duration/60, na.rm = TRUE)) %>%
arrange(member_casual) %>%
ggplot(aes(x = "",
y = average_duration,
fill = member_casual)) +
labs(fill='Rider Group') +
geom_col(position = "dodge") +
ggtitle(label = 'Average Trip Duration on Weekends', subtitle = 'Casual Riders vs Members') +
theme(plot.title = element_text(hjust = 0.5)) +
theme(plot.subtitle = element_text(hjust = 0.5)) +
xlab('Rider Type') + ylab('Trip Duration in Minutes') +
geom_text(aes(label = round(average_duration, digits = 2)), position = position_dodge(width = 0.9), vjust = 0)
