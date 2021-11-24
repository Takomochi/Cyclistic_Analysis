install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("tinytex")

tinytex::install_tinytex()
library(tidyverse)
library(skimr)
library(janitor)

q1_2020 <- read.csv("R/bike_sharing/csv_file/Divvy_Trips_2020_Q1.csv")
q4_2019 <- read.csv("R/bike_sharing/csv_file/Divvy_Trips_2019_Q4.csv")
q3_2019 <- read.csv("R/bike_sharing/csv_file/Divvy_Trips_2019_Q3.csv")
q2_2019 <- read.csv("R/bike_sharing/csv_file/Divvy_Trips_2019_Q2.csv")

# Change value "Subscriber or Customer" to "member or casual" (2019 data)
q4_2019$member_casual <- gsub("Subscriber", "member",q4_2019$member_casual)
q4_2019$member_casual <- gsub("Customer","casual",q4_2019$member_casual)

q3_2019$member_casual <- gsub("Subscriber", "member",q3_2019$member_casual)
q3_2019$member_casual <- gsub("Customer","casual",q3_2019$member_casual)

q2_2019$member_casual <- gsub("Subscriber", "member",q2_2019$member_casual)
q2_2019$member_casual <- gsub("Customer","casual",q2_2019$member_casual)

# Check data types
glimpse(q1_2020)
glimpse(q4_2019)
glimpse(q3_2019)
glimpse(q2_2019)

# Change datatypes for 2019 data to be combined
q4_2019$ride_id <- as.character(q4_2019$ride_id)
q3_2019$ride_id <- as.character(q3_2019$ride_id)
q2_2019$ride_id <- as.character(q2_2019$ride_id)


# Combine four dataframes
library("dplyr")
all_data <- bind_rows(q2_2019,q3_2019,q4_2019,q1_2020)


# Add columns
all_data$ride_length <- difftime(all_data$ended_at,all_data$started_at)/60
all_data$ride_length <- as.integer(all_data$ride_length)
all_data$month <- as.Date(all_data$started_at,format = "%Y-%m-%d")
all_data$month <- format(all_data$month, "%m")
all_data$year <- as.Date(all_data$started_at,format = "%Y-%m-%d")
all_data$year <- format(all_data$year, "%Y") 
all_data$year <- as.integer(all_data$year)
all_data$age <- all_data$year - all_data$birthyear

View(all_data)

# Riders for Each weekday grouped by member/casual
day_week <- c( 'Sun','Mon', 'Tue', 'Wed', 'Thu', 'Fri', 
             'Sat')
library(ggplot2)
ggplot(data=all_data)+
  geom_bar(mapping = aes(x=day_of_week,fill=member_casual))+
  labs(title="Riders for day of the week") + 
  xlim(day_week)

# Each Month for member and casual riders 
num_month <- all_data %>% 
  group_by(month) %>% 
  count(member_casual)

num_month <- data.frame(num_month)
ggplot(data = num_month)+
  geom_line(mapping = aes(x=month, y=n, group=member_casual,color=member_casual), size=1.3)+
  labs(title = "Riders for each month")


# Average duration(minutes) for member and casual 
average_length <-all_data %>% 
  group_by(member_casual) %>% 
  summarize(average_ride_duration = mean(ride_length))

ggplot(data=average_length)+
  geom_bar(stat = "identity",mapping = aes(x=member_casual,y=average_ride_duration,fill=member_casual))+
  labs(title="Average ride length grouped by Member and Casual riders")


# Member/Casual by Gender
p <- all_data %>% 
  drop_na(gender) %>% 
  group_by(member_casual) %>% 
  count(gender)

p<- data.frame(p)
p <- p[p$gender!= "",]

ggplot(data=p)+
  geom_bar(stat="identity",mapping = aes(x=member_casual,y=n,fill=gender),position="dodge")+
  labs(title = "Number of each user by gender")


# Number of member/casual riders grouped by end station(Destination)
num_station <- all_data %>% 
  group_by(end_station_name) %>% 
  count(member_casual,sort = (decreasing = TRUE))

#Create data frame
num_station <- data.frame(num_station)

top_member <- subset(num_station,member_casual == "member") %>% 
  top_n(5)
top_casual <- subset(num_station,member_casual == "casual") %>% 
  top_n(5)
  
ggplot(data=top_member)+
  geom_bar(stat = "identity",mapping=aes(x=end_station_name,y=n),fill='#1569C7')+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title="Top 5 destinations for Member")

ggplot(data=top_casual)+
  geom_bar(stat = "identity",mapping=aes(x=end_station_name,y=n),fill='#FFA500')+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title="Top 5 destinations for Casual")

# Histogram by age
ggplot(data=all_data)+
  geom_histogram(mapping = aes(x=age,fill=member_casual),position = "dodge",bins = 30)+
  scale_x_continuous(limits = c(0, 80))+
  labs(title="Distribution of user's age")+
  xlim(10,80)
