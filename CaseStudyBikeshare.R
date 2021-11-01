#First step I did is to add all the necessary libraries for the analysis

library("tidyverse")
library("ggplot2")
library("lubridate")
library("geosphere")
library("gridExtra") 
library("ggmap")
library("readr")
library("dplyr")

#load all the data to their respective variables
tripdata_202006 <- read_csv("202006-divvy-tripdata.csv")
tripdata_202007 <- read_csv("202007-divvy-tripdata.csv")
tripdata_202008 <- read_csv("202008-divvy-tripdata.csv")
tripdata_202009 <- read_csv("202009-divvy-tripdata.csv")
tripdata_202010 <- read_csv("202010-divvy-tripdata.csv")
tripdata_202011 <- read_csv("202011-divvy-tripdata.csv")
tripdata_202012 <- read_csv("202012-divvy-tripdata.csv")
tripdata_202101 <- read_csv("202101-divvy-tripdata.csv")
tripdata_202102 <- read_csv("202102-divvy-tripdata.csv")
tripdata_202103 <- read_csv("202103-divvy-tripdata.csv")
tripdata_202104 <- read_csv("202104-divvy-tripdata.csv")
tripdata_202105 <- read_csv("202105-divvy-tripdata.csv")


#Then  join all the data from 2020 and change the type of two columns (start_station_id,end_station_id) to match the 2021 data:


tripdata_withDoubles <- rbind(tripdata_202006,tripdata_202007,tripdata_202008,tripdata_202009,tripdata_202010,tripdata_202011,tripdata_202012)

tripdata_withDoubles <-  mutate(tripdata_withDoubles, start_station_id = as.character(start_station_id)
                                ,end_station_id = as.character(end_station_id)) 


tripdata_withChar <- rbind(tripdata_202101,tripdata_202102,tripdata_202103,tripdata_202104,tripdata_202105)

#Then  join all the data:


#Lets see that data:
print("####### GLIMPSE #######")
glimpse(all_tripdata)
print("####### SUMMARY #######")
summary(all_tripdata)

#Now lets clean the data to be able to properly work with it:

#Fist we drop all NA:

all_tripdata_clean <- drop_na(all_tripdata)

#Then lets create some new columns.

#First lets separate the dates into month, day, year and day of the week:

all_tripdata_clean$date <- as.Date(all_tripdata_clean$started_at) 
all_tripdata_clean$month <- format(as.Date(all_tripdata_clean$date), "%m")
all_tripdata_clean$day <- format(as.Date(all_tripdata_clean$date), "%d")
all_tripdata_clean$year <- format(as.Date(all_tripdata_clean$date), "%Y")
all_tripdata_clean$day_of_week <- format(as.Date(all_tripdata_clean$date), "%A")

#Then lets make some useful new columns with the duration of the ride, distance traveled, and speed:

#First the ride length in seconds:
all_tripdata_clean$ride_length <- difftime(all_tripdata_clean$ended_at,all_tripdata_clean$started_at)

#Then the ride distance traveled in km
all_tripdata_clean$ride_distance <- distGeo(matrix(c(all_tripdata_clean$start_lng, all_tripdata_clean$start_lat), ncol = 2), matrix(c(all_tripdata_clean$end_lng, all_tripdata_clean$end_lat), ncol = 2))
all_tripdata_clean$ride_distance <- all_tripdata_clean$ride_distance/1000

#At last the speed in Km/h
all_tripdata_clean$ride_speed = c(all_tripdata_clean$ride_distance)/as.numeric(c(all_tripdata_clean$ride_length), units="hours")


all_tripdata_clean <- all_tripdata_clean[!(all_tripdata_clean$start_station_name == "HQ QR" | all_tripdata_clean$ride_length<0),]


```{r}
#Fist we calculate the average distance, distance for both the casual and member type users:

userType_means <- all_tripdata_clean %>% 
  group_by(member_casual) %>% 
  summarise(mean_time = mean(ride_length),mean_distance = mean(ride_distance))

membervstime <- ggplot(userType_means) + 
  geom_col(mapping=aes(x=member_casual,y=mean_time,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean travel time by User type",x="User Type",y="Mean time in sec")

membervsdistance <- ggplot(userType_means) + 
  geom_col(mapping=aes(x=member_casual,y=mean_distance,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean travel distance by User type",x="User Type",y="Mean distance In Km",caption = "Data by Motivate International Inc")

grid.arrange(membervstime, membervsdistance, ncol = 2)  

#The we check  the number of rides diferences by weekday:
all_tripdata_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length),.groups = 'drop') %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of rides by User type during the week",x="Days of the week",y="Number of
       rides",caption = "Data by Motivate International Inc", fill="User type") +
  theme(legend.position="top")


#Create a new data frame with only the rows with info in the "bike type" column:

with_bike_type <- all_tripdata_clean %>% filter(rideable_type=="classic_bike" | rideable_type=="electric_bike")

#Then lets check the bike type usage by user type:

with_bike_type %>%
  group_by(member_casual,rideable_type) %>%
  summarise(totals=n(), .groups="drop")  %>%
  
  ggplot()+
  geom_col(aes(x=member_casual,y=totals,fill=rideable_type), position = "dodge") + 
  labs(title = "Bike type usage by user type",x="User type",y=NULL, fill="Bike type") +
  scale_fill_manual(values = c("classic_bike" = "#746F72","electric_bike" = "#FFB100")) +
  theme_minimal() +
  theme(legend.position="top")

#And their usage by both user types during a week:

with_bike_type %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,rideable_type,weekday) %>%
  summarise(totals=n(), .groups="drop") %>%
  
  ggplot(aes(x=weekday,y=totals, fill=rideable_type)) +
  geom_col(, position = "dodge") + 
  facet_wrap(~member_casual) +
  labs(title = "Bike type usage by user type during a week",x="User type",y=NULL,caption = "Data by Motivate International Inc") +
  scale_fill_manual(values = c("classic_bike" = "#746F72","electric_bike" = "#FFB100")) +
  theme_minimal() +
  theme(legend.position="none")


#Lets check now the coordinates data of the rides, to see if is there any interesting pattern:

#First we create a table only for the most popular routes (>250 times)
coordinates_table <- all_tripdata_clean %>% 
  filter(start_lng != end_lng & start_lat != end_lat) %>%
  group_by(start_lng, start_lat, end_lng, end_lat, member_casual, rideable_type) %>%
  summarise(total = n(),.groups="drop") %>%
  filter(total > 250)

#Then we create two sub tables for each user type
casual <- coordinates_table %>% filter(member_casual == "casual")
member <- coordinates_table %>% filter(member_casual == "member")

#Lets store bounding box coordinates for ggmap:
chi_bb <- c(
  left = -87.700424,
  bottom = 41.790769,
  right = -87.554855,
  top = 41.990119
)

#Here we store the stamen map of Chicago
chicago_stamen <- get_stamenmap(
  bbox = chi_bb,
  zoom = 12,
  maptype = "toner"
)


#Then we plot the data on the map
ggmap(chicago_stamen,darken = c(0.8, "white")) +
  geom_curve(casual, mapping = aes(x = start_lng, y = start_lat, xend = end_lng, yend = end_lat, alpha= total, color=rideable_type), size = 0.5, curvature = .2,arrow = arrow(length=unit(0.2,"cm"), ends="first", type = "closed")) +
  coord_cartesian() +
  labs(title = "Most popular routes by casual users",x=NULL,y=NULL, color="User type", caption = "Data by Motivate International Inc") +
  theme(legend.position="none")

ggmap(chicago_stamen,darken = c(0.8, "white")) +
  geom_curve(member, mapping = aes(x = start_lng, y = start_lat, xend = end_lng, yend = end_lat, alpha= total, color=rideable_type), size = 0.5, curvature = .2,arrow = arrow(length=unit(0.2,"cm"), ends="first", type = "closed")) +  
  coord_cartesian() +
  labs(title = "Most popular routes by annual members",x=NULL,y=NULL, caption = "Data by Motivate International Inc") +
  theme(legend.position="none")

### the end ###

all_tripdata <- rbind(tripdata_withChar,tripdata_withDoubles)
        
