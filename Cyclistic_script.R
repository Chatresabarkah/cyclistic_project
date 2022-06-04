
#Import Libraries and Set Working Directory

library("tidyverse")
library("here")
library("skimr")
library("janitor")
library("dplyr")

setwd("/Volumes/Jaeger/Capstone_1_cyclistic")

#Import CSV Files
df_1 <- readr::read_csv(here::here("Cyclistic_dataset","March2021-divvy-tripdata.csv"))
df_2 <- readr::read_csv(here::here("Cyclistic_dataset","April2021-divvy-tripdata.csv"))
df_3 <- readr::read_csv(here::here("Cyclistic_dataset","May2021-divvy-tripdata.csv"))
df_4 <- readr::read_csv(here::here("Cyclistic_dataset","June2021-divvy-tripdata.csv"))
df_5 <- readr::read_csv(here::here("Cyclistic_dataset","July2021-divvy-tripdata.csv"))
df_6 <- readr::read_csv(here::here("Cyclistic_dataset","August2021-divvy-tripdata.csv"))
df_7 <- readr::read_csv(here::here("Cyclistic_dataset","Sept2021-divvy-tripdata.csv"))
df_8 <- readr::read_csv(here::here("Cyclistic_dataset","Oct2021-divvy-tripdata.csv"))
df_9 <- readr::read_csv(here::here("Cyclistic_dataset","Nov2021-divvy-tripdata.csv"))
df_10 <- readr::read_csv(here::here("Cyclistic_dataset","Dec2021-divvy-tripdata.csv"))
df_11 <- readr::read_csv(here::here("Cyclistic_dataset","Jan2022-divvy-tripdata.csv"))
df_12 <- readr::read_csv(here::here("Cyclistic_dataset","Feb2022-divvy-tripdata.csv"))

#Compare Columns, check if they will successfully bind together by rows
compare_df_cols(df_1, df_2, df_3, df_4, df_5, df_6, 
                df_7, df_8, df_9, df_10, df_11, df_12, return = "mismatch")

#Combine all rows into a single dataset
df_combined_raw <- rbind(df_1, df_2, df_3, df_4, df_5, df_6, 
                         df_7, df_8, df_9, df_10, df_11, df_12)

#remove remaining uncombined datasets
rm(df_1, df_2,  df_3, df_4, df_5, df_6, 
   df_7, df_8, df_9, df_10, df_11, df_12)

##------------View Data------

glimpse(df_combined_raw)

summary(df_combined_raw)

##------------Data Cleaning------------

#'- Rename Docked Bike into Classic Bike, omit Data points with NA values
df_combined_raw <- df_combined_raw %>% mutate(rideable_type = recode(rideable_type, "docked_bike" = "classic_bike")) %>% 
  na.omit()

#-----Create hour, day, month, year variable ---------- 
df_combined_raw$date <- as.Date(df_combined_raw$started_at)

#Starting Time
df_combined_raw$startedAt_time <- format(as.POSIXct(df_combined_raw$started_at), format = "%H:%M:%S")
df_combined_raw$startedAt_hour <- format(as.POSIXct(df_combined_raw$started_at), format = "%H")
df_combined_raw$startedAt_hour <- as.numeric(as.character(df_combined_raw$startedAt_hour))

#Ending Time
df_combined_raw$endedAt_time <- format(as.POSIXct(df_combined_raw$ended_at), format = "%H:%M:%S")
df_combined_raw$endedAt_hour <- format(as.POSIXct(df_combined_raw$ended_at), format = "%H")
df_combined_raw$endedAt_hour <- as.numeric(as.character(df_combined_raw$endedAt_hour))

#Month
df_combined_raw$month <- format(as.Date(df_combined_raw$date), "%m")
df_combined_raw <- df_combined_raw %>% 
  mutate(month, month_str = as.numeric(as.character(df_combined_raw$month)))
df_combined_raw$month_str <- month.name[df_combined_raw$month_str]
df_combined_raw$month_str <- ordered(df_combined_raw$month_str, levels=c("January", "February", "March", "April", "May", "June", "July",
                                                                 "August", "September", "October", "November", "December"))

#Day
df_combined_raw$day <- format(as.Date(df_combined_raw$date), "%d")
df_combined_raw <- df_combined_raw %>% 
  mutate(day, day_str = as.numeric(as.character(df_combined_raw$day)))

df_combined_raw$day_str <- format(as.Date(df_combined_raw$date), "%A")  
df_combined_raw$day_str <- ordered(df_combined_raw$day_str, 
                                       levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#Year
df_combined_raw$year <- format(as.Date(df_combined_raw$date), "%Y")
df_combined_raw$year <- as.numeric(as.character(df_combined_raw$year))


#' - Create new variable called ride length (start time - end time) 
df_combined_raw$ride_length <- difftime(df_combined_raw$ended_at, df_combined_raw$started_at)
df_combined_raw$ride_length <- as.numeric(as.character(df_combined_raw$ride_length))

#'- Remove Ride length below 1 second and above 23 hours (maintenance)  
df_combined_raw <- filter(df_combined_raw, ride_length > 1, ride_length < 82800)

#' - Remove trips that contained bike maintenance stations and testing rides
df_combined_raw <- filter(df_combined_raw, start_station_name != "HQ QR",
             end_station_name != "HQ QR",
             start_station_name != "DIVVY CASSETTE REPAIR MOBILE STATION",
             end_station_name != "DIVVY CASSETTE REPAIR MOBILE STATION",
             start_station_name != "Base - 2132 W Hubbard Warehouse",
             end_station_name != "Base - 2132 W Hubbard Warehouse",
             start_station_name != "Hubbard Bike-checking (LBS-WH-TEST)",
             end_station_name != "Hubbard Bike-checking (LBS-WH-TEST)",
             start_station_name != "351",
             end_station_name != "351")


# ---------Ride Length Outlier Analysis With Z-Score Method----------

# Calculate the Z-score and create new data frame
df_mean <- mean(df_combined_raw$ride_length)
df_sd <- sd(df_combined_raw$ride_length)
df_zscore <- (df_combined_raw$ride_length-mean(df_combined_raw$ride_length))/sd(df_combined_raw$ride_length)
df_outliers <- data.frame(df_combined_raw, df_zscore)

#Only rows in the data frame with all z-scores less than 3 are kept.
no_outliers <- df_outliers[!(df_zscore>3), ]
df_clean_raw <- no_outliers
df_clean_raw <-subset(df_clean_raw, select = -c(df_zscore))

#remove unused values before beginning analysis and visualization
rm(df_outliers, no_outliers, df_mean, df_sd, df_zscore, df_combined_raw)


#---------Time/Product/Consumer Analysis----------

---------#Descriptive---------

#In our dataset, are there more casuals or annual members?
df_clean_raw %>% ggplot(aes(member_casual)) +
  geom_bar(aes(fill = member_casual), width = 0.7) + 
  labs(title = "In Our Dataset, Are There More Casual or Annual members?", 
       subtitle= "The Number of Casual Riders Vs. Members - Difference of 652,270", x=NULL) +
  theme(plot.subtitle = element_text(face = "italic")) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 2, colour = "white")

#Which Bike type is the most popular?
df_clean_raw %>% ggplot(aes(rideable_type)) +
  geom_bar(aes(x = rideable_type), width = 0.7, fill = "steelblue") + 
  labs(title = "Which Bike Type is the Most Popular? (Overall)", 
       subtitle="The Number of Classic Bike Rides vs. Electric Bike Rides - Difference of 2,534,240", x=NULL) +
  theme(plot.subtitle = element_text(face = "italic")) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 2, colour = "white")

#Which Bike type is the most popular amongst members?
df_clean_raw %>% filter(member_casual == "member") %>% 
  group_by(rideable_type) %>% 
  ggplot(aes(rideable_type)) +
  geom_bar(aes(x = rideable_type), width = 0.7, fill = "steelblue") +
  labs(title = "Which Bike Type is the Most Popular Amongst Members?", 
       subtitle = "The Number of Classic Bike Rides vs. Electric Bike Rides for members - Difference of 1,463,467", x=NULL) +
  theme(plot.subtitle = element_text(face = "italic")) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 2, colour = "white")

#Which Bike type is the most popular amongst casuals?
df_clean_raw %>% filter(member_casual == "casual") %>% 
  group_by(rideable_type) %>% 
  ggplot(aes(rideable_type)) +
  geom_bar(aes(x = rideable_type), width = 0.7, fill = "steelblue") +
  labs(title = "Which Bike Type is the Most Popular Amongst Casuals?", 
       subtitle = "The Number of Classic Bike Rides vs. Electric Bike Rides for members - Difference of 1070773", x=NULL) +
  theme(plot.subtitle = element_text(face = "italic")) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 2, colour = "white")

---------#Frequency---------

#Which hour do members and casuals start their rides the most?  
df_clean_raw %>% group_by(member_casual, startedAt_hour) %>% 
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(startedAt_hour, number_of_rides, colour = member_casual, group = member_casual)) +
  geom_line(linetype = "dashed") +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(0:24))+
  labs(title = "Which Hour has the Most Frequency of Rides?", 
       subtitle = "The Most & Least Popular Hours for Cyclistic Bike Rides", 
       x= "Hour", 
       y= "Number of Rides") + 
  theme(plot.subtitle = element_text(face = "italic"))

#Which day has the most frequency of rides?
df_clean_raw %>% group_by(member_casual, day_str) %>% 
  summarise(number_of_rides =n()) %>% 
  ggplot(aes(x = day_str, y = number_of_rides, colour = member_casual, group = member_casual))+
  geom_col(aes(fill = member_casual), position = position_dodge(width = 0.9)) + 
  geom_line(aes(color = member_casual), position = position_dodge(width = 1)) +
  labs(title = "Which Day has the Most Frequency of Rides?", 
       subtitle = "The Most Popular Days to Ride", 
       x= NULL, y= "Number of Rides") + 
  theme(plot.subtitle = element_text(face = "italic")) 


#Which month has the most frequency of rides?
df_clean_raw %>% group_by(member_casual, month_str) %>% 
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x = month_str, y = number_of_rides, colour = member_casual, group = member_casual)) +
  geom_line(linetype = "dashed") +
  geom_point(size = 2) +  
  labs(title = "Which Month has the Most Frequency of Rides?", 
       subtitle = "The Most Popular Months to Ride", 
       x= NULL, y= "Number of Rides") + 
  theme(plot.subtitle = element_text(face = "italic")) 

---------#Ride Time---------
#Who has the longer bike rides? Members or casuals?
df_clean_raw %>% group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length)) %>% 
  ggplot(aes(x = member_casual, y = average_ride_length)) +
  geom_col(aes(fill = member_casual), width = 0.7)+
  geom_text(aes(label = format(round(average_ride_length/60, 2), nsmall = 2)), 
            vjust = 2, colour = "white") +
  labs(title = "Ride Length Comparison (In Minutes)", 
       subtitle = "Who has the longer bike rides? Members or casuals? - Casuals, with a difference of Difference of 10,75 Minutes", 
       y= NULL, x = NULL) +
  theme(plot.subtitle = element_text(face = "italic"))

#What is the average ride length by each hour for members compared to casuals? 
avgRidelengthhour <- aggregate(df_clean_raw$ride_length ~ df_clean_raw$member_casual + df_clean_raw$startedAt_hour, FUN = mean) 
colnames(avgRidelengthhour) <- c("member_casual", "hour", "ride_length")
avgRidelengthhour <- mutate(avgRidelengthhour, ride_length = ride_length / 60)

ggplot(avgRidelengthhour, aes(x = factor(hour), y = ride_length, colour = member_casual, group = member_casual)) +
  geom_line(linetype = "dashed") +
  geom_point(size = 3) +   
  labs(title = "Which Starting Hours End Up With the Longest / Shortest Bike Rides?", 
       subtitle = "Ride Length vs. Hour", 
       x= "Hour", y= "Mean Ride Length - in Minutes") + 
  theme(plot.subtitle = element_text(face = "italic"))

#What is the average ride time each day for members vs casual users?? 
avgRidelengthDay <- aggregate(df_clean_raw$ride_length ~ df_clean_raw$member_casual + df_clean_raw$day_str, FUN = mean) 
colnames(avgRidelengthDay) <- c("member_casual", "day_str", "ride_length") 
avgRidelengthDay <- mutate(avgRidelengthDay, ride_length = ride_length / 60)

ggplot(avgRidelengthDay, aes(day_str, ride_length, group = member_casual)) +
  geom_col(aes(fill = member_casual), position = position_dodge(width = 1)) + 
  geom_line(aes(color = member_casual), position = position_dodge(width = 1)) +
  labs(title = "Which Days End Up With the Longest / Shortest Bike Rides?", 
       subtitle = "Ride Length vs. Day", 
       x= NULL, y= "Mean Ride Length - in Minutes") + 
  theme(plot.subtitle = element_text(face = "italic"))

#What is the average ride time each month for members vs casual users?
avgRidelengthMonth <- aggregate(df_clean_raw$ride_length ~ df_clean_raw$member_casual + df_clean_raw$month_str, FUN = mean) 
colnames(avgRidelengthMonth) <- c("member_casual", "month_str", "ride_length") 
avgRidelengthMonth <- mutate(avgRidelengthMonth, ride_length = ride_length / 60)

ggplot(avgRidelengthMonth, aes(x = month_str, y = ride_length, colour = member_casual, group = member_casual)) +
  geom_line(linetype = "dashed") +
  geom_point(size = 3) +   
  labs(title = "Which Months End Up With the Longest / Shortest Bike Rides?", 
       subtitle = "Ride Length vs. Month", 
       x= NULL, y= " Mean Ride Length - in Minutes") + 
  theme(plot.subtitle = element_text(face = "italic"))


#Remove Unused Data before Geospatial Analysis 
rm(avgRidelengthMonth, avgRidelengthDay, avgRidelengthhour)

