#Collecting and Cleaning done in R,
#Mapping the Geospatial Visualizations will be done in Tableau 

#Create Folders to store csv files 
ss_folder <- "/Volumes/Jaeger/Capstone_1_cyclistic/Geospatial Data/SS_analysis"
es_folder <- "/Volumes/Jaeger/Capstone_1_cyclistic/Geospatial Data/ES_analysis"
aa_folder <- "/Volumes/Jaeger/Capstone_1_cyclistic/Geospatial Data/Advanced_analysis"

#---------Start Station Analysis------

#Popularity Analysis
ss_data <- df_clean_raw %>% select(member_casual, rideable_type, start_station_name, 
                                   start_station_id, start_lat, start_lng, startedAt_hour, day_str, ride_length)

#Which are the top 50 most popular starting stations, for both members and casuals? 
top50ss_MC <- ss_data %>% select(start_station_name, start_lat, start_lng) %>% 
  group_by(start_station_name, start_lat, start_lng) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

top50ss_MC <- top50ss_MC[1:50,]

write.csv(top50ss_MC, 
          file.path(ss_folder, "top50ss_MC.csv"),
          row.names = FALSE)

#Which are the top 50 most popular starting stations for casuals? 
top50ss_C <- ss_data %>% filter(member_casual == "casual") %>%
  select(start_station_name, start_lat, start_lng) %>%
  group_by(start_station_name, start_lat, start_lng) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

top50ss_C <- top50ss_C[1:50,]

write.csv(top50ss_C, 
          file.path(ss_folder, "top50ss_C.csv"),
          row.names = FALSE)

#Which are the top 50 most popular starting stations for members? 
top50ss_M <- ss_data %>% filter(member_casual == "member") %>%
  select(start_station_name, start_lat, start_lng) %>%
  group_by(start_station_name, start_lat, start_lng) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

top50ss_M <- top50ss_M[1:50,]

write.csv(top50ss_M, 
          file.path(ss_folder, "top50ss_M.csv"),
          row.names = FALSE)


#---------End Station Analysis------
es_data <- df_clean_raw %>% select(member_casual, rideable_type, end_station_name, 
                                   end_station_id, end_lat, end_lng, endedAt_hour, day_str, ride_length)

#Which are the top 50 most popular ending stations, for both members and casuals? 
top50es_MC <- es_data %>% select(end_station_name, end_lat, end_lng) %>% 
  group_by(end_station_name, end_lat, end_lng) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

top50es_MC <- top50es_MC[1:50,]

write.csv(top50es_MC, 
          file.path(es_folder, "top50es_MC.csv"),
          row.names = FALSE)

#Which are the top 50 most popular ending stations for members? 
top50es_M <-  es_data %>% filter(member_casual == "member") %>% 
  select(end_station_name, end_lat, end_lng) %>% 
  group_by(end_station_name, end_lat, end_lng) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

top50es_M <- top50es_M[1:50,]

write.csv(top50es_M, 
          file.path(es_folder, "top50es_M.csv"),
          row.names = FALSE)

#Which are the top 50 most popular ending stations for casuals? 
top50es_C <- es_data %>% filter(member_casual == "casual") %>% 
  select(end_station_name, end_lat, end_lng) %>% 
  group_by(end_station_name, end_lat, end_lng) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

top50es_C <- top50es_C[1:50,]

write.csv(top50es_C, 
          file.path(es_folder, "top50es_C.csv"),
          row.names = FALSE)

#Which stations do casuals end their trips most of the time, as filtered by ride lengths which are over 20 minutes (long ride) ? 
top50es_C_LR <-  es_data %>% filter(member_casual == "casual", ride_length > 1200) %>% 
  select(end_station_name, end_lat, end_lng) %>% 
  group_by(end_station_name, end_lat, end_lng) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

top50es_C_LR <- top50es_C_LR[1:50,]

write.csv(top50es_C_LR, 
          file.path(es_folder, "top50es_C_LR.csv"),
          row.names = FALSE)

#Which stations do members end their trips most of the time, as filtered by ride lengths which are over 20 minutes (long ride) ? 
top50es_M_LR <-  es_data %>% filter(member_casual == "member", ride_length > 1200) %>% 
  select(end_station_name, end_lat, end_lng) %>% 
  group_by(end_station_name, end_lat, end_lng) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

top50es_M_LR <- top50es_M_LR[1:50,]

write.csv(top50es_M_LR, 
          file.path(es_folder, "top50es_M_LR.csv"),
          row.names = FALSE)


#---------Advanced Analysis------

springLongride <-  df_clean_raw %>% filter(member_casual == "casual", 
                                 between(endedAt_hour, 10, 14), #Ride length rises between 10-14 hours
                                 between(month, 3, 5), #Ride length rises in the months of spring (between march - may)
                                 day_str == "Sunday") %>% #Ride Length Highest on a Sunday
  select(end_station_name, end_lat, end_lng) %>% 
  group_by(end_station_name, end_lat, end_lng) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

springLongride <- springLongride[1:50, ]

write.csv(springLongride, 
          file.path(aa_folder, "springLongride.csv"),
          row.names = FALSE)

#Remove unused data
remove(es_data, ss_data, top50ss_MC, top50ss_C, top50ss_M, 
       top50es_C_LR, top50es_C, top50es_M_LR, top50es_M, top50es_MC, springLongride)

#Routes 

top50routes_C <- df_clean_raw %>% filter(member_casual == "casual") %>% 
  select(start_station_name, start_lat, start_lng, end_station_name, end_lat, end_lng) %>% 
  group_by(start_station_name, start_lat, start_lng, end_station_name, end_lat, end_lng) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

top50routes_C <- top50routes_C[1:50, ]

write.csv(top50routes_C, 
          file.path(aa_folder, "top50routes_C.csv"),
          row.names = FALSE)

top50routes_M <- df_clean_raw %>% filter(member_casual == "member") %>% 
  select(start_station_name, start_lat, start_lng, end_station_name, end_lat, end_lng) %>% 
  group_by(start_station_name, start_lat, start_lng, end_station_name, end_lat, end_lng) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

top50routes_M <- top50routes_M[1:50, ]

write.csv(top50routes_M, 
          file.path(aa_folder, "top50routes_M.csv"),
          row.names = FALSE)

