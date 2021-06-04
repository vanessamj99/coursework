library(tidyverse)
trips <- read_csv('201402-citibike-tripdata.csv')
names(trips) <- gsub(' ','_',names(trips))

# count the number of trips (= rows in the data frame)
nrow(filter(trips))

# find the earliest and latest birth years (see help for max and min to deal with NAs)
max(trips$'birth year')

nonNan <- gsub('\\N', '',trips$birth_year)
nonNan <- gsub('\\', '',trips$birth_year, fixed=TRUE)
min(nonNan)


# use filter and grepl to find all trips that either start or end on broadway

filter(trips,grepl('Broadway',trips$start_station_name) | grepl('Broadway',trips$end_station_name))
# do the same, but find all trips that both start and end on broadway

bothStartAndEnd <- (filter(trips,grepl('Broadway',trips$start_station_name)))
bothStartAndEnd <- (filter(bothStartAndEnd,grepl('Broadway',bothStartAndEnd$end_station_name)))
(filter(bothStartAndEnd,grepl('Broadway',bothStartAndEnd$end_station_name)))

# find all unique station names

unique(trips$start_station_name)

# count the number of trips by gender, the average trip time by gender, and the standard deviation in trip time by gender
# do this all at once, by using summarize() with multiple arguments

trips_by_gender <- group_by(trips,trips$gender)
summarize(trips_by_gender,count = n(),avg_Trip_Time = mean(tripduration), sd_Trip_Time = sd(tripduration))

# find the 10 most frequent station-to-station trips

trips %>%
  group_by(start_station_name, end_station_name) %>%
  summarize(count = n()) %>%
  arrange(desc(count))%>%
  head(n=10)

# find the top 3 end stations for trips starting from each start station
trips %>%
  group_by(end_station_name) %>%
  summarize(count = n()) %>%
            arrange(desc(count))%>%
            head(n=3)


# find the top 3 most common station-to-station trips by gender

trips %>%
  group_by(gender, start_station_name,end_station_name) %>%
  summarize(count = n()) %>%
  arrange(desc(count))%>%
  head(n=3)

# find the day with the most trips
# tip: first add a column for year/month/day without time of day (use as.Date or floor_date from the lubridate package)

mutate(trips, day = substr(starttime, start=1, stop=10))  %>%
  group_by(day)  %>%
  summarize(count = n())  %>%
  arrange(desc(count)) %>%
  head(n=1)

# compute the average number of trips taken during each of the 24 hours of the day across the entire month
# what time(s) of day tend to be peak hour(s)?

#trips$starttime <- strtrim(trips$starttime,10)
#trips$stoptime <- strtrim(trips$stoptime,10)


mutate(trips, day = substr(starttime, start=1, stop=10),
  startTime = substr(starttime,start=12,stop=13)) %>% 
  group_by(day,starttime) %>% summarize(avg = mean(tripduration)) %>% View
 
  
  # group_by(day)  %>%
  #group_by(startTime) %>%
  #summarize(avg = mean(tripduration))


