<<<<<<< HEAD
########################################
# load libraries
########################################

# load some packages that we'll need
library(tidyverse)
library(scales)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides (compare a histogram vs. a density plot)
ggplot(trips,aes(x = tripduration/60))+ geom_histogram()+ xlim(c(0,60))
# plot the distribution of trip times by rider type indicated using color and fill (compare a histogram vs. a density plot)
ggplot(trips, aes(x = tripduration/60, color = usertype, fill = usertype)) +
  geom_histogram(bins = 100)+scale_x_log10(label=comma) + xlim(c(0,60)) +
  scale_y_continuous(labels = comma)+ facet_wrap(~usertype)

ggplot(trips, aes(x = tripduration/60, color = usertype, fill = usertype)) +
  geom_histogram(bins = 100)+scale_x_log10(label=comma) + xlim(c(0,60)) + facet_wrap(~usertype)

# plot the total number of trips on each day in the dataset
ggplot(trips, aes(x = tripduration/60, color = ymd, fill = ymd)) +
  geom_histogram(bins = 100)+scale_x_log10(label=comma) + xlim(c(0,60)) +
  scale_y_continuous(labels = comma)+ facet_wrap(~ymd)
# plot the total number of trips (on the y axis) by age (on the x axis) and gender (indicated with color)


# plot the ratio of male to female trips (on the y axis) by age (on the x axis)
# hint: use the spread() function to reshape things to make it easier to compute this ratio
# (you can skip this and come back to it tomorrow if we haven't covered spread() yet)

trips %>% select(gender,birth_year) %>% mutate(age=2014-birth_year) %>% count(gender,age)

trips %>% select(gender,birth_year) %>% mutate(age=2014-birth_year) %>% group_by(gender,age) %>% summarize(count = n())

#line graph
trips %>% mutate(age=2014-birth_year) %>% count(gender,age) %>% pivot_wider(names_from = gender, values_from = n)%>% 
  mutate(ratio = Male/Female) %>% ggplot(aes(x=age,y=ratio)) + geom_line()
#bar graph
trips %>% mutate(age=2014-birth_year) %>% count(gender,age) %>% pivot_wider(names_from = gender, values_from = n)%>% 
  mutate(ratio = Male/Female) %>% ggplot(aes(x=age,y=ratio)) + geom_bar(stat="identity") + xlim(c(18,90))

########################################
# plot weather data
########################################
# plot the minimum temperature (on the y axis) over each day (on the x axis)
ggplot(data=weather,aes(x = ymd, y=tmin)) +geom_point()
# plot the minimum temperature and maximum temperature (on the y axis, with different colors) over each day (on the x axis)
# hint: try using the gather() function for this to reshape things before plotting
# (you can skip this and come back to it tomorrow if we haven't covered reshaping data yet)
trips_with_weather %>% pivot_longer(c(tmin,tmax),values_to = "temperature") %>% ggplot(aes(x=ymd,y=temperature)) + geom_point()
########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this
# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this
tripsSub <- trips %>% group_by(ymd) %>% summarize(per_day = n())
trips_with_weatherCount <- inner_join(tripsSub, weather, by="ymd")
trips_with_weatherCount %>% ggplot() + geom_point(mapping = aes(x = ymd, y = per_day))
filter(weather, prcp >0.1474) %>% ggplot() + geom_line(mapping = aes(x =ymd, y = prcp))
trips_with_weatherSub <- mutate(trips_with_weatherCount, substantial = prcp >0.1474)
trips_with_weatherSub %>% ggplot()+geom_point(mapping = aes(x =ymd, y = per_day, color = substantial))



# add a smoothed fit on top of the previous plot, using geom_smooth
trips_with_weatherSub %>% ggplot(mapping = aes(x =ymd, y = per_day, color = substantial))+geom_point()+geom_smooth()

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package
tripsHours <- trips
tripsHours <- mutate(tripsHours,date= ymd_hms(starttime))
tripsHours <- mutate(tripsHours,hour= hour(date))

tripsHours %>% group_by(ymd,hour) %>% summarize(n()) %>% group_by(hour) %>% group_by(hour)%>% summarize(ymd, mean(count),sd(count))
# plot the above
tripsHours %>% ggplot() + geom_point(mapping = aes(x = hour, y = avg),color = "blue") +
  geom_point(mapping = aes(x = hour, y = std),color = "green") + 
  geom_line(mapping = aes(x = hour, y = avg),color = "blue") + geom_line(mapping = aes(x = hour, y = avg),color = "blue")
# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package
=======
########################################
# load libraries
########################################

# load some packages that we'll need
library(tidyverse)
library(scales)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides (compare a histogram vs. a density plot)

# plot the distribution of trip times by rider type indicated using color and fill (compare a histogram vs. a density plot)

# plot the total number of trips on each day in the dataset

# plot the total number of trips (on the y axis) by age (on the x axis) and gender (indicated with color)

# plot the ratio of male to female trips (on the y axis) by age (on the x axis)
# hint: use the pivot_wider() function to reshape things to make it easier to compute this ratio
# (you can skip this and come back to it tomorrow if we haven't covered pivot_wider() yet)

########################################
# plot weather data
########################################
# plot the minimum temperature (on the y axis) over each day (on the x axis)

# plot the minimum temperature and maximum temperature (on the y axis, with different colors) over each day (on the x axis)
# hint: try using the pivot_longer() function for this to reshape things before plotting
# (you can skip this and come back to it tomorrow if we haven't covered reshaping data yet)

########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this

# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this

# add a smoothed fit on top of the previous plot, using geom_smooth

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package

# plot the above

# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package
>>>>>>> 32a0ce8d514b579d587edbc2b6caae1044ddb608
