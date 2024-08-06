#' Midterm Project - Ahmed Mokhtar

station <- read.csv("station.csv")
trip <- read.csv("trip.csv")
weather <- read.csv("weather.csv")

#' Loading necessary packages

library(funModeling)
library(tidyverse)
library(Hmisc)

#' Checking and replacing empty cells with NAs for trip
sum(trip$id == "")
sum(trip$duration == "")
sum(trip$start_date == "")
sum(trip$start_station_name == "")
sum(trip$start_station_id == "")
sum(trip$end_date == "")
sum(trip$end_station_name == "")
sum(trip$end_station_id == "")
sum(trip$bike_id == "")
sum(trip$subscription_type == "")
sum(trip$zip_code == "")
trip$zip_code[trip$zip_code == ""] <- NA


#' EDA for trip

basic_eda_t <- function(trip) {
  glimpse(trip)
  print(status(trip))
  freq(trip) 
  print(profiling_num(trip))
  plot_num(trip)
  describe(trip)
}

basic_eda_t(trip)

#' Checking and replacing empty cells with NAs for station
sum(station == "", na.rm = T)

#' EDA for station

basic_eda_s <- function(station) {
  glimpse(station)
  print(status(station))
  freq(station) 
  print(profiling_num(station))
  plot_num(station)
  describe(station)
}

basic_eda_s(station)

#' Checking and replacing empty cells for weather. It was observed that
#' in the events column, there was no entries when there are no events.
#' For consistency, I just replaced those with "None" instead of leaving
#' them blank.
sum(weather == "", na.rm = T)
sum(weather$events == "", na.rm = T)
weather$events[weather$events == ""] <- "None"

#' EDA for weather

basic_eda_w <- function(weather) {
  glimpse(weather)
  print(status(weather))
  freq(weather) 
  print(profiling_num(weather))
  plot_num(weather)
  describe(weather)
}

basic_eda_w(weather)

#' Now, we will begin the data cleaning process.
#' We will start with the trip dataset.

library(dplyr)

#' We will have to remove all rows where duration is less than 
#' 180 seconds and have started and ended on the same station.
#' Before that, it's necessary to get their ids first. 

#' This filters only rows where the two station ids are equal,
#' with a duration less than 180. 
cancelled_trips <- trip %>%
  filter(start_station_id == end_station_id & duration < 180)

#' This simply retrieves the ids for those trips into one 
#' variable, which will be later used to export as a csv.
cancelled_trips_ids <- cancelled_trips %>%
  select(id, start_station_id, end_station_id)

#' Exporting the ids as a csv file.
write.csv(cancelled_trips_ids, "cancelled_trips_ids.csv", row.names = F)

#' I am here simply removing the rows where duration is less than 
#' 180 seconds, and have the same station ids.
trip <- trip %>%
  filter(!(trip$start_station_id == trip$end_station_id & trip$duration < 180))

#' Checking for NAs
any(is.na(trip$duration))

#' Next, we have to remove outliers from our data. The profiling_num
#' function was used to determine the 98% range. Anything outside of
#' that range is considered an outlier, and was
#' subsequently removed the from the dataset.
summary(trip$duration)
profiling_num(trip$duration)

#' This identifies all the rows that are outside of the 98%
#' range.
outliers <- trip %>%
  filter(duration < 137 | duration > 13351.44)

#' This simply narrows down the id columns for the those rows.
outlier_ids <- outliers %>%
  select(id, start_station_id, end_station_id)

#' This exports that dataframe as a csv.
write.csv(outlier_ids, "outlier_ids.csv", row.names = F)

#' This simply reassigns all the data that is within the 98% range
#' to the dataset, excluding everything outside of that.
trip <- trip[trip$duration >= 137 & trip$duration <= 13351.44, ]

#' The weather and station datasets are clean. No further 
#' cleaning required. The weather dataset records daily observations.
#' Eliminating rows that have NAs may remove the data
#' for an entire day.

#' We will now start the rush hour analysis.

library(lubridate)
library(ggplot2)

#' I intend to use the start time for the rush hour analysis,
#' since the demand for the bikes is generated at the start 
#' of the rides, becayse that is when people are looking for
#' bikes.

#' This converts the start_date column into the proper format
#' for further analysis. 
trip$start_date <- as.POSIXct(trip$start_date, format="%m/%d/%Y %H:%M")

#' I will now create a new column with just the hours of when 
#' rides began. I will also do the same, recording the day
#' that trips began.
trip$time <- hour(trip$start_date)
trip$day <- weekdays(trip$start_date)

#' This simply creates a new dataset version of trip, but only 
#' for trips that happen on weekdays. 
wtrips <- trip[trip$day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), ]

#' Here, I create a new variable called hours, which groups 
#' trips based on the hour they took place. I arrange it so 
#' that I get the number of trips in each hour.
hours <- wtrips %>%
  group_by(time) %>%
  summarise(number_of_trips = n()) %>%
  arrange(time)

#' This creates a histogram showing the number of trips per hour.
barplot(hours$number_of_trips, names.arg = hours$time, main = "Volume of Trips per Hour on Weekdays", xlab = "Hour of the Day", ylab = "Number of Trips", col = "steelblue")

#' We now have to find the most frequent start and end stations
#' during rush hours. 

##### OVERALL STATION FREQUENCIES #####

#' To start, I will create a new dataset, with only the parameters
#' we are looking for. 
rushtrips <- wtrips %>%
  filter((time >= 7 & time <= 9) | (time >= 16 & time <= 18))
unique(rushtrips$time)

#' Here, I create a table with the station names of the stations
#' with the 10 highest number of trips, for starting points.
most_start <- rushtrips %>%
  group_by(start_station_name) %>%
  summarise(number_of_trips = n()) %>%
  arrange(desc(number_of_trips)) %>%
  head(10)
print(most_start)

#' Here, I created a table with the station names of the stations
#' with the 10 highest number of trips, for end points.
most_end <- rushtrips %>%
  group_by(end_station_name) %>%
  summarise(number_of_trips = n()) %>%
  arrange(desc(number_of_trips)) %>%
  head(10)
print(most_end)

#' I have done some research on ways to create nicer tables, and 
#' export them nicely as an image. 

#' Loading the grid packages, which achieves that for me.

library(gridExtra)
library(grid)

#' This generates an image containing a table for the
#' start and end stations. 
start_table <- tableGrob(most_start, rows = NULL)
grid.newpage()
grid.arrange(start_table, top = textGrob("Top 10 Starting Stations During Rush Hours"))

end_table <- tableGrob(most_end, rows = NULL)
grid.newpage()
grid.arrange(end_table, top = textGrob("Top 10 Ending Stations During Rush Hours"))

##### MORNING STATION FREQUENCIES #####

#' To start, I will create a new dataset, with only the parameters
#' we are looking for, morning hours. 
rushtrips_m <- wtrips %>%
  filter(time >= 7 & time <= 9)
unique(rushtrips_m$time)

#' Here, I create a table with the station names of the stations
#' with the 10 highest number of trips, for starting points in the morning.
most_start_m <- rushtrips_m %>%
  group_by(start_station_name) %>%
  summarise(number_of_trips = n()) %>%
  arrange(desc(number_of_trips)) %>%
  head(10)
print(most_start_m)

#' Here, I created a table with the station names of the stations
#' with the 10 highest number of trips, for end points in the morning.
most_end_m <- rushtrips_m %>%
  group_by(end_station_name) %>%
  summarise(number_of_trips = n()) %>%
  arrange(desc(number_of_trips)) %>%
  head(10)
print(most_end_m)

#' This generates an image containing a table for the
#' start and end stations, specifically for morning rush hours. 
start_table_m <- tableGrob(most_start_m, rows = NULL)
grid.newpage()
grid.arrange(start_table_m, top = textGrob("Top 10 Starting Stations During Morning Rush Hours"))

end_table_m <- tableGrob(most_end_m, rows = NULL)
grid.newpage()
grid.arrange(end_table_m, top = textGrob("Top 10 Ending Stations During Morning Rush Hours"))


##### EVENING STATION FREQUENCIES #####

#' To start, I will create a new dataset, with only the parameters
#' we are looking for, evening hours. 
rushtrips_e <- wtrips %>%
  filter(time >= 16 & time <= 18)
unique(rushtrips_e$time)

#' Here, I create a table with the station names of the stations
#' with the 10 highest number of trips, for starting points in the evening.
most_start_e <- rushtrips_e %>%
  group_by(start_station_name) %>%
  summarise(number_of_trips = n()) %>%
  arrange(desc(number_of_trips)) %>%
  head(10)
print(most_start_e)

#' Here, I created a table with the station names of the stations
#' with the 10 highest number of trips, for end points in the evening.
most_end_e <- rushtrips_e %>%
  group_by(end_station_name) %>%
  summarise(number_of_trips = n()) %>%
  arrange(desc(number_of_trips)) %>%
  head(10)
print(most_end_e)

#' This generates an image containing a table for the
#' start and end stations, specifically for evening rush hours. 
start_table_e <- tableGrob(most_start_e, rows = NULL)
grid.newpage()
grid.arrange(start_table_e, top = textGrob("Top 10 Starting Stations During Evening Rush Hours"))

end_table_e <- tableGrob(most_end_e, rows = NULL)
grid.newpage()
grid.arrange(end_table_e, top = textGrob("Top 10 Ending Stations During Evening Rush Hours"))





















