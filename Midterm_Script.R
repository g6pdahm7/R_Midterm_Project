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










