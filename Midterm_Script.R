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

#' Test commit for data cleaning branch