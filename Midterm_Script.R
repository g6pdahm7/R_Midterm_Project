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

##### EDA #####

#' EDA for trip
# SK Good idea to put EDA commands in a function

basic_eda_t <- function(trip) {
  glimpse(trip)
  print(status(trip))
  freq(trip) 
  print(profiling_num(trip))
  plot_num(trip)
  describe(trip)
}
# SK (Points taken) The output of the code below shows 70 unique start/end station IDs, and 
# 74 unique start/end station names. This is a discrepancy worth looking into.
basic_eda_t(trip)

#' Checking and replacing empty cells with NAs for station
sum(station == "", na.rm = T)

#' EDA for station
# SK (Points taken) But it's a really bad idea to re-create that same function
# with a different input. "Create once, use multiple times" is the idea of a function.
# Instead of re-creating it below, you could have called the above function as:
# basic_eda_t(station)
# and you would have got the exact same result. 

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
# SK Same as above, bad idea!
basic_eda_w <- function(weather) {
  glimpse(weather)
  print(status(weather))
  freq(weather) 
  print(profiling_num(weather))
  plot_num(weather)
  describe(weather)
}

basic_eda_w(weather)

##### DATA CLEANING #####

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

# SK Detecting and removing outliers does not mean removing every value that
# does not contribute to a normal distribution. Outliers are typically 
# abnormal values that are likely unnatural/incorrect. Unfortunately,
# you removed very plausible 60-137 second trips between two close stations,
# and many very plausible trips over 3.7 hours.
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

#' I noticed that there is some additional cleaning that needs to be done. 
#' This is specifically for the weather dataset. Since the letter "T"
#' represents trace amounts of precipitation, below 0.01 inches, I simply
#' replaced it with 0.005, which is the halfway point of that range.
unique(weather$precipitation_inches)
weather$precipitation_inches[weather$precipitation_inches == "T"] <- 0.005

#' I also later noticed that in the events column, has some 
#' inconsistency with the letter casings. 
unique(weather$events)
weather$events[weather$events == "rain"] <- "Rain"


#' The weather and station datasets are clean. No further 
#' cleaning required. The weather dataset records daily observations.
#' Eliminating rows that have NAs may remove the data
#' for an entire day.

##### RUSH HOUR ANALYSIS #####

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
# SK Good job!
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

# SK Great idea to compare station usage frequencies between morning and
# evening rush hours

##### WEEKEND STATION FREQUENCIES #####

#' Similar to what I did previously for isolating weekdays,
#' I will start with creating a dataset for only
#' weekend trips.
Ltrips <- trip[trip$day %in% c("Saturday", "Sunday"), ]

#' This is a similar process to what was done for the 
#' rush hours. The only difference, is that I am using
#' weekend data instead.
most_start_w <- Ltrips %>%
  group_by(start_station_name) %>%
  summarise(number_of_trips = n()) %>%
  arrange(desc(number_of_trips)) %>%
  head(10)
print(most_start_w)

most_end_w <- Ltrips %>%
  group_by(end_station_name) %>%
  summarise(number_of_trips = n()) %>%
  arrange(desc(number_of_trips)) %>%
  head(10)
print(most_end_w)

#' Again, similar to what was done previously, I will 
#' use functions from the grid packages to export my 
#' tables as nice images.

start_table_w <- tableGrob(most_start_w, rows = NULL)
grid.newpage()
grid.arrange(start_table_w, top = textGrob("Top 10 Starting Stations During Weekends"))

end_table_w <- tableGrob(most_end_w, rows = NULL)
grid.newpage()
grid.arrange(end_table_w, top = textGrob("Top 10 Ending Stations During Weekends"))

##### AVERAGE UTILIZATION #####

#' The average utilization can be calculated by adding up the total 
#' duration the bikes were used per month, and then dividing it
#' by the total time in each respective month. I will also have to 
#' watch out for the units, as duration is recorded in seconds.

#' For streamlining, I will use the day the trip was started as the
#' day of the trip. This prevents conflict for trips that went 
#' through midnight into the next month.

#' I am going to start off by creating a table containing the 
#' total amount of time bikes were used every month. Divided by 
#' 3600 to convert to hours.
trip$ym <- format(trip$start_date, "%Y-%m")
monthly_use <- trip %>%
  group_by(ym) %>%
  summarise(total_use_hours = sum(duration) / 3600)
str(monthly_use)

#' Next, we have to figure out how many hours are in each month.
#' The problem is that each month has a different number of days.

#' To fix this problem, I will create a data frame with the 
#' number of hours in each month in 2014.

# SK lubridate package has some really nice functions that will
# make your life easier

total_hours <- data.frame(
  ym = c("2014-01", "2014-02", "2014-03", "2014-04", "2014-05", "2014-06", 
                 "2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12"),
  overall_hours = c(31*24, 28*24, 31*24, 30*24, 31*24, 30*24, 31*24, 31*24, 30*24, 31*24, 30*24, 31*24)
)
str(total_hours)

#' Next we have to calculate the utilization ratio for each month.
#' To do that I will join the two tables created, then divide the 
#' columns as appropriate. I joined them by the ym column, which 
#' is the same in both tables.
ratio <- monthly_use %>%
  left_join(total_hours, by = "ym") %>%
  mutate(utilization_ratio = total_use_hours / overall_hours) %>%
  select(ym, total_use_hours, overall_hours, utilization_ratio)

#' Similar to the processes done before. I will export this data
#' in the form of a table in my report. 
utilization_table <- tableGrob(ratio, rows = NULL)
grid.newpage()
grid.arrange(utilization_table, top = textGrob(" Average Total Utilization Ratio per Month"))

##### WEATHER JOINING #####

#' Next we have to do the weather analysis for the data science
#' team.

#' The first step is to join the datasets as appropriate so its ready
#' for analysis. In order to do that, I left join the city 
#' based on the condition that the two ids match from the two different
#' datasets. 
idcity <- station %>% select(id, city)
trip <- trip %>%
  left_join(idcity, by = c("start_station_id" = "id"))

#' Next we have to follow a similar process to join the 
#' weather columns. 

#' I will start by making sure the dates in the datasets 
#' are appropriately formatted. 
trip$start_date <- as.Date(trip$start_date)
weather$date <- as.Date(weather$date, format="%m/%d/%Y")

#' Now that I have fixed up the dates so they match, 
#' I will create a new dataset with the columns I desire
#' from weather.
weatheradjusted <- weather %>%
  select(date, city, max_temperature_f, mean_temperature_f, min_temperature_f, 
         max_visibility_miles, mean_visibility_miles, min_visibility_miles, 
         max_wind_Speed_mph, mean_wind_speed_mph, max_gust_speed_mph, 
         precipitation_inches, cloud_cover, events)

#' Now that I have the new dataset with the desired columns,
#' I will join it based on the dates and cities being the same.
trip <- trip %>%
  left_join(weatheradjusted, by = c("start_date" = "date", "city" = "city"))

##### WEATHER ANALYSIS #####

library(corrplot)

#' The next thing I want to do is think about my correlation matrix. Instead of
#' making a matrix comparing everything together, I think it would be 
#' more meaningful to compare the weather factors with the performance of
#' bike use. To do that, I have opted to use duration of rides and number of 
#' trips per day.

#' To start, we have to determine the number of trips per day in each city.
dailytrips <- trip %>%
  group_by(start_date, city) %>%
  summarise(number_of_trips = n())

#' Now that we have the number of trips per day in each city calculated, 
#' it is necessary to left join in back to trip so we can do the analysis.
trip <- trip %>%
  left_join(dailytrips, by = c("start_date" = "start_date", "city" = "city"))

#' Next I extracted all the columns that are going to be necessary 
#' in my correlation analysis. I converted everything to numeric and 
#' made it into a matric.
corrdata <- trip %>%
  select(duration, number_of_trips, max_temperature_f, mean_temperature_f, min_temperature_f,
         max_visibility_miles, mean_visibility_miles, min_visibility_miles,
         max_wind_Speed_mph, mean_wind_speed_mph, max_gust_speed_mph,
         precipitation_inches, cloud_cover) %>%
  mutate(across(everything(), as.numeric))
matrix <- cor(corrdata, use = "complete.obs")

#' This is the part where I created a secondary matrix, with the 
#' rows and columns labelled as needed in order for me to do the 
#' table and the plot later on.
goodmatrix <- matrix[c("duration", "number_of_trips"), c("max_temperature_f", "mean_temperature_f", "min_temperature_f", "max_visibility_miles", "mean_visibility_miles", "min_visibility_miles", "max_wind_Speed_mph", "mean_wind_speed_mph", "max_gust_speed_mph", "precipitation_inches", "cloud_cover")]

#' The next step is that I am going to make  table to show the data
#' using the grid package in a similar way to what I have done previously. 
#' I will start off by rounding to 3 decimal places. I will then 
#' create a dataframe and table as need and done previously in order to 
#' put it through the grid.arrange function. I had to play around with the
#' display of th columns so it all fit in the exported image.
goodmatrix_rounded <- round(goodmatrix, 3)
goodmatrix_df <- as.data.frame(goodmatrix_rounded)
goodmatrix_df$Weather_Factors <- rownames(goodmatrix_df)
goodmatrix_df <- goodmatrix_df[, c(ncol(goodmatrix_df), 1:(ncol(goodmatrix_df)-1))]
matrix_table <- tableGrob(goodmatrix_df, rows = NULL, theme = ttheme_default(colhead = list(fg_params = list(rot = 90, just = "center", x = 0.5))))
grid.newpage()
grid.arrange(matrix_table, top = textGrob("Correlation Matrix of Weather Factors with Duration and Number of Trips"))

#' Now that I have prepared my table, I will also prepare the correlation
#' matrix plot using the corrplot function. I am also going to add a legend
#' so the colors are easier to be comprehended by the reader.
corrplot(goodmatrix, method = "color", type = "full", tl.col = "black", tl.srt = 90, tl.cex = 0.8, cl.pos = "n", addgrid.col = NA, title = "Correlation Matrix of Weather Factors with Bike Use Indicators", mar = c(7, 2, 7, 2), cex.main = 0.77) 
legend("topleft", legend = seq(-1, 1, by = 0.2), fill = colorRampPalette(c("blue", "white", "red"))(11), title = "Corr. Legend", cex = 0.8)





