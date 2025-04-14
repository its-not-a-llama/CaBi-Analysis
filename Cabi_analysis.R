#' ---
#' title: "CaBi analysis"
#' author: "its-not-a-llama"
#' output:
#'   html_document:
#'     toc: true
#'     toc_depth: 3
#'   pdf_document:
#'     toc: true
#'     toc_depth: '3'
#' ---
#' 
## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)

#' 
## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(here)
library(dplyr)
library(janitor)

#' 
#' ## Introduction
#' 
#' In this project, data is analyzed from bike ride sharing platform CaBi, which serves the users in Washington DC. The user data details records of rides undertaken by users such as information like ride duration, bike type, starting/ending locations, docking stations' locations, etc.
#' 
#' The data can be analyzed to improve the overall rider experience and enhance sales. This is achieved by understanding when and where users use the services, their affinity for different bike types, impacts of CaBi policies on the users' interaction with the service, etc.
#' 
#' ## Data
#' 
#' The data has been collected from Capital Bikeshare from their website (<https://capitalbikeshare.com/system-data>) The data contains details pertaining to the start/end times of a ride along with their duration. Geolocation such as longitudes, latitudes and dock locations are also provided for the rides' start and end. Other information such as the bike unique IDs and types are sometimes included, along with the membership status of the users.
#' 
#' In Q2, data above 5000 seconds is excluded from the visualization as the number of rides over this duration is statistically insignificant and causes the graph to be overplotted. Data with rides with a negative duration are also excluded to prevent compromising the results. In Q3, Stations with the ID NA are excluded from the results, there is a significant number of rides with a starting/ending dock ID of N/A, which far exceeds the value of any station making the graph hard to interpret.
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df <-read_csv(file = here("data", "rides_2020_2021_extract.csv"))
# data is imported using read_csv and here function is applied
df_clean <- df %>% 
  clean_names()
#janitor is applied to reformat column names into snakecase

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_clean <- df_clean %>% 
  mutate(
    duration = if(is.character(duration)) parse_integer(duration) 
    else as.integer(duration),
    start_date= parse_date_time(start_date, orders= "ymd HMS"),
    end_date= parse_date_time(end_date, orders= "ymd HMS")
    ) %>% 
    filter(duration > 0) 
#data in columns duration, start_date, end_date is parsed into appropriate data 
#types
#use of parse integer alone for the date still produced an error as some data 
#was in character and integer form. Use of if/else function circumvents 
#that issue

#' 
#' ## Question 1
#' 
#' How does the activity level of users range across the day?
#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
activity_level <- df_clean %>% 
  mutate(
    start_hour = hour(start_date),
    end_hour = hour(end_date)
  )
#the starting and ending hours are extracted from the start_date and end_date

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

activity_level <- df_clean %>% 
  mutate(
    start_hour = hour(start_date),
    end_hour = hour(end_date)
  )
#the starting and ending hours are extracted from the start_date and end_date
activity_level <- activity_level %>% 
  select(start_hour, end_hour) %>% 
  pivot_longer(
    cols = c(start_hour, end_hour),
    names_to = "type",
    values_to = "hour") %>% 
    #pivot_longer is used to find all records under set start and end hour
  count(hour)
    #records under each hour is collected

ggplot(activity_level, aes(x = hour, y = n, fill = n)) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_fill_gradient(low = "blue", high = "red") +  
  labs(title = "Activity Levels Throughout the Day",
       x = "Hour of the Day",
       y = "Number of Rides") +
  scale_x_continuous(breaks = 0:23) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# theme used to make the axis fit better and x-axis titles not overlap


#' 
#' The activity levels reflect user behavior across a 24-hour period. The activity levels throughout the start of the day remain low until 7 AM. From 7 AM to 7 PM the activity levels are relatively high, peaking at 5 PM which corresponds to end of the working day for multiple users. After 8 PM, the number of riders decreases significantly and continues falling till it reach a low at 4 AM.
#' 
#' ## Question 2
#' 
#' The website states that members can take unlimited rides of duration shorter than 45 minutes, but until 2021, this was limited to rides of duration shorter than 30 minutes. Was there any difference between members' ride duration in 2020 and 2021?
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_clean <- df_clean %>%
  mutate(year = format(as.Date(start_date), "%Y"))
# Using the start_date column, the year is extracted

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_filtered <- df_clean %>%
  filter(duration > 0 & duration <= 5000) %>%
  filter(year %in% c("2020", "2021")) %>%
  filter(member_casual == "member")
# members and rides with negative duration or over 5000 seconds are filtered

ggplot(df_filtered, aes(x = duration / 60, fill = year)) +
  geom_histogram() +
  labs(title = "Frequency of Rides vs Ride Duration for 2020 and 2021 
       for Members",
       x = "Ride Duration (Minutes)",
       y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("2020" = "blue", "2021" = "red")) +
  facet_wrap(~ year)

#' 
#' Whilst exclusively looking at members' data after the policy changes. The frequency of shorter rides during 2021 has significantly increased. This can be deduced from the visualization by a higher frequency of shorter rides (<20 minutes) in the year 2021. It is interesting to note that the number of rides recorded during 2021 by members is also way higher. Whether this is caused by the COVID-19 pandemic, a change in numbers of members, or other reasons, that can be determined by examining more data sets.
#' 
#' ## Question 3
#' 
#' What are the busiest stations in Washington DC?
#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
na_count_start <- sum(is.na(df_clean$start_station_id))
na_count_end <- sum(is.na(df_clean$end_station_id))
total_na_count <- na_count_start + na_count_end
# The number of stations with NA in both columns are counted
print(paste("Total number of NA values in station IDs:", total_na_count))
# number of NA stations are displayed

station_counts <- df_clean %>%
  pivot_longer(cols = c(start_station_id, end_station_id), 
               names_to = "station_type", 
               values_to = "station_id") %>%
  drop_na(station_id) %>%  
  # remove rows with NA station_id
  count(station_id, name = "total_count") %>%
  arrange(desc(total_count))
# pivot_longer is used to count the IDs of stations in start and end docks
# stations are arranged from most to least busy

top_stations <- station_counts %>%
  top_n(10, total_count)
# the top 10 most frequented stations are counted

ggplot(top_stations, aes(x = reorder(station_id, -total_count), 
                         y = total_count)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Top 10 Most Frequently Used Stations",
       x = "Station ID",
       y = "Total Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#' 
#' 
#' To determine the most frequented stations, the starting and ending stations were used to examine riders' behavior. The methodology used here is primitive but showcases the main areas of attraction. It is worth noting that there is a significant number of rides with no known starting or ending stations (235733) which compromises the accuracy of the results.
#' 
#' ##   Conclusion
#' The research questions which have been operationalized above, provide valuable insight about the riders' behaviors and the impact of policies. These insights can subsequently be used to improve the overall efficiency, boost customer satisfaction for CaBi.
#' 
#' 
#' 
#' 
#' 
#' 
