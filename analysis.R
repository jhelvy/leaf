library(surveydown)
library(tidyverse)
library(lubridate)

db <- sd_db_connect()
data <- sd_get_data(db) %>% 
    select(
        date = time_start, 
        starts_with('start_'), 
        starts_with('end_'), 
        note
    ) %>% 
    filter(!is.na(end_mileage)) %>% 
    mutate(
        year = year(date), 
        month = month(date), 
        day = day(date), 
        date = ymd(paste(year, month, day, sep = '-')),
        start_mileage = as.numeric(start_mileage), 
        start_percent = as.numeric(start_percent), 
        start_range = as.numeric(start_range), 
        end_mileage = as.numeric(end_mileage), 
        end_percent = as.numeric(end_percent), 
        end_range = as.numeric(end_range)
    )
    
data %>% 
    mutate(
        trip_miles = end_mileage - start_mileage, 
        trip_percent = start_percent - end_percent, 
        trip_range = start_range - end_range, 
        range_factor = trip_miles / trip_range
    )
