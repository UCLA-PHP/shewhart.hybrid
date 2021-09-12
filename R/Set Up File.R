#===============================================================================
# This code:
# 1) Create a file that can be passed to the "Covid Limits" function*
# 2) Runs the "Covid Limits" function
# 3) Sets Chart titles and Sub-titles for the Google Studio Charts
# 4) Saves the data needed for the Google Studio Charts

# For example purposes, the data is currently extracted from the 
# New York Times Github site reporting COIVD cases and deaths for 
# all US States and Territories


# * We need to pass the following to the "Covid limits" function:

#   COLUMN      DESCRIPTION                   EXAMPLE DATA COLUMN NAME
#   PLACE:      location                      us_states$state
#   DATE:       date                          us_states$date
#   EVENTS:     cumulative number of events   us_states$deaths
#   NEW_EVENTS: Number of new daily events    us_states$new_deaths

#===============================================================================


#===============================================================================
# Set up all the library connections we need
#===============================================================================

library(readxl)
library(writexl)
library(utils)
library(httr)
library(DT)
library(tidyverse)
library(broom)
library(readr)
require(dplyr)
library(dplyr)
library(lubridate)
library(googlesheets4)

#===============================================================================
# Run the Function Code if you need to refresh it
#===============================================================================

source("C:/Users/ch109010/Documents/COVID/R Code/Function COVID Phase.R")
source("C:/Users/ch109010/Documents/COVID/R Code/Function COVID Limits.R")

#===============================================================================

# Read the data from the NY Times GitHub Site
us_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# Sort the data by state and date
us_states <- us_states[order(us_states$state, us_states$date),]

# Calculate the number of daily deaths
us_states$new_deaths <- us_states$deaths - lag(us_states$deaths)

# Select only those days with daily deaths >=0
us_states <- us_states[ which(us_states$deaths > 0 & us_states$new_deaths >=0) ,]

#===============================================================================
# Run the limit calculations using the function "Covid_limits"
#===============================================================================

US_limits <- Covid_limits(us_states$state, us_states$date, us_states$deaths, us_states$new_deaths)

#===============================================================================
# Create Chart Title and Sub-Titles
#
#   Chart_Title:  Chart Title to show in the Google Studio chart display
#   Epoch_txt:    Description of the current phase/Epoch
#   Sub_Title:    Sub-Title to show in the Google Studio chart display
#===============================================================================


US_limits$Chart_Title <- paste(US_limits$place,": Unadjusted Daily Covid-19 Reported Deaths")

US_limits$Epoch_txt <- if_else(US_limits$EPOCH == 1, "1: Pre-Growth in Daily Reported Deaths",
                               if_else(US_limits$EPOCH == 2, "2: Growth in Daily Reported Deaths",
                                       if_else(US_limits$EPOCH == 3, "3: Plateau or Descent in Daily Reported Deaths",
                                               if_else(US_limits$EPOCH == 4, "4: Stability After Descent in Daily Reported Deaths", ""))))

US_limits$Sub_Title <- paste(US_limits$place, " is in Epoch ", US_limits$Epoch_txt)

#===============================================================================
# Save "US_limits to Google Sheets
#===============================================================================

ToGoogle <- gs4_find("xxCOVID US States")
write_sheet(US_limits, ss = ToGoogle, sheet = 4)

