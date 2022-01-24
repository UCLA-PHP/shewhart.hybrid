
#===============================================================================
# Read in the Percent Positive by age group data
#===============================================================================
library(tidyverse)
Pchart_Test_Data =
  read_csv("inst/extdata/P Chart Test.csv") %>%
  # Make sure the date is formatted correctly
  mutate(
    date = as.Date(date, "%m/%d/%Y"))## code to prepare `Pchart_Test_Data` dataset goes here

usethis::use_data(Pchart_Test_Data, overwrite = TRUE)
