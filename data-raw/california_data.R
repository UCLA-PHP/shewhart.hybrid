library(magrittr)
us_states = read_csv("inst/extdata/california_data.csv",name_repair = "unique",
                     na = c("", "NA", "null"),
                     col_types = cols("date" = "c"))

# Calculate the number of daily deaths
us_states$new_deaths <- us_states$`Daily Deaths`
us_states %<>%
  mutate(
    date = as.Date(date, format = "%b %d, %Y"),
    state = "California",
    deaths = cumsum(new_deaths)
  )

# Select only those days with daily deaths >=0
us_states <- us_states[ which(us_states$deaths > 0 & us_states$new_deaths >=0) ,]

usethis::use_data(us_states, overwrite = TRUE)
