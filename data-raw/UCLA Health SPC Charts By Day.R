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


#=============================================================================
# Read the data from Excel
#=============================================================================


UCLA <- read_xlsx("C:/Users/garet/OneDrive/Documents/Current Work/UCLA/UCLA Health/Data/All data.xlsx",
                  sheet = "Use This", range = NULL, col_names = TRUE,
                  col_types = NULL, na = "", trim_ws = TRUE, skip = 0,
                  progress = readxl_progress(), .name_repair = "unique")

UCLA <- UCLA %>%
  mutate(Dept...6 = ifelse(Dept...6 == "CCIC [80708]", "Century City", Dept...6),
         Dept...6 = ifelse(Dept...6 == "MALIBUUC [80546]", "Malibu", Dept...6),
         Dept...6 = ifelse(Dept...6 == "CPN CUL C IC [80718]", "Culver City", Dept...6),
         Dept...6 = ifelse(Dept...6 == "CPN MDR IC [80008]", "Marina Del Rey IC", Dept...6),
         Dept...6 = ifelse(Dept...6 == "CPN WH IC [80716]", "Woodland Hills", Dept...6),
         Dept...6 = ifelse(Dept...6 == "CPN WLS IC [70011]", "Wilshire IC", Dept...6),
         Dept...6 = ifelse(Dept...6 == "EIMGTLIC [80482]", "Toluca Lake IC", Dept...6),
         Dept...6 = ifelse(Dept...6 == "ETCRB [80128]", "Redondo Beach", Dept...6),
         Dept...6 = ifelse(Dept...6 == "ETCSC [80414]", "Santa Clarita", Dept...6),
         Dept...6 = ifelse(Dept...6 == "IM SM EVAL [70187]", "IM SM", Dept...6), )

#=============================================================================
# Make sure the fields we need are formatted and ordered correctly
#=============================================================================

UCLA$date <- as.Date(UCLA$Date)
UCLA <- UCLA[order(UCLA$Dept...6, UCLA$date, UCLA$`ChkIn Time`) , ]
UCLA$RowN <- seq.int(nrow(UCLA))

#=============================================================================

UCLA$ID <- UCLA$Dept...6

UCLA$ID_r <- rank(UCLA$ID, ties.method = "min")
UCLA$ID_r <- match(UCLA$ID_r, sort(unique(UCLA$ID_r)))
UCLA$ID <- UCLA$ID_r


#=============================================================================
# Set-up for the by site and date charts
#
# Create more manageable variable names for the measures and transform them as
# minutes
#
#   Wait  = Wait time
#   Seq   = Difference between successive wait times
#   Tb    = Time between successive patient arrivals
#
#=============================================================================


UCLA <- UCLA %>%
  mutate(Wait = `Wait time after start`,
         Hours = as.numeric(format( Wait, format = "%H")),
         Minutes = as.numeric(format( Wait, format = "%M")),
         Seconds = as.numeric(format( Wait, format = "%S")),
         Wait = 60*Hours + Minutes + Seconds/60 ,

         Seq = Wait - lag(Wait),
         Hours = as.numeric(format( Seq, format = "%H")),
         Minutes = as.numeric(format( Seq, format = "%M")),
         Seconds = as.numeric(format( Seq, format = "%S")),
         Seq = 60*Hours + Minutes + Seconds/60,

         Tb = `Time in between pt arrivals`,
         Hours = as.numeric(format( Tb, format = "%H")),
         Minutes = as.numeric(format( Tb, format = "%M")),
         Seconds = as.numeric(format( Tb, format = "%S")),
         Tb = 60*Hours + Minutes + Seconds/60 )



#===============================================================================
#===============================================================================
#
# LAST WEEK OF DATA
#
#===============================================================================
#===============================================================================

#===============================================================================
#
# Create a field PlaceDate concatenating the Place and date
# use PlaceDate as the place when running the I_chart function
#
#===============================================================================

UCLA$place <- UCLA$Dept...6
ID_End <- max(UCLA$ID)

n = 1

for (n in 1:ID_End) {
IDn <- filter(UCLA, ID == n)
Last_Date <- last(IDn$Date)
Last_Week <- as.Date(Last_Date) - 6
IDn <- filter(IDn, Date >= Last_Week & Date <= Last_Date)
IDn$PlaceDate <- paste(IDn$place, IDn$Date)

if (n==1) {LastWeek <- IDn }
if (n > 1) {LastWeek <- rbind(LastWeek, IDn) }

} # n loop

#===============================================================================
# Create the X-Axis Label Display within LastWeek for merging with the
# subsequent limits below
#===============================================================================

LastWeek$ID_r <- rank(LastWeek$PlaceDate, ties.method = "min")
LastWeek$ID_r <- match(LastWeek$ID_r, sort(unique(LastWeek$ID_r)))
LastWeek$ID_n <- LastWeek$ID_r


LastWeek$ChkIn_Label <- NA

LastWeek <- LastWeek %>%
  group_by(ID, date) %>%
  mutate(ChkIn_First = first(`ChkIn Time`),
         RowN_First = first(RowN)) %>%
  ungroup()


LastWeek$RowN_N <- LastWeek$RowN - LastWeek$RowN_First + 1

LastWeek$ChkIn_Label <- ""


LastWeek$LabelDy <- substr(LastWeek$`Day of Week`, start = 1, stop = 3)

LastWeek$LabelD <- paste(as.character(format( LastWeek$date, format = "%m")),
                         as.character(format( LastWeek$date, format = "%d")),
                         sep = "/")

LastWeek$Labelt <- paste(as.character(format( LastWeek$`ChkIn Time`, format = "%H")),
                         as.character(format( LastWeek$`ChkIn Time`, format = "%M")),
                         sep = ":")

LastWeek$Labeln <- paste(" (", LastWeek$RowN_N,")", sep = "")

LastWeek$Label <- paste(LastWeek$LabelDy,
                        LastWeek$LabelD,
                        LastWeek$Labelt,
                        LastWeek$Labeln )


#=============================================================================
#=============================================================================
# WAIT TIME: Sequential Patients: I-chart
#
# Run through the sites from the first (k=1) to the last [max(LastWeek$ID_n)]
# Use the function I_Chart to calculate the limits
# Add on the extended limits
# Store the data for the limits in a data.frame called LastWeek_Seq_1
#
#=============================================================================
#=============================================================================

k = 1
for (k in 1:max(LastWeek$ID_n)) {

  IDi <- filter(LastWeek, ID_n == k)

  LastWeek_seq <- I_Chart(IDi$PlaceDate, IDi$date, IDi$Wait)
  if (k==1) {LastWeek_Seq_1 <- LastWeek_seq }
  if (k > 1) {LastWeek_Seq_1 <- rbind(LastWeek_Seq_1, LastWeek_seq) }

  k = k + 1

} #k loop

#=============================================================================
# Recode Lower Limits < 0 to NA
#=============================================================================

LastWeek_Seq_1 <- LastWeek_Seq_1 %>%
  mutate(LOWERa = ifelse(LOWERa < 0, NA, LOWERa),
         LOWERb = ifelse(LOWERb < 0, NA, LOWERb) )

#=============================================================================
# Bolt on Place
#=============================================================================

LastWeek_Seq_1 <- rename(LastWeek_Seq_1, PlaceDate = place)
LastWeek_Seq_1$day <- LastWeek$`Day (formula)`
LastWeek_Seq_1$place <- LastWeek$place

#=============================================================================
# Organize the limits for display
#=============================================================================


LastWeek_Seq_1 <- LastWeek_Seq_1 %>%
  mutate(Odd_MLa = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , MIDLINEa, NA),
         Odd_ULa = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , UPPERa, NA),
         Odd_LLa = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , LOWERa, NA),

         Odd_MLb = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , MIDLINEb, NA),
         Odd_ULb = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , UPPERb, NA),
         Odd_LLb = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , LOWERb, NA),

         Odd_dot = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , Dot, NA),

         Eve_MLa = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", MIDLINEa, NA),
         Eve_ULa = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", UPPERa, NA),
         Eve_LLa = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", LOWERa, NA),

         Eve_MLb = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", MIDLINEb, NA),
         Eve_ULb = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", UPPERb, NA),
         Eve_LLb = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", LOWERb, NA),

         Eve_dot = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", Dot, NA) )


#-------------------------------------------------------------------------------
# Create the X-Axis labels for the displays
#-------------------------------------------------------------------------------

LastWeek <- LastWeek[order(LastWeek$place, LastWeek$Date) , ]
LastWeek_Seq_1 <- LastWeek_Seq_1[order(LastWeek_Seq_1$place, LastWeek_Seq_1$date) , ]
LastWeek_Seq_1 <- cbind(LastWeek_Seq_1, LastWeek$Label)

LastWeek_Seq_1$Label <- LastWeek_Seq_1$`LastWeek$Label`

LastWeek_Seq_1$RowN <- seq.int(nrow(LastWeek_Seq_1))

LastWeek_Seq_1 <- LastWeek_Seq_1 %>% select(date, place,
                                             Odd_dot, Odd_MLa, Odd_ULa, Odd_LLa, Odd_MLb, Odd_ULb, Odd_LLb,
                                             Eve_dot, Eve_MLa, Eve_ULa, Eve_LLa, Eve_MLb, Eve_ULb, Eve_LLb,
                                             Phase_Ch, PhaseCount, SC, RowN, Label)



ToGoogle <- gs4_find("XBarCharts")
ToGoogle <- filter(ToGoogle, name == "XBarCharts")
write_sheet(LastWeek_Seq_1, ss = ToGoogle, sheet = 6)



#=============================================================================
# TIME BETWEEN PATIENT ARRIVALS Sequential Patients: I-chart
#
# Run through the sites from the first (k=1) to the last [max(LastWeek$ID_n)]
# Use the function I_Chart to calculate the limits
# Add on the extended limits
# Store the data for the limits in a data.frame called LastWeek_tb_1
#
#=============================================================================

k = 1
for (k in 1:max(LastWeek$ID_n)) {

  IDi <- filter(LastWeek, ID_n == k)

  LastWeek_tb <- I_Chart(IDi$PlaceDate, IDi$date, IDi$Tb)
  if (k==1) {LastWeek_tb_1 <- LastWeek_tb }
  if (k > 1) {LastWeek_tb_1 <- rbind(LastWeek_tb_1, LastWeek_tb) }

  k = k + 1

} #k loop


#=============================================================================
# Recode Lower Limits < 0 to NA
#=============================================================================

LastWeek_tb_1 <- LastWeek_tb_1 %>%
  mutate(LOWERa = ifelse(LOWERa < 0, NA, LOWERa),
         LOWERb = ifelse(LOWERb < 0, NA, LOWERb) )


#=============================================================================
# Bolt on Place
#=============================================================================

LastWeek_tb_1 <- rename(LastWeek_tb_1, PlaceDate = place)
LastWeek_tb_1$day <- LastWeek$`Day (formula)`
LastWeek_tb_1$place <- LastWeek$place


#=============================================================================
# Organize the limits for display
#=============================================================================


LastWeek_tb_1 <- LastWeek_tb_1 %>%
  mutate(Odd_MLa = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , MIDLINEa, NA),
         Odd_ULa = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , UPPERa, NA),
         Odd_LLa = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , LOWERa, NA),

         Odd_MLb = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , MIDLINEb, NA),
         Odd_ULb = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , UPPERb, NA),
         Odd_LLb = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , LOWERb, NA),

         Odd_dot = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , Dot, NA),

         Eve_MLa = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", MIDLINEa, NA),
         Eve_ULa = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", UPPERa, NA),
         Eve_LLa = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", LOWERa, NA),

         Eve_MLb = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", MIDLINEb, NA),
         Eve_ULb = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", UPPERb, NA),
         Eve_LLb = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", LOWERb, NA),

         Eve_dot = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", Dot, NA) )

#-------------------------------------------------------------------------------
# Create the X-Axis labels for the displays
#-------------------------------------------------------------------------------

LastWeek_tb_1 <- LastWeek_tb_1[order(LastWeek_tb_1$place, LastWeek_tb_1$date) , ]
LastWeek_tb_1 <- cbind(LastWeek_tb_1, LastWeek$Label)

LastWeek_tb_1$Label <- LastWeek_tb_1$`LastWeek$Label`

LastWeek_tb_1$RowN <- seq.int(nrow(LastWeek_tb_1))

LastWeek_tb_1 <- LastWeek_tb_1 %>% select(date, place,
                                          Odd_dot, Odd_MLa, Odd_ULa, Odd_LLa, Odd_MLb, Odd_ULb, Odd_LLb,
                                          Eve_dot, Eve_MLa, Eve_ULa, Eve_LLa, Eve_MLb, Eve_ULb, Eve_LLb,
                                          Phase_Ch, PhaseCount, SC, RowN, Label)


ToGoogle <- gs4_find("XBarCharts")
ToGoogle <- filter(ToGoogle, name == "XBarCharts")
write_sheet(LastWeek_tb_1, ss = ToGoogle, sheet = 7)



#===============================================================================
#===============================================================================
#
# LAST SIX WEEKS OF DATA
#
#===============================================================================
#===============================================================================


#===============================================================================
#
# Select the last six weeks of data
# Create a field PlaceDay concatenating the Place and Day
# use PlaceDay as the place when running the XBar function
#
#===============================================================================

UCLA$place <- UCLA$Dept...6
ID_End <- max(UCLA$ID)

n = 1

for (n in 1:ID_End) {
  IDn <- filter(UCLA, ID == n)
  Last_Date <- last(IDn$Date)
  Last_6W <- as.Date(Last_Date) - 42
  IDn <- filter(IDn, Date >= Last_6W & Date <= Last_Date)
  IDn$PlaceDay <- paste(IDn$place, IDn$`Day of Week`)

  if (n==1) {Last6W <- IDn }
  if (n > 1) {Last6W <- rbind(Last6W, IDn) }

} # n loop


# Find the hour of day

Last6W$ChkIn_hr <- as.character(format( Last6W$`ChkIn Time`, format = "%H"))
Last6W$Day <- Last6W$`Day of Week`
Last6W$PlaceDay <- paste(Last6W$place, Last6W$Day)

Last6W$ID_r <- rank(Last6W$PlaceDay, ties.method = "min")
Last6W$ID_r <- match(Last6W$ID_r, sort(unique(Last6W$ID_r)))
Last6W$ID_n <- Last6W$ID_r

Last6W_HR <- Last6W %>%
  group_by(PlaceDay, ChkIn_hr) %>%
  summarise(place = first(Dept...6),
            ID_n = first(ID_n),
            day = first(Day),
            count = n(),
            time_mn = mean(Wait),
            time_sd = sd(Wait),
            time_1 = first(Wait),
            Seq_mn = mean(Seq, na.rm = TRUE),
            Seq_sd = sd(Seq, na.rm = TRUE),
            Tb_mn = mean(Tb),
            Tb_sd = sd(Tb),
            ChkIn_First = first(`ChkIn Time` )) %>%
 ungroup()

#remove missing ChkIn hour and set NA sd to be the mean sd for now

Last6W_HR <- Last6W_HR[ is.na(Last6W_HR$ChkIn_hr)==FALSE ,]

#===============================================================================
#===============================================================================
#
# WAIT TIME: Wait time by hour of Check-In: Xbar-chart by hour for last 6 weeks
#
#===============================================================================
#===============================================================================

k = 1
for (k in 1:max(Last6W$ID_n)) {

  IDi <- filter(Last6W_HR, ID_n == k)
  IDi$time_sd <- ifelse(is.na(IDi$time_sd), mean(IDi$time_sd, na.rm = TRUE), IDi$time_sd)
  Last6W_HRi <- XBar_Chart(IDi$PlaceDay, IDi$ChkIn_hr, IDi$time_mn, IDi$time_sd, IDi$count)

  if (k==1) {Hour_Dy <- Last6W_HRi }
  if (k > 1) {Hour_Dy <- rbind(Hour_Dy, Last6W_HRi) }

  k = k + 1

} #k loop


#=============================================================================
# Recode Lower Limits < 0 to NA
#=============================================================================

Hour_Dy <- Hour_Dy %>%
  mutate(LOWERa = ifelse(LOWERa < 0, NA, LOWERa),
         LOWERb = ifelse(LOWERb < 0, NA, LOWERb) )

#=============================================================================
# Bolt on Place & Day
#=============================================================================

Hour_Dy$PlaceDay <- Hour_Dy$place
Hour_Dy$place <- Last6W_HR$place
Hour_Dy$ChkIn_hr <- Hour_Dy$date
Hour_Dy$day <- Last6W_HR$day

#=============================================================================
# Organize the limits for display
#=============================================================================


Hour_Dy <- Hour_Dy %>%
  mutate(Odd_MLa = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , MIDLINEa, NA),
         Odd_ULa = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , UPPERa, NA),
         Odd_LLa = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , LOWERa, NA),

         Odd_MLb = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , MIDLINEb, NA),
         Odd_ULb = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , UPPERb, NA),
         Odd_LLb = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , LOWERb, NA),

         Odd_dot = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , Dot, NA),

         Eve_MLa = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", MIDLINEa, NA),
         Eve_ULa = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", UPPERa, NA),
         Eve_LLa = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", LOWERa, NA),

         Eve_MLb = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", MIDLINEb, NA),
         Eve_ULb = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", UPPERb, NA),
         Eve_LLb = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", LOWERb, NA),

         Eve_dot = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", Dot, NA) )


#=============================================================================
# Organize the data by Place and Friday to Thursday for display
#=============================================================================

Hour_Dy$Day_order <- 0
Hour_Dy <- Hour_Dy %>%
  mutate(Day_order = ifelse(day == "Friday", 1, Day_order),
         Day_order = ifelse(day == "Saturday", 2, Day_order),
         Day_order = ifelse(day == "Sunday", 3, Day_order),
         Day_order = ifelse(day == "Monday", 4, Day_order),
         Day_order = ifelse(day == "Tuesday", 5, Day_order),
         Day_order = ifelse(day == "Wednesday", 6, Day_order),
         Day_order = ifelse(day == "Thursday", 7, Day_order) )

Hour_Dy <- Hour_Dy[order(Hour_Dy$place, Hour_Dy$Day_order, Hour_Dy$date) , ]

#-------------------------------------------------------------------------------
# Create the X-Axis labels for the displays
#-------------------------------------------------------------------------------


Hour_Dy$Label <- paste(Hour_Dy$day, Hour_Dy$ChkIn_hr)
Hour_Dy$Label <- paste(Hour_Dy$day, " (", Hour_Dy$ChkIn_hr, ":00)", sep = "")


Hour_Dy$RowN <- seq.int(nrow(Hour_Dy))

Hour_Dy <- Hour_Dy %>% select(date, place,
                                          Odd_dot, Odd_MLa, Odd_ULa, Odd_LLa, Odd_MLb, Odd_ULb, Odd_LLb,
                                          Eve_dot, Eve_MLa, Eve_ULa, Eve_LLa, Eve_MLb, Eve_ULb, Eve_LLb,
                                          Phase_Ch, PhaseCount, SC, RowN, Label)



ToGoogle <- gs4_find("XBarCharts")
ToGoogle <- filter(ToGoogle, name == "XBarCharts")
write_sheet(Hour_Dy, ss = ToGoogle, sheet = 11)


#===============================================================================
#===============================================================================
#
# VOLUME: Volume by day of week over time: Ibar-chart by day of week
#
#===============================================================================
#===============================================================================

UCLA$place <- UCLA$Dept...6
UCLA$PlaceDay <- paste(UCLA$place, UCLA$day)
ID_End <- max(UCLA$ID)



UCLA$ID_r <- rank(UCLA$PlaceDay, ties.method = "min")
UCLA$ID_r <- match(UCLA$ID_r, sort(unique(UCLA$ID_r)))
UCLA$ID_n <- UCLA$ID_r

UCLA_vol<- UCLA %>%
  group_by(place, date) %>%
  summarise(place = first(Dept...6),
            ID = first(ID_n),
            count = n(),
            date = first(date),
            day = first(`Day of Week`) ) %>%
  ungroup()




UCLA_vol <- UCLA_vol[order(UCLA_vol$place, UCLA_vol$day, UCLA_vol$date) , ]

UCLA_vol$PlaceDay <- paste(UCLA_vol$place, UCLA_vol$day)

UCLA_vol$ID_r <- rank(UCLA_vol$PlaceDay, ties.method = "min")
UCLA_vol$ID_r <- match(UCLA_vol$ID_r, sort(unique(UCLA_vol$ID_r)))
UCLA_vol$ID_n <- UCLA_vol$ID_r


#=============================================================================
#=============================================================================
# 2 count: Number of patients per day: I-chart
#=============================================================================
#=============================================================================
source("C:/Users/garet/OneDrive/Documents/COVID R/Function I Chart II.R")

ID_End <- max(UCLA_vol$ID_n)

k = 1
for (k in 1:ID_End) {

  IDi <- filter(UCLA_vol, ID_n == k)

  Vol_D <- I_Chart(IDi$PlaceDay, IDi$date, IDi$count)
  if (k==1) {Vol_Day <- Vol_D }
  if (k > 1) {Vol_Day <- rbind(Vol_Day, Vol_D) }

  k = k + 1

} #k loop


#=============================================================================
# Recode Lower Limits < 0 to NA
#=============================================================================

Vol_Day <- Vol_Day %>%
  mutate(LOWERa = ifelse(LOWERa < 0, NA, LOWERa),
         LOWERb = ifelse(LOWERb < 0, NA, LOWERb) )

#=============================================================================
# Bolt on Place & Day
#=============================================================================

Vol_Day$PlaceDay <- Vol_Day$place
Vol_Day$place <- UCLA_vol$place
Vol_Day$day <- UCLA_vol$day

#=============================================================================
# Organize the limits for display
#=============================================================================


Vol_Day <- Vol_Day %>%
  mutate(Odd_MLa = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , MIDLINEa, NA),
         Odd_ULa = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , UPPERa, NA),
         Odd_LLa = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , LOWERa, NA),

         Odd_MLb = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , MIDLINEb, NA),
         Odd_ULb = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , UPPERb, NA),
         Odd_LLb = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , LOWERb, NA),

         Odd_dot = ifelse(day == "Friday" | day == "Sunday" | day == "Tuesday" | day == "Thursday" , Dot, NA),

         Eve_MLa = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", MIDLINEa, NA),
         Eve_ULa = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", UPPERa, NA),
         Eve_LLa = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", LOWERa, NA),

         Eve_MLb = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", MIDLINEb, NA),
         Eve_ULb = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", UPPERb, NA),
         Eve_LLb = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", LOWERb, NA),

         Eve_dot = ifelse(day == "Saturday" | day == "Monday" | day == "Wednesday", Dot, NA) )


#=============================================================================
# Organize the data by Place and Friday to Thursday for display
#=============================================================================

Vol_Day$Day_order <- 0
Vol_Day <- Vol_Day %>%
  mutate(Day_order = ifelse(day == "Friday", 1, Day_order),
         Day_order = ifelse(day == "Saturday", 2, Day_order),
         Day_order = ifelse(day == "Sunday", 3, Day_order),
         Day_order = ifelse(day == "Monday", 4, Day_order),
         Day_order = ifelse(day == "Tuesday", 5, Day_order),
         Day_order = ifelse(day == "Wednesday", 6, Day_order),
         Day_order = ifelse(day == "Thursday", 7, Day_order) )

Vol_Day <- Vol_Day[order(Vol_Day$place, Vol_Day$Day_order, Vol_Day$date) , ]

#-------------------------------------------------------------------------------
# Create the X-Axis labels for the displays
#-------------------------------------------------------------------------------


Vol_Day$Label <- paste(Vol_Day$day, Vol_Day$date, sep = ": ")


Vol_Day$RowN <- seq.int(nrow(Vol_Day))

Vol_Day <- Vol_Day %>% select(date, place,
                              Odd_dot, Odd_MLa, Odd_ULa, Odd_LLa, Odd_MLb, Odd_ULb, Odd_LLb,
                              Eve_dot, Eve_MLa, Eve_ULa, Eve_LLa, Eve_MLb, Eve_ULb, Eve_LLb,
                              Phase_Ch, PhaseCount, SC, RowN, Label)



ToGoogle <- gs4_find("XBarCharts")
ToGoogle <- filter(ToGoogle, name == "XBarCharts")
write_sheet(Vol_Day, ss = ToGoogle, sheet = 12)





