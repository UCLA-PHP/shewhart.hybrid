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
# Run the functions
#=============================================================================

source("C:/Users/garet/OneDrive/Documents/COVID R/Function XBar Chart II.R")
source("C:/Users/garet/OneDrive/Documents/COVID R/Function I Chart II.R")

#=============================================================================
# Read the data from Excel 
#=============================================================================

UCLA <- read_xlsx("C:/Users/garet/OneDrive/Documents/Current Work/UCLA/UCLA Health/Data/All data.xlsx", 
                  sheet = "Use This", range = NULL, col_names = TRUE,
                  col_types = NULL, na = "", trim_ws = TRUE, skip = 0,
                  progress = readxl_progress(), .name_repair = "unique")

#=============================================================================
# Recode the site names
#=============================================================================

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
# Create a numeric value ID to indicate each site
#=============================================================================

UCLA$ID <- UCLA$Dept...6
UCLA$ID_r <- rank(UCLA$ID, ties.method = "min")
UCLA$ID_r <- match(UCLA$ID_r, sort(unique(UCLA$ID_r)))
UCLA$ID <- UCLA$ID_r


#=============================================================================
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

#=============================================================================
# Set-up for the by site and date charts 
# Aggregate the data by site (ID) & Date
#
# Create a new data.frame UCLA_agg containing:
# 
#   place   = Site name
#   count   = Number of patients per day
#
#   Wait_mn = Mean wait time per day
#   Wait_sd = SD of the wait time per day
#
#   Wait_1  = First wait time for each day
#
#   Seq_mn  = Mean Difference in successive wait times per day
#   Seq_sd  = SD in successive wait times per day
# 
#   Tb_mn   = Mean time between successive patient arrivals per day
#   Seq_sd  = SD itime between successive patient arrivals per day
#
#=============================================================================

UCLA_agg<- UCLA %>%
  group_by(ID, date) %>%
  summarise(place = first(Dept...6),
            count = n(), 
            Wait_mn = mean(Wait),
            Wait_sd = sd(Wait),
            Wait_1 = first(Wait),
            Seq_mn = mean(Seq, na.rm = TRUE),
            Seq_sd = sd(Seq, na.rm = TRUE),
            Tb_mn = mean(Tb),
            Tb_sd = sd(Tb) ) %>%
  ungroup()

#=============================================================================
#=============================================================================
# 1 Wait: Average Patient Wait per day: XBar Chart
#
# Run through the sites from the first (k=1) to the last (K=ID_End)
# Use the function XBar_Chart to calculate the limits
# Add on the extended limits
# Store the data for the limits in a data.frame called Wait_MN
#
#=============================================================================
#=============================================================================

ID_End <- max(UCLA_agg$ID)

k = 1
for (k in 1:ID_End) {

  IDi <- filter(UCLA_agg, ID == k)  
  Wait_MN <- XBar_Chart(IDi$place, IDi$date, IDi$Wait_mn, IDi$Wait_sd, IDi$count)
  
  if (k==1) {Wait_MNx <- Wait_MN }
  if (k > 1) {Wait_MNx <- rbind(Wait_MNx, Wait_MN) }
  
    k = k + 1
  
} #k loop

#=============================================================================
# Recode Lower Limits < 0 to NA
#=============================================================================

Wait_MNx <- Wait_MNx %>% 
  mutate(LOWERa = ifelse(LOWERa < 0, NA, LOWERa),
         LOWERb = ifelse(LOWERb < 0, NA, LOWERb) )

Wait_MN <- Wait_MNx
rm(Wait_MNx)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# CODE FOR EXTENDING THE LIMITS
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Limit_Ext <- Wait_MN %>%
  group_by(place) %>%
  summarise(date = last(date),
            place = last(place),
            Dot = last(Dot), 
            MIDLINEa = last(MIDLINEa), 
            UPPERa = last(UPPERa), 
            LOWERa = last(LOWERa), 
            MIDLINEb = last(MIDLINEb), 
            UPPERb = last(UPPERb), 
            LOWERb = last(LOWERb),
            Phase_Ch = last(Phase_Ch),
            PhaseCount = last(PhaseCount),
            SC = last(SC) ) %>%
  ungroup()

Limit_Ext$Phase_Ch <- NA
Limit_Ext$Dot <- NA
Limit_Ext$SC <- ""

Ext_days <- 7

for (d in 1:Ext_days) {
  
  Limit_Ext$date <- as.Date(Limit_Ext$date) + 1
  
  if (d==1) {Limit_Ext_Full <- Limit_Ext }
  if (d > 1) {Limit_Ext_Full <- rbind(Limit_Ext_Full, Limit_Ext) }
  
} # d loop

Limit_Ext <- Limit_Ext_Full
rm(Limit_Ext_Full)

Wait_MN <- rbind(Wait_MN, Limit_Ext )
Wait_MN <- Wait_MN[order(Wait_MN$place, Wait_MN$date) , ]


#=============================================================================
#=============================================================================
# 2 count: Number of patients per day: I-chart
#
# Run through the sites from the first (k=1) to the last (K=ID_End)
# Use the function I_Chart to calculate the limits
# Add on the extended limits
# Store the data for the limits in a data.frame called Vol_D
#
#=============================================================================
#=============================================================================
#=============================================================================
#=============================================================================

ID_End <- max(UCLA_agg$ID)

k = 1
for (k in 1:ID_End) {
  
  IDi <- filter(UCLA_agg, ID == k)  
  
  Vol_D <- I_Chart(IDi$place, IDi$date, IDi$count)
  if (k==1) {Vol_Dx <- Vol_D }
  if (k > 1) {Vol_Dx <- rbind(Vol_Dx, Vol_D) }
  
  k = k + 1
  
} #k loop

#=============================================================================
# Recode Lower Limits < 0 to NA
#=============================================================================

Vol_Dx <- Vol_Dx %>% 
  mutate(LOWERa = ifelse(LOWERa < 0, NA, LOWERa),
         LOWERb = ifelse(LOWERb < 0, NA, LOWERb) )

Vol_D <- Vol_Dx
rm(Vol_Dx)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# CODE FOR EXTENDING THE LIMITS
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Limit_Ext <- Vol_D %>%
  group_by(place) %>%
  summarise(date = last(date),
            place = last(place),
            Dot = last(Dot), 
            MIDLINEa = last(MIDLINEa), 
            UPPERa = last(UPPERa), 
            LOWERa = last(LOWERa), 
            MIDLINEb = last(MIDLINEb), 
            UPPERb = last(UPPERb), 
            LOWERb = last(LOWERb),
            Phase_Ch = last(Phase_Ch),
            PhaseCount = last(PhaseCount),
            SC = last(SC),
            RowN = last(RowN)) %>%
  ungroup()

Limit_Ext$Phase_Ch <- NA
Limit_Ext$Dot <- NA
Limit_Ext$SC <- ""

Ext_days <- 7

for (d in 1:Ext_days) {
  
  Limit_Ext$date <- as.Date(Limit_Ext$date) + 1
  Limit_Ext$RowN <- Limit_Ext$RowN + 1
  
  if (d==1) {Limit_Ext_Full <- Limit_Ext }
  if (d > 1) {Limit_Ext_Full <- rbind(Limit_Ext_Full, Limit_Ext) }
  
} #d loop

Limit_Ext <- Limit_Ext_Full
rm(Limit_Ext_Full)

Vol_D <- rbind(Vol_D, Limit_Ext )
Vol_D <- Vol_D[order(Vol_D$place, Vol_D$date) , ]

#=============================================================================
#=============================================================================
# 3 Wait_1: First Patient Wait per day: I-chart
#
# Run through the sites from the first (k=1) to the last (K=ID_End)
# Use the function I_Chart to calculate the limits
# Add on the extended limits
# Store the data for the limits in a data.frame called Wait_1st
#
#=============================================================================
#=============================================================================
#=============================================================================
#=============================================================================

k = 1
for (k in 1:ID_End) {
  
  IDi <- filter(UCLA_agg, ID == k)  
  
  Wait_1st <- I_Chart(IDi$place, IDi$date, IDi$Wait_1)
  if (k==1) {Wait_1stx <- Wait_1st }
  if (k > 1) {Wait_1stx <- rbind(Wait_1stx, Wait_1st) }
  
  k = k + 1
  
} #k loop

#=============================================================================
# Recode Lower Limits < 0 to NA
#=============================================================================

Wait_1stx <- Wait_1stx %>% 
  mutate(LOWERa = ifelse(LOWERa < 0, NA, LOWERa),
         LOWERb = ifelse(LOWERb < 0, NA, LOWERb) )

Wait_1st <- Wait_1stx
rm(Wait_1stx)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# CODE FOR EXTENDING THE LIMITS
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Create a Data-frame containing the last row of Wait_1st for each place

Limit_Ext <- Wait_1st %>%
  group_by(place) %>%
  summarise(date = last(date),
            place = last(place),
            Dot = last(Dot), 
            MIDLINEa = last(MIDLINEa), 
            UPPERa = last(UPPERa), 
            LOWERa = last(LOWERa), 
            MIDLINEb = last(MIDLINEb), 
            UPPERb = last(UPPERb), 
            LOWERb = last(LOWERb),
            Phase_Ch = last(Phase_Ch),
            PhaseCount = last(PhaseCount),
            SC = last(SC),
            RowN = last(RowN)) %>%
  ungroup()


Limit_Ext$Phase_Ch <- NA
Limit_Ext$Dot <- NA
Limit_Ext$SC <- ""

Ext_days <- 7

for (d in 1:Ext_days) {
  
  Limit_Ext$date <- as.Date(Limit_Ext$date) + 1
  Limit_Ext$RowN <- Limit_Ext$RowN + 1
  
  if (d==1) {Limit_Ext_Full <- Limit_Ext }
  if (d > 1) {Limit_Ext_Full <- rbind(Limit_Ext_Full, Limit_Ext) }
  
} #d loop

Limit_Ext <- Limit_Ext_Full
rm(Limit_Ext_Full)

Wait_1st <- rbind(Wait_1st, Limit_Ext )
Wait_1st <- Wait_1st[order(Wait_1st$place, Wait_1st$date) , ]


#=============================================================================
# Save the files to Google Drive
#=============================================================================


ToGoogle <- gs4_find("XBarCharts")
ToGoogle <- filter(ToGoogle, name == "XBarCharts")

write_sheet(Wait_MN, ss = ToGoogle, sheet = 1)
write_sheet(Vol_D, ss = ToGoogle, sheet = 2)
write_sheet(Wait_1st, ss = ToGoogle, sheet = 3)


rm(IDi, Limit_Ext, UCLA, UCLA_agg, Vol_D, Wait_1st, Wait_MN)




