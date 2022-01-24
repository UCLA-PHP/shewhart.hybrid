
#===============================================================================
# This code:
# 1) Imports a data_frame to use for the limit calculations*
# 2) Sets up the data and for each "place":
# 2a) Runs the firs Phase to identify the first special cause
# 2b) If a first special cause is found, calculates the limits for subsequent
#     using "COVID Phase" function




#-------------------------------------------------------------------------------
# * We need to pass the following to the "Covid limits" function:

#   COLUMN      DESCRIPTION                   EXAMPLE DATA COLUMN NAME
#   PLACE:      location                      us_states$state
#   DATE:       date                          us_states$date
#   EVENTS:     cumulative number of events   us_states$deaths
#   NEW_EVENTS: Number of new daily events    us_states$new_deaths

#===============================================================================


# Receive a clean dataset starting with the first event and sorted by place & date

#' Title
#'
#' @param place
#' @param date
#' @param events
#' @param new_events
#'
#' @return
#' @export
#'
Covid_limits <- function(place, date, events, new_events) {
  Place_data <- data_frame(place, date, events, new_events)


#Calculate place name as integer values (PlaceRR)
  Place_data$PlaceR <- rank(Place_data$place, ties.method = "min")
  Place_data$PlaceRR <- match(Place_data$PlaceR, sort(unique(Place_data$PlaceR)))


  Place_END <- max(Place_data$PlaceRR)


#===============================================================================
# Go through each place (i) and calculate the limits
#===============================================================================

  i=1
  for (i in 1:Place_END) {
#    for (i in 1:141) {
  Statei <- filter(Place_data, PlaceRR ==i)
  Statei$Days_N <- rank(Statei$date)
  Statei$events <- cumsum(Statei$new_events)


  #=============================================================================
  # Set up and initialize some variables and values for later use
  #=============================================================================

  Statei$Y_Max <- 2*Statei$new_events
  Statei$Y_Min <- 20
  Statei$Y_Max <- if_else( Statei$Y_Max < Statei$Y_Min, Statei$Y_Min, Statei$Y_Max )

  Statei$Phase_Ch <- -99.0

  Statei$MIDLINEa <- 0
  Statei$LOWERa <- 0
  Statei$UPPERa <- 0

  Statei$MIDLINEb <- 0
  Statei$LOWERb <- 0
  Statei$UPPERb <- 0

  Statei$EPOCH <- 1
  Statei$Phase_N <- 0
  Statei$Phase_Count <- 1

  EPOCH <- 1
  Day_P2 <- Inf
  LimTypeA <- TRUE

  Statei$Phase1 <-0

  #=============================================================================
  # Create the variables to store the Extended limits and initialize them to 0
  #=============================================================================

  Ext_Days <- min(nrow(Statei), 14)
  Statei$Days_EXT <- max(Statei$Days_N)
  Statei$date_EXT <- max(Statei$date)

  d=1
  for (d in 1:Ext_Days) {
    Statei$Days_EXT[d] <- Statei$Days_EXT[d] + d
    Statei$date_EXT[d] <- Statei$date_EXT[d] + d
    d <- d + 1
  }

  Statei$MIDLINE_EXT <- 0.0
  Statei$LOWER_EXT <- 0.0
  Statei$UPPER_EXT <- 0.0
  #=============================================================================

  #===============================================================================
  # PHASE 1 ANALYSIS
  #-------------------------------------------------------------------------------
  # Start with a C-Chart and look for special cause above the
  # upper limit
  #
  # Key Variable names:
  # Date_P1 (Constant):     Date when Phase1 starts
  # Date_P2 (Constant):     Date when Phase2 starts
  # Day_P2 (Constant):     Day number when Phase2 Starts
  # State$Days_N (Vector):  Number of days since the first
  #                         death occurred
  #===============================================================================


  # Calculate the mean number of events per day (Phase_1MN) based on the cumulative
  # number of events then calculate the upper limit (UL) above which indicates
  # special cause (Criteria1 ==1) and the start of phase 2

  Statei$Phase_1MN <- Statei$events/Statei$Days_N
  Statei$UL <- (Statei$events/Statei$Days_N) + (3*sqrt(Statei$events/Statei$Days_N))

  Statei$Criteria1 <- 0
  Statei$Criteria1 = if_else(Statei$new_events >= Statei$UL, 1,0)


  # Calculate two tokens indicating the starting date (Date_P1) and
  # end date (Date_P2) of Phase 1
  Date_P1 <- min(Statei$date)
  Date_P2 <- min(Statei$date[Statei$Criteria1==1])


  # Calculate a field "Phase1" indicating 1 if the date is in Phase 1 and 0 if it is
  # after Phase 1
  Statei$Phase1 <- if_else(Statei$date < Date_P2, 1,0)

  # Calculate a token (Day_P2) indicating the number of days since the start of
  # Phase 1 that special cause was found and Phase 2 starts
  Day_P2 <- min(Statei$Days_N[Statei$Criteria1==1])


  # Calculate the limits for Phase 1


  # Pull Upper limit (UL) and midline (Phase_1MN) values from the last date in Phase 1
  # then put them in the fields UPPER_1 and MIDLINE_1

  UL_USE <- if_else(Day_P2==Inf, last(Statei$UL), Statei$UL[Day_P2])
  MN_USE <- if_else(Day_P2==Inf, last(Statei$Phase_1MN), Statei$Phase_1MN[Day_P2])

  Statei$UPPER_1 <- if_else(Statei$Days_N <= Day_P2 & Statei$Phase1 == 1, UL_USE, 0)
  Statei$MIDLINE_1 <- if_else(Statei$Days_N <= Day_P2 & Statei$Phase1==1, MN_USE,0)

  Statei$UPPER_N <- UL_USE
  Statei$MIDLINE_N <- MN_USE
  Statei$LOWER_N <- 0


  # Set the values to put in any extended limits we are using in the
  # Google Studio chart displays
  Statei$MIDLINE_EXT <- MN_USE
  Statei$LOWER_EXT <- 0.0
  Statei$UPPER_EXT <- UL_USE

  #-------------------------------------------------------------------------------
  # EPOCH Set: The first phase is EPOCH 1
  #-------------------------------------------------------------------------------

  Statei$Date_PC <- Date_P2
  EPOCH <- 1

  # Calculate Day_PC = Day_P2 -- the day on which the Phase changes and the
  # new phase starts

  Statei$Day_PC <- Day_P2

  #=============================================================================
  # Put the limits into MIDLINEa, LOWERa & UPPERa
  # Note to draw the charts in Google studio with distinct limits for each phase
  # we need to store the limits in fields ending a/b for each phase.
  #=============================================================================

  Statei$MIDLINEa <- Statei$MIDLINE_1
  Statei$LOWERa <- 0
  Statei$UPPERa <- Statei$UPPER_1
  Statei$Phase_Count <- 2

  New_Phase <- (is.infinite(Day_P2)==FALSE)

  #=============================================================================
  # IF there is a new phase (ie phase 2) then run through the days in this place
  # setting and calculate the rest of the limits using the "Covid_Phase" function
  #=============================================================================

  if (New_Phase) {

  p <- 1
  while (p < nrow(Statei)) {

    Statei <- Covid_Phase(Statei$PlaceRR, Statei$place, Statei$date, Statei$events, Statei$new_events,
                             Statei$MIDLINE_N, Statei$LOWER_N, Statei$UPPER_N,
                             Statei$Days_N, Statei$Day_PC, Statei$Date_PC, Statei$EPOCH, Statei$Phase_N, Statei$Phase_Count,
                             Statei$MIDLINEa, Statei$LOWERa, Statei$UPPERa, Statei$MIDLINEb, Statei$LOWERb, Statei$UPPERb, Statei$Phase_Ch)

    New_Phase <- (is.infinite(last(Statei$Day_PC) )==FALSE)

    p <- if_else(New_Phase, p+1, nrow(Statei) + 1)
  }


  } # END if(New_Phase)

# ==============================================================================
# Calculate the extended limits used in the Google Studio chart displays
# ==============================================================================

  Statei_EXT <- Statei %>% select(date_EXT, place, PlaceRR,
                                     MIDLINE_EXT, LOWER_EXT, UPPER_EXT, Y_Max, EPOCH)

  Statei_EXT$date <- Statei_EXT$date_EXT
  Statei_EXT$new_events <- -9
  Statei_EXT$Y_Max <- max(Statei_EXT$Y_Max)
  Statei_EXT$Phase_Ch <- -99.0
  Statei_EXT$EPOCH <- last(Statei_EXT$EPOCH)

  Statei_EXT <- Statei_EXT[ which(Statei_EXT$date_EXT >= first(Statei_EXT$date_EXT)) ,]

  if(LimTypeA == TRUE) {
    Statei_EXT$MIDLINEa <- Statei_EXT$MIDLINE_EXT
    Statei_EXT$LOWERa <- Statei_EXT$LOWER_EXT
    Statei_EXT$UPPERa <- Statei_EXT$UPPER_EXT

    Statei_EXT$MIDLINEb <- 0.0
    Statei_EXT$LOWERb <- 0.0
    Statei_EXT$UPPERb <- 0.0
  }

  if(LimTypeA == FALSE) {
    Statei_EXT$MIDLINEb <- Statei_EXT$MIDLINE_EXT
    Statei_EXT$LOWERb <- Statei_EXT$LOWER_EXT
    Statei_EXT$UPPERb <- Statei_EXT$UPPER_EXT

    Statei_EXT$MIDLINEa <- 0.0
    Statei_EXT$LOWERa <- 0.0
    Statei_EXT$UPPERa <- 0.0
  }


  #=============================================================================
  # Select the fields from Statei_EXT we need for the Google Studio Displays
  #=============================================================================

  Statei_EXT <- Statei_EXT %>% select(date, place, new_events, PlaceRR,
                                      MIDLINEa, UPPERa, LOWERa,
                                      MIDLINEb, UPPERb, LOWERb,
                                      Y_Max, Phase_Ch, EPOCH)


  #=============================================================================
  # Select the fields we need from Statei for the Google Studio Displays
  # and then use "rbind" to merge them with the extended limits dataframe
  #=============================================================================

  Statei <- Statei %>% select(date, place, new_events, PlaceRR,
                                  MIDLINEa, UPPERa, LOWERa,
                                  MIDLINEb, UPPERb, LOWERb,
                                  Y_Max, Phase_Ch, EPOCH)


  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  Statei <- rbind(Statei, Statei_EXT)


  #=============================================================================

  # Recode to Scale the limits used in the Google studio chart displays to
  # to be  < YMax

  Statei$Y_Max <- 2*Statei$new_events
  Statei$Y_Min <- 20
  Statei$Y_Max <- if_else( Statei$Y_Max < Statei$Y_Min, Statei$Y_Min, Statei$Y_Max )

  Y_MAX_C <- max(Statei$Y_Max)

  Statei$MIDLINEa <- if_else(Statei$MIDLINEa >  Y_MAX_C , 0, Statei$MIDLINEa)
  Statei$MIDLINEb <- if_else(Statei$MIDLINEb >  Y_MAX_C , 0, Statei$MIDLINEb)

  Statei$LOWERa <- if_else(Statei$LOWERa >  Y_MAX_C , 0, Statei$LOWERa)
  Statei$LOWERb <- if_else(Statei$LOWERb >  Y_MAX_C , 0, Statei$LOWERb)

  Statei$UPPERa <- if_else(Statei$UPPERa >  Y_MAX_C , 0, Statei$UPPERa)
  Statei$UPPERb <- if_else(Statei$UPPERb >  Y_MAX_C , 0, Statei$UPPERb)


  #=============================================================================


  # Recode missing values into NA

  Statei$MIDLINEa <- na_if(Statei$MIDLINEa, 0)
  Statei$MIDLINEb <- na_if(Statei$MIDLINEb, 0)

  Statei$UPPERa <- na_if(Statei$UPPERa, 0)
  Statei$UPPERb <- na_if(Statei$UPPERb, 0)

  Statei$LOWERa <- na_if(Statei$LOWERa, 0)
  Statei$LOWERb <- na_if(Statei$LOWERb, 0)

  Statei$new_events <- na_if(Statei$new_events, -9)

  Statei$Phase_Ch <- na_if(Statei$Phase_Ch, -99.0)

  #=============================================================================
  # Select the fields from Statei we need for the Google Studio Displays and
  # use rbind to create a final dataframe "Outputdata" with all the limits
  #=============================================================================

  Statei <- Statei %>% select(date, place, new_events, PlaceRR,
                              MIDLINEa, UPPERa, LOWERa,
                              MIDLINEb, UPPERb, LOWERb,
                              Y_Max, Phase_Ch, EPOCH)

  if (i==1) {OutputData <- Statei }
  if (i > 1) {OutputData <- rbind(OutputData, Statei) }

  i= i + 1

} #End of i (place) loop

  return(OutputData)


} #End of Covid_limits function

