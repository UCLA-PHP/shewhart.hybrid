
#===============================================================================
# This code:
# 1) Imports a data_frame to use for the limit calculations*
# 2) Sets up the data
# 2a) Runs the firs Phase to identify the first special cause
# 2b) If a first special cause is found, calculates the limits for subsequent
#     using "COVID Phase" function




#-------------------------------------------------------------------------------
# * We need to pass the following to the "Covid limits" function:

#   COLUMN      DESCRIPTION                   EXAMPLE DATA COLUMN NAME
#   DATE:       date                          us_states$date
#   EVENTS:     cumulative number of events   us_states$deaths
#   NEW_EVENTS: Number of new daily events    us_states$new_deaths

#===============================================================================


# Receive a clean dataset starting with the first event and sorted by date

#' Title
#'
#' @param dataset a data.frame or tibble with columns "date" and "new_events"; use this function one population at a time.
#' @return
#' @export
#'
#' @importFrom magrittr %<>% %>%
#'
Covid_limits2 = function(dataset)
{

  dataset %<>%
    arrange(date) %>%
    mutate(
      Days_N = 1:n(),
      events = cumsum(new_events)) %>%


    #=============================================================================
  # Set up and initialize some variables and values for later use
  #=============================================================================
  mutate(
    Y_Min = 20,

    Y_Max = pmax(2*new_events, Y_Min),

    Phase_Ch = -99.0,
    MIDLINEa = 0,
    LOWERa = 0,
    UPPERa = 0,
    MIDLINEb = 0,
    LOWERb = 0,
    UPPERb = 0,

    EPOCH = 1,
    Phase_N = 0,
    Phase_Count = 1,
    Phase1 = 0
  )

  #-------------------------------------------------------------------------------
  # EPOCH Set: The first phase is EPOCH 1
  #-------------------------------------------------------------------------------

  EPOCH = 1
  LimTypeA = TRUE


  #=============================================================================
  # Create the variables to store the Extended limits and initialize them to 0
  #=============================================================================

  dataset %<>%
    mutate(
      Days_EXT = max(Days_N) + 1:n(),
      date_EXT = max(date)  + 1:n(),
      MIDLINE_EXT = 0.0,
      LOWER_EXT = 0.0,
      UPPER_EXT = 0.0
    )

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
  # Days_N (Vector):  Number of days since the first death occurred
  #===============================================================================


  # Calculate the mean number of events per day (Phase_1MN) based on the cumulative
  # number of events then calculate the upper limit (UL) above which indicates
  # special cause (Criteria1) and the start of phase 2

  dataset %<>%
    mutate(
      Phase_1MN = events/Days_N,
      UL = Phase_1MN + (3*sqrt(Phase_1MN)),
      Criteria1 = new_events >= UL)


  # Calculate two tokens indicating the starting date (Date_P1) and
  # end date (Date_P2) of Phase 1
  Date_P1 = min(dataset$date)
  Date_P2 = with(dataset, min(date[Criteria1], na.rm = TRUE))


  # Calculate a field "Phase1" indicating whether the date is in Phase 1
  dataset %<>%
    mutate(
      Phase1 = date < Date_P2)

  # Calculate a token (Day_P2) indicating the number of days since the start of
  # Phase 1 that special cause was found and Phase 2 starts
  Day_P2 = with(dataset, min(Days_N[Criteria1], na.rm = TRUE))


  # Calculate the limits for Phase 1


  # Pull Upper limit (UL) and midline (Phase_1MN) values from the last date in Phase 1
  # then put them in the fields UPPER_1 and MIDLINE_1

  UL_USE = with(dataset, if_else(Day_P2 == Inf, last(UL), UL[Day_P2]))
  MN_USE = with(dataset, if_else(Day_P2 == Inf, last(Phase_1MN), Phase_1MN[Day_P2]))

  dataset %<>%
    mutate(
      UPPER_1 = if_else(Phase1, UL_USE, 0),
      MIDLINE_1 = if_else(Phase1, MN_USE, 0),
      UPPER_N = UL_USE,
      MIDLINE_N = MN_USE,
      LOWER_N = 0,

      # Set the values to put in any extended limits we are using in the
      # Google Studio chart displays
      MIDLINE_EXT = MN_USE,
      LOWER_EXT = 0.0,
      UPPER_EXT = UL_USE,

      # Calculate Day_PC = Day_P2 -- the day on which the Phase changes and the
      # new phase starts
      Date_PC = Date_P2,
      Day_PC = Day_P2)



  #=============================================================================
  # Put the limits into MIDLINEa, LOWERa & UPPERa
  # Note to draw the charts in Google studio with distinct limits for each phase
  # we need to store the limits in fields ending a/b for each phase.
  #=============================================================================

  dataset %<>%
    mutate(
      MIDLINEa = MIDLINE_1,
      LOWERa = 0,
      UPPERa = UPPER_1,
      Phase_Count = 2)

  New_Phase = is.finite(Day_P2)

  #=============================================================================
  # IF there is a new phase (ie phase 2) then run through the days in this place
  # setting and calculate the rest of the limits using the "Covid_Phase" function
  #=============================================================================

  if (New_Phase) {

    p = 1
    while (p < nrow(dataset)) {

      dataset = Covid_Phase2(dataset)

      New_Phase = (is.finite(last(dataset$Day_PC) ))

      p = if_else(New_Phase, p + 1, nrow(dataset) + 1)
    }


  } # END if(New_Phase)

  # ==============================================================================
  # Calculate the extended limits used in the Google Studio chart displays
  # ==============================================================================

  dataset_EXT =
    dataset %>%
    select(date_EXT,
           MIDLINE_EXT, LOWER_EXT, UPPER_EXT, Y_Max, EPOCH) %>%
    mutate(
      date = date_EXT,
      new_events = -9,
      Y_Max = max(Y_Max),
      Phase_Ch = -99.0,
      EPOCH = last(EPOCH)) %>%
    filter(date_EXT >= first(date_EXT))

  if(LimTypeA)
  {
    dataset_EXT %<>%
      mutate(
        MIDLINEa = MIDLINE_EXT,
        LOWERa = LOWER_EXT,
        UPPERa = UPPER_EXT,
        MIDLINEb = 0.0,
        LOWERb = 0.0,
        UPPERb = 0.0)

  } else
  {

    dataset_EXT %<>% mutate(
      MIDLINEb = MIDLINE_EXT,
      LOWERb = LOWER_EXT,
      UPPERb = UPPER_EXT,
      MIDLINEa = 0.0,
      LOWERa = 0.0,
      UPPERa = 0.0)
  }


  #=============================================================================
  # Select the fields from dataset_EXT we need for the Google Studio Displays
  #=============================================================================

  dataset_EXT %<>% select(date, new_events,
                          MIDLINEa, UPPERa, LOWERa,
                          MIDLINEb, UPPERb, LOWERb,
                          Y_Max, Phase_Ch, EPOCH)


  #=============================================================================
  # Select the fields we need from dataset for the Google Studio Displays
  # and then use "rbind" to merge them with the extended limits dataframe
  #=============================================================================

  dataset %<>% select(date, new_events,
                      MIDLINEa, UPPERa, LOWERa,
                      MIDLINEb, UPPERb, LOWERb,
                      Y_Max, Phase_Ch, EPOCH)


  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  Ext_Days = min(nrow(dataset), 14)
  dataset = rbind(dataset, dataset_EXT %>% head(Ext_Days))


  #=============================================================================

  # Recode to Scale the limits used in the Google studio chart displays to
  # to be  < YMax

  dataset %<>% mutate(

    Y_Max = 2*new_events,
    Y_Min = 20,
    Y_Max = pmax(Y_Min,  2*new_events),
    Y_MAX_C = max(Y_Max),

    MIDLINEa = if_else(MIDLINEa >  Y_MAX_C ,  NA_real_, MIDLINEa),
    MIDLINEb = if_else(MIDLINEb >  Y_MAX_C ,  NA_real_, MIDLINEb),

    LOWERa = if_else(LOWERa >  Y_MAX_C ,  NA_real_, LOWERa),
    LOWERb = if_else(LOWERb >  Y_MAX_C ,  NA_real_, LOWERb),

    UPPERa = if_else(UPPERa >  Y_MAX_C ,  NA_real_, UPPERa),
    UPPERb = if_else(UPPERb >  Y_MAX_C ,  NA_real_, UPPERb))


  #=============================================================================


  # Recode missing values into NA

  dataset %<>% mutate(
    MIDLINEa = na_if(MIDLINEa, 0),
    MIDLINEb = na_if(MIDLINEb, 0),

    UPPERa = na_if(UPPERa, 0),
    UPPERb = na_if(UPPERb, 0),

    LOWERa = na_if(LOWERa, 0),
    LOWERb = na_if(LOWERb, 0),

    new_events = na_if(new_events, -9),

    Phase_Ch = na_if(Phase_Ch, -99.0))

  #=============================================================================
  # Select the fields from dataset we need for the Google Studio Displays and
  # use rbind to create a final dataframe "Outputdata" with all the limits
  #=============================================================================

  dataset %<>% select(date, new_events,
                      MIDLINEa, UPPERa, LOWERa,
                      MIDLINEb, UPPERb, LOWERb,
                      Y_Max, Phase_Ch, EPOCH)

  return(dataset)


} #End of Covid_limits function

