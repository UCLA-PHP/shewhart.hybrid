

#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#'
#' @importFrom magrittr %<>% %>%
#'
Covid_Phase2 = function(dataset)
{



  #-------------------------------------------------------------------------------

  # date : date
  # events : cumulative number of events
  # new_events : daily number of events
  # Day_PC : Day (N) the last phase changed

  #-------------------------------------------------------------------------------



  #-------------------------------------------------------------------------------
  # Initialize the Phase values

  Freeze_days = 21
  Day_PC_NN = Inf
  dataset %<>%
    mutate(
      Phase_Ch = if_else(
        Days_N == last(Day_PC),
        new_events,
        Phase_Ch))



  #-------------------------------------------------------------------------------
  # Set up the following two filter variables:
  # Statei$Phase_N:      Indicating we are in Phase N
  # Statei$Phase_N_F21:  Indicating the first 21 days into Phase_N
  #-------------------------------------------------------------------------------

  dataset %<>%
    mutate(
      Phase_N = date >= Date_PC,
      Phase_N_F21 = Phase_N & Days_N < (Day_PC + Freeze_days))

  # Calculate the mean New_deaths for the first 21 days
  Phase_MN = with(dataset, mean(new_events[Phase_N_F21]))

  Constant_OLD = 0
  Slope_OLD = 0

  Day_PCuse = max(dataset$Day_PC)

  if (Day_PCuse > 7)
  {
    Phase_days = max(dataset$Days_N) - Day_PCuse

    # Slope_OLD = difference between the two previous midline points
    dataset %<>%
      mutate(MID = MIDLINEa + MIDLINEb)

    Slope_OLD = with(dataset, log10(MID[Day_PCuse])   - log10(MID[Day_PCuse - 1]))

    #  Constant_OLD  = current middline point - Slope_x*days
    Constant_OLD = with(dataset, log10(MID[Day_PCuse]) - Slope_OLD*Days_N[Day_PCuse])

    # PV_X = Constant_OLD + Slope_OLD * dataset$Days_N
    dataset %<>% mutate(
      PV_x = Constant_OLD + Slope_OLD * Days_N)

    #  Res_X = PV_X - dataset$new_events -- where Phase_N_F21==1
    dataset$Res_x =  log10(dataset$new_events+1) - dataset$PV_x

    Res_MN = mean(dataset$Res_x[dataset$Phase_N_F21==1])
    Constant_NEW = Constant_OLD + Res_MN

    dataset$PV_x = Constant_NEW + Slope_OLD * dataset$Days_N

  }

  #-------------------------------------------------------------------------------
  # Do the regression model
  #-------------------------------------------------------------------------------

  dataset$new_events = if_else(dataset$new_events < 0.0, 0.0, dataset$new_events)
  dataset$Log_Events = log10(dataset$new_events + 1)
  linearMod = lm(Log_Events ~ Days_N, data = dataset, subset = Phase_N_F21==1)
  summary(linearMod)
  ModSum = tidy(linearMod)
  PValue = ModSum$p.value[2]
  Slope = ModSum$estimate[2]
  Int = ModSum$estimate[1]
  PValue = if_else(is.nan(PValue) | is.na(PValue),1, PValue)
  Slope = if_else(is.nan(Slope) | is.na(Slope),0, Slope)


  # If we are in EPOCH 1 or 4 and there's somehow a downward slope, then set the PValue = 1, so we don't fit a slope
  PValue = if_else((last(dataset$EPOCH) == 1 | last(dataset$EPOCH) == 4) & Slope <= 0, 1, PValue)

  # If the Phase_MN is low (less than 10), then set the PValue = 1, so we don't fit a slope
  PValue = if_else(Phase_MN < 10, 1, PValue)

  # If the slope is significant calculate the limits as an I-Chart from the regression
  if (PValue <= 0.05)
  {
    dataset$PRE_2 = Int + Slope*dataset$Days_N

    dataset$RES_2 = dataset$Log_Events - dataset$PRE_2
    dataset$MR_2 = abs(dataset$RES_2 - lag(dataset$RES_2))

    MR_UCL_2 = 3.27*mean(dataset$MR_2[dataset$Phase_N_F21==1], na.rm = TRUE)
    dataset$Screened_MR = if_else(dataset$MR_2 <= MR_UCL_2, 1, 0)
    MRr_mean_2 = mean(dataset$MR_2[dataset$Screened_MR==1 & dataset$Phase_N_F21==1 ], na.rm = TRUE)

    dataset$MIDLINE_NN = 10**(dataset$PRE_2)*dataset$Phase_N
    dataset$UPPER_NN = 10**(dataset$PRE_2 + 2.66*MRr_mean_2)*dataset$Phase_N
    dataset$LOWER_NN = 10**(dataset$PRE_2 - 2.66*MRr_mean_2)*dataset$Phase_N

    #EXTENDED LIMITS
    dataset$MIDLINE_EXT = 10**(Int + Slope*dataset$Days_EXT)
    dataset$UPPER_EXT = 10**(Int + Slope*dataset$Days_EXT + 2.66*MRr_mean_2)
    dataset$LOWER_EXT = 10**(Int + Slope*dataset$Days_EXT - 2.66*MRr_mean_2)

    if (last(dataset$EPOCH) == 1) { dataset$EPOCH_NN = if_else(Slope > 0, 2, 1) }
    if (last(dataset$EPOCH) == 2) { dataset$EPOCH_NN = if_else(Slope > 0, 2, 3) }
    if (last(dataset$EPOCH) == 3) { dataset$EPOCH_NN = if_else(Slope > 0, 2, 3) }
    if (last(dataset$EPOCH) == 4) { dataset$EPOCH_NN = if_else(Slope > 0, 2, 1) }
  }

  # If the slope is not significant and we are in EPOCH 1 or EPOCH 4 calculate the limits based on a C-Chart
  if (PValue > 0.05)
  {

    if (Phase_MN <= 20)
    {
      dataset$MIDLINE_NN = Phase_MN*dataset$Phase_N
      dataset$UPPER_NN = (Phase_MN + 3*sqrt(Phase_MN))*dataset$Phase_N
      dataset$LOWER_NN = 0

      #EXTENDED LIMITS
      dataset$MIDLINE_EXT = Phase_MN
      dataset$UPPER_EXT = Phase_MN + 3*sqrt(Phase_MN)
      dataset$LOWER_EXT = 0


      if (last(dataset$EPOCH) == 1) { dataset$EPOCH_NN = 1 }
      if (last(dataset$EPOCH) == 2) { dataset$EPOCH_NN = 3 }
      if (last(dataset$EPOCH) == 3) { dataset$EPOCH_NN = if_else(Phase_MN <=10, 4, 3) }
      if (last(dataset$EPOCH) == 4) { dataset$EPOCH_NN = if_else(Phase_MN <=10, 4, 3) }
    }
  }

  # If the slope is not significant and the Phase mean > 20 calculate the limits
  # based on an I-Chart with slope=0 and intercept = Phase mean

  if (PValue > 0.05 & Phase_MN > 20)
  {
    dataset$PRE_2 = log10(Phase_MN)

    dataset$RES_2 = dataset$Log_Events - dataset$PRE_2
    dataset$MR_2 = abs(dataset$RES_2 - lag(dataset$RES_2))

    MR_UCL_2 = 3.27*mean(dataset$MR_2[dataset$Phase_N_F21==1], na.rm = TRUE)
    dataset$Screened_MR = if_else(dataset$MR_2 <= MR_UCL_2, 1, 0)
    MRr_mean_2 = mean(dataset$MR_2[dataset$Screened_MR==1 & dataset$Phase_N_F21==1 ], na.rm = TRUE)

    dataset$MIDLINE_NN = 10**(dataset$PRE_2)*dataset$Phase_N
    dataset$UPPER_NN = 10**(dataset$PRE_2 + 2.66*MRr_mean_2)*dataset$Phase_N
    dataset$LOWER_NN = 10**(dataset$PRE_2 - 2.66*MRr_mean_2)*dataset$Phase_N

    #EXTENDED LIMITS
    dataset$MIDLINE_EXT = Phase_MN
    dataset$UPPER_EXT = 10**(log10(Phase_MN) + 2.66*MRr_mean_2)
    dataset$LOWER_EXT = 10**(log10(Phase_MN) - 2.66*MRr_mean_2)

    dataset$EPOCH_NN = ( if_else(last(dataset$EPOCH) == 1, 1, if_else(last(dataset$EPOCH) == 2, 3, if_else(last(dataset$EPOCH ==3) , 3, last(dataset$EPOCH))) ) )*dataset$Phase_N


    if (Day_PCuse > 7 & last(dataset$EPOCH > 1)) {
      # CALCULATE THE TOTAL SS_Y based on the above fit
      dataset$SS_x =  (dataset$Log_Events - dataset$PRE_2)**2
      SS_x_sum = sum(dataset$SS_x[dataset$Phase_N_F21==1])
      # CALCULATE THE TOTAL SS_X based on adjusted constant and old slope
      dataset$SS_y = (dataset$Log_Events - dataset$PV_x)**2
      SS_y_sum = sum(dataset$SS_y[dataset$Phase_N_F21==1])

      # IF SS_X < SS_Y then fit adjusted constant and old slope with MRr_mean_2
      if (SS_y_sum <=  SS_x_sum) {
        dataset$MIDLINE_NN = 10**(dataset$PV_x)*dataset$Phase_N
        dataset$UPPER_NN = 10**(dataset$PV_x + 2.66*MRr_mean_2)*dataset$Phase_N
        dataset$LOWER_NN = 10**(dataset$PV_x - 2.66*MRr_mean_2)*dataset$Phase_N

        dataset$MIDLINE_EXT = 10**(Constant_NEW + Slope_OLD*dataset$Days_EXT)
        dataset$UPPER_EXT = 10**(Constant_NEW + Slope_OLD*dataset$Days_EXT + 2.66*MRr_mean_2)
        dataset$LOWER_EXT = 10**(Constant_NEW + Slope_OLD*dataset$Days_EXT - 2.66*MRr_mean_2)

        dataset$EPOCH_NN = last(dataset$EPOCH)
      } # SS_x_sum < SS_y_sum

      # If SS_Y < SS_X then fir use as is.

    } # Day_PCuse >7


  } #  if (PValue > 0.05 & Phase_MN > 20)


  #-------------------------------------------------------------------------------
  # Assign the new limits to limits A or B
  #-------------------------------------------------------------------------------

  LimTypeA = ( (max(dataset$Phase_Count)/2) - trunc(max(dataset$Phase_Count)/2) > 0)


  if(LimTypeA == TRUE) {
    dataset$MIDLINEa = if_else(dataset$Days_N >= dataset$Day_PC, dataset$MIDLINE_NN, dataset$MIDLINEa)
    dataset$LOWERa = if_else(dataset$Days_N >= dataset$Day_PC, dataset$LOWER_NN, dataset$LOWERa)
    dataset$UPPERa = if_else(dataset$Days_N >= dataset$Day_PC, dataset$UPPER_NN, dataset$UPPERa)

    dataset$MIDLINEb = if_else(dataset$Days_N >= dataset$Day_PC, 0, dataset$MIDLINEb)
    dataset$LOWERb = if_else(dataset$Days_N >= dataset$Day_PC, 0, dataset$LOWERb)
    dataset$UPPERb = if_else(dataset$Days_N >= dataset$Day_PC, 0, dataset$UPPERb)

  }
  if(LimTypeA == FALSE) {
    dataset$MIDLINEb = if_else(dataset$Days_N >= dataset$Day_PC, dataset$MIDLINE_NN, dataset$MIDLINEb)
    dataset$LOWERb = if_else(dataset$Days_N >= dataset$Day_PC, dataset$LOWER_NN, dataset$LOWERb)
    dataset$UPPERb = if_else(dataset$Days_N >= dataset$Day_PC, dataset$UPPER_NN, dataset$UPPERb)

    dataset$MIDLINEa = if_else(dataset$Days_N >= dataset$Day_PC, 0, dataset$MIDLINEa)
    dataset$LOWERa = if_else(dataset$Days_N >= dataset$Day_PC, 0, dataset$LOWERa)
    dataset$UPPERa = if_else(dataset$Days_N >= dataset$Day_PC, 0, dataset$UPPERa)
  }


  dataset$EPOCH = if_else(dataset$Days_N >= dataset$Day_PC, dataset$EPOCH_NN, dataset$EPOCH)

  #-------------------------------------------------------------------------------

  # a) Look for 3 points below the Lower Limit
  dataset$SC_a = if_else(
    dataset$Phase_N==1 & dataset$new_events < dataset$LOWER_NN & dataset$new_events > 5 & dataset$Days_N - dataset$Day_PC > 15, 1,0) # DM: could just be a logical

  dataset$SC_ax = dataset$SC_a + lag(dataset$SC_a) + lag(dataset$SC_a, 2)
  dataset$SC_ax[is.na(dataset$SC_ax)] = 0
  Days_Pa = min(dataset$Days_N[dataset$SC_ax==3]) - 2

  # b) Look for a downward Shift
  dataset$SC_b = if_else(dataset$Phase_N==1 & dataset$new_events < dataset$MIDLINE_NN, 1, 0)
  dataset$SC_bx = dataset$SC_b + lag(dataset$SC_b) + lag(dataset$SC_b, 2) + lag(dataset$SC_b, 3) + lag(dataset$SC_b, 4) + lag(dataset$SC_b, 5) + lag(dataset$SC_b, 6) + lag(dataset$SC_b, 7)
  dataset$SC_bx[is.na(dataset$SC_bx)] = 0
  Days_Pb = min(dataset$Days_N[dataset$SC_bx==8])

  # c) Look for 3 points above the Upper Limit
  dataset$SC_c = if_else(dataset$Phase_N==1 & dataset$new_events > dataset$UPPER_NN, 1,0)
  dataset$SC_cx = dataset$SC_c + lag(dataset$SC_c) + lag(dataset$SC_c, 2)
  dataset$SC_cx[is.na(dataset$SC_cx)] = 0
  Days_Pc = min(dataset$Days_N[dataset$SC_cx==3]) - 2

  # d) Look for an upwards Shift
  dataset$SC_d = if_else(dataset$Phase_N==1 & dataset$new_events > dataset$MIDLINE_NN, 1, 0)
  dataset$SC_dx = dataset$SC_d + lag(dataset$SC_d) + lag(dataset$SC_d, 2) + lag(dataset$SC_d, 3) + lag(dataset$SC_d, 4) + lag(dataset$SC_d, 5) + lag(dataset$SC_d, 6) + lag(dataset$SC_d, 7)
  dataset$SC_dx[is.na(dataset$SC_dx)] = 0
  Days_Pd = min(dataset$Days_N[dataset$SC_dx==8])



  #-------------------------------------------------------------------------------
  # Set-up a condition so that if there is no new phase, we
  # assign missing values to the limits
  # If there is a new phase, then we do the analysis
  #-------------------------------------------------------------------------------

  New_Phase = (is.infinite(Day_PC_NN)==FALSE)


  dataset$Y_Max = 2*dataset$new_events

  #IDENTIFY THE DAY THAT THE NEXT Phase STARTS
  Date_Ch = c(Days_Pa, Days_Pb, Days_Pc, Days_Pd)
  Date_Ch[sapply(Date_Ch, is.infinite)] = NA
  Day_PC_NN = min(Date_Ch, na.rm = TRUE )
  Date_PC_NN = dataset$date[Day_PC_NN]


  dataset$Day_PC = Day_PC_NN
  dataset$Date_PC = Date_PC_NN
  dataset$Phase_Count = dataset$Phase_Count + 1


  return(dataset)

} #End of Covid Phase

#===============================================================================
# END OF PHASE
#===============================================================================
