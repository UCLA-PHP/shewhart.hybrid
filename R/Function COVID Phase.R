

Covid_Phase <- function(PlaceRR, place, date, events, new_events, MIDLINE_N, LOWER_N, UPPER_N, Days_N, Day_PC, Date_PC, EPOCH, Phase_N, Phase_Count, MIDLINEa, LOWERa, UPPERa, MIDLINEb, LOWERb, UPPERb, Phase_Ch) {
  Phase_data <- data_frame(PlaceRR, place, date, events, new_events, MIDLINE_N, LOWER_N, UPPER_N, Days_N, Day_PC, Date_PC, EPOCH, Phase_N, Phase_Count, MIDLINEa, LOWERa, UPPERa, MIDLINEb, LOWERb, UPPERb, Phase_Ch)


#-------------------------------------------------------------------------------
  
# place : Location
# date : date
# events : cumulative number of events
# new_events : daily number of events
# Day_PC : Day (N) the last phase changed

#-------------------------------------------------------------------------------
  
    

#-------------------------------------------------------------------------------
# Initialize the Phase values

Freeze_days <- 21
Day_PC_NN <- Inf
Phase_data$Phase_Ch[last(Phase_data$Day_PC)] <- Phase_data$new_events[last(Phase_data$Day_PC)]


#-------------------------------------------------------------------------------
# Create the variables to store the Extended limits

Ext_Days <- min(14, max(Phase_data$Days_N))
Phase_data$Days_EXT <- max(Phase_data$Days_N)
Phase_data$date_EXT <- max(Phase_data$date)

d=1
for (d in 1:Ext_Days) {
  Phase_data$Days_EXT[d] <- Phase_data$Days_EXT[d] + d
  Phase_data$date_EXT[d] <- Phase_data$date_EXT[d] + d
  d <- d + 1
}

Phase_data$MIDLINE_EXT <- 0.0
Phase_data$LOWER_EXT <- 0.0
Phase_data$UPPER_EXT <- 0.0
#-------------------------------------------------------------------------------


  #-------------------------------------------------------------------------------
  # Set up the following two filter variables:
  # Statei$Phase_N:      Indicating we are in Phase N
  # Statei$Phase_N_F21:  Indicating the first 21 days into Phase_N
  #-------------------------------------------------------------------------------
  
  Phase_data$Phase_N <- if_else(Phase_data$date >= Phase_data$Date_PC,1,0)
  Phase_data$Phase_N_F21 <- if_else(Phase_data$Phase_N==1 & Phase_data$Days_N < (Phase_data$Day_PC + Freeze_days), 1, 0)
  
  # Calculate the mean New_deaths for the first 21 days
  Phase_MN <- mean(Phase_data$new_events[Phase_data$Phase_N_F21==1])

  Constant_OLD <- 0
  Slope_OLD <- 0
  
  Day_PCuse <- max(Phase_data$Day_PC)

  if (Day_PCuse > 7) {
  Phase_days <- max(Phase_data$Days_N) - Day_PCuse
  
# Slope_OLD = difference between the two previous midline points
  Phase_data$MID <- Phase_data$MIDLINEa + Phase_data$MIDLINEb 
  Slope_OLD = log10(Phase_data$MID[Day_PCuse])   - log10(Phase_data$MID[Day_PCuse - 1])
  
#  Constant_OLD  = current middline point - Slope_x*days
  Constant_OLD = log10(Phase_data$MID[Day_PCuse]) - Slope_OLD*Phase_data$Days_N[Day_PCuse]
  
# PV_X = Constant_OLD + Slope_OLD * Phase_data$Days_N  
  Phase_data$PV_x <- Constant_OLD + Slope_OLD * Phase_data$Days_N
  
#  Res_X = PV_X - Phase_data$new_events -- where Phase_N_F21==1
  Phase_data$Res_x <-  log10(Phase_data$new_events+1) - Phase_data$PV_x
 
  Res_MN <- mean(Phase_data$Res_x[Phase_data$Phase_N_F21==1]) 
  Constant_NEW = Constant_OLD + Res_MN
  
  Phase_data$PV_x <- Constant_NEW + Slope_OLD * Phase_data$Days_N
  
  }
  
  #-------------------------------------------------------------------------------
  # Do the regression model
  #-------------------------------------------------------------------------------
  
  Phase_data$new_events <- if_else(Phase_data$new_events < 0.0, 0.0, Phase_data$new_events)
  Phase_data$Log_Events <- log10(Phase_data$new_events + 1)
  linearMod <- lm(Log_Events ~ Days_N, data = Phase_data, subset = Phase_N_F21==1)
  summary(linearMod)
  ModSum <- tidy(linearMod)
  PValue <- ModSum$p.value[2]
  Slope <- ModSum$estimate[2]
  Int <- ModSum$estimate[1]
  PValue <- if_else(is.nan(PValue) | is.na(PValue),1, PValue)
  Slope <- if_else(is.nan(Slope) | is.na(Slope),0, Slope)
  
  
  # If we are in EPOCH 1 or 4 and there's somehow a downward slope, then set the PValue = 1, so we don't fit a slope
  PValue <- if_else((last(Phase_data$EPOCH) == 1 | last(Phase_data$EPOCH) == 4) & Slope <= 0, 1, PValue)
  
  # If the Phase_MN is low (less than 10), then set the PValue = 1, so we don't fit a slope
  PValue <- if_else(Phase_MN < 10, 1, PValue)
  
  # If the slope is significant calculate the limits as an I-Chart from the regression
  if (PValue <= 0.05)
  {
    Phase_data$PRE_2 <- Int + Slope*Phase_data$Days_N
    
    Phase_data$RES_2 <- Phase_data$Log_Events - Phase_data$PRE_2
    Phase_data$MR_2 <- abs(Phase_data$RES_2 - lag(Phase_data$RES_2))
    
    MR_UCL_2 <- 3.27*mean(Phase_data$MR_2[Phase_data$Phase_N_F21==1], na.rm = TRUE)
    Phase_data$Screened_MR <- if_else(Phase_data$MR_2 <= MR_UCL_2, 1, 0)
    MRr_mean_2 <- mean(Phase_data$MR_2[Phase_data$Screened_MR==1 & Phase_data$Phase_N_F21==1 ], na.rm = TRUE)
    
    Phase_data$MIDLINE_NN <- 10**(Phase_data$PRE_2)*Phase_data$Phase_N
    Phase_data$UPPER_NN <- 10**(Phase_data$PRE_2 + 2.66*MRr_mean_2)*Phase_data$Phase_N
    Phase_data$LOWER_NN <- 10**(Phase_data$PRE_2 - 2.66*MRr_mean_2)*Phase_data$Phase_N
    
    #EXTENDED LIMITS
    Phase_data$MIDLINE_EXT <- 10**(Int + Slope*Phase_data$Days_EXT)
    Phase_data$UPPER_EXT <- 10**(Int + Slope*Phase_data$Days_EXT + 2.66*MRr_mean_2)    
    Phase_data$LOWER_EXT <- 10**(Int + Slope*Phase_data$Days_EXT - 2.66*MRr_mean_2)

    if (last(Phase_data$EPOCH) == 1) { Phase_data$EPOCH_NN <- if_else(Slope > 0, 2, 1) }
    if (last(Phase_data$EPOCH) == 2) { Phase_data$EPOCH_NN <- if_else(Slope > 0, 2, 3) }
    if (last(Phase_data$EPOCH) == 3) { Phase_data$EPOCH_NN <- if_else(Slope > 0, 2, 3) }
    if (last(Phase_data$EPOCH) == 4) { Phase_data$EPOCH_NN <- if_else(Slope > 0, 2, 1) }
    }
  
  # If the slope is not significant and we are in EPOCH 1 or EPOCH 4 calculate the limits based on a C-Chart
  if (PValue > 0.05)
  {

        if (Phase_MN <= 20)
    {
    Phase_data$MIDLINE_NN <- Phase_MN*Phase_data$Phase_N
    Phase_data$UPPER_NN <- (Phase_MN + 3*sqrt(Phase_MN))*Phase_data$Phase_N
    Phase_data$LOWER_NN <- 0
    
    #EXTENDED LIMITS
     Phase_data$MIDLINE_EXT <- Phase_MN
     Phase_data$UPPER_EXT <- Phase_MN + 3*sqrt(Phase_MN)
     Phase_data$LOWER_EXT <- 0
    

    if (last(Phase_data$EPOCH) == 1) { Phase_data$EPOCH_NN <- 1 }
    if (last(Phase_data$EPOCH) == 2) { Phase_data$EPOCH_NN <- 3 }
    if (last(Phase_data$EPOCH) == 3) { Phase_data$EPOCH_NN <- if_else(Phase_MN <=10, 4, 3) }
    if (last(Phase_data$EPOCH) == 4) { Phase_data$EPOCH_NN <- if_else(Phase_MN <=10, 4, 3) }
    }
  }
  
  # If the slope is not significant and the Phase mean > 20 calculate the limits 
  # based on an I-Chart with slope=0 and intercept = Phase mean
  
  if (PValue > 0.05 & Phase_MN > 20)
  {
    Phase_data$PRE_2 <- log10(Phase_MN)
    
    Phase_data$RES_2 <- Phase_data$Log_Events - Phase_data$PRE_2
    Phase_data$MR_2 <- abs(Phase_data$RES_2 - lag(Phase_data$RES_2))
    
    MR_UCL_2 <- 3.27*mean(Phase_data$MR_2[Phase_data$Phase_N_F21==1], na.rm = TRUE)
    Phase_data$Screened_MR <- if_else(Phase_data$MR_2 <= MR_UCL_2, 1, 0)
    MRr_mean_2 <- mean(Phase_data$MR_2[Phase_data$Screened_MR==1 & Phase_data$Phase_N_F21==1 ], na.rm = TRUE)
    
    Phase_data$MIDLINE_NN <- 10**(Phase_data$PRE_2)*Phase_data$Phase_N
    Phase_data$UPPER_NN <- 10**(Phase_data$PRE_2 + 2.66*MRr_mean_2)*Phase_data$Phase_N
    Phase_data$LOWER_NN <- 10**(Phase_data$PRE_2 - 2.66*MRr_mean_2)*Phase_data$Phase_N
    
    #EXTENDED LIMITS
    Phase_data$MIDLINE_EXT <- Phase_MN
    Phase_data$UPPER_EXT <- 10**(log10(Phase_MN) + 2.66*MRr_mean_2)    
    Phase_data$LOWER_EXT <- 10**(log10(Phase_MN) - 2.66*MRr_mean_2)
    
    Phase_data$EPOCH_NN <- ( if_else(last(Phase_data$EPOCH) == 1, 1, if_else(last(Phase_data$EPOCH) == 2, 3, if_else(last(Phase_data$EPOCH ==3) , 3, last(Phase_data$EPOCH))) ) )*Phase_data$Phase_N
    
    
    if (Day_PCuse > 7 & last(Phase_data$EPOCH > 1)) {
    # CALCULATE THE TOTAL SS_Y based on the above fit
    Phase_data$SS_x <-  (Phase_data$Log_Events - Phase_data$PRE_2)**2
    SS_x_sum <- sum(Phase_data$SS_x[Phase_data$Phase_N_F21==1])
    # CALCULATE THE TOTAL SS_X based on adjusted constant and old slope
    Phase_data$SS_y <- (Phase_data$Log_Events - Phase_data$PV_x)**2
    SS_y_sum <- sum(Phase_data$SS_y[Phase_data$Phase_N_F21==1])
 
    # IF SS_X < SS_Y then fit adjusted constant and old slope with MRr_mean_2
    if (SS_y_sum <=  SS_x_sum) {
      Phase_data$MIDLINE_NN <- 10**(Phase_data$PV_x)*Phase_data$Phase_N
      Phase_data$UPPER_NN <- 10**(Phase_data$PV_x + 2.66*MRr_mean_2)*Phase_data$Phase_N
      Phase_data$LOWER_NN <- 10**(Phase_data$PV_x - 2.66*MRr_mean_2)*Phase_data$Phase_N 
      
      Phase_data$MIDLINE_EXT <- 10**(Constant_NEW + Slope_OLD*Phase_data$Days_EXT)
      Phase_data$UPPER_EXT <- 10**(Constant_NEW + Slope_OLD*Phase_data$Days_EXT + 2.66*MRr_mean_2)    
      Phase_data$LOWER_EXT <- 10**(Constant_NEW + Slope_OLD*Phase_data$Days_EXT - 2.66*MRr_mean_2)
      
      Phase_data$EPOCH_NN <- last(Phase_data$EPOCH)
          } # SS_x_sum < SS_y_sum

        # If SS_Y < SS_X then fir use as is.
    
    } # Day_PCuse >7


  } #  if (PValue > 0.05 & Phase_MN > 20)
  
  
  #-------------------------------------------------------------------------------
  # Assign the new limits to limits A or B
  #-------------------------------------------------------------------------------

  LimTypeA <- ( (max(Phase_data$Phase_Count)/2) - trunc(max(Phase_data$Phase_Count)/2) > 0)
  
  
  if(LimTypeA == TRUE) {
    Phase_data$MIDLINEa <- if_else(Phase_data$Days_N >= Phase_data$Day_PC, Phase_data$MIDLINE_NN, Phase_data$MIDLINEa)
    Phase_data$LOWERa <- if_else(Phase_data$Days_N >= Phase_data$Day_PC, Phase_data$LOWER_NN, Phase_data$LOWERa)
    Phase_data$UPPERa <- if_else(Phase_data$Days_N >= Phase_data$Day_PC, Phase_data$UPPER_NN, Phase_data$UPPERa)
    
    Phase_data$MIDLINEb <- if_else(Phase_data$Days_N >= Phase_data$Day_PC, 0, Phase_data$MIDLINEb)
    Phase_data$LOWERb <- if_else(Phase_data$Days_N >= Phase_data$Day_PC, 0, Phase_data$LOWERb)
    Phase_data$UPPERb <- if_else(Phase_data$Days_N >= Phase_data$Day_PC, 0, Phase_data$UPPERb)   
    
  }
  if(LimTypeA == FALSE) {
    Phase_data$MIDLINEb <- if_else(Phase_data$Days_N >= Phase_data$Day_PC, Phase_data$MIDLINE_NN, Phase_data$MIDLINEb)
    Phase_data$LOWERb <- if_else(Phase_data$Days_N >= Phase_data$Day_PC, Phase_data$LOWER_NN, Phase_data$LOWERb)
    Phase_data$UPPERb <- if_else(Phase_data$Days_N >= Phase_data$Day_PC, Phase_data$UPPER_NN, Phase_data$UPPERb)
    
    Phase_data$MIDLINEa <- if_else(Phase_data$Days_N >= Phase_data$Day_PC, 0, Phase_data$MIDLINEa)
    Phase_data$LOWERa <- if_else(Phase_data$Days_N >= Phase_data$Day_PC, 0, Phase_data$LOWERa)
    Phase_data$UPPERa <- if_else(Phase_data$Days_N >= Phase_data$Day_PC, 0, Phase_data$UPPERa)
  }
  

  Phase_data$EPOCH <- if_else(Phase_data$Days_N >= Phase_data$Day_PC, Phase_data$EPOCH_NN, Phase_data$EPOCH)

#-------------------------------------------------------------------------------

# a) Look for 3 points below the Lower Limit
Phase_data$SC_a <- if_else(Phase_data$Phase_N==1 & Phase_data$new_events < Phase_data$LOWER_NN & Phase_data$new_events > 5 & Phase_data$Days_N - Phase_data$Day_PC > 15, 1,0)
Phase_data$SC_ax <- Phase_data$SC_a + lag(Phase_data$SC_a) + lag(Phase_data$SC_a, 2)
Phase_data$SC_ax[is.na(Phase_data$SC_ax)] <- 0
Days_Pa <- min(Phase_data$Days_N[Phase_data$SC_ax==3]) - 2

# b) Look for a downward Shift
Phase_data$SC_b <- if_else(Phase_data$Phase_N==1 & Phase_data$new_events < Phase_data$MIDLINE_NN, 1, 0) 
Phase_data$SC_bx <- Phase_data$SC_b + lag(Phase_data$SC_b) + lag(Phase_data$SC_b, 2) + lag(Phase_data$SC_b, 3) + lag(Phase_data$SC_b, 4) + lag(Phase_data$SC_b, 5) + lag(Phase_data$SC_b, 6) + lag(Phase_data$SC_b, 7)
Phase_data$SC_bx[is.na(Phase_data$SC_bx)] <- 0
Days_Pb <- min(Phase_data$Days_N[Phase_data$SC_bx==8])

# c) Look for 3 points above the Upper Limit
Phase_data$SC_c <- if_else(Phase_data$Phase_N==1 & Phase_data$new_events > Phase_data$UPPER_NN, 1,0)
Phase_data$SC_cx <- Phase_data$SC_c + lag(Phase_data$SC_c) + lag(Phase_data$SC_c, 2)
Phase_data$SC_cx[is.na(Phase_data$SC_cx)] <- 0
Days_Pc <- min(Phase_data$Days_N[Phase_data$SC_cx==3]) - 2

# d) Look for an upwards Shift
Phase_data$SC_d <- if_else(Phase_data$Phase_N==1 & Phase_data$new_events > Phase_data$MIDLINE_NN, 1, 0)
Phase_data$SC_dx <- Phase_data$SC_d + lag(Phase_data$SC_d) + lag(Phase_data$SC_d, 2) + lag(Phase_data$SC_d, 3) + lag(Phase_data$SC_d, 4) + lag(Phase_data$SC_d, 5) + lag(Phase_data$SC_d, 6) + lag(Phase_data$SC_d, 7)
Phase_data$SC_dx[is.na(Phase_data$SC_dx)] <- 0
Days_Pd <- min(Phase_data$Days_N[Phase_data$SC_dx==8])



#-------------------------------------------------------------------------------
# Set-up a condition so that if there is no new phase, we 
# assign missing values to the limits
# If there is a new phase, then we do the analysis
#-------------------------------------------------------------------------------

New_Phase <- (is.infinite(Day_PC_NN)==FALSE)


Phase_data$Y_Max <- 2*Phase_data$new_events

#IDENTIFY THE DAY THAT THE NEXT Phase STARTS
Date_Ch <- c(Days_Pa, Days_Pb, Days_Pc, Days_Pd)
Date_Ch[sapply(Date_Ch, is.infinite)] <- NA
Day_PC_NN <- min(Date_Ch, na.rm = TRUE )
Date_PC_NN <- Phase_data$date[Day_PC_NN]


Phase_data$Day_PC <- Day_PC_NN
Phase_data$Date_PC <- Date_PC_NN
Phase_data$Phase_Count <- Phase_data$Phase_Count + 1


  return(Phase_data)

  } #End of Covid Phase

#===============================================================================
# END OF PHASE
#===============================================================================
