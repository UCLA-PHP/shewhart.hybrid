#===============================================================================
# To run this function, use:
#
# df <- XBar_Chart(place, date, Dot_mn, Dot_sd, count)
#
# Where:
#     df:     The data.frame where you want to store the limits
#     place:  A string indicating the sites or settings 
#               -- which can contain only one setting
#     date:   The date associated with the measure
#     Dot_mn: The mean of the measure for the specific place and date
#     Dot_sd: The standard deviation of the measure for the specific 
#               place and date
#     count:  The number of cases forming the mean and sd of the measure 
#               for the specific place and date
#
#     Note: The data.frame you send should be sorted by place and then date
#
#-------------------------------------------------------------------------------
#
# The function will produce a data.frame (df) containing:
#
#     date:   The date associated with the measure
#     place:  A string indicating the sites or settings 
#     Dot:    The mean of the measure for the specific place and date
#
#     MIDLINEa, UPPERa, LOWERa:
#              Limits for the 1st, 3rd, 5th and on-going odd numbered phases

#     MIDLINEb, UPPERb, LOWERb:
#              Limits for the 2nd, 4th, 6th and on-going even numbered phases
#
#     Phase_Ch: The value of "Dot" when a phase change is detected
#               (Used to display the 'red dot' in the subsequent display)
#     PhaseCount: The number of phases
#     SC:       The reason for the phase change
#
#===============================================================================


XBar_Chart <- function(place, date, Dot_mn, Dot_sd, count) {
  XBar_Data <- data_frame(place, date, Dot_mn, Dot_sd, count)
  
  
  
  #XBar_Data <- IDi %>% select(place, ChkIn_hr, time_mn, time_sd, count) 
  
  #XBar_Data$date <- XBar_Data$ChkIn_hr
  #XBar_Data$Dot_mn <- XBar_Data$time_mn
  #XBar_Data$Dot_sd <- XBar_Data$time_sd
  
  #===============================================================================
  # Calculate the SPC Constants C4 and then A3 (p196 Data Guide)
  #===============================================================================
  
  XBar_Data$c4 <- ( gamma(XBar_Data$count/2)/gamma((XBar_Data$count-1)/2) )*sqrt(2/(XBar_Data$count-1))
  XBar_Data$a3 <- 3/(XBar_Data$c4*sqrt(XBar_Data$count))  
 
  #===============================================================================
  # Set the minimum number of dots to form a set of limits
  #===============================================================================
  
  Lim_Min <- 15
  
    #=============================================================================
    # Calculate:
    #   Days_N:   The number of days since the first data point
    #   Days_Tot: A token containing the total number of data points
    #   Date_PC:  A token containing the date of the first data point
    #   Days_PC:  A token indicating the number of days on which a phase change
    #             occurs, initialized to 1
    #=============================================================================
    
    XBar_Data$Days_N <- rank(XBar_Data$date)
    Days_Tot <- nrow(XBar_Data)
    Date_PC <- min(XBar_Data$date)
    Days_PC <- 1
   
    #=============================================================================
    # Calculate:
    #   PhaseCount: The number of special cause phases
    #               Initialized to 1, the first phase
    #   Phase_Ch:   The measure on the day of a phase change, initialized to -99
    #               Used in the charts to show the red dots on the day a phase 
    #               change occurs
    #=============================================================================
    
    XBar_Data <- XBar_Data %>%
      mutate(PhaseCount = 1,
             Phase_Ch = -99)
    
    Phases <- 1

    #=============================================================================
    # Initialize the limits to 0 and New_Phase to FALSE
    #=============================================================================
    
    XBar_Data <- XBar_Data %>%
      mutate(Centerline = 0,
             Upper = 0,
             Lower =0,
             SC = "")
    
    New_Phase <- FALSE
    
    i <- 1
    j <- as.integer(1)
    
    while (j <= Days_Tot) {
     
      m <- i + max(j-i, Lim_Min-1)
      m <- min(m, Days_Tot)
      
      if (j < Lim_Min | New_Phase) {
        
        Xbar <- sum(XBar_Data$count[i:m]*XBar_Data$Dot_mn[i:m])/sum(XBar_Data$count[i:m])
        XBar_Data$CL <- Xbar
        
        XBar_Data$Dot_sd[is.na( XBar_Data$Dot_sd)] <- 0
        
        Sbar <- sum(XBar_Data$count[i:m]*XBar_Data$Dot_sd[i:m])/sum(XBar_Data$count[i:m])
        
        XBar_Data$LL <- XBar_Data$CL - XBar_Data$a3*Sbar
        XBar_Data$UL <- XBar_Data$CL + XBar_Data$a3*Sbar
        
      } #if 
      
      #=============================================================================
      # Look for all four types of special cause signals  
      #
      #   a) Two points below the lower limit
      #   b) A downward shift (8 points below the mid-line)
      #   c) Two points above the upper limit
      #   d) An upward shift (8 points above the mid-line)
      #
      #   Criteria:   Initialized to 0, is set to 1 when special cause occurs
      #   New_Phase:  A token indicating special cause was detected and a new phase
      #               starts
      #   Days_PC:    A token indicating the number of days from the first data point
      #               the new phase starts
      #   Date_PC:    A token indicating the date the new phase starts
      #   Phase_Ch:   The proportion on the day the new phase starts.
      #               Used in Google Studio to plot a red dot when special cause 
      #               is detected
      #   CL, LL, UL:
      #               The limits for the current phase
      #   PhaseCount: The number of phases
      #=============================================================================
      
      # a) Look for 2 points below the Lower Limit
      XBar_Data$SC_a <- if_else(XBar_Data$Days_N > i & XBar_Data$Days_N <=j & XBar_Data$Days_N >= i+Lim_Min-1 & XBar_Data$Dot_mn < XBar_Data$LL & XBar_Data$LL > 0, 1,0)
      XBar_Data$SC_ax <- XBar_Data$SC_a + lag(XBar_Data$SC_a)
      XBar_Data$SC_ax[is.na(XBar_Data$SC_ax)] <- 0
      Days_Pa <- min(XBar_Data$Days_N[XBar_Data$SC_ax==2]) - 1
      
      # b) Look for a downward Shift
      XBar_Data$SC_b <- if_else(XBar_Data$Days_N > i &  XBar_Data$Days_N <=j & XBar_Data$Dot_mn < XBar_Data$CL, 1, 0) 
      XBar_Data$SC_bx <- XBar_Data$SC_b + lag(XBar_Data$SC_b) + lag(XBar_Data$SC_b, 2) + lag(XBar_Data$SC_b, 3) + lag(XBar_Data$SC_b, 4) + lag(XBar_Data$SC_b, 5) + lag(XBar_Data$SC_b, 6) + lag(XBar_Data$SC_b, 7)
      XBar_Data$SC_bx[is.na(XBar_Data$SC_bx)] <- 0
      Days_Pb <- min(XBar_Data$Days_N[XBar_Data$SC_bx==8])
      
      # c) Look for 2 points above the Upper Limit
      XBar_Data$SC_c <- if_else(XBar_Data$Days_N > i & XBar_Data$Days_N <=j & XBar_Data$Days_N >= i+Lim_Min-1 & XBar_Data$Dot_mn > XBar_Data$UL & XBar_Data$UL > 0, 1,0)
      XBar_Data$SC_cx <- XBar_Data$SC_c + lag(XBar_Data$SC_c) 
      XBar_Data$SC_cx[is.na(XBar_Data$SC_cx)] <- 0
      Days_Pc <- min(XBar_Data$Days_N[XBar_Data$SC_cx==2]) - 1
      
      # d) Look for an upwards Shift
      XBar_Data$SC_d <- if_else(XBar_Data$Days_N > i &  XBar_Data$Days_N <=j & XBar_Data$Dot_mn > XBar_Data$CL, 1, 0) 
      XBar_Data$SC_dx <- XBar_Data$SC_d + lag(XBar_Data$SC_d) + lag(XBar_Data$SC_d, 2) + lag(XBar_Data$SC_d, 3) + lag(XBar_Data$SC_d, 4) + lag(XBar_Data$SC_d, 5) + lag(XBar_Data$SC_d, 6) + lag(XBar_Data$SC_d, 7)
      XBar_Data$SC_dx[is.na(XBar_Data$SC_dx)] <- 0
      Days_Pd <- min(XBar_Data$Days_N[XBar_Data$SC_dx==8])
      
      #-------------------------------------------------------------------------------
      # If there is a new phase,  assign the limits and move to the next phase
      #-------------------------------------------------------------------------------
      
      New_Phase <- (is.infinite(Days_Pa)==FALSE | is.infinite(Days_Pb)==FALSE | is.infinite(Days_Pc)==FALSE | is.infinite(Days_Pd)==FALSE)
      
      Days_PC <- min(Days_Pa, Days_Pb, Days_Pc, Days_Pd)
      date_PC <- min(XBar_Data$date[XBar_Data$Days_N == Days_PC + i])
      
      #-------------------------------------------------------------------------------
      # If there is a new phase (special cause),  assign the reason why
      #-------------------------------------------------------------------------------
      
      SC <- ""
      SC <- if_else(New_Phase & Days_PC == Days_Pa, "2 points below the lower limit" , SC)
      SC <- if_else(New_Phase & Days_PC == Days_Pb, "8 points below the midline" , SC)
      SC <- if_else(New_Phase & Days_PC == Days_Pc, "2 points above the upper limit" , SC)
      SC <- if_else(New_Phase & Days_PC == Days_Pd, "8 points above the midline" , SC)
      
      XBar_Data$SC <- if_else(Days_PC == XBar_Data$Days_N, SC, XBar_Data$SC)
      
      #-------------------------------------------------------------------------------
      
      XBar_Data$Phase_Ch <- if_else(Days_PC == XBar_Data$Days_N, XBar_Data$Dot_mn, XBar_Data$Phase_Ch)
      
      XBar_Data$Centerline <- if_else(New_Phase & XBar_Data$Days_N >= i, XBar_Data$CL, XBar_Data$Centerline)
      XBar_Data$Centerline <- if_else(XBar_Data$Days_N >= i & j >= Days_Tot, XBar_Data$CL, XBar_Data$Centerline)
      
      XBar_Data$Upper <- if_else(New_Phase & XBar_Data$Days_N >= i, XBar_Data$UL, XBar_Data$Upper)
      XBar_Data$Upper <- if_else(XBar_Data$Days_N >= i & j >= Days_Tot, XBar_Data$UL, XBar_Data$Upper)
      
      XBar_Data$Lower <- if_else(New_Phase & XBar_Data$Days_N >= i, XBar_Data$LL, XBar_Data$Lower)
      XBar_Data$Lower <- if_else(XBar_Data$Days_N >= i & j >= Days_Tot, XBar_Data$LL, XBar_Data$Lower)
      
      XBar_Data$PhaseCount <- if_else(New_Phase & XBar_Data$Days_N >= Days_PC, XBar_Data$PhaseCount + 1, XBar_Data$PhaseCount)
      
      j <- if_else(New_Phase, Days_PC, j + 1)
      i <- if_else(New_Phase, Days_PC, i)
      
    }  # j Within Setting Loop
    
    XBar_Data$Phase_Ch <- na_if(XBar_Data$Phase_Ch, -99.0)
    
    #===============================================================================
    #   Store the limits in one of two sets of fields (ending a or b)
    #   This enables the limits to be plotted without the wonky join line from
    #   one phase to the next
    #===============================================================================
    
    XBar_Data <- XBar_Data %>%
      mutate(LimTypeA =  ( ((PhaseCount)/2) - trunc((PhaseCount)/2) > 0),
             
             MIDLINEa = if_else(LimTypeA == TRUE, Centerline, -99),
             UPPERa = if_else(LimTypeA == TRUE, Upper, -99),
             LOWERa = if_else(LimTypeA == TRUE, Lower, -99),
             
             MIDLINEb = if_else(LimTypeA == FALSE, Centerline, -99),
             UPPERb = if_else(LimTypeA == FALSE, Upper, -99),
             LOWERb = if_else(LimTypeA == FALSE, Lower, -99) )
 
    #===============================================================================
    #   Tidy the data, keeping only those values we need 
    #===============================================================================
    
    XBar_Data$Dot <- XBar_Data$Dot_mn
    
    XBar_Data <- XBar_Data %>% select(date, place, Dot, 
                          MIDLINEa, UPPERa, LOWERa, 
                          MIDLINEb, UPPERb, LOWERb,  
                          Phase_Ch, PhaseCount, SC)  
    
    XBar_Data <- XBar_Data %>%
      mutate(MIDLINEa = na_if(MIDLINEa, -99),
             MIDLINEb = na_if(MIDLINEb, -99),
             
             UPPERa = na_if(UPPERa, -99),
             UPPERb = na_if(UPPERb, -99),
             
             LOWERa = na_if(LOWERa, -99),
             LOWERb = na_if(LOWERb, -99),
             
             Dot = na_if(Dot, -9),
             Phase_Ch = na_if(Phase_Ch, -99.0))
    
  return(XBar_Data)
  
} #XBarChart Function
  
  

  
  