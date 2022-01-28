#=============================================================================
# U_Chart Function:
#   Send a data frame containing:
#     date: Date formatted field containing the date on which events are reported
#     n:    The numerator for the event proportion
#     N:    The denominator for the event proportion
#=============================================================================

U_Chart <- function(Uchart_Data) {
  
    #=============================================================================
    # Calculate:
    #   Days_N:   The number of days since the first data point
    #   Days_Tot: A token containing the total number of data points
    #   Date_PC:  A token containing the date of the first data point
    #   Days_PC:  A token indicating the number of days on which a phase change
    #             occurs, initialized to 1
    #=============================================================================
    
    Uchart_Data$Days_N <- rank(Uchart_Data$date)
    Days_Tot <- nrow(Uchart_Data)
    Date_PC <- min(Uchart_Data$date)
    Days_PC <- 1
    
    #=============================================================================
    # Calculate:
    #   PhaseCount: The number of special cause phases
    #               Initialized to 1, the first phase
    #   Phase_Ch:   The measure on the day of a phase change, initialized to -99
    #               Used in the charts to show the red dots on the day a phase 
    #               change occurs
    #=============================================================================
    
    Uchart_Data$PhaseCount <- 1
    Phases <- 1
    Uchart_Data$Phase_Ch <- -99
    
    #=============================================================================
    # Initialize the limits to 0 and New_Phase to FALSE
    #=============================================================================
    
    Uchart_Data$Centerline <- 0
    Uchart_Data$Upper <- 0
    Uchart_Data$Lower <- 0
    
    New_Phase <- FALSE
    Uchart_Data$SC <- ""
    
    Uchart_Data$Dot <- Uchart_Data$n/Uchart_Data$N
    
    
    #-------------------------------------------------------------------------------
    # Calculate the limits for each Phase
    #-------------------------------------------------------------------------------
    
    i <- 1
    j <- as.integer(1)
    
    
    while (j <= Days_Tot) {
      
      m <- i + max(j-i, 6)
      m <- min(m, Days_Tot)
      
      
      if (j < 7 | New_Phase) {
        
        Uchart_Data$CL <- sum(Uchart_Data$n[i:m])/sum(Uchart_Data$N[i:m])
        Uchart_Data$LL <- Uchart_Data$CL - 3*sqrt(Uchart_Data$CL/Uchart_Data$N)
        Uchart_Data$UL <- Uchart_Data$CL + 3*sqrt(Uchart_Data$CL/Uchart_Data$N)
        

      }
      
      

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
      Uchart_Data$SC_a <- if_else(Uchart_Data$Days_N > i & Uchart_Data$Days_N <=j & Uchart_Data$Dot < Uchart_Data$LL & Uchart_Data$LL > 0, 1,0)
      
      Uchart_Data$SC_ax <- Uchart_Data$SC_a + lag(Uchart_Data$SC_a)
      Uchart_Data$SC_ax[is.na(Uchart_Data$SC_ax)] <- 0
      Days_Pa <- min(Uchart_Data$Days_N[Uchart_Data$SC_ax==2]) - 1
      
      # b) Look for a downward Shift
      Uchart_Data$SC_b <- if_else(Uchart_Data$Days_N > i &  Uchart_Data$Days_N <=j & Uchart_Data$Dot < Uchart_Data$CL, 1, 0) 
      Uchart_Data$SC_bx <- Uchart_Data$SC_b + lag(Uchart_Data$SC_b) + lag(Uchart_Data$SC_b, 2) + lag(Uchart_Data$SC_b, 3) + lag(Uchart_Data$SC_b, 4) + lag(Uchart_Data$SC_b, 5) + lag(Uchart_Data$SC_b, 6) + lag(Uchart_Data$SC_b, 7)
      Uchart_Data$SC_bx[is.na(Uchart_Data$SC_bx)] <- 0
      Days_Pb <- min(Uchart_Data$Days_N[Uchart_Data$SC_bx==8])
      
      # c) Look for 2 points above the Upper Limit
      Uchart_Data$SC_c <- if_else(Uchart_Data$Days_N > i & Uchart_Data$Days_N <=j & Uchart_Data$Dot > Uchart_Data$UL & Uchart_Data$UL > 0, 1,0)
      Uchart_Data$SC_cx <- Uchart_Data$SC_c + lag(Uchart_Data$SC_c) 
      Uchart_Data$SC_cx[is.na(Uchart_Data$SC_cx)] <- 0
      Days_Pc <- min(Uchart_Data$Days_N[Uchart_Data$SC_cx==2]) - 1
      
      # d) Look for an upwards Shift
      Uchart_Data$SC_d <- if_else(Uchart_Data$Days_N > i &  Uchart_Data$Days_N <=j & Uchart_Data$Dot > Uchart_Data$CL, 1, 0) 
      Uchart_Data$SC_dx <- Uchart_Data$SC_d + lag(Uchart_Data$SC_d) + lag(Uchart_Data$SC_d, 2) + lag(Uchart_Data$SC_d, 3) + lag(Uchart_Data$SC_d, 4) + lag(Uchart_Data$SC_d, 5) + lag(Uchart_Data$SC_d, 6) + lag(Uchart_Data$SC_d, 7)
      Uchart_Data$SC_dx[is.na(Uchart_Data$SC_dx)] <- 0
      Days_Pd <- min(Uchart_Data$Days_N[Uchart_Data$SC_dx==8])
      
      #-------------------------------------------------------------------------------
      # If there is a new phase,  assign the limits and move to the next phase
      #-------------------------------------------------------------------------------
      
      New_Phase <- (is.infinite(Days_Pa)==FALSE | is.infinite(Days_Pb)==FALSE | is.infinite(Days_Pc)==FALSE | is.infinite(Days_Pd)==FALSE)
      
      
      Days_PC <- min(Days_Pa, Days_Pb, Days_Pc, Days_Pd)
      date_PC <- min(Uchart_Data$date[Uchart_Data$Days_N == Days_PC + i])
      
      #-------------------------------------------------------------------------------
      # If there is a new phase (special cause),  assign the reason why
      #-------------------------------------------------------------------------------
      
      SC <- ""
      SC <- if_else(New_Phase & Days_PC == Days_Pa, "2 points below the lower limit" , SC)
      SC <- if_else(New_Phase & Days_PC == Days_Pb, "8 points below the midline" , SC)
      SC <- if_else(New_Phase & Days_PC == Days_Pc, "2 points above the upper limit" , SC)
      SC <- if_else(New_Phase & Days_PC == Days_Pd, "8 points above the midline" , SC)
      
      
      Uchart_Data$SC <- if_else(Days_PC == Uchart_Data$Days_N, SC, Uchart_Data$SC)
      
      #-------------------------------------------------------------------------------

      Uchart_Data$Phase_Ch <- if_else(Days_PC == Uchart_Data$Days_N, Uchart_Data$Dot, Uchart_Data$Phase_Ch)
      
      Uchart_Data$Centerline <- if_else(New_Phase & Uchart_Data$Days_N >= i, Uchart_Data$CL, Uchart_Data$Centerline)
      Uchart_Data$Centerline <- if_else(Uchart_Data$Days_N >= i & j >= Days_Tot, Uchart_Data$CL, Uchart_Data$Centerline)
      
      Uchart_Data$Upper <- if_else(New_Phase & Uchart_Data$Days_N >= i, Uchart_Data$UL, Uchart_Data$Upper)
      Uchart_Data$Upper <- if_else(Uchart_Data$Days_N >= i & j >= Days_Tot, Uchart_Data$UL, Uchart_Data$Upper)
      
      
      Uchart_Data$Lower <- if_else(New_Phase & Uchart_Data$Days_N >= i, Uchart_Data$LL, Uchart_Data$Lower)
      Uchart_Data$Lower <- if_else(Uchart_Data$Days_N >= i & j >= Days_Tot, Uchart_Data$LL, Uchart_Data$Lower)
      
      Uchart_Data$PhaseCount <- if_else(New_Phase & Uchart_Data$Days_N >= Days_PC, Uchart_Data$PhaseCount + 1, Uchart_Data$PhaseCount)
      
      
      j <- if_else(New_Phase, Days_PC, j + 1)
      i <- if_else(New_Phase, Days_PC, i)
      
    }  # j Within Setting Loop
    
    
    Uchart_Data$Phase_Ch <- na_if(Uchart_Data$Phase_Ch, -99.0)
    
    #===============================================================================
    #   Store the limits in one of two sets of fields (ending a or b)
    #   This enables the limits to be plotted without the wonky join line from
    #   one phase to the next
    #===============================================================================
    
    
    Uchart_Data$LimTypeA <- ( ((Uchart_Data$PhaseCount)/2) - trunc((Uchart_Data$PhaseCount)/2) > 0)
    
    Uchart_Data$MIDLINEa <- if_else(Uchart_Data$LimTypeA == TRUE, Uchart_Data$Centerline, -99)
    Uchart_Data$UPPERa <- if_else(Uchart_Data$LimTypeA == TRUE, Uchart_Data$Upper, -99)
    Uchart_Data$LOWERa <- if_else(Uchart_Data$LimTypeA == TRUE, Uchart_Data$Lower, -99)
    
    Uchart_Data$MIDLINEb <- if_else(Uchart_Data$LimTypeA == FALSE, Uchart_Data$Centerline, -99)
    Uchart_Data$UPPERb <- if_else(Uchart_Data$LimTypeA == FALSE, Uchart_Data$Upper, -99)
    Uchart_Data$LOWERb <- if_else(Uchart_Data$LimTypeA == FALSE, Uchart_Data$Lower, -99)
    
    

    #===============================================================================
    #   Tidy the data, keeping only those values we need 
    #===============================================================================
    
    Uchart_Data <- Uchart_Data %>% select(date, n, N, Dot, 
                                MIDLINEa, UPPERa, LOWERa, 
                                MIDLINEb, UPPERb, LOWERb,  
                                Phase_Ch, PhaseCount, SC)  
    
    
    Uchart_Data$MIDLINEa <- na_if(Uchart_Data$MIDLINEa, -99)
    Uchart_Data$MIDLINEb <- na_if(Uchart_Data$MIDLINEb, -99)
    
    Uchart_Data$UPPERa <- na_if(Uchart_Data$UPPERa, -99)
    Uchart_Data$UPPERb <- na_if(Uchart_Data$UPPERb, -99)
    
    Uchart_Data$LOWERa <- na_if(Uchart_Data$LOWERa, -99)
    Uchart_Data$LOWERb <- na_if(Uchart_Data$LOWERb, -99)
    
    
    Uchart_Data$Dot <- na_if(Uchart_Data$Dot, -9)
    Uchart_Data$Phase_Ch <- na_if(Uchart_Data$Phase_Ch, -99.0)
   

  return(Uchart_Data)
  
}

