#=============================================================================
# P_Prime Function:
#   Send a data frame containing:
#     date: Date formatted field containing the date on which events are reported
#     n:    The numerator for the event proportion
#     N:    The denominator for the event proportion
#=============================================================================

#' Title
#'
#' @param Pchart_Data
#'
#' @return
#' @export
#'

P_Prime <- function(Pchart_Data) {

    #=============================================================================
    # Calculate:
    #   Days_N:   The number of days since the first data point
    #   Days_Tot: A token containing the total number of data points
    #   Date_PC:  A token containing the date of the first data point
    #   Days_PC:  A token indicating the number of days on which a phase change
    #             occurs, initialized to 1
    #=============================================================================

    Pchart_Data$Days_N <- rank(Pchart_Data$date)
    Days_Tot <- nrow(Pchart_Data)
    Date_PC <- min(Pchart_Data$date)
    Days_PC <- 1

    #=============================================================================
    # Calculate:
    #   PhaseCount: The number of special cause phases
    #               Initialized to 1, the first phase
    #   Phase_Ch:   The measure on the day of a phase change, initialized to -99
    #               Used in the charts to show the red dots on the day a phase
    #               change occurs
    #=============================================================================

    Pchart_Data$PhaseCount <- 1
    Phases <- 1
    Pchart_Data$Phase_Ch <- -99

    #=============================================================================
    # Initialize the limits to 0 and New_Phase to FALSE
    #=============================================================================

    Pchart_Data$Centerline <- 0
    Pchart_Data$Upper <- 0
    Pchart_Data$Lower <- 0

    New_Phase <- FALSE
    Pchart_Data$SC <- ""

    Pchart_Data$Dot <- Pchart_Data$n/Pchart_Data$N


    #-------------------------------------------------------------------------------
    # Calculate the limits for each Phase
    #-------------------------------------------------------------------------------

    i <- 1
    j <- as.integer(1)


    while (j <= Days_Tot) {

      m <- i + max(j-i, 6)
      m <- min(m, Days_Tot)


      if (j < 7 | New_Phase) {

        Pchart_Data$CL <- sum(Pchart_Data$n[i:m])/sum(Pchart_Data$N[i:m])

        Pchart_Data$Sigma_i <- sqrt(Pchart_Data$CL*(1-Pchart_Data$CL)/Pchart_Data$N)
        Pchart_Data$Zi <- (Pchart_Data$Dot - Pchart_Data$CL)/Pchart_Data$Sigma_i

        Pchart_Data$MR <- abs( Pchart_Data$Zi - lag(Pchart_Data$Zi) )


        MR_UCL_2 <- 3.27*mean(Pchart_Data$MR[i:m], na.rm = TRUE)

        Pchart_Data$Screened_MR <- if_else(Pchart_Data$MR <= MR_UCL_2, 1, 0)
        SigmaZ <- ( sum(Pchart_Data$MR[i:m], na.rm = TRUE) ) / ( 1.128*sum(Pchart_Data$Screened_MR[i:m], na.rm = TRUE) )


        Pchart_Data$LL <- Pchart_Data$CL - 3*SigmaZ*sqrt(Pchart_Data$CL*(1-Pchart_Data$CL)/Pchart_Data$N)
        Pchart_Data$UL <- Pchart_Data$CL + 3*SigmaZ*sqrt(Pchart_Data$CL*(1-Pchart_Data$CL)/Pchart_Data$N)

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
      Pchart_Data$SC_a <- if_else(Pchart_Data$Days_N > i & Pchart_Data$Days_N <=j & Pchart_Data$Dot < Pchart_Data$LL & Pchart_Data$LL > 0, 1,0)

      Pchart_Data$SC_ax <- Pchart_Data$SC_a + lag(Pchart_Data$SC_a)
      Pchart_Data$SC_ax[is.na(Pchart_Data$SC_ax)] <- 0
      Days_Pa <- min(Pchart_Data$Days_N[Pchart_Data$SC_ax==2]) - 1

      # b) Look for a downward Shift
      Pchart_Data$SC_b <- if_else(Pchart_Data$Days_N > i &  Pchart_Data$Days_N <=j & Pchart_Data$Dot < Pchart_Data$CL, 1, 0)
      Pchart_Data$SC_bx <- Pchart_Data$SC_b + lag(Pchart_Data$SC_b) + lag(Pchart_Data$SC_b, 2) + lag(Pchart_Data$SC_b, 3) + lag(Pchart_Data$SC_b, 4) + lag(Pchart_Data$SC_b, 5) + lag(Pchart_Data$SC_b, 6) + lag(Pchart_Data$SC_b, 7)
      Pchart_Data$SC_bx[is.na(Pchart_Data$SC_bx)] <- 0
      Days_Pb <- min(Pchart_Data$Days_N[Pchart_Data$SC_bx==8])

      # c) Look for 2 points above the Upper Limit
      Pchart_Data$SC_c <- if_else(Pchart_Data$Days_N > i & Pchart_Data$Days_N <=j & Pchart_Data$Dot > Pchart_Data$UL & Pchart_Data$UL > 0, 1,0)
      Pchart_Data$SC_cx <- Pchart_Data$SC_c + lag(Pchart_Data$SC_c)
      Pchart_Data$SC_cx[is.na(Pchart_Data$SC_cx)] <- 0
      Days_Pc <- min(Pchart_Data$Days_N[Pchart_Data$SC_cx==2]) - 1

      # d) Look for an upwards Shift
      Pchart_Data$SC_d <- if_else(Pchart_Data$Days_N > i &  Pchart_Data$Days_N <=j & Pchart_Data$Dot > Pchart_Data$CL, 1, 0)
      Pchart_Data$SC_dx <- Pchart_Data$SC_d + lag(Pchart_Data$SC_d) + lag(Pchart_Data$SC_d, 2) + lag(Pchart_Data$SC_d, 3) + lag(Pchart_Data$SC_d, 4) + lag(Pchart_Data$SC_d, 5) + lag(Pchart_Data$SC_d, 6) + lag(Pchart_Data$SC_d, 7)
      Pchart_Data$SC_dx[is.na(Pchart_Data$SC_dx)] <- 0
      Days_Pd <- min(Pchart_Data$Days_N[Pchart_Data$SC_dx==8])

      #-------------------------------------------------------------------------------
      # If there is a new phase,  assign the limits and move to the next phase
      #-------------------------------------------------------------------------------

      New_Phase <- (is.infinite(Days_Pa)==FALSE | is.infinite(Days_Pb)==FALSE | is.infinite(Days_Pc)==FALSE | is.infinite(Days_Pd)==FALSE)


      Days_PC <- min(Days_Pa, Days_Pb, Days_Pc, Days_Pd)
      date_PC <- min(Pchart_Data$date[Pchart_Data$Days_N == Days_PC + i])

      #-------------------------------------------------------------------------------
      # If there is a new phase (special cause),  assign the reason why
      #-------------------------------------------------------------------------------

      SC <- ""
      SC <- if_else(New_Phase & Days_PC == Days_Pa, "2 points below the lower limit" , SC)
      SC <- if_else(New_Phase & Days_PC == Days_Pb, "8 points below the midline" , SC)
      SC <- if_else(New_Phase & Days_PC == Days_Pc, "2 points above the upper limit" , SC)
      SC <- if_else(New_Phase & Days_PC == Days_Pd, "8 points above the midline" , SC)


      Pchart_Data$SC <- if_else(Days_PC == Pchart_Data$Days_N, SC, Pchart_Data$SC)

      #-------------------------------------------------------------------------------

      Pchart_Data$Phase_Ch <- if_else(Days_PC == Pchart_Data$Days_N, Pchart_Data$Dot, Pchart_Data$Phase_Ch)

      Pchart_Data$Centerline <- if_else(New_Phase & Pchart_Data$Days_N >= i, Pchart_Data$CL, Pchart_Data$Centerline)
      Pchart_Data$Centerline <- if_else(Pchart_Data$Days_N >= i & j >= Days_Tot, Pchart_Data$CL, Pchart_Data$Centerline)

      Pchart_Data$Upper <- if_else(New_Phase & Pchart_Data$Days_N >= i, Pchart_Data$UL, Pchart_Data$Upper)
      Pchart_Data$Upper <- if_else(Pchart_Data$Days_N >= i & j >= Days_Tot, Pchart_Data$UL, Pchart_Data$Upper)


      Pchart_Data$Lower <- if_else(New_Phase & Pchart_Data$Days_N >= i, Pchart_Data$LL, Pchart_Data$Lower)
      Pchart_Data$Lower <- if_else(Pchart_Data$Days_N >= i & j >= Days_Tot, Pchart_Data$LL, Pchart_Data$Lower)

      Pchart_Data$PhaseCount <- if_else(New_Phase & Pchart_Data$Days_N >= Days_PC, Pchart_Data$PhaseCount + 1, Pchart_Data$PhaseCount)


      j <- if_else(New_Phase, Days_PC, j + 1)
      i <- if_else(New_Phase, Days_PC, i)

    }  # j Within Setting Loop


    Pchart_Data$Phase_Ch <- na_if(Pchart_Data$Phase_Ch, -99.0)

    #===============================================================================
    #   Store the limits in one of two sets of fields (ending a or b)
    #   This enables the limits to be plotted without the wonky join line from
    #   one phase to the next
    #===============================================================================


    Pchart_Data$LimTypeA <- ( ((Pchart_Data$PhaseCount)/2) - trunc((Pchart_Data$PhaseCount)/2) > 0)

    Pchart_Data$MIDLINEa <- if_else(Pchart_Data$LimTypeA == TRUE, Pchart_Data$Centerline, -99)
    Pchart_Data$UPPERa <- if_else(Pchart_Data$LimTypeA == TRUE, Pchart_Data$Upper, -99)
    Pchart_Data$LOWERa <- if_else(Pchart_Data$LimTypeA == TRUE, Pchart_Data$Lower, -99)

    Pchart_Data$MIDLINEb <- if_else(Pchart_Data$LimTypeA == FALSE, Pchart_Data$Centerline, -99)
    Pchart_Data$UPPERb <- if_else(Pchart_Data$LimTypeA == FALSE, Pchart_Data$Upper, -99)
    Pchart_Data$LOWERb <- if_else(Pchart_Data$LimTypeA == FALSE, Pchart_Data$Lower, -99)



    #===============================================================================
    #   Tidy the data, keeping only those values we need in a data frame called
    #   OutputData
    #===============================================================================

    Pchart_Data <- Pchart_Data %>% select(date, n, N, Dot,
                                MIDLINEa, UPPERa, LOWERa,
                                MIDLINEb, UPPERb, LOWERb,
                                Phase_Ch, PhaseCount, SC)


    Pchart_Data$MIDLINEa <- na_if(Pchart_Data$MIDLINEa, -99)
    Pchart_Data$MIDLINEb <- na_if(Pchart_Data$MIDLINEb, -99)

    Pchart_Data$UPPERa <- na_if(Pchart_Data$UPPERa, -99)
    Pchart_Data$UPPERb <- na_if(Pchart_Data$UPPERb, -99)

    Pchart_Data$LOWERa <- na_if(Pchart_Data$LOWERa, -99)
    Pchart_Data$LOWERb <- na_if(Pchart_Data$LOWERb, -99)


    Pchart_Data$Dot <- na_if(Pchart_Data$Dot, -9)
    Pchart_Data$Phase_Ch <- na_if(Pchart_Data$Phase_Ch, -99.0)


  return(Pchart_Data)

}

