#=============================================================================
# U_Prime Function:
#   Send a data frame containing:
#     date: Date formatted field containing the date on which events are reported
#     n:    The numerator for the event proportion
#     N:    The denominator for the event proportion
#=============================================================================

#' Title
#'
#' @param dataset a data frame containing:
#'
#'     `date`: Date formatted field containing the date on which events are reported
#'
#'    `n`:    The numerator for the event proportion
#'
#'     `N`:    The denominator for the event proportion
#'
#' @return
#' @export
#' @author Gareth Parry

U_Prime <- function(dataset) {

    #=============================================================================
    # Calculate:
    #   Days_N:   The number of days since the first data point
    #   Days_Tot: A token containing the total number of data points
    #   Date_PC:  A token containing the date of the first data point
    #   Days_PC:  A token indicating the number of days on which a phase change
    #             occurs, initialized to 1
    #=============================================================================

    dataset$Days_N <- rank(dataset$date)
    Days_Tot <- nrow(dataset)
    Date_PC <- min(dataset$date)
    Days_PC <- 1

    #=============================================================================
    # Calculate:
    #   PhaseCount: The number of special cause phases
    #               Initialized to 1, the first phase
    #   Phase_Ch:   The measure on the day of a phase change, initialized to -99
    #               Used in the charts to show the red dots on the day a phase
    #               change occurs
    #=============================================================================

    dataset$PhaseCount <- 1
    Phases <- 1
    dataset$Phase_Ch <- -99

    #=============================================================================
    # Initialize the limits to 0 and New_Phase to FALSE
    #=============================================================================

    dataset$Centerline <- 0
    dataset$Upper <- 0
    dataset$Lower <- 0

    New_Phase <- FALSE
    dataset$SC <- ""

    dataset$Dot <- dataset$n/dataset$N


    #-------------------------------------------------------------------------------
    # Calculate the limits for each Phase
    #-------------------------------------------------------------------------------

    i <- 1
    j <- as.integer(1)


    while (j <= Days_Tot) {

      m <- i + max(j-i, 6)
      m <- min(m, Days_Tot)


      if (j < 7 | New_Phase) {

        dataset$CL <- sum(dataset$n[i:m])/sum(dataset$N[i:m])

        dataset$Sigma_i <- sqrt(dataset$CL/dataset$N)
        dataset$Zi <- (dataset$Dot - dataset$CL)/dataset$Sigma_i


        dataset$MR <- abs( dataset$Zi - lag(dataset$Zi) )


        MR_UCL_2 <- 3.27*mean(dataset$MR[i:m], na.rm = TRUE)

        dataset$Screened_MR <- if_else(dataset$MR <= MR_UCL_2, 1, 0)


        SigmaZ <- ( mean(dataset$MR[i:m], na.rm = TRUE) ) /  1.128



        dataset$LL <- dataset$CL - 3*SigmaZ*sqrt(dataset$CL/dataset$N)
        dataset$UL <- dataset$CL + 3*SigmaZ*sqrt(dataset$CL/dataset$N)

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
      dataset$SC_a <- if_else(dataset$Days_N > i & dataset$Days_N <=j & dataset$Dot < dataset$LL & dataset$LL > 0, 1,0)

      dataset$SC_ax <- dataset$SC_a + lag(dataset$SC_a)
      dataset$SC_ax[is.na(dataset$SC_ax)] <- 0
      Days_Pa <- min(dataset$Days_N[dataset$SC_ax==2]) - 1

      # b) Look for a downward Shift
      dataset$SC_b <- if_else(dataset$Days_N > i &  dataset$Days_N <=j & dataset$Dot < dataset$CL, 1, 0)
      dataset$SC_bx <- dataset$SC_b + lag(dataset$SC_b) + lag(dataset$SC_b, 2) + lag(dataset$SC_b, 3) + lag(dataset$SC_b, 4) + lag(dataset$SC_b, 5) + lag(dataset$SC_b, 6) + lag(dataset$SC_b, 7)
      dataset$SC_bx[is.na(dataset$SC_bx)] <- 0
      Days_Pb <- min(dataset$Days_N[dataset$SC_bx==8])

      # c) Look for 2 points above the Upper Limit
      dataset$SC_c <- if_else(dataset$Days_N > i & dataset$Days_N <=j & dataset$Dot > dataset$UL & dataset$UL > 0, 1,0)
      dataset$SC_cx <- dataset$SC_c + lag(dataset$SC_c)
      dataset$SC_cx[is.na(dataset$SC_cx)] <- 0
      Days_Pc <- min(dataset$Days_N[dataset$SC_cx==2]) - 1

      # d) Look for an upwards Shift
      dataset$SC_d <- if_else(dataset$Days_N > i &  dataset$Days_N <=j & dataset$Dot > dataset$CL, 1, 0)
      dataset$SC_dx <- dataset$SC_d + lag(dataset$SC_d) + lag(dataset$SC_d, 2) + lag(dataset$SC_d, 3) + lag(dataset$SC_d, 4) + lag(dataset$SC_d, 5) + lag(dataset$SC_d, 6) + lag(dataset$SC_d, 7)
      dataset$SC_dx[is.na(dataset$SC_dx)] <- 0
      Days_Pd <- min(dataset$Days_N[dataset$SC_dx==8])

      #-------------------------------------------------------------------------------
      # If there is a new phase,  assign the limits and move to the next phase
      #-------------------------------------------------------------------------------

      New_Phase <- (is.infinite(Days_Pa)==FALSE | is.infinite(Days_Pb)==FALSE | is.infinite(Days_Pc)==FALSE | is.infinite(Days_Pd)==FALSE)


      Days_PC <- min(Days_Pa, Days_Pb, Days_Pc, Days_Pd)
      date_PC <- min(dataset$date[dataset$Days_N == Days_PC + i])

      #-------------------------------------------------------------------------------
      # If there is a new phase (special cause),  assign the reason why
      #-------------------------------------------------------------------------------

      SC <- ""
      SC <- if_else(New_Phase & Days_PC == Days_Pa, "2 points below the lower limit" , SC)
      SC <- if_else(New_Phase & Days_PC == Days_Pb, "8 points below the midline" , SC)
      SC <- if_else(New_Phase & Days_PC == Days_Pc, "2 points above the upper limit" , SC)
      SC <- if_else(New_Phase & Days_PC == Days_Pd, "8 points above the midline" , SC)


      dataset$SC <- if_else(Days_PC == dataset$Days_N, SC, dataset$SC)

      #-------------------------------------------------------------------------------

      dataset$Phase_Ch <- if_else(Days_PC == dataset$Days_N, dataset$Dot, dataset$Phase_Ch)

      dataset$Centerline <- if_else(New_Phase & dataset$Days_N >= i, dataset$CL, dataset$Centerline)
      dataset$Centerline <- if_else(dataset$Days_N >= i & j >= Days_Tot, dataset$CL, dataset$Centerline)

      dataset$Upper <- if_else(New_Phase & dataset$Days_N >= i, dataset$UL, dataset$Upper)
      dataset$Upper <- if_else(dataset$Days_N >= i & j >= Days_Tot, dataset$UL, dataset$Upper)


      dataset$Lower <- if_else(New_Phase & dataset$Days_N >= i, dataset$LL, dataset$Lower)
      dataset$Lower <- if_else(dataset$Days_N >= i & j >= Days_Tot, dataset$LL, dataset$Lower)

      dataset$PhaseCount <- if_else(New_Phase & dataset$Days_N >= Days_PC, dataset$PhaseCount + 1, dataset$PhaseCount)


      j <- if_else(New_Phase, Days_PC, j + 1)
      i <- if_else(New_Phase, Days_PC, i)

    }  # j Within Setting Loop


    dataset$Phase_Ch <- na_if(dataset$Phase_Ch, -99.0)

    #===============================================================================
    #   Store the limits in one of two sets of fields (ending a or b)
    #   This enables the limits to be plotted without the wonky join line from
    #   one phase to the next
    #===============================================================================


    dataset$LimTypeA <- ( ((dataset$PhaseCount)/2) - trunc((dataset$PhaseCount)/2) > 0)

    dataset$MIDLINEa <- if_else(dataset$LimTypeA == TRUE, dataset$Centerline, -99)
    dataset$UPPERa <- if_else(dataset$LimTypeA == TRUE, dataset$Upper, -99)
    dataset$LOWERa <- if_else(dataset$LimTypeA == TRUE, dataset$Lower, -99)

    dataset$MIDLINEb <- if_else(dataset$LimTypeA == FALSE, dataset$Centerline, -99)
    dataset$UPPERb <- if_else(dataset$LimTypeA == FALSE, dataset$Upper, -99)
    dataset$LOWERb <- if_else(dataset$LimTypeA == FALSE, dataset$Lower, -99)



    #===============================================================================
    #   Tidy the data, keeping only those values we need
    #===============================================================================

    dataset <- dataset %>% select(date, n, N, Dot,
                                MIDLINEa, UPPERa, LOWERa,
                                MIDLINEb, UPPERb, LOWERb,
                                Phase_Ch, PhaseCount, SC)


    dataset$MIDLINEa <- na_if(dataset$MIDLINEa, -99)
    dataset$MIDLINEb <- na_if(dataset$MIDLINEb, -99)

    dataset$UPPERa <- na_if(dataset$UPPERa, -99)
    dataset$UPPERb <- na_if(dataset$UPPERb, -99)

    dataset$LOWERa <- na_if(dataset$LOWERa, -99)
    dataset$LOWERb <- na_if(dataset$LOWERb, -99)


    dataset$Dot <- na_if(dataset$Dot, -9)
    dataset$Phase_Ch <- na_if(dataset$Phase_Ch, -99.0)


  return(dataset)

}

