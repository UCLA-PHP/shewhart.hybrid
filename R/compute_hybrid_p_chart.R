#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
compute_hybrid_p_chart = function(dataset)
{




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
  dataset$Phase_Ch <- -99

  #=============================================================================
  # Initialize the limits to 0
  #=============================================================================

  dataset$Centerline <- 0
  dataset$Upper <- 0
  dataset$Lower <- 0


  #===============================================================================
  # Identify astronomical values
  # These are likely reflective of data issues that can still make it into
  # the dataset
  #
  # This approach for each point, uses the previous 2 values to indicate if the
  # current value differs by more than 0.05 (5%) from the previous point and
  # if so, flags it as an outlier not used to calculate the centerline
  #
  # Outputs:
  #   N_use:    Denominator to use in calculating the limits
  #   n_use:    Numerator to use in calculating the limits
  #   Dot_use:  Proportion (dot) to use in calculating the limits
  #
  #===============================================================================

  dataset$Dot <- dataset$n/dataset$N

  dataset$outlier <- 1
  dataset$Dot_lag1 <- 0
  dataset$Dot_lag1 <- abs(dataset$Dot - lag(dataset$Dot))
  dataset$Dot_lag1 <- if_else(is.na(dataset$Dot_lag1), 0, dataset$Dot_lag1)

  dataset$Dot_lag2 <- 0
  dataset$Dot_lag2 <- abs(dataset$Dot - lag(dataset$Dot, 2))
  dataset$Dot_lag2 <- if_else(is.na(dataset$Dot_lag2), 0, dataset$Dot_lag2)

  dataset$outlier <- if_else(dataset$Dot_lag1 > 0.05 & dataset$Dot_lag2 > 0.05, 0, 1)

  dataset$N_use <- dataset$N * dataset$outlier
  dataset$n_use <- dataset$n * dataset$outlier
  dataset$Dot_use <- dataset$Dot * dataset$outlier


  #===============================================================================
  # Run Phase 1 to get started
  #===============================================================================

  #-------------------------------------------------------------------------------
  # Initialize:
  #   i:  The starting day number of the current phase
  #   j:  The ending day number of the current phase
  #-------------------------------------------------------------------------------

  i <- 1
  j <- as.integer(1)


  #===============================================================================
  # Work through the data from day number j to the final day number (Days_Tot)
  # Make sure the initial limits are based on at least six data points
  #===============================================================================

  while (j < Days_Tot) {
    m <- i + max(j-i, 6)
    m <- min(m, Days_Tot)
    dataset$CL <- sum(dataset$n_use[i:m])/sum(dataset$N_use[i:m])
    dataset$LL <- dataset$CL - 3*sqrt(dataset$CL*(1-dataset$CL)/dataset$N)
    dataset$UL <- dataset$CL + 3*sqrt(dataset$CL*(1-dataset$CL)/dataset$N)


    #=============================================================================
    # Look for two points above the upper limit
    #
    #   Criteria:   Initialized to 0, is set to 1 when special cause occurs
    #   SC_c:       Special Cause Type c) A point above the upper limit
    #   SC_cx:      Special Cause Type c) TWO points above the upper limit
    #   New_Phase:  A token indicating special cause was detected and a new phase
    #               starts
    #   Days_PC:    A token indicating the number of days from the first data point
    #               the new phase starts
    #   Date_PC:    A token indicating the date the new phase starts
    #   Phase_Ch:   The proportion on the day the new phase starts
    #   Centerline, Lower, Upper:
    #               The limits for the first phase
    #   PhaseCount: The number of phases
    #=============================================================================


    dataset$Criteria <- 0
    dataset$Criteria <- if_else(dataset$Days_N > i & dataset$Dot_use > dataset$UL & dataset$UL > 0, 1, 0)

    dataset$SC_c <- dataset$Criteria
    dataset$SC_cx <- dataset$SC_c + lag(dataset$SC_c)
    dataset$SC_cx[is.na(dataset$SC_cx)] <- 0

    New_Phase <- (max(dataset$SC_cx) == 2)

    Days_PC <- min(dataset$Days_N[dataset$SC_cx==2]) - 1
    Date_PC <- min(dataset$date[dataset$SC_cx==2]) -1


    dataset$Phase_Ch <- if_else(Days_PC == dataset$Days_N, dataset$Dot, dataset$Phase_Ch)
    dataset$Centerline <- dataset$CL
    dataset$Upper <- dataset$UL
    dataset$Lower <- dataset$LL

    dataset$PhaseCount <- if_else(dataset$Days_N >= Days_PC, 2, 1)


    #-------------------------------------------------------------------------------
    # If there is a new phase, stop the loop and move onto the next phase,
    # otherwise keep working through the data
    #-------------------------------------------------------------------------------

    j <- if_else(New_Phase, Days_Tot, as.integer(j + 1))

  }



  #===============================================================================
  # End of Phase 1
  #===============================================================================



  #-------------------------------------------------------------------------------
  # Calculate the limits for the subsequent Phases
  #-------------------------------------------------------------------------------

  i <- Days_PC
  j <- Days_PC

  while (j <= Days_Tot) {

    m <- i + max(j-i, 6)
    m <- min(m, Days_Tot)


    dataset$CL <- sum(dataset$n_use[i:m])/sum(dataset$N_use[i:m])
    dataset$LL <- dataset$CL - 3*sqrt(dataset$CL*(1-dataset$CL)/dataset$N)
    dataset$UL <- dataset$CL + 3*sqrt(dataset$CL*(1-dataset$CL)/dataset$N)


    #=============================================================================
    # Look for two points above the upper limit
    #
    #   Criteria:   Initialized to 0, is set to 1 when special cause occurs
    #   SC_c:       Special Cause Type c) A point above the upper limit
    #   SC_cx:      Special Cause Type c) TWO points above the upper limit
    #   New_Phase:  A token indicating special cause was detected and a new phase
    #               starts
    #   Days_PC:    A token indicating the number of days from the first data point
    #               the new phase starts
    #   Date_PC:    A token indicating the date the new phase starts
    #   Phase_Ch:   The proportion on the day the new phase starts.
    #               Used in Google Studio to plot a red dot when special cause
    #               is detected
    #   Centerline, Lower, Upper:
    #               The limits for the first phase
    #   PhaseCount: The number of phases
    #=============================================================================

    dataset$Criteria <- 0
    dataset$Criteria <- if_else(dataset$Days_N > i & dataset$Days_N <=j & dataset$Dot_use > dataset$UL & dataset$UL > 0, 1,0)

    dataset$SC_c <- dataset$Criteria
    dataset$SC_cx <- dataset$SC_c + lag(dataset$SC_c)
    dataset$SC_cx[is.na(dataset$SC_cx)] <- 0

    New_Phase <- (max(dataset$SC_cx) == 2)

    Days_PC <- min(dataset$Days_N[dataset$SC_cx==2]) - 1
    date_PC <- min(dataset$date[dataset$SC_cx==2]) -1


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
  # Calculate values with names that align with the existing names
  # in Google Studio
  #===============================================================================

  dataset$Y_Max <- 0.15
  dataset$EPOCH <- dataset$PhaseCount
  dataset$new_events <- dataset$Dot

  #===============================================================================
  #   Tidy the data, keeping only those values we need
  #===============================================================================

  dataset %<>%
    select(date, place, new_events, StateRR,
           MIDLINEa, UPPERa, LOWERa,
           MIDLINEb, UPPERb, LOWERb,
           Y_Max, Phase_Ch, EPOCH, N, n)


  dataset$MIDLINEa <- na_if(dataset$MIDLINEa, -99)
  dataset$MIDLINEb <- na_if(dataset$MIDLINEb, -99)

  dataset$UPPERa <- na_if(dataset$UPPERa, -99)
  dataset$UPPERb <- na_if(dataset$UPPERb, -99)

  dataset$LOWERa <- na_if(dataset$LOWERa, -99)
  dataset$LOWERb <- na_if(dataset$LOWERb, -99)

  dataset$new_events <- na_if(dataset$new_events, -9)

  dataset$Phase_Ch <- na_if(dataset$Phase_Ch, -99.0)

  dataset %<>% mutate(
    phase_change = !is.na(Phase_Ch),
    `Observed %` = n/N,
    Midline =
      if_else(
        is.na(MIDLINEa), MIDLINEb, MIDLINEa),
    `Upper Limit` =
      if_else(is.na(MIDLINEa), UPPERb, UPPERa),
    `Lower Limit` =
      if_else(is.na(MIDLINEa), LOWERb, LOWERa),
    `Lower Limit` =
      if_else(`Lower Limit` < 0, NA_real_, `Lower Limit`)
  )

  #-------------------------------------------------------------------------------
  #   Create fields containing the Chart Title, and Sub-Title for use in
  #   Google Studio
  #-------------------------------------------------------------------------------

  dataset$Chart_Title <- paste(dataset$place,": Daily Covid-19 Percent Positive Tests")

  dataset$Epoch_txt <- dataset$EPOCH

  dataset$Sub_Title <- paste(dataset$place, " is in Phase ", dataset$Epoch_txt)


  return(dataset)


}
