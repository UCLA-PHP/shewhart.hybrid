#' I Chart
#'
#' @param date
#' @param Dot
#' @param IChart_Data
#' @author Gareth Parry
#' @return
#' @export
#'
I_Chart <- function(
    date = IChart_Data$date,
  Dot = IChart_Data$Dot,
  IChart_Data = data_frame(date, Dot)) {


  #IChart_Data <- IDi %>% select(PlaceDate, date, Dot)
  #IChart_Data$place <- IChart_Data$PlaceDate
  #IChart_Data$Dot <- IChart_Data$count
  #IIChart_Data$Dot_sd <- IChart_Data$time_sd


  #===============================================================================
  # Number of cases for the limits
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


  IChart_Data$RowN <- as.numeric(row.names(IChart_Data))
  IChart_Data$Days_N <- rank(IChart_Data$RowN)
  Days_Tot <- nrow(IChart_Data)
  Date_PC <- min(IChart_Data$RowN)
  Days_PC <- 1


  #=============================================================================
  # Calculate:
  #   PhaseCount: The number of special cause phases
  #               Initialized to 1, the first phase
  #   Phase_Ch:   The measure on the day of a phase change, initialized to -99
  #               Used in the charts to show the red dots on the day a phase
  #               change occurs
  #=============================================================================

  IChart_Data <- IChart_Data %>%
    mutate(PhaseCount = 1,
      Phase_Ch = -99)

  Phases <- 1


  #=============================================================================
  # Initialize the limits to 0 and New_Phase to FALSE
  #=============================================================================

  IChart_Data <- IChart_Data %>%
    mutate(Centerline = 0,
      Upper = 0,
      Lower =0,
      SC = "")

  New_Phase <- FALSE


  i <- 1
  j <- as.integer(1)

  #HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  while (j <= Days_Tot) {

    m <- i + max(j-i, Lim_Min-1)
    m <- min(m, Days_Tot)


    if (j < Lim_Min | New_Phase) {


      IChart_Data <- IChart_Data %>%
        mutate(CL = mean(Dot[i:m]),
          MR = abs(Dot - lag(Dot)) )

      MR_UCL_2 <- 3.27*mean(IChart_Data$MR[i:m], na.rm = TRUE)
      IChart_Data <- IChart_Data %>%
        mutate(MR = ifelse(MR < MR_UCL_2, MR, NA))

      MRr_mean_2 <- mean(IChart_Data$MR[i:m], na.rm = TRUE)
      IChart_Data <- IChart_Data %>%
        mutate(UL = CL + 2.66*MRr_mean_2,
          LL = CL - 2.66*MRr_mean_2)

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
    IChart_Data <- IChart_Data %>%
      mutate(SC_a = if_else(Days_N > i & Days_N <=j & Days_N >= i+Lim_Min-1 & Dot < LL & LL > 0, 1,0),
        SC_ax = SC_a + lag(SC_a))
    IChart_Data$SC_ax[is.na(IChart_Data$SC_ax)] <- 0
    Days_Pa <- min(IChart_Data$Days_N[IChart_Data$SC_ax==2]) - 1

    # b) Look for a downward Shift
    IChart_Data <- IChart_Data %>%
      mutate(SC_b = if_else(Days_N > i &  Days_N <=j & Dot < CL, 1, 0) ,
        SC_bx = SC_b + lag(SC_b) + lag(SC_b, 2) + lag(SC_b, 3) + lag(SC_b, 4) + lag(SC_b, 5) + lag(SC_b, 6) + lag(SC_b, 7))
    IChart_Data$SC_bx[is.na(IChart_Data$SC_bx)] <- 0
    Days_Pb <- min(IChart_Data$Days_N[IChart_Data$SC_bx==8])

    # c) Look for 2 points above the Upper Limit
    IChart_Data <- IChart_Data %>%
      mutate(SC_c = if_else(Days_N > i & Days_N <=j & Days_N >= i+Lim_Min-1 & Dot > UL & UL > 0, 1,0),
        SC_cx = SC_c + lag(SC_c) )
    IChart_Data$SC_cx[is.na(IChart_Data$SC_cx)] <- 0
    Days_Pc <- min(IChart_Data$Days_N[IChart_Data$SC_cx==2]) - 1

    # d) Look for an upwards Shift
    IChart_Data <- IChart_Data %>%
      mutate(SC_d = if_else(Days_N > i &  Days_N <=j & Dot > CL, 1, 0),
        SC_dx = SC_d + lag(SC_d) + lag(SC_d, 2) + lag(SC_d, 3) + lag(SC_d, 4) + lag(SC_d, 5) + lag(SC_d, 6) + lag(SC_d, 7))
    IChart_Data$SC_dx[is.na(IChart_Data$SC_dx)] <- 0
    Days_Pd <- min(IChart_Data$Days_N[IChart_Data$SC_dx==8])

    #-------------------------------------------------------------------------------
    # If there is a new phase,  assign the limits and move to the next phase
    #-------------------------------------------------------------------------------

    New_Phase <- (is.infinite(Days_Pa)==FALSE | is.infinite(Days_Pb)==FALSE | is.infinite(Days_Pc)==FALSE | is.infinite(Days_Pd)==FALSE)

    # CHECK HERE IF THERE IS AN ISSUE WITH USING DATES RATHER THAN THE ROW NUMBER


    Days_PC <- min(Days_Pa, Days_Pb, Days_Pc, Days_Pd)
    date_PC <- min(IChart_Data$date[IChart_Data$Days_N == Days_PC + i])

    #-------------------------------------------------------------------------------
    # If there is a new phase (special cause),  assign the reason why
    #-------------------------------------------------------------------------------

    SC <- ""
    SC <- if_else(New_Phase & Days_PC == Days_Pa, "2 points below the lower limit" , SC)
    SC <- if_else(New_Phase & Days_PC == Days_Pb, "8 points below the midline" , SC)
    SC <- if_else(New_Phase & Days_PC == Days_Pc, "2 points above the upper limit" , SC)
    SC <- if_else(New_Phase & Days_PC == Days_Pd, "8 points above the midline" , SC)


    IChart_Data$SC <- if_else(Days_PC == IChart_Data$Days_N, SC, IChart_Data$SC)

    #-------------------------------------------------------------------------------

    IChart_Data <- IChart_Data %>%
      mutate(Phase_Ch = if_else(Days_PC == Days_N, Dot/1, Phase_Ch),

        Centerline = if_else(New_Phase & Days_N >= i, CL, Centerline),
        Centerline = if_else(Days_N >= i & j >= Days_Tot, CL, Centerline),

        Upper = if_else(New_Phase & Days_N >= i, UL, Upper),
        Upper = if_else(Days_N >= i & j >= Days_Tot, UL, Upper),

        Lower = if_else(New_Phase & Days_N >= i, LL, Lower),
        Lower = if_else(Days_N >= i & j >= Days_Tot, LL, Lower),

        PhaseCount = if_else(New_Phase & Days_N >= Days_PC, PhaseCount + 1, PhaseCount) )


    j <- if_else(New_Phase, Days_PC, j + 1)
    i <- if_else(New_Phase, Days_PC, i)

  }  # j Within Setting Loop


  IChart_Data$Phase_Ch <- na_if(IChart_Data$Phase_Ch, -99.0)

  #===============================================================================
  #   Store the limits in one of two sets of fields (ending a or b)
  #   This enables the limits to be plotted without the wonky join line from
  #   one phase to the next
  #===============================================================================


  IChart_Data <- IChart_Data %>%
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

  IChart_Data <- IChart_Data %>% select(date, Dot,
    MIDLINEa, UPPERa, LOWERa,
    MIDLINEb, UPPERb, LOWERb,
    Phase_Ch, PhaseCount, SC, RowN)

  IChart_Data <- IChart_Data %>%
    mutate(MIDLINEa = na_if(MIDLINEa, -99),
      MIDLINEb = na_if(MIDLINEb, -99),

      UPPERa = na_if(UPPERa, -99),
      UPPERb = na_if(UPPERb, -99),

      LOWERa = na_if(LOWERa, -99),
      LOWERb = na_if(LOWERb, -99),

      Dot = na_if(Dot, -9),
      Phase_Ch = na_if(Phase_Ch, -99.0))

  return(IChart_Data)

} #I Chart Function

