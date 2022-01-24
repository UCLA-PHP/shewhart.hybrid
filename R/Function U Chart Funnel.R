#=============================================================================
# U_Chart_Funnel Function:
#   Send a data frame containing:
#     Place:  The setting from where the events are reported
#     n:      The numerator for the event rate
#     N:      The denominator for the event rate
#=============================================================================

#' Title
#'
#' @param Uchart_Data
#'
#' @return
#' @export
#'
U_Chart_Funnel <- function(Uchart_Data) {

    Uchart_Data <- Uchart_Data[order(Uchart_Data$N), ]
    Uchart_Data$Dot <- Uchart_Data$n/Uchart_Data$N
    Uchart_Data$MIDLINE <- sum(Uchart_Data$n)/sum(Uchart_Data$N)
    Uchart_Data$LOWER <- Uchart_Data$MIDLINE - 3*sqrt(Uchart_Data$MIDLINE/Uchart_Data$N)
    Uchart_Data$UPPER <- Uchart_Data$MIDLINE + 3*sqrt(Uchart_Data$MIDLINE/Uchart_Data$N)



    #===============================================================================
    #   Tidy the data, keeping only those values we need
    #===============================================================================


    Uchart_Data <- Uchart_Data %>% select(place, n, N, Dot,
                                          MIDLINE, UPPER, LOWER)


  return(Uchart_Data)

}

