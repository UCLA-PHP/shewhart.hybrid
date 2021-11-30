#=============================================================================
# P_Chart_Funnel Function:
#   Send a data frame containing:
#     n:      The numerator for the event proportion
#     N:      The denominator for the event proportion
#=============================================================================

#' Title
#'
#' @param Pchart_Data
#'
#' @return
#' @export
#'

P_Chart_Funnel <- function(Pchart_Data) {

    Pchart_Data <- Pchart_Data[order(Pchart_Data$N), ]
    Pchart_Data$Dot <- Pchart_Data$n/Pchart_Data$N
    Pchart_Data$MIDLINE <- sum(Pchart_Data$n)/sum(Pchart_Data$N)
    Pchart_Data$LOWER <- pmax(0, Pchart_Data$MIDLINE - 3*sqrt(Pchart_Data$MIDLINE*(1-Pchart_Data$MIDLINE)/Pchart_Data$N))
    Pchart_Data$UPPER <- pmin(1, Pchart_Data$MIDLINE + 3*sqrt(Pchart_Data$MIDLINE*(1-Pchart_Data$MIDLINE)/Pchart_Data$N))


  return(Pchart_Data)

}

