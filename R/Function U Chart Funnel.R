#' U_Chart_Funnel Function
#'
#' @param Uchart_Data Send a data frame containing:
#'     n:      The numerator for the event rate
#'     N:      The denominator for the event rate
#'
#' @return a data frame...
#' @export
#'
U_Chart_Funnel <- function(Uchart_Data) {

    Uchart_Data <- Uchart_Data[order(Uchart_Data$N), ]
    Uchart_Data$rate <- Uchart_Data$n/Uchart_Data$N
    Uchart_Data$MIDLINE <- sum(Uchart_Data$n)/sum(Uchart_Data$N)
    Uchart_Data$LOWER <- pmax(0, Uchart_Data$MIDLINE - 3*sqrt(Uchart_Data$MIDLINE/Uchart_Data$N))
    Uchart_Data$UPPER <- pmax(0, Uchart_Data$MIDLINE + 3*sqrt(Uchart_Data$MIDLINE/Uchart_Data$N))

  return(Uchart_Data)

}

