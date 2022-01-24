#=============================================================================
# U_Prime_Funnel Function:
#   Send a data frame containing:
#     Place:  The setting from where the events are reported
#     n:    The numerator for the event rate
#     N:    The denominator for the event rate
#=============================================================================

#' Title
#'
#' @param Uchart_Data
#' @param to_remove
#'
#' @return
#' @export
#'

U_Prime_Funnel <- function(Uchart_Data,
                           to_remove = c("MR", "Sigma_i", "Zi", "Screened_MR"))
  {


  Uchart_Data <- Uchart_Data[order(Uchart_Data$N), ]
  Uchart_Data$rate <- Uchart_Data$n/Uchart_Data$N

  Uchart_Data$MIDLINE <- sum(Uchart_Data$n)/sum(Uchart_Data$N)

  Uchart_Data$Sigma_i <- sqrt(Uchart_Data$MIDLINE/Uchart_Data$N)
  Uchart_Data$Zi <- (Uchart_Data$rate - Uchart_Data$MIDLINE)/Uchart_Data$Sigma_i

  Uchart_Data$MR <- abs( Uchart_Data$Zi - lag(Uchart_Data$Zi) )


  MR_UCL_2 <- 3.27*mean(Uchart_Data$MR, na.rm = TRUE)

  Uchart_Data$Screened_MR <- if_else(Uchart_Data$MR <= MR_UCL_2, 1, 0)
  SigmaZ <- ( mean(Uchart_Data$MR, na.rm = TRUE) ) / 1.128


  Uchart_Data$LOWER <- Uchart_Data$MIDLINE - 3*SigmaZ*sqrt(Uchart_Data$MIDLINE/Uchart_Data$N)
  Uchart_Data$UPPER <- Uchart_Data$MIDLINE + 3*SigmaZ*sqrt(Uchart_Data$MIDLINE/Uchart_Data$N)


    #===============================================================================
    #   Tidy the data, keeping only those values we need
    #===============================================================================

    Uchart_Data <- Uchart_Data %>% select(-any_of(to_remove))


  return(Uchart_Data)

}

