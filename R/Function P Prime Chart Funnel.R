#=============================================================================
# P_Prime_Funnel Function:
#   Send a data frame containing:
#     date: Date formatted field containing the date on which events are reported
#     n:    The numerator for the event proportion
#     N:    The denominator for the event proportion
#=============================================================================

P_Prime_Funnel <- function(Pchart_Data) {
  
  
  Pchart_Data <- Pchart_Data[order(Pchart_Data$N), ]
  Pchart_Data$Dot <- Pchart_Data$n/Pchart_Data$N

  Pchart_Data$MIDLINE <- sum(Pchart_Data$n)/sum(Pchart_Data$N)
  
  Pchart_Data$Sigma_i <- sqrt(Pchart_Data$MIDLINE*(1-Pchart_Data$MIDLINE)/Pchart_Data$N)
  Pchart_Data$Zi <- (Pchart_Data$Dot - Pchart_Data$MIDLINE)/Pchart_Data$Sigma_i
  
  Pchart_Data$MR <- abs( Pchart_Data$Zi - lag(Pchart_Data$Zi) )
  
  
  MR_UCL_2 <- 3.27*mean(Pchart_Data$MR[i:m], na.rm = TRUE)
  
  Pchart_Data$Screened_MR <- if_else(Pchart_Data$MR <= MR_UCL_2, 1, 0)
  SigmaZ <- ( sum(Pchart_Data$MR[i:m], na.rm = TRUE) ) / ( 1.128*sum(Pchart_Data$Screened_MR[i:m], na.rm = TRUE) )
  
  
  Pchart_Data$LOWER <- Pchart_Data$MIDLINE - 3*SigmaZ*sqrt(Pchart_Data$MIDLINE*(1-Pchart_Data$MIDLINE)/Pchart_Data$N)
  Pchart_Data$UPPER <- Pchart_Data$MIDLINE + 3*SigmaZ*sqrt(Pchart_Data$MIDLINE*(1-Pchart_Data$MIDLINE)/Pchart_Data$N)
  
  

    #===============================================================================
    #   Tidy the data, keeping only those values we need 
    #===============================================================================
    
    Pchart_Data <- Pchart_Data %>% select(place, n, N, Dot, 
                                MIDLINE, UPPER, LOWER)  
    

  return(Pchart_Data)
  
}

