#=============================================================================
# P_Chart_Funnel Function:
#   Send a data frame containing:
#     Place:  The setting from where the events are reported
#     n:      The numerator for the event proportion
#     N:      The denominator for the event proportion
#=============================================================================

P_Chart_Funnel <- function(Pchart_Data) {
  
    Pchart_Data <- Pchart_Data[order(Pchart_Data$N), ]
    Pchart_Data$Dot <- Pchart_Data$n/Pchart_Data$N 
    Pchart_Data$MIDLINE <- sum(Pchart_Data$n)/sum(Pchart_Data$N)
    Pchart_Data$LOWER <- Pchart_Data$MIDLINE - 3*sqrt(Pchart_Data$MIDLINE*(1-Pchart_Data$MIDLINE)/Pchart_Data$N)
    Pchart_Data$UPPER <- Pchart_Data$MIDLINE + 3*sqrt(Pchart_Data$MIDLINE*(1-Pchart_Data$MIDLINE)/Pchart_Data$N)
    

  return(Pchart_Data)
  
}

