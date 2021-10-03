#' Title
#'
#' @param data
#' @param reference_date
#'
#' @return
#' @export
#'

summarize_p_chart = function(
  data,
  place_vars = "place",
  reference_date = max(data$date))
{

  ################################################################################
  #
  # Some very draft code to create the summary data table
  # This needs work to correct and tidy up
  #
  ################################################################################

  Summ_Tab <- data %>%
    group_by(across(all_of(place_vars)), EPOCH) %>%
    summarise(
      MIDLINEa = last(MIDLINEa),
      MIDLINEb = last(MIDLINEb),
      date = min(date) )


  Summ_Tab2 <- Summ_Tab %>%
    group_by(across(all_of(place_vars))) %>%
    mutate(EPOCH_max = max(EPOCH)) %>%
    filter(EPOCH == EPOCH_max | EPOCH == EPOCH_max - 1)


  Summ_Tab2[is.na(Summ_Tab2)] <- 0
  Summ_Tab2$MIDLINE <- Summ_Tab2$MIDLINEa + Summ_Tab2$MIDLINEb

  Summ_Tab3 <- Summ_Tab2 %>%
    group_by(across(all_of(place_vars))) %>%
    summarise(MIDLINE_0 = first(MIDLINE), date_0 = first(date), MIDLINE_1 = last(MIDLINE), date_1 = last(date) )


  Summ_Tab3$datex <- reference_date
  Summ_Tab3$days <- Summ_Tab3$datex - Summ_Tab3$date_1


  Summ_Tab3$Feedback <- "Blank"
  Summ_Tab3$Feedback <- if_else(Summ_Tab3$days <= 7 & Summ_Tab3$MIDLINE_1 > Summ_Tab3$MIDLINE_0,
                                paste(sep = "", "RED: Increased to ", round(100*Summ_Tab3$MIDLINE_1, 2), "%, ", Summ_Tab3$days, " days ago" ),
                                Summ_Tab3$Feedback)

  Summ_Tab3$Feedback <- if_else(Summ_Tab3$days > 7 & Summ_Tab3$MIDLINE_1 > Summ_Tab3$MIDLINE_0,
                                paste(sep = "", "GRAY: Has been at ", round(100*Summ_Tab3$MIDLINE_1, 2), "% for ", Summ_Tab3$days, " days" ),
                                Summ_Tab3$Feedback)

  Summ_Tab3$Feedback <- if_else(Summ_Tab3$days <= 7 & Summ_Tab3$MIDLINE_1 <= Summ_Tab3$MIDLINE_0,
                                paste(sep = "", "GREEN: Decreased to ", round(100*Summ_Tab3$MIDLINE_1, 2), "%, ", Summ_Tab3$days, " days ago" ),
                                Summ_Tab3$Feedback)

  Summ_Tab3$Feedback <- if_else(Summ_Tab3$days > 7 & Summ_Tab3$MIDLINE_1 <= Summ_Tab3$MIDLINE_0,
                                paste(sep = "", "GRAY: Has been at ", round(100*Summ_Tab3$MIDLINE_1, 2), "% for ", Summ_Tab3$days, " days" ),
                                Summ_Tab3$Feedback)


  Summ_Tab3$Feedback <- paste(sep = "", "Since ", Summ_Tab3$date_1, ", ",
                              # Summ_Tab3$place,
                              "percent positive cases has been varying around a mid line of ", round(Summ_Tab3$MIDLINE_1, 3) )

  return(Summ_Tab3)

}
