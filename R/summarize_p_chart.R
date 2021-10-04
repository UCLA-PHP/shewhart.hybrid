#' Title
#'
#' @param data
#' @param reference_date
#'
#' @return
#' @export
#'

#' @importFrom dplyr group_by across all_of summarise last mutate filter first if_else rename
#' @importFrom tidyr replace_na
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
    dplyr::group_by(dplyr::across(dplyr::all_of(place_vars)), EPOCH) %>%
    dplyr::summarise(
      MIDLINEa = dplyr::last(MIDLINEa),
      MIDLINEb = dplyr::last(MIDLINEb),
      date = min(date) )


  Summ_Tab2 <- Summ_Tab %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(place_vars))) %>%
    dplyr::mutate(EPOCH_max = max(EPOCH)) %>%
    dplyr::filter(EPOCH == EPOCH_max | EPOCH == EPOCH_max - 1)

  Summ_Tab2 %<>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), tidyr::replace_na, replace = 0)
    )

  Summ_Tab2$MIDLINE <- Summ_Tab2$MIDLINEa + Summ_Tab2$MIDLINEb

  Summ_Tab3 <- Summ_Tab2 %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(place_vars))) %>%
    dplyr::summarise(
      MIDLINE_0 = dplyr::first(MIDLINE),
      date_0 = dplyr::first(date),
      MIDLINE_1 = dplyr::last(MIDLINE),
      date_1 = dplyr::last(date)
    )


  Summ_Tab3$datex <- reference_date
  Summ_Tab3$days <- Summ_Tab3$datex - Summ_Tab3$date_1


  Summ_Tab3$Feedback <- "Blank"
  Summ_Tab3$Feedback <- dplyr::if_else(Summ_Tab3$days <= 7 & Summ_Tab3$MIDLINE_1 > Summ_Tab3$MIDLINE_0,
                                       paste(sep = "", "RED: Increased to ", 100*round(Summ_Tab3$MIDLINE_1, 2), "%, ", Summ_Tab3$days, " days ago" ),
                                       Summ_Tab3$Feedback)

  Summ_Tab3$Feedback <- dplyr::if_else(Summ_Tab3$days > 7 & Summ_Tab3$MIDLINE_1 > Summ_Tab3$MIDLINE_0,
                                       paste(sep = "", "GRAY: Has been at ", 100*round(Summ_Tab3$MIDLINE_1, 2), "% for ", Summ_Tab3$days, " days" ),
                                       Summ_Tab3$Feedback)

  Summ_Tab3$Feedback <- dplyr::if_else(Summ_Tab3$days <= 7 & Summ_Tab3$MIDLINE_1 <= Summ_Tab3$MIDLINE_0,
                                       paste(sep = "", "GREEN: Decreased to ", 100*round(Summ_Tab3$MIDLINE_1, 2), "%, ", Summ_Tab3$days, " days ago" ),
                                       Summ_Tab3$Feedback)

  Summ_Tab3$Feedback <- dplyr::if_else(Summ_Tab3$days > 7 & Summ_Tab3$MIDLINE_1 <= Summ_Tab3$MIDLINE_0,
                                       paste(sep = "", "GRAY: Has been at ", 100*round(Summ_Tab3$MIDLINE_1, 2), "% for ", Summ_Tab3$days, " days" ),
                                       Summ_Tab3$Feedback)


  Summ_Tab3$Feedback <- paste(sep = "", "Since ", Summ_Tab3$date_1, ", ",
                              # Summ_Tab3$place,
                              "the test positivity rate has been varying around a mid line of ", 100*round(Summ_Tab3$MIDLINE_1, 3), "%" )

  Summ_Tab3 %<>%
    dplyr::select(-datex) %>%
    dplyr::rename(
      `Previous midline` = MIDLINE_0,
      `Current midline` = MIDLINE_1,
      `Start of prev. epoch` = date_0,
      `Start of cur. epoch` = date_1,
      `Days since cur. epoch start` = days
    )

  return(Summ_Tab3)

}
