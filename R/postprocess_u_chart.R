#' Postprocess u and u-prime chart data
#'
#' @param data
#' @param digits
#' @param events
#' @param trials
#' @param proportion
#'
#' @return
#' @export
#' @importFrom dplyr mutate rename if_else
#'
postprocess_u_chart = function(data,
  digits = 2,
  date_label = "{date} ({weekdays(date)})\n",
  events = "close contacts",
  trials = "primary case",
  proportion = "Observed rate",
  multiplier = 1,
  suffix = paste0(" ", events, " per ", trials))
{
  data |>
    dplyr::rename(
      `Observed %` = Dot,
      EPOCH = PhaseCount
    ) |>
    dplyr::mutate(
      phase_change = !is.na(Phase_Ch),
      Midline = dplyr::if_else(is.na(MIDLINEa), MIDLINEb, MIDLINEa),
      `Upper Limit` = dplyr::if_else(is.na(MIDLINEa), UPPERb, UPPERa),
      # `Upper Limit` = pmin(1, `Upper Limit`),
      `Lower Limit` = dplyr::if_else(is.na(MIDLINEa), LOWERb, LOWERa),
      `Lower Limit` = pmax(0, `Lower Limit`),

      label = glue::glue(
        date_label,
        "# {events}: {n |> format(big.mark = ",")}",
        "\n# {trials}: {N |> format(big.mark = ",")}",
        "\n{proportion}: {round(`Observed %`*multiplier, digits)}{suffix}",
        "\nIn Phase #{EPOCH}: Midline = {round(Midline*multiplier, digits)}{suffix}",
        "{if_else(phase_change, '\nDetected new phase due to ', '')}{SC}"
      )

    )
}
