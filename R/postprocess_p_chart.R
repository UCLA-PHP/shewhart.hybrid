#' Postprocess p and p-prime chart data
#'
#' @param data
#'
#' @return
#' @export
#' @importFrom dplyr mutate rename if_else
#' @importFrom glue glue
#'
postprocess_p_chart = function(
  data,
  digits = 2,
  date_label = "",
  events = "positive tests",
  trials = "tests",
  proportion = "Positivity rate",
  multiplier = 100)
{
  data |>
  dplyr::rename(
    `Observed %` = new_events,
    EPOCH = PhaseCount
  ) |>
    dplyr::mutate(
      phase_change = !is.na(Phase_Ch),
      Midline =
        dplyr::if_else(
          is.na(MIDLINEa), MIDLINEb, MIDLINEa),
      `Upper Limit` = dplyr::if_else(is.na(MIDLINEa), UPPERb, UPPERa),
      `Upper Limit` = pmin(1, `Upper Limit`),
      `Lower Limit` =
        dplyr::if_else(is.na(MIDLINEa), LOWERb, LOWERa),
      `Lower Limit` = pmax(0, `Lower Limit`),

      label = glue::glue(
        "{date_label}{weekdays(date)}, {date}",
        "\n# ", events, ": ", n |> format(big.mark = ","),
        "\n# ", trials, ": ", N |> format(big.mark = ","),
        "\n", proportion, ": ",round(`Observed %`*multiplier, digits), "%",
        "\nIn Phase #", EPOCH, ": Midline = ", round(Midline*multiplier, digits), "%",
        if_else(phase_change, "\nDetected new phase due to ", ""),
        SC)
    )
}
