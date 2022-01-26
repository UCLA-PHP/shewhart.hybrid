#' Postprocess p and p-prime chart data
#'
#' @param data
#'
#' @return
#' @export
#' @importFrom dplyr mutate rename if_else
#'
postprocess_p_chart = function(data, digits = 2, events = "positive tests", trials = "tests", proportion = "Positivity rate")
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

      label = paste0(
        "# ", events, ": ", n,
        "\n# ", trials, ": ", N,
        "\n", proportion, ": ",round(`Observed %`*100, digits), "%",
        "\nIn Phase #", EPOCH, ": Midline = ", round(Midline*100, digits), "%",
        if_else(phase_change, "\nDetected new phase due to ", ""),
        SC)
    )
}
