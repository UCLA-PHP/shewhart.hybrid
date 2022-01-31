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
postprocess_u_chart = function(data, digits = 2, events = "close contacts", trials = "primary cases", proportion = "Rate")
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

      label = paste0(
        "# ", events, ": ", n,
        "\n# ", trials, ": ", N,
        "\n", proportion, ": ",round(`Observed %`*100, digits), "%",
        "\nIn Phase #", EPOCH, ": Midline = ", round(Midline*100, digits), "%",
        if_else(phase_change, "\nDetected new phase due to ", ""),
        SC)
    )
}
