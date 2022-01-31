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
postprocess_u_chart = function(data, digits = 2,
                               events = "close contact",
                               trials = "primary case",
                               proportion = "Observed rate",
                               multiplier = 1,
                               suffix = paste0(" ", events, "s per ", trials))
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
        "# ", events, "s: ", n,
        "\n# ", trials, "s: ", N,
        "\n", proportion, ": ",round(`Observed %`*multiplier, digits), suffix,
        "\nIn Phase #", EPOCH, ": Midline = ", round(Midline*multiplier, digits), suffix,
        if_else(phase_change, "\nDetected new phase due to ", ""),
        SC)
    )
}
