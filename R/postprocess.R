#' Postprocess chart data
#'
#' @param data
#'
#' @return
#' @export
#' @importFrom dplyr mutate rename if_else
#'
postprocess = function(data, digits = 2)
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
      `Upper Limit` =
        dplyr::if_else(is.na(MIDLINEa), UPPERb, UPPERa),
      `Lower Limit` =
        dplyr::if_else(is.na(MIDLINEa), LOWERb, LOWERa),
      `Lower Limit` =
        dplyr::if_else(`Lower Limit` < 0, 0, `Lower Limit`),

      label = paste0(
        "# positive tests: ", n,
        "\n# tests: ", N,
        "\nPositivity rate: ",round(`Observed %`*100, digits), "%",
        "\nIn Epoch #", EPOCH, ": Midline = ", round(Midline*100, digits), "%",
        if_else(phase_change, "\nDetected new epoch due to ", ""),
        SC)
    )
}
