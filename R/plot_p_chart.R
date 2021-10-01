#' Title
#'
#' @param data
#' @param title
#'
#' @return
#' @export
#'
#'
#' @importFrom dplyr filter group_by
#' @importFrom plotly plot_ly add_lines layout
plot_p_chart = function(data, title = "", mode1 = "lines")
{

  plotly::plot_ly(
    data,
    # data = data %>% dplyr::group_by(EPOCH),
    x = ~date,
    y = ~`Observed %` * 100,
    name = "Observed %",
    type = "scatter",
    # color = ~factor(EPOCH),
    mode = "lines+markers"
  ) %>%
    plotly::add_trace(
      mode = mode1,
      data = data %>% dplyr::group_by(EPOCH),
      name = "Midline",
      y = ~Midline * 100,
      color = I("red"),
      line = list(width = 1)
    ) %>%
    plotly::add_trace(
      mode = mode1,
      y = ~`Lower Limit` * 100,
      name = "Lower Limit",
      color = I("gray"),
      line = list(width = 2)

    ) %>%
    plotly::add_trace(
      mode = mode1,
      y = ~`Upper Limit` * 100,
      name = "Upper Limit",
      color = I("gray"),
      line = list(width = 2)
    ) %>%
    plotly::add_markers(
      # data %>% dplyr::filter(phase_change) %>% dplyr::group_by(EPOCH),
      name = "Start of Epoch",
      color = I("orange"),
      x = ~ date,
      # y = ~`Observed %` * 100
      y = ~Phase_Ch *100
    ) %>%
    plotly::layout(
      title = title,
      yaxis = list(title = 'Positivity rate (%)',
                   rangemode = "tozero"),
      xaxis = list(title = NA),
      legend = list(orientation = 'h')
    )
}
