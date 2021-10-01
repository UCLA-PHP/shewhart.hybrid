#' Title
#'
#' @param chart_data
#'
#' @return
#' @export
#'
#'
#' @importFrom dplyr filter group_by
#' @importFrom plotly plot_ly add_lines layout
plot_p_chart = function(data)
{

  plotly::plot_ly(
    data = data %>% dplyr::group_by(EPOCH),
    x = ~date,
    y = ~`Observed %` * 100,
    name = "Observed %",
    type = "scatter",
    # color = ~factor(is.na(Phase_Ch)),
    mode = "lines+markers"
  ) %>%
    plotly::add_lines(
      name = "Midline",
      y = ~Midline * 100,
      color = I("red"),
      line = list(width = 1)
    ) %>%
    plotly::add_lines(
      y = ~`Lower Limit` * 100,
      name = "Lower Limit",
      color = I("gray"),
      line = list(width = 2)

    ) %>%
    plotly::add_lines(
      y = ~`Upper Limit` * 100,
      name = "Upper Limit",
      color = I("gray"),
      line = list(width = 2)
    ) %>%
    plotly::layout(
      yaxis = list(title = 'Positivity rate (%)'),
      xaxis = list(title = NA),
      legend = list(orientation = 'h')
    )
}
