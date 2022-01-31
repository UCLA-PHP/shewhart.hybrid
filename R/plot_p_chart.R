#' Title
#'
#' @param data
#' @param title
#' @param mode1
#' @param sizes
#' @param yname
#' @param xname
#'
#' @return
#' @export
#'
#'
#' @importFrom dplyr filter group_by
#' @importFrom plotly plot_ly add_lines layout
plot_p_chart = function(data, title = "", mode1 = "lines", sizes = c(1,100),
                        yname = 'Positivity rate (%)', xname = NA)
{

  plotly::plot_ly(
    data,
    # data = data %>% dplyr::group_by(EPOCH),
    x = ~date,
    y = ~`Observed %` * 100,
    name = "Observed %",
    type = "scatter",
    # color = ~factor(EPOCH),
    mode = "lines+markers",
    text = ~label,
    size = ~N,
    sizes = sizes,
    marker = list(
      # opacity = 0.7,
      sizemode = "area"
    )

  ) %>%
    plotly::add_trace(
      mode = mode1,
      data = data %>% dplyr::group_by(EPOCH),
      name = "Midline",
      y = ~Midline * 100,
      size = NULL,
      text = NULL,
      marker = NULL,
      color = I("red"),
      line = list(width = 1)
    ) %>%
    plotly::add_trace(
      mode = mode1,
      size = NULL,
      text = NULL,
      marker = NULL,
      y = ~`Lower Limit` * 100,
      name = "Lower Limit",
      color = I("gray"),
      line = list(width = 2)

    ) %>%
    plotly::add_trace(
      mode = mode1,
      size = NULL,
      text = NULL,
      marker = NULL,
      y = ~`Upper Limit` * 100,
      name = "Upper Limit",
      color = I("gray"),
      line = list(width = 2)
    ) %>%
    plotly::add_markers(
      # data %>% dplyr::filter(phase_change) %>% dplyr::group_by(EPOCH),
      name = "Start of Phase",
      color = I("orange"),
      x = ~ date,
      # y = ~`Observed %` * 100
      y = ~Phase_Ch *100,
      size = ~N,
      sizes = sizes,
      marker = list(
        # opacity = 0.7,
        sizemode = "area"
      )
    ) %>%
    plotly::layout(
      title = title,
      yaxis = list(title = yname,
                   rangemode = "tozero"),
      xaxis = list(title = xname),
      legend = list(orientation = 'h')
    )
}
