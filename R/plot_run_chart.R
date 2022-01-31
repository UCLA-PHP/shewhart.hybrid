#' Title
#'
#' @param data
#' @param title
#' @param mode1
#' @param sizes
#' @param yname
#' @param xname
#' @param legend.y
#' @param multiplier
#' @param point_name
#' @param hoverinfo
#'
#' @return
#' @export
#'
#'
#' @importFrom dplyr filter group_by
#' @importFrom plotly plot_ly add_lines layout
plot_run_chart = function(
  data,
  title = "",
  mode1 = "lines",
  sizes = c(1,100),
  yname = 'Positivity rate (%)',
  xname = NA,
  legend.y = 0,
  multiplier = 100,
  point_name = "Observed %",
  hoverinfo = "x+text",
  ylab.position = 0)
{

  plotly::plot_ly(
    data,
    # data = data %>% dplyr::group_by(EPOCH),
    x = ~date,
    y = ~`Observed %` * multiplier,
    name = point_name,
    type = "scatter",
    # color = ~factor(EPOCH),
    mode = "lines+markers",
    hoverinfo = hoverinfo,
    text = ~label,
    size = ~N,
    sizes = sizes,
    marker = list(
      # opacity = 0.7,
      sizemode = "area"
    )

  ) %>%
    plotly::add_trace(
      data = data %>% dplyr::group_by(EPOCH),
      mode = mode1,
      size = NULL,
      text = NULL,
      marker = NULL,
      y = ~`Lower Limit` * multiplier,
      name = "Lower Limit",
      color = I("gray"),
      line = list(width = 2)

    ) %>%
    plotly::add_trace(
      mode = mode1,
      size = NULL,
      text = NULL,
      marker = NULL,
      y = ~ `Upper Limit` * multiplier,
      name = "Upper Limit",
      color = I("gray"),
      line = list(width = 2)
    ) %>%
    plotly::add_markers(
      # data %>% dplyr::filter(phase_change) %>% dplyr::group_by(EPOCH),
      name = "Start of Phase",
      color = I("orange"),
      x = ~ date,
      y = ~ Phase_Ch * multiplier,
      size = ~ N,
      sizes = sizes,
      marker = list(
        # opacity = 0.7,
        sizemode = "area"
      )
    ) %>%
    plotly::add_trace(
      mode = mode1,
      name = "Midline",
      y = ~ Midline * multiplier,
      size = NULL,
      text = NULL,
      marker = NULL,
      color = I("red"),
      line = list(width = 1)
    ) %>%

    plotly::layout(
      title = title,
      yaxis = list(title = yname,
                   position = ylab.position,
                   rangemode = "tozero"),
      xaxis = list(title = xname),
      legend = list(orientation = 'h',  y = legend.y)
    )
}
