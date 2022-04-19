#' Title
#'
#' @param data
#' @param title
#' @param mode1
#' @param yname
#' @param xname
#' @param legend.y
#' @param multiplier
#' @param point_name
#' @param hoverinfo
#' @param ylab.position
#' @param limit_width
#' @param midline_width
#' @param marker_size
#' @param marker_size_range
#' @param sizemode
#' @param line_width
#' @param download_format
#'
#' @return
#' @export
#'
#' @author Doug Ezra Morrison <demorrison@ucdavis.edu>
#' @importFrom dplyr filter group_by
#' @importFrom plotly plot_ly add_lines layout
plot_run_chart = function(
  data,
  title = "",
  mode1 = "lines",
  yname = 'Positivity rate (%)',
  xname = NA,
  legend.y = -0.2,
  multiplier = 100,
  marker_size = ~ N,
  marker_size_range = c(1,100),
  sizemode = "area",
  point_name = "Observed %",
  hoverinfo = "x+text",
  ylab.position = 0,
  line_width = 2,
  limit_width = line_width,
  midline_width = 1,
  download_format = "svg")
{

  data =
    data |>
    dplyr::mutate(
      label_high = glue::glue("Upper control limit: {(`Upper Limit` * multiplier) |> formatC(digits = 2, format = 'f')}"),
      label_low = glue::glue("Lower control limit: {(`Lower Limit` * multiplier) |> formatC(digits = 2, format = 'f')}"),
      label_overall = glue::glue("Midline: {(Midline * multiplier) |> formatC(digits = 2, format = 'f')}")
    )

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
    size = marker_size,
    sizes = marker_size_range,
    line = list(width = line_width),
    marker = list(
      # opacity = 0.7,
      sizemode = sizemode
    )

  ) %>%
    plotly::add_trace(
      data = data %>% dplyr::group_by(EPOCH),
      mode = mode1,
      size = NULL,
      text = ~label_low,
      marker = NULL,
      y = ~`Lower Limit` * multiplier,
      name = "Lower Limit",
      color = I("gray"),
      line = list(width = limit_width)

    ) %>%
    plotly::add_trace(
      mode = mode1,
      size = NULL,
      text = ~label_high,
      marker = NULL,
      y = ~ `Upper Limit` * multiplier,
      name = "Upper Limit",
      color = I("gray"),
      line = list(width = limit_width)
    ) %>%
    plotly::add_markers(
      # data %>% dplyr::filter(phase_change) %>% dplyr::group_by(EPOCH),
      name = "Start of Phase",
      color = I("orange"),
      x = ~ date,
      text =  ~label,
      y = ~ Phase_Ch * multiplier,
      size = marker_size,
      sizes = marker_size_range,
      line = NULL,
      marker = list(
        # opacity = 0.7,
        sizemode = sizemode
      )
    ) %>%
    plotly::add_trace(
      mode = mode1,
      name = "Midline",
      y = ~ Midline * multiplier,
      size = NULL,
      text = ~label_overall,
      marker = NULL,
      color = I("red"),
      line = list(width = midline_width)
    ) %>%

    plotly::layout(
      title = title,
      yaxis = list(title = yname,
        position = ylab.position,
        rangemode = "tozero"),
      xaxis = list(title = xname),
      legend = list(orientation = 'h',  y = legend.y)
    ) %>%
    plotly::config(
      toImageButtonOptions = list(
        format = download_format,
        filename = "hybrid-shewhart-chart"
      )
    )
}
