#' Radar Plot
#'
#' Create radar plot of sensory attributes for each product
#'
#' @param .data an aggregated dataset containing columns of product and sensory attributes
#' @param scale_min minimum value of the scales
#' @param scale_max maximum value of the scales
#' @param point_size point size
#' @param line_width line width
#' @param title title of the plot
#' @param subtitle subtitle of the plot
#' @param caption caption of the plot
#'
#' @import dplyr ggplot2 ggradar
#' @importFrom scales rescale
#'
#' @return a radar plot
#'
#' @export

plot_radar <- function(.data, scale_min = 1, scale_max = 10, point_size = 4, line_width = 1, title = "", subtitle = "", caption = "") {
  .data %>%
    mutate_if(is.numeric, ~ scales::rescale(.x, to = c(scale_min, scale_max), from = c(scale_min, scale_max))) %>%
    ggradar(
      grid.label.size = 5,
      grid.min = scale_min,
      grid.mid = (scale_min + scale_max) / 2,
      grid.max = scale_max,
      values.radar = c(scale_min, (scale_min + scale_max) / 2, scale_max),
      group.point.size = point_size,
      group.line.width = line_width
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    theme(
      legend.position = "right"
    )
}
