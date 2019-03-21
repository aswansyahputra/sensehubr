#' Plot JAR
#'
#' @param res output of JAR analysis
#' @param frequency_threshold threshold for frequency
#' @param drop_threshold threshold for liking drop
#' @param title title
#' @param xlab x label
#' @param ylab y label
#'
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#'
#' @return ggplot2 object
#' @export

plot_jar <- function(res, frequency_threshold = 20, drop_threshold = 1, title = "Penalty analysis", xlab = "Frequency (%)", ylab = "Liking drop") {
  res %>%
    ggplot(aes(x = frequency, y = penalty, colour = class)) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = attribute), show.legend = FALSE) +
    scale_color_manual(values = c("blue", "red")) +
    geom_vline(xintercept = frequency_threshold, lty = 2, colour = "grey30") +
    geom_hline(yintercept = drop_threshold, lty = 2, colour = "grey30") +
    labs(
      title = title,
      x = xlab,
      y = ylab,
      colour = NULL
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank()
    )
}
