plot_eigen <- function(res, threshold = TRUE) {
  res_plot <- res %>%
    `[[`("eig") %>%
    as_tibble() %>%
    mutate(
      dimension = paste("Dim", 1:nrow(.)),
      dimension = factor(dimension, unique(dimension)),
      `percentage of variance` = round(`percentage of variance`, 2)
    ) %>%
    # filter(row_number(dimension) <= 5)
    ggplot(aes(x = dimension, y = eigenvalue)) +
    geom_bar(stat = "identity") +
    # geom_text(aes(label = `percentage of variance`), vjust = -1, size = 3) +
    labs(
      x = "Dimensi",
      y = "Eigenvalue",
      title = "Screeplot"
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  if (threshold == TRUE) {
    res_plot <- res_plot +
      geom_hline(yintercept = 1, lty = 2, col = "grey40")
  } else {
    res_plot
  }
  return(res_plot)
}
