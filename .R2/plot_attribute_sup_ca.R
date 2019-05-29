plot_attribute_sup_ca <- function(res, axes = c(1, 2), main = "", lab.size = 4) {
  library(tidyverse)
  library(ggrepel)
  dims <- res %>%
    `[[`("eig") %>%
    as.data.frame() %>%
    select(2) %>%
    pull() %>%
    round(2) %>%
    paste0("Dim ", 1:length(.), " (", ., "%)")

  df_sup <- res %>%
    extract2(c("quanti.sup", "coord")) %>%
    `colnames<-`(paste0("Dim", 1:ncol(.))) %>%
    as_tibble(rownames = "Label") %>%
    select(-Label, Label)

  res_plot <- df_sup %>%
    ggplot(aes_string(x = names(df_sup)[axes[1]], y = names(df_sup)[axes[2]])) +
    geom_text_repel(aes(label = Label), size = lab.size) +
    geom_segment(aes_string(x = 0, y = 0, xend = names(df_sup)[axes[1]], yend = names(df_sup)[axes[2]]),
      arrow = arrow(length = unit(0.3, "cm")), col = "blue"
    ) +
    annotate("path",
      linetype = 2,
      col = "grey40",
      x = 0 + 1 * cos(seq(0, 2 * pi, length.out = 100)),
      y = 0 + 1 * sin(seq(0, 2 * pi, length.out = 100))
    ) +
    geom_vline(xintercept = 0, lty = 2, col = "grey40") +
    geom_hline(yintercept = 0, lty = 2, col = "grey40") +
    labs(
      x = dims[axes[1]],
      y = dims[axes[2]],
      title = main
    ) +
    coord_fixed(ratio = 1) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank()
    )
  return(res_plot)
}
