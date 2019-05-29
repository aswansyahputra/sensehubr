plot_consensus <- function(res, axes = c(1, 2), main = "", lab.size = 4) {
  library(SensoMineR)
  library(tidyverse)
  library(ggrepel)
  dims <- res %>%
    `[[`("eig") %>%
    as.data.frame() %>%
    select(2) %>%
    pull() %>%
    round(2) %>%
    paste0("Dim ", 1:length(.), " (", ., "%)")
  res_consensus <- ConsensualWords(res, graph = FALSE)
  df_main <- res %>%
    `[[`(c("ind", "coord")) %>%
    `colnames<-`(paste0("Dim", 1:ncol(.))) %>%
    as_tibble(rownames = "Label") %>%
    select(-Label, Label)
  res_plot <- df_main %>%
    ggplot(aes_string(x = names(df_main)[axes[1]], y = names(df_main)[axes[2]])) +
    geom_point() +
    geom_text_repel(aes(label = Label), size = lab.size) +
    geom_vline(xintercept = 0, lty = 2, col = "grey40") +
    geom_hline(yintercept = 0, lty = 2, col = "grey40") +
    labs(
      x = dims[axes[1]],
      y = dims[axes[2]],
      title = main
    ) +
    # coord_fixed(ratio = 3 / 4) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank()
    )
  attribute_consensus <- res_consensus %>%
    `[[`("Consensual.words")
  df_consensus <- res_consensus %>%
    `[[`("Centroids") %>%
    `colnames<-`(paste0("Dim", 1:ncol(.))) %>%
    as_tibble(rownames = "Label") %>%
    select(-Label, Label) %>%
    filter(Label %in% attribute_consensus)
  res_plot <- res_plot +
    geom_point(data = df_consensus, aes_string(x = names(df_consensus)[axes[1]], y = names(df_consensus)[axes[2]]), col = "red", pch = 17) +
    geom_text_repel(data = df_consensus, aes(label = Label), col = "red", size = lab.size)
  return(res_plot)
}
