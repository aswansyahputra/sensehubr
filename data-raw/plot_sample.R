plot_sample <- function(res, axes = c(1, 2), main = "", lab.size = 4) {
  dims <- res %>%
    `[[`("eig") %>%
    as.data.frame() %>%
    select(2) %>%
    pull() %>%
    round(2) %>%
    paste0("Dim ", 1:length(.), " (", ., "%)")

  if (any(class(res) %in% c("PCA", "MFA"))) {
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
  } else if (any(class(res) %in% c("CA"))) {
    df_main <- res %>%
      `[[`(c("row", "coord")) %>%
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
  } else if (any(class(res) %in% c("MCA"))) {
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
  }
  return(res_plot)
}
