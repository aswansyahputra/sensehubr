#' Plot product
#'
#' Plot product on sensory space
#'
#' @param .res Output from multivariate analyses
#' @param axes Selection of dimentions
#' @param main Title of the plot
#' @param lab_size Label size
#'
#' @import dplyr magrittr ggplot2 ggrepel
#' @return a plot
#' @export

plot_product <- function(.res, axes = c(1, 2), main = "", lab_size = 4) {
  if (missing(.res)) {
    stop("Data is not supplied", call. = FALSE)
  } else if (!any(class(.res) %in% c("PCA", "CA", "MFA", "MCA"))) {
    stop("Data is not one of PCA, CA, MFA, MCA class", call. = FALSE)
  }

  dims <- .res %>%
    magrittr::extract2("eig") %>%
    magrittr::extract(, 1) %>%
    round(2) %>%
    str_c("Dim ", 1:length(.), " (", ., "%)")

  if (any(class(.res) %in% c("PCA", "MFA"))) {
    df_main <-
      .res %>%
      magrittr::extract2(c("ind", "coord")) %>%
      set_colnames(str_c("dim", 1:ncol(.))) %>%
      as_tibble(rownames = "product") %>%
      select(-product, product)
  } else if (any(class(.res) %in% c("CA"))) {
    df_main <-
      .res %>%
      magrittr::extract2(c("row", "coord")) %>%
      set_colnames(str_c("dim", 1:ncol(.))) %>%
      as_tibble(rownames = "product") %>%
      select(-product, product)
  } else if (any(class(.res) %in% c("MCA"))) {
    df_main <-
      .res %>%
      magrittr::extract2(c("ind", "coord")) %>%
      set_colnames(str_c("dim", 1:ncol(.))) %>%
      as_tibble(rownames = "product") %>%
      select(-product, product)
  }
  res_plot <-
    df_main %>%
    ggplot(aes_string(x = names(df_main)[axes[1]], y = names(df_main)[axes[2]])) +
    geom_point() +
    geom_text_repel(aes(label = product), size = lab_size) +
    geom_vline(
      xintercept = 0,
      lty = 2,
      col = "grey40"
    ) +
    geom_hline(
      yintercept = 0,
      lty = 2,
      col = "grey40"
    ) +
    labs(
      x = dims[axes[1]],
      y = dims[axes[2]],
      title = main
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank()
    )
  return(res_plot)
}
