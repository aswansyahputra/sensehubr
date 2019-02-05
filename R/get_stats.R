#' Get statistics
#'
#' Perform univariate statistical analysis for all sensory attributes simultaniously
#'
#' @param .data a dataframe
#' @param stat statistical procedure to be performed. Choice: "anova", "cochran", and "friedman"
#' @param round whether to round the statistic and p-value, default to TRUE
#'
#' @import dplyr tidyr purrr magrittr broom agricolae
#' @return a tibble
#' @export

get_stats <- function(.data, stat = "anova", round = TRUE) {
  if (missing(.data)) {
    stop("Data is not supplied", call. = FALSE)
  } else if (!stat %in% c("anova", "cochran", "friedman")) {
    stop("'stat' is not one of anova, cochran, or friedman", call. = FALSE)
  }
  if (stat == "anova") {
    res <-
      .data %>%
      mutate(
        model = map(data, ~ aov(
          value ~ product + panelist, data = .
        )),
        stats = map(
          model,
          ~ anova(.x) %>%
            tidy() %>%
            filter(term == "product") %>%
            select(statistic, p.value)
        ),
        means = map(
          model,
          ~ HSD.test(y = ., trt = "product", console = FALSE) %>%
            use_series("groups") %>%
            as_tibble(rownames = "product") %>%
            mutate(value = signif(value, 3)) %>%
            unite("value", c(value, groups), sep = " ") %>%
            spread(product, value)
        )
      ) %>%
      select(attribute, stats, means) %>%
      unnest(stats, means) %>%
      arrange(desc(statistic))

    # if (any("session" %in% colnames(.data[["data"]][[1]]))) {
    #   res <-
    #     .data %>%
    #     mutate(
    #       model = map(data, ~ aov(
    #         value ~ product + panelist + session + panelist:product + panelist:session + product:session + pres_order, data = .
    #       )),
    #       stats = map(
    #         model,
    #         ~ anova(.x) %>%
    #           tidy() %>%
    #           filter(term == "product") %>%
    #           select(statistic, p.value)
    #       ),
    #       means = map(
    #         model,
    #         ~ HSD.test(y = ., trt = "product", console = FALSE) %>%
    #           use_series("groups") %>%
    #           as_tibble(rownames = "product") %>%
    #           mutate(value = signif(value, 3)) %>%
    #           unite("value", c(value, groups), sep = " ") %>%
    #           spread(product, value)
    #       )
    #     ) %>%
    #     select(attribute, stats, means) %>%
    #     unnest(stats, means) %>%
    #     arrange(desc(statistic))
    # } else {
    #   res <-
    #     .data %>%
    #     mutate(
    #       model = map(data, ~ aov(
    #         value ~ product + panelist + pres_order, data = .
    #       )),
    #       stats = map(
    #         model,
    #         ~ anova(.x) %>%
    #           tidy() %>%
    #           filter(term == "product") %>%
    #           select(statistic, p.value)
    #       ),
    #       means = map(
    #         model,
    #         ~ HSD.test(y = ., trt = "product", console = FALSE) %>%
    #           use_series("groups") %>%
    #           as_tibble(rownames = "product") %>%
    #           mutate(value = signif(value, 3)) %>%
    #           unite("value", c(value, groups), sep = " ") %>%
    #           spread(product, value)
    #       )
    #     ) %>%
    #     select(attribute, stats, means) %>%
    #     unnest(stats, means) %>%
    #     arrange(desc(statistic))
    # }
  }
  if (!isTRUE(round)) {
    return(res)
  } else {
    res <-
      res %>%
      mutate(
        statistic = round(statistic, 2),
        p.value = signif(p.value, 3)
      )
    return(res)
  }
}
