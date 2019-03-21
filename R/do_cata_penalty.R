#' Perform JAR on CATA with Ideal data
#'
#' @param .data a dataframe
#' @param panelist a column containing information of panelist
#' @param product a column containing information of product
#' @param ideal_product name of ideal product
#' @param attribute set of columns containing information of  sensory attributes
#' @param liking a column containing information of liking
#'
#' @import dplyr
#' @importFrom broom tidy
#'
#' @return a tibble
#' @export

do_cata_penalty <- function(.data, panelist, product, ideal_product, attribute, liking){

  .data <-
    .data %>%
    select(!!enquo(panelist),
           !!enquo(product),
           !!enquo(attribute),
           !!enquo(liking)) %>%
    rename(
      panelist = !!enquo(panelist),
      product = !!enquo(product),
      liking = !!enquo(liking)
    ) %>%
    mutate(liking = replace_na(liking, 0))

  trans_data <-
    left_join(
      .data %>%
        filter(product != ideal_product) %>%
        gather(key = "attribute", value = "score", attribute),
      .data %>%
        filter(product == ideal_product) %>%
        gather(key = "attribute", value = "ideal_score", attribute) %>%
        select(panelist, attribute, ideal_score)
    ) %>%
    mutate(
      status = check_status(x = score, ref = ideal_score)
    ) %>%
    select(panelist, product, attribute, status, liking)

  penalties <-
    bind_rows(
      trans_data %>%
        filter(status %in% c("P(Yes)|(Yes)", "P(No)|(Yes)")) %>%
        mutate(
          status = factor(status, levels = c("P(Yes)|(Yes)", "P(No)|(Yes)"))
        ) %>%
        group_by(attribute) %>%
        nest() %>%
        mutate(
          model = map(data, ~aov(liking ~ status, data = .)),
          anova = map(model, ~broom::tidy(summary.lm(.)))
        ) %>%
        unnest(anova) %>%
        filter(term != '(Intercept)') %>%
        mutate(
          estimate = -1*estimate,
          term = str_remove(term, "status")
        ),
      trans_data %>%
        filter(status %in% c("P(No)|(No)", "P(Yes)|(No)")) %>%
        mutate(
          status = factor(status, levels = c("P(No)|(No)", "P(Yes)|(No)"))
        ) %>%
        group_by(attribute) %>%
        nest() %>%
        mutate(
          model = map(data, ~aov(liking ~ status, data = .)),
          anova = map(model, ~broom::tidy(summary.lm(.)))
        ) %>%
        unnest(anova) %>%
        filter(term != '(Intercept)') %>%
        mutate(
          estimate = 1*estimate,
          term = str_remove(term, "status")
        )
    ) %>%
    arrange(attribute) %>%
    rename(penalty = estimate,
           status = term)

  frequencies <-
    trans_data %>%
    count(attribute, status, .drop = FALSE) %>%
    group_by(attribute) %>%
    mutate(
      N = sum(n),
      frequency = 100*n/N
    ) %>%
    filter(status %in% c("P(No)|(Yes)", "P(Yes)|(No)"))

  res <- left_join(
    penalties,
    frequencies
  ) %>%
    rename(class = status) %>%
    select(-N)
  return(res)
}
