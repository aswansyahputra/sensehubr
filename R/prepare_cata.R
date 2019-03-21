#' Prepare data for CATA analysis
#'
#' @param .data a dataframe
#' @param panelist name of column containing panelist information
#' @param product name of column containing product information
#' @param attribute set of columns containing information of  sensory attributes
#' @param liking name of column containing liking information
#'
#' @import dplyr

#' @return @ a tibble
#'
#' @export

prepare_cata <- function(.data, panelist, product, attribute, liking) {
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

  res <-
    left_join(
      .data %>%
        group_by(product) %>%
        summarise_at(vars(!!attribute), ~sum(.x, na.rm = TRUE)),
      .data %>%
        group_by(product) %>%
        summarise_at(vars(liking), ~mean(.x, na.rm = TRUE))
    )
  return(res)
}

