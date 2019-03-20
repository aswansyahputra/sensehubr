#' Prepare data
#'
#' Prepare result from local test for global test
#'
#' @param .data result from get_stats
#'
#' @import dplyr tidyr
#' @importFrom readr parse_number
#' @return a tibble
#' @export

prepare <- function(.data) {
  .data %>%
    select(-statistic, -p.value) %>%
    mutate_at(vars(-attribute), funs(parse_number)) %>%
    gather("product", "value", -attribute) %>%
    spread("attribute", "value")
}

prepare2 <- function(.data) {
  .data %>%
    select(-statistic, -p.value) %>%
    gather("product", "value", -attribute) %>%
    spread("attribute", "value")
}

