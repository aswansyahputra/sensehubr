#' Prepare data
#'
#' Prepare result from local test for global test
#'
#' @param .data result from get_stats
#' @param type type of original data, valid values: "string" and "number"
#'
#' @import dplyr tidyr
#' @importFrom readr parse_number
#' @return a tibble
#' @export

prepare <- function(.data, type = "string") {
  if (type == "string") {
    .data %>%
      select(-statistic, -p.value) %>%
      mutate_at(vars(-attribute), funs(parse_number)) %>%
      gather("product", "value", -attribute) %>%
      spread("attribute", "value")
  } else if (type == "number") {
    .data %>%
      select(-statistic, -p.value) %>%
      gather("product", "value", -attribute) %>%
      spread("attribute", "value")
  }
}
