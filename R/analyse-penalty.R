#' Penalty analysis of sensory data
#'
#' Perform penalty analysis on sensory table.
#'
#' @param data a sensory table
#' @param reference_value a score used as JAR value (reference)
#'
#' @examples
#' perfume_jar %>%
#'   specify(
#'     sensory_method = "JAR",
#'     panelist = consumer,
#'     product = product,
#'     attribute = intensity:green,
#'     hedonic = liking
#'   ) %>%
#'   analyse_penalty(reference_value = 0)
#' @export
analyse_penalty <- function(data, reference_value) {
  UseMethod("analyse_penalty")
}

#' @export
analyse_penalty.default <- function(data, reference_value) {
  stop("`data` should be a sensory table.", call. = FALSE)
}

#' @importFrom dplyr select filter group_by mutate mutate_at transmute case_when arrange count
#' @importFrom tidyr gather nest unnest
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom tibble new_tibble
#'
#' @export
analyse_penalty.tbl_sensory_jar <- function(data, reference_value) {
  meta_panelist <- parse_meta(data, "panelist")
  meta_product <- parse_meta(data, "product")
  meta_attribute <- parse_meta(data, "attribute")
  meta_hedonic <- parse_meta(data, "hedonic")

  fmla <- "liking ~ category"
  
  tbl <- data %>%
    select(
      panelist = meta_panelist,
      product = meta_product,
      hedonic = meta_hedonic,
      meta_attribute,
      liking = meta_hedonic
    ) %>%
    mutate_at(
      vars(meta_attribute),
      ~ case_when(
        .x < reference_value ~ "Not enough",
        .x > reference_value ~ "Too high",
        TRUE ~ "JAR"
      )
    ) %>%
    gather("attribute", "category", meta_attribute) %>%
    mutate(category = factor(category, levels = c("JAR", "Too high", "Not enough"))) %>%
    group_by(product, attribute) %>%
    nest() %>%
    mutate(
      model = map(data, ~ aov(as.formula(fmla), data = .x)),
      stats = map(
        model,
        ~ summary.lm(.x) %>%
          tidy() %>%
          filter(term != "(Intercept)")
      ),
      values = map(
        data,
        ~ count(.x, category) %>%
          mutate(value = 100 * n / sum(n)) %>%
          filter(category != "JAR") %>%
          select(category, value)
      )
    ) %>%
    unnest(stats, values) %>%
    transmute(
      product = as.character(product),
      attribute,
      category = as.character(category),
      frequency = value,
      penalty = abs(estimate),
      std.error,
      statistic,
      p.value
    ) %>%
    arrange(product)

  res <- new_tibble(tbl,
    "sensory_method" = parse_meta(data, "sensory_method"),
    "method_local" = "T-test",
    "model" = fmla,
    nrow = NROW(tbl),
    class = "tbl_sensory_penalty"
  )
  return(res)
}
