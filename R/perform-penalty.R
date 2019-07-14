#' Penalty analysis
#' 
#' Perform penalty analysis on sensory table.
#' 
#' @param tbl_sensory a sensory table
#' @param ref_value a score used as JAR reference value
#' 
#' @importFrom dplyr select filter group_by mutate mutate_at transmute case_when arrange count
#' @importFrom tidyr gather nest unnest
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom tibble new_tibble
perform_penalty <- function(tbl_sensory, ref_value) {
  if (is.null(parse_meta(tbl_sensory, "hedonic"))) {
    stop("No hedonic data is available in sensory table", call. = FALSE)
  }
  
  meta_panelist <- parse_meta(tbl_sensory, "panelist")
  meta_product <- parse_meta(tbl_sensory, "product")
  meta_attribute <- parse_meta(tbl_sensory, "attribute")
  meta_hedonic <- parse_meta(tbl_sensory, "hedonic")
  
  fmla <- "liking ~ category"
  
  tbl <- tbl_sensory %>%
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
        .x < ref_value ~ "Not enough",
        .x > ref_value ~ "Too high",
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
      # penalty = abs(estimate),
      penalty = (estimate),
      std.error,
      statistic,
      p.value
    ) %>%
    arrange(product)
  
  res <- new_tibble(tbl,
                    "sensory_method" = parse_meta(tbl_sensory, "sensory_method"),
                    "method_local" = "T-test",
                    "model" = fmla,
                    nrow = NROW(tbl),
                    class = "tbl_sensory_penalty"
  )
  
  return(res)
}
