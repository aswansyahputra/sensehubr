#' Friedman rank sum test
#' 
#' Perform Friedman rank sum test on sensory table.
#' 
#' @param tbl_sensory a sensory table
#' 
#' @importFrom dplyr select group_by mutate arrange
#' @importFrom tidyr gather spread nest unnest
#' @importFrom purrr map map_dbl
#' @importFrom broom tidy
#' @importFrom tibble new_tibble
perform_friedman <- function(tbl_sensory) {
  meta_panelist <- parse_meta(tbl_sensory, "panelist")
  meta_product <- parse_meta(tbl_sensory, "product")
  meta_attribute <- parse_meta(tbl_sensory, "attribute")
  
  fmla <- "value ~ product | panelist"
  
  tbl <- tbl_sensory %>%
    select(
      panelist = meta_panelist,
      product = meta_product,
      meta_attribute
    ) %>%
    gather("attribute", "value", meta_attribute) %>%
    group_by(attribute) %>%
    nest() %>%
    mutate(
      model = map(
        data, ~ friedman.test(as.formula(fmla), data = .x)
      ),
      statistic = map_dbl(model, ~tidy(.x)[["statistic"]]),
      p.value = map_dbl(model, ~tidy(.x)[["p.value"]]),
      values = map(
        data,
        ~ group_by(.x, product) %>%
          summarise(value = sum(value, na.rm = TRUE)) %>%
          spread(product, value))
    ) %>%
    unnest(values) %>%
    spread(product, values) %>%
    arrange(desc(statistic))
  
  res <- new_tibble(tbl,
                    "sensory_method" = parse_meta(tbl_sensory, "sensory_method"),
                    "method_local" = "Friedman rank sum test",
                    "model" = fmla,
                    nrow = NROW(tbl),
                    class = "tbl_sensory_local"
  )
  
  return(res)
}
