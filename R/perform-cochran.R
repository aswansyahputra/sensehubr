#' Cochran's Q test
#' 
#' Perform Cochran's Q test on sensory table.
#' 
#' @param tbl_sensory a sensory table
#' 
#' @importFrom dplyr select group_by mutate arrange
#' @importFrom tidyr gather spread nest unnest
#' @importFrom purrr map map_dbl
#' @importFrom RVAideMemoire cochran.qtest
#' @importFrom tibble enframe new_tibble
perform_cochran <- function(tbl_sensory) {
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
        data, ~ cochran.qtest(as.formula(fmla), data = .x)
      ),
      statistic = map_dbl(model, "statistic"),
      p.value = map_dbl(model, "p.value"),
      values = map(model, ~ `[[`(.x, "estimate") %>%
                     enframe(name = "product", value = "values") %>%
                     mutate(product = str_remove_all(product, "proba in group ")))
    ) %>%
    unnest(values) %>%
    spread(product, values) %>%
    arrange(desc(statistic))
  
  res <- new_tibble(tbl,
                    "sensory_method" = parse_meta(tbl_sensory, "sensory_method"),
                    "method_local" = "Cochran's Q test",
                    "model" = fmla,
                    nrow = NROW(tbl),
                    class = "tbl_sensory_local"
  )
  
  return(res)
}
