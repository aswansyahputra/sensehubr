#' Analysis of variance
#' 
#' Perform Analysis of variance (ANOVA) on sensory table.
#' 
#' @param tbl_sensory a sensory table
#' 
#' @importFrom dplyr select group_by mutate summarise arrange
#' @importFrom tidyr gather spread nest unnest
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom tibble new_tibble
perform_anova <- function(tbl_sensory) {
  meta_panelist <- parse_meta(tbl_sensory, "panelist")
  meta_product <- parse_meta(tbl_sensory, "product")
  meta_session <- parse_meta(tbl_sensory, "session")
  meta_pres_order <- parse_meta(tbl_sensory, "pres_order")
  meta_attribute <- parse_meta(tbl_sensory, "attribute")
  
  if (!is.null(meta_session)) {
    if (!is.null(meta_pres_order)) {
      fmla <- "value ~ product + panelist + session + panelist:product + panelist:session + product:session + pres_order"
    } else if (is.null(meta_pres_order)) {
      fmla <- "value ~ product + panelist + session + panelist:product + panelist:session + product:session"
    }
  } else if (is.null(meta_session)) {
    if (!is.null(meta_pres_order)) {
      fmla <- "value ~ product + panelist + pres_order"
    } else if (is.null(meta_pres_order)) {
      fmla <- "value ~ product + panelist"
    }
  }
  
  tbl <- tbl_sensory %>%
    select(
      panelist = meta_panelist,
      product = meta_product,
      session = meta_session,
      pres_order = meta_pres_order,
      meta_attribute
    ) %>%
    gather("attribute", "value", meta_attribute) %>%
    group_by(attribute) %>%
    nest() %>%
    mutate(
      model = map(
        data,
        ~ aov(as.formula(fmla),
              data = .x
        )
      ),
      stats = map(
        model,
        ~ anova(.x) %>%
          tidy() %>%
          filter(term == "product") %>%
          select(statistic, p.value)
      ),
      values = map(
        data,
        ~ group_by(.x, product) %>%
          summarise(value = mean(value, na.rm = TRUE)) %>%
          spread(product, value)
      )
    ) %>%
    select(attribute, stats, values) %>%
    unnest(stats, values) %>%
    arrange(desc(statistic))
  
  res <- new_tibble(tbl,
                    "sensory_method" = parse_meta(tbl_sensory, "sensory_method"),
                    "method_local" = "Analysis of variance",
                    "model" = fmla,
                    nrow = NROW(tbl),
                    class = "tbl_sensory_local"
  )
  
  return(res)
}
