#' Liking analysis of sensory data
#'
#' Perform liking analysis on sensory table.
#' 
#' @param data a sensory table
#'
#' @export
analyse_liking <- function(data) {
  UseMethod("analyse_liking")
}

#' @export
analyse_liking.default <- function(data) {
  stop("`data` should be a sensory table.", call. = FALSE)
}

#' @importFrom dplyr select group_by mutate arrange
#' @importFrom tidyr gather spread nest unnest
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom tibble new_tibble
#'
#' @export
analyse_liking.tbl_sensory_qda <- function(data) {
  meta_panelist <- parse_meta(data, "panelist")
  meta_product <- parse_meta(data, "product")
  meta_session <- parse_meta(data, "session")
  meta_pres_order <- parse_meta(data, "pres_order")
  meta_hedonic <- parse_meta(data, "hedonic")
  
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
  
  tbl <- data %>%
    select(
      panelist = meta_panelist,
      product = meta_product,
      session = meta_session,
      pres_order = meta_pres_order,
      meta_hedonic
    ) %>%
    gather("attribute", "value", meta_hedonic) %>%
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
                    "sensory_method" = parse_meta(data, "sensory_method"),
                    "method_local" = "Analysis of Variance",
                    "model" = fmla,
                    nrow = NROW(tbl),
                    class = "tbl_sensory_liking"
  )
  
  return(res)
}

#' @export
analyse_liking.tbl_sensory_cata <- function(data) {
  res <- analyse_liking.tbl_sensory_qda(data)
  return(res)
}

#' @export
analyse_liking.tbl_sensory_rata <- function(data) {
  res <- analyse_liking.tbl_sensory_qda(data)
  return(res)
}
