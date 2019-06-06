#' Local analysis of sensory data
#' 
#' Perform local analysis on sensory table.
#' 
#' @param .data a sensory table
#' @param ... other arguments to pass on specific method
#' 
#' @examples
#' data(perfume_qda_consumers)
#' (df <- specify(.data = perfume_qda_consumers, 
#'   panelist = consumer, 
#'   product = product, 
#'   attribute = intensity:green, 
#'   hedonic = NULL,
#'   method = "QDA"))
#' analyse_local(df)
#' 
#' # Using pipe %>%
#' data(perfume_qda_experts)
#' perfume_qda_experts %>% 
#' specify(
#'   panelist = panelist,
#'   product = product,
#'   session = session,
#'   pres_order = rank,
#'   attribute = spicy:wrapping,
#'   method = "QDA"
#' ) %>% 
#' analyse_local()
#' 
#' @export
analyse_local <- function(.data, ...) {
  UseMethod("analyse_local")
}

#' @export
analyse_local.default <- function(.data, ...) {
  stop("`.data` should be a sensory table.", call. = FALSE)
}

#' @importFrom dplyr select group_by mutate arrange
#' @importFrom tidyr gather spread nest unnest
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom tibble new_tibble
#' 
#' @export
analyse_local.tbl_sensory_qda <- function(.data, ...) {
  meta_panelist <- attr(.data, "panelist")
  meta_product <- attr(.data, "product")
  meta_attribute <- attr(.data, "attribute")
  
  if (attr(.data, "session") != "NULL") {
    meta_session <- attr(.data, "session")
    if (attr(.data, "pres_order") != "NULL") {
      meta_pres_order <- attr(.data, "pres_order")
      fmla <- "value ~ product + panelist + session + panelist:product + panelist:session + product:session + pres_order"
    } else if (attr(.data, "pres_order") == "NULL") {
      meta_pres_order <- NULL
      fmla <- "value ~ product + panelist + session + panelist:product + panelist:session + product:session"
    }
  } else if (attr(.data, "session") == "NULL") {
    meta_session <- NULL
    if (attr(.data, "pres_order") != "NULL") {
      meta_pres_order <- attr(.data, "pres_order")
      fmla <- "value ~ product + panelist + pres_order"
    } else if (attr(.data, "pres_order") == "NULL") {
      meta_pres_order <- NULL
      fmla <- "value ~ product + panelist"
    }
  }
  
  tbl <- .data %>% 
    select(panelist = meta_panelist,
           product = meta_product,
           session = meta_session,
           pres_order = meta_pres_order,
           meta_attribute) %>% 
    gather("attribute", "value", meta_attribute) %>%
    group_by(attribute) %>% 
    nest() %>% 
    mutate(
      model = map(
        data, 
        ~ aov(as.formula(fmla),
              data = .x)),
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
                    "method" = attr(.data, "method"),
                    "method_local" = "Analysis of Variance",
                    "model" = fmla,
                    nrow = NROW(tbl), 
                    class = "tbl_sensory_local")
  
  return(res)
}

#' @importFrom dplyr select group_by mutate arrange
#' @importFrom tidyr gather spread nest unnest
#' @importFrom purrr map map_dbl
#' @importFrom RVAideMemoire cochran.qtest
#' @importFrom tibble enframe new_tibble
#' 
#' @export
analyse_local.tbl_sensory_cata <- function(.data, ...) {
  meta_panelist <- attr(.data, "panelist")
  meta_product <- attr(.data, "product")
  meta_attribute <- attr(.data, "attribute")
  
  fmla <- "value ~ product | panelist"
  
  tbl <- .data %>% 
    select(panelist = meta_panelist,
           product = meta_product,
           meta_attribute) %>% 
    gather("attribute", "value", meta_attribute) %>%
    group_by(attribute) %>% 
    nest() %>% 
    mutate(
      model = map(
        data, ~ cochran.qtest(as.formula(fmla), data = .x)
      ),
      statistic = map_dbl(model, "statistic"),
      p.value = map_dbl(model, "p.value"),
      probs = map(model, ~`[[`(.x, "estimate") %>%
                    enframe(name = "product") %>%
                    mutate(product = str_remove_all(product, "proba in group ")))
    ) %>% 
    unnest(probs) %>%
    spread(key = product, value = value) %>%
    arrange(desc(statistic))
  
  res <- new_tibble(tbl, 
                    "method" = attr(.data, "method"),
                    "method_local" = "Cochran's Q test",
                    "model" = fmla,
                    nrow = NROW(tbl), 
                    class = "tbl_sensory_local")
  
  return(res)
}
