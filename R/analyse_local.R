#' Local analysis of sensory data
#' 
#' Perform local analysis on sensory table.
#'
#' @param .data a sensory table
#'
#' @import dplyr
#' @importFrom tidyr gather spread nest unnest
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom tibble new_tibble
#' 
#' @return a sensory table of local analysis
#' @export
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
#' perfume_qda_consumers %>% 
#' specify(panelist = consumer, 
#'   product = product, 
#'   attribute = intensity:green, 
#'   hedonic = NULL,
#'   method = "QDA") %>% 
#' analyse_local()

analyse_local <- function(.data) {
  UseMethod("analyse_local")
}

#' @export

analyse_local.default <- function(.data) {
  stop("`.data` should be a sensory table.", call. = FALSE)
}

#' @rdname analyse_local
#' @export

analyse_local.tbl_sensory_qda <- function(.data) {
  tbl <- .data %>% 
    select(panelist = attr(.data, "panelist"),
           product = attr(.data, "product"),
           attr(.data, "attribute")) %>% 
    gather("attribute", "value", attr(.data, "attribute")) %>%
    group_by(attribute) %>% 
    nest() %>% 
    mutate(
      model = map(
        data, 
        ~ aov(value ~ product + panelist,
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
                    "panelist" = attr(.data, "panelist"),
                    "n_panelist" = attr(.data, "n_panelist"),
                    "product" = attr(.data, "product"),
                    "n_product" = attr(.data, "n_product"),
                    "attribute" = attr(.data, "attribute"),
                    "n_attribute" = attr(.data, "n_attribute"),
                    "method_local" = "Analysis of Variance",
                    nrow = NROW(tbl), 
                    class = "tbl_sensory_local")
  return(res)
}
