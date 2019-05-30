#' Local analysis of sensory data
#' 
#' Perform local analysis on sensory table.
#'
#' @param .data a sensory table
#' @param method sensory method, one of "QDA", "CATA", ....
#'
#' @import dplyr
#' @importFrom tidyr gather spread nest unnest
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom tibble new_tibble
#' 
#' @return a sensory table
#' @export
#'
#' @examples
#' data(perfume_qda_consumers)
#' (df <- prepare(.data = perfume_qda_consumers, 
#'   panelist = consumer, 
#'   product = product, 
#'   attribute = intensity:green, 
#'   hedonic = NULL))
#' analyse_local(df, method = "QDA")
#' 
#' # Using pipe %>%
#' perfume_qda_consumers %>% 
#' prepare(panelist = consumer, 
#'   product = product, 
#'   attribute = intensity:green, 
#'   hedonic = NULL) %>% 
#' analyse_local(method = "QDA")

analyse_local <- function(.data, method = "QDA") {
  if (class(.data)[[1]] != "tbl_sensory") {
    stop("`.data` should be a sensory table.", call. = FALSE)
  }
  
  if (method == "QDA") {
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
    res <- new_tibble(tbl, "method_local" = "Analysis of Variance",nrow = NROW(tbl), class = "tbl_sensory_qda")
  }
  return(res)
}
