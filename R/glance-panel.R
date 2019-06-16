#' Glance panel
#'
#' Evaluate panel performance.
#'
#' @param res_performance output of performance analysis
#'
#' @export
glance_panel <- function(res_performance) {
  UseMethod("glance_panel")
}

#' @importFrom tibble as_tibble new_tibble
#' @importFrom janitor clean_names
#' @importFrom dplyr arrange
#'
#' @export
glance_panel.default <- function(res_performance) {
  tbl <- res_performance$p.value %>%
    as_tibble(rownames = "attribute") %>%
    clean_names() %>%
    arrange(product)

  res <- new_tibble(tbl,
    nrow = NROW(tbl),
    class = "tbl_sensory_performance_panel"
  )
  return(res)
}

glance_panel.tbl_sensory_performance <- function(res_performance) {
  res_performance_extracted <- res_performance$res_performance
  res <- glance_panel(res_performance_extracted)
  return(res)
}
