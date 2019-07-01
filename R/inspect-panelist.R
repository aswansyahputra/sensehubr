#' Inspect panelist
#'
#' Evaluate panelist performance.
#'
#' @param res_performance output of performance analysis
#' @param metric discrimination, agreement, consistency
#'
#' @export
inspect_panelist <- function(res_performance, metric) {
  UseMethod("inspect_panelist")
}

#' @importFrom rlang arg_match
#' @importFrom SensoMineR magicsort
#' @importFrom stats median
#' @importFrom tibble as_tibble new_tibble
#' @importFrom dplyr select filter
#'
#' @export
inspect_panelist.default <- function(res_performance, metric = c("discrimination", "agreement", "consistency")) {
  metric <- arg_match(metric)

  if (metric[[1]] == "discrimination") {
    tbl <- res_performance$prob.ind %>%
      magicsort(method = "median") %>%
      as_tibble(rownames = "panelist") %>%
      select(-median) %>%
      filter(panelist != "median")
  }

  if (metric[[1]] == "agreement") {
    tbl <- res_performance$agree.ind %>%
      magicsort(method = "median", ascending = FALSE) %>%
      as_tibble(rownames = "panelist") %>%
      select(-median) %>%
      filter(panelist != "median")
  }

  if (metric[[1]] == "consistency") {
    tbl <- res_performance$res.ind %>%
      magicsort(method = "median") %>%
      as_tibble(rownames = "panelist") %>%
      select(-median) %>%
      filter(panelist != "median")
  }

  res <- new_tibble(tbl,
    nrow = NROW(tbl),
    metric = metric[[1]],
    class = "tbl_sensory_performance_panelist"
  )

  return(res)
}

#' @export
inspect_panelist.tbl_sensory_performance <- function(res_performance, metric = c("discrimination", "agreement", "consistency")) {
  res_performance_extracted <- res_performance$res_performance
  res <- inspect_panelist(res_performance_extracted, metric)
  return(res)
}
