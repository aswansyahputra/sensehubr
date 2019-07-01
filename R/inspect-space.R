#' Inspect sensory space
#'
#' Evaluate the dimension of sensory space in global analysis.
#'
#' @param res_global output of global analysis
#'
#' @importFrom dplyr transmute
#' @importFrom factoextra get_eig
#' @importFrom tibble new_tibble
#'
#' @return a dataframe
#' @export

inspect_space <- function(res_global) {
  UseMethod("inspect_space")
}

#' @export
inspect_space.default <- function(res_global) {
  stop("`res_global` is not valid.", call. = FALSE)
}

inspect_space.PCA <- function(res_global) {
  tbl <- get_eig(res_global) %>%
    as_tibble(rownames = "dimension") %>%
    transmute(
      dimension = str_remove_all(dimension, "Dim\\."),
      dimension = as.numeric(dimension),
      eigenvalue,
      pct_variance = variance.percent,
      pct_cum_variance = cumulative.variance.percent
    )

  res <- new_tibble(tbl,
    "n_dimension" = NROW(tbl),
    nrow = NROW(tbl),
    class = "tbl_sensory_global_eigenvalue"
  )

  return(res)
}

#' @export
inspect_space.CA <- function(res_global) {
  tbl <- get_eig(res_global) %>%
    as_tibble(rownames = "dimension") %>%
    transmute(
      dimension = str_remove_all(dimension, "Dim\\."),
      dimension = as.numeric(dimension),
      eigenvalue,
      pct_variance = variance.percent,
      pct_cum_variance = cumulative.variance.percent
    )

  res <- new_tibble(tbl,
    "n_dimension" = NROW(tbl),
    nrow = NROW(tbl),
    class = "tbl_sensory_global_eigenvalue"
  )

  return(res)
}
#' @export
inspect_space.tbl_sensory_global <- function(res_global) {
  res_global_extracted <- res_global$res_global
  res <- inspect_space(res_global_extracted)
  return(res)
}
