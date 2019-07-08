#' Preference analysis of sensory data
#'
#' Perform preference analysis on sensory table.
#' 
#' @param data a sensory table
#'
#' @export
analyse_preference <- function(data) {
  UseMethod("analyse_preference")
}

#' @export
analyse_preference.default <- function(data) {
  stop("`data` should be a sensory table.", call. = FALSE)
}

#' @importFrom dplyr select group_by mutate ungroup arrange
#' @importFrom tidyr gather spread nest unnest
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom tibble new_tibble
#'
#' @export
analyse_preference.tbl_sensory_qda <- function(data) {
  meta_panelist <- parse_meta(data, "panelist")
  meta_product <- parse_meta(data, "product")
  meta_session <- parse_meta(data, "session")
  meta_pres_order <- parse_meta(data, "pres_order")
  meta_hedonic <- parse_meta(data, "hedonic")
  
  res_preference <- data %>%
    select(
      panelist = meta_panelist,
      product = meta_product,
      liking = meta_hedonic
    ) %>% 
    group_by(panelist) %>% 
    mutate(liking = scale(liking, center = TRUE, scale = TRUE)) %>% 
    ungroup() %>% 
    spread(product, liking) %>% 
    as.data.frame() %>% 
    column_to_rownames("panelist") %>% 
    PCA(scale.unit = FALSE, graph = FALSE)
  
  tbl_space <- inspect_space(res_preference)
  tbl_product <- inspect_preference_product(res_preference)
  tbl_panelist <- inspect_preference_panelist(res_preference)
  
  res <- list(
    eigenvalue = tbl_space,
    product = tbl_product,
    panelist = tbl_panelist,
    res_preference = res_preference
  )
  
  attr(res, "sensory_method") <- parse_meta(data, "sensory_method")
  attr(res, "method_global") <- "Principal Component Analysis"
  attr(res, "n_product") <- parse_meta(data, "n_product")
  attr(res, "n_panelist") <- parse_meta(data, "n_panelist")
  attr(res, "hedonic") <- parse_meta(data, "hedonic")
  class(res) <- append(class(res), "tbl_sensory_preference")
  
  return(res)
}

#' @export
analyse_preference.tbl_sensory_cata <- function(data) {
  res <- analyse_preference.tbl_sensory_qda(data)
  return(res)
}

#' @export
analyse_preference.tbl_sensory_rata <- function(data) {
  res <- analyse_preference.tbl_sensory_qda(data)
  return(res)
}
