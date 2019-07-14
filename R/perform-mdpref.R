#' Internal preference mapping
#' 
#' Perform internal preference mapping (MDPref) on sensory table.
#' 
#' @param tbl_sensory a sensory table
#' 
#' @importFrom dplyr select group_by mutate ungroup arrange
#' @importFrom tidyr gather spread nest unnest
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom tibble new_tibble
perform_mdpref <- function(tbl_sensory) {
  if (is.null(parse_meta(tbl_sensory, "hedonic"))) {
    stop("No hedonic data is available in sensory table", call. = FALSE)
  }
  
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
    mutate(liking = scale(liking, center = TRUE, scale = FALSE)) %>% 
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
  class(res) <- append(class(res), "tbl_sensory_mdpref")
  
  return(res)  
}
