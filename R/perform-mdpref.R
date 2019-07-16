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
  
  meta_panelist <- parse_meta(tbl_sensory, "panelist")
  meta_product <- parse_meta(tbl_sensory, "product")
  meta_session <- parse_meta(tbl_sensory, "session")
  meta_pres_order <- parse_meta(tbl_sensory, "pres_order")
  meta_hedonic <- parse_meta(tbl_sensory, "hedonic")
  
  res_preference <- tbl_sensory %>%
    select(
      panelist = meta_panelist,
      product = meta_product,
      liking = meta_hedonic
    ) %>% 
    group_by(panelist, product) %>% 
    summarise(liking = mean(liking)) %>% 
    ungroup() %>% 
    spread(panelist, liking) %>% 
    as.data.frame() %>% 
    column_to_rownames("product") %>% 
    PCA(graph = FALSE)
  
  tbl_space <- inspect_space(res_preference)
  tbl_product <- inspect_product_preference(res_preference)
  tbl_panelist <- inspect_panelist_preference(res_preference)
  
  res <- list(
    eigenvalue = tbl_space,
    product = tbl_product,
    panelist = tbl_panelist,
    res_preference = res_preference
  )
  
  attr(res, "sensory_method") <- parse_meta(tbl_sensory, "sensory_method")
  attr(res, "method_global") <- "Principal Component Analysis"
  attr(res, "n_product") <- parse_meta(tbl_sensory, "n_product")
  attr(res, "n_panelist") <- parse_meta(tbl_sensory, "n_panelist")
  attr(res, "hedonic") <- parse_meta(tbl_sensory, "hedonic")
  class(res) <- append(class(res), "tbl_sensory_mdpref")
  
  return(res)  
}
