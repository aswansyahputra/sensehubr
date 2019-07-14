#' Principal Component Analysis
#' 
#' Perform Principal Component Analysis (PCA) on sensory table.
#' 
#' @param tbl_sensory a sensory table
#' 
#' @importFrom dplyr select group_by summarise_all left_join
#' @importFrom tibble column_to_rownames
#' @importFrom FactoMineR PCA
perform_pca <- function(tbl_sensory) {
  meta_product <- parse_meta(tbl_sensory, "product")
  meta_attribute <- parse_meta(tbl_sensory, "attribute")
  meta_hedonic <- parse_meta(tbl_sensory, "hedonic")
  
  if (is.null(meta_hedonic)) {
    res_global <- tbl_sensory %>%
      select(
        product = meta_product,
        meta_attribute,
        meta_hedonic
      ) %>%
      group_by(product) %>%
      summarise_at(., vars(meta_attribute), ~ mean(.x, na.rm = TRUE)) %>%
      as.data.frame() %>%
      column_to_rownames("product") %>%
      PCA(quanti.sup = NULL, graph = FALSE)
  }
  
  res_global <- tbl_sensory %>%
    select(
      product = meta_product,
      meta_attribute,
      meta_hedonic
    ) %>%
    group_by(product) %>%
    {
      left_join(
        summarise_at(., vars(meta_attribute), ~ mean(.x, na.rm = TRUE)),
        summarise_at(., vars(meta_hedonic), ~ mean(.x, na.rm = TRUE)),
        by = "product"
      )
    } %>%
    as.data.frame() %>%
    column_to_rownames("product") %>%
    PCA(quanti.sup = NCOL(.), graph = FALSE)
  
  tbl_space <- inspect_space(res_global)
  tbl_product <- inspect_product(res_global)
  tbl_attribute <- inspect_attribute(res_global)
  
  res <- list(
    eigenvalue = tbl_space,
    product = tbl_product,
    attribute = tbl_attribute,
    res_global = res_global
  )
  
  attr(res, "sensory_method") <- parse_meta(tbl_sensory, "sensory_method")
  attr(res, "method_global") <- "Principal Component Analysis"
  attr(res, "n_product") <- parse_meta(tbl_sensory, "n_product")
  attr(res, "n_attribute") <- parse_meta(tbl_sensory, "n_attribute")
  attr(res, "hedonic") <- parse_meta(tbl_sensory, "hedonic")
  class(res) <- append(class(res), "tbl_sensory_global")
  return(res)
}
