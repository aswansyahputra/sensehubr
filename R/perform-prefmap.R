#' External preference mapping
#'
#' Perform external preference mapping (PrefMap) on sensory table.
#' 
#' @param tbl_sensory a sensory table
#' 
#' @importFrom dplyr select mutate
#' @importFrom tidyr gather spread
#' @importFrom SensoMineR carto
#' @importFrom tibble column_to_rownames as_tibble
perform_prefmap <- function(tbl_sensory) {
  if (is.null(parse_meta(tbl_sensory, "hedonic"))) {
    stop("No hedonic data is available in sensory table", call. = FALSE)
  }
  
  meta_panelist <- parse_meta(tbl_sensory, "panelist")
  meta_product <- parse_meta(tbl_sensory, "product")
  meta_hedonic <- parse_meta(tbl_sensory, "hedonic")
  meta_n_panelist <- parse_meta(tbl_sensory, "n_panelist")
  
  tbl_hedonic <- tbl_sensory %>%
    select(
      panelist = meta_panelist,
      product = meta_product,
      liking = meta_hedonic
    ) %>%
    mutate(product = as.character(product)) %>%
    spread(panelist, liking) %>%
    as.data.frame() %>%
    column_to_rownames("product")
  
  if (any(class(tbl_sensory) == "tbl_sensory_qda")) {
    tbl_space <- tbl_sensory %>%
      perform_pca() %>%
      inspect_product() %>%
      select(1:3) %>%
      as.data.frame() %>%
      column_to_rownames("product")
  } else if (any(class(tbl_sensory) %in% c("tbl_sensory_cata", "tbl_sensory_rata"))) {
    tbl_space <- tbl_sensory %>%
      perform_ca() %>%
      inspect_product() %>%
      select(1:3) %>%
      as.data.frame() %>%
      column_to_rownames("product")
    
  }
  
  res_prefmap <- SensoMineR::carto(Mat = tbl_space,
                                   MatH = tbl_hedonic,
                                   graph.tree = FALSE,
                                   graph.corr = FALSE,
                                   graph.carto = FALSE)
  
  tbl_above_average <- res_prefmap$nb.depasse %>%
    `dimnames<-`(list(res_prefmap$f1, res_prefmap$f2)) %>%
    as_tibble(rownames = "dim1") %>%
    gather("dim2", "n_panelist", -1, convert = TRUE) %>%
    mutate(dim1 =  as.numeric(dim1),
           prop_panelist = n_panelist/meta_n_panelist)

  tbl_product <- res_prefmap$matrice %>%
    as_tibble(rownames = "product") %>%
    select(product, dim1, dim2)
  
  res <- list(
    above_average = tbl_above_average,
    product = tbl_product,
    res_prefmap = res_prefmap
  )
  
  attr(res, "sensory_method") <- parse_meta(data, "sensory_method")
  attr(res, "method_global") <- "Principal Component Regression"
  attr(res, "n_product") <- parse_meta(data, "n_product")
  attr(res, "n_panelist") <- parse_meta(data, "n_panelist")
  attr(res, "hedonic") <- parse_meta(data, "hedonic")
  class(res) <- append(class(res), "tbl_sensory_prefmap")

  return(res)
}
