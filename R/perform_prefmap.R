#' Perform External Preference Mapping
#'
#' Perform external preference mapping on sensory table.
#' 
#' @param data a sensory table
#'
#' @export
perform_prefmap <- function(data) {
  UseMethod("perform_prefmap")
}

#' @export
perform_prefmap.default <- function(data) {
  stop("`data` should be a sensory table.", call. = FALSE)
}

#' @importFrom dplyr select mutate
#' @importFrom tidyr gather spread
#' @importFrom SensoMineR carto
#' @importFrom tibble column_to_rownames as_tibble
#'
#' @export
perform_prefmap.tbl_sensory_qda <- function(data) {
  meta_panelist <- parse_meta(data, "panelist")
  meta_product <- parse_meta(data, "product")
  meta_hedonic <- parse_meta(data, "hedonic")
  meta_n_panelist <- parse_meta(data, "n_panelist")
  
  if (is.null(meta_hedonic)) {
    stop("Hedonic data is not supplied.", call. = FALSE)
  }
  
  tbl_hedonic <- data %>%
    select(
      panelist = meta_panelist,
      product = meta_product,
      liking = meta_hedonic
    ) %>%
    mutate(product = as.character(product)) %>%
    spread(panelist, liking) %>%
    as.data.frame() %>%
    column_to_rownames("product")
  
  tbl_space <- data %>%
    analyse_global() %>%
    inspect_product() %>%
    select(1:3) %>%
    as.data.frame() %>%
    column_to_rownames("product")
  
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
  
  attr(res, "method_global") <- "Principal Component Regression"
  class(res) <- append(class(res), "tbl_sensory_prefmap")

  return(res)
}
