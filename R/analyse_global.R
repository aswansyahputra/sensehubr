#' Global analysis for sensory data
#' 
#' Perform global analysis on sensory table.
#' 
#' @param .data a sensory table
#' 
#' @import dplyr
#' @import FactoMineR
#' @importFrom tibble as_tibble column_to_rownames trunc_mat
#' @importFrom factoextra get_eig
#' @importFrom stringr str_remove_all
#'
#' @return a sensory table of global analysis
#' @export
#'
#' @examples
#' data(perfume_qda_consumers)
#' (df <- specify(.data = perfume_qda_consumers, 
#'   panelist = consumer, 
#'   product = product, 
#'   attribute = intensity:green, 
#'   hedonic = NULL,
#'   method = "QDA"))
#' analyse_global(df)
#' 
#' # Using pipe %>%
#' perfume_qda_consumers %>% 
#' specify(panelist = consumer, 
#'   product = product, 
#'   attribute = intensity:green, 
#'   hedonic = NULL,
#'   method = "QDA") %>% 
#' analyse_global()

analyse_global <- function(.data) {
  UseMethod("analyse_global")
}

#' @export

analyse_global.default <- function(.data) {
  stop("`.data` should be a sensory table.", call. = FALSE)
}

#' @rdname analyse_global
#' @export

analyse_global.tbl_sensory_qda <- function(.data) {
  if (attr(.data, "hedonic") == "NULL") {
    liking <- NULL
  } else {
    liking <- attr(.data, "hedonic")
  }
  res_global <- .data %>% 
    select(product = attr(.data, "product"),
           attr(.data, "attribute"),
           liking) %>% 
    group_by(product) %>% 
    summarise_all(~mean(.x, na.rm = TRUE)) %>% 
    as.data.frame() %>% 
    column_to_rownames("product") %>% 
    PCA(quanti.sup = liking, graph = FALSE)
  
  tbl_eig <- 
    get_eig(res_global) %>% 
    as_tibble(rownames = "dimension") %>% 
    transmute(dimension = str_remove_all(dimension, "Dim\\."),
              dimension = as.numeric(dimension),
              eigenvalue,
              pct_variance = variance.percent,
              pct_cum_variance = cumulative.variance.percent)
  
  
  tbl_eig <- trunc_mat(as_tibble(tbl_eig))
  tbl_eig$summary <- c(
    "A sensory table" = meta_info(.data, "method"),
    "Type" = "Global analysis",
    "Method" = "PCA"
  )
  
  tbl_product <- glance_product(res_global)
  
  tbl_attribute <- glance_attribute(res_global)
  
  # tbl_eig <-  
  #   get_eig(res_global) %>% 
  #   as_tibble(rownames = "dimension") %>% 
  #   rename(pct_variance = variance.percent,
  #          pct_cum_variance = cumulative.variance.percent)
  # 
  # tbl_product <- 
  #   list(
  #     coord = get_pca_ind(res_global)$coord %>% 
  #       as_tibble(rownames = "attribute"),
  #     cos2 = get_pca_ind(res_global)$cos2 %>% 
  #       as_tibble(rownames = "attribute"),
  #     contrib = get_pca_ind(res_global)$contrib %>% 
  #       as_tibble(rownames = "attribute")
  #   ) %>% 
  #   bind_rows(.id = "parameter") %>% 
  #   nest(-parameter)
  # 
  # tbl_attribute <- 
  #   list(
  #     coord = get_pca_var(res_global)$coord %>% 
  #       as_tibble(rownames = "attribute"),
  #     cor = get_pca_var(res_global)$cor %>% 
  #       as_tibble(rownames = "attribute"),
  #     cos2 = get_pca_var(res_global)$cos2 %>% 
  #       as_tibble(rownames = "attribute"),
  #     contrib = get_pca_var(res_global)$contrib %>% 
  #       as_tibble(rownames = "attribute")
  #   ) %>% 
  #   bind_rows(.id = "parameter") %>% 
  #   nest(-parameter)
  
  res <- list(
    method = "pca",
    eigenvalue = tbl_eig,
    product = tbl_product,
    attribute = tbl_attribute,
    res_global = res_global
  )
  class(res) <- append(class(res), "tbl_sensory_global")
  return(res)
}
