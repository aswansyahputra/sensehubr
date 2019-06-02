#' Global analysis of sensory data
#' 
#' Perform global analysis on sensory table.
#' @param .data a sensory table
#' @param ... other arguments to pass on specific method
#' 
#' @export
analyse_global <- function(.data, ...) {
  UseMethod("analyse_global")
}

#' @export
analyse_global.default <- function(.data, ...) {
  stop("`.data` should be a sensory table.", call. = FALSE)
}

#' Global analysis for sensory data
#' 
#' Perform global analysis on sensory table.
#' 
#' @param .data a sensory table
#' @param ... not yet implemented
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
#' @name analyse-global_qda
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
#' data(perfume_qda_experts)
#' perfume_qda_experts %>% 
#' specify(
#'   panelist = panelist,
#'   product = product,
#'   session = session,
#'   pres_order = rank,
#'   attribute = spicy:wrapping,
#'   method = "QDA"
#' ) %>% 
#' analyse_global()

analyse_global.tbl_sensory_qda <- function(.data, ...) {
  meta_product <- attr(.data, "product")
  meta_attribute <- attr(.data, "attribute")
  
  if (attr(.data, "hedonic") == "NULL") {
    meta_hedonic <- NULL
  } else {
    meta_hedonic <- attr(.data, "hedonic")
  }
  
  res_global <- .data %>% 
    select(product = meta_product,
           meta_attribute,
           meta_hedonic) %>% 
    group_by(product) %>% 
    summarise_all(~mean(.x, na.rm = TRUE)) %>% 
    as.data.frame() %>% 
    column_to_rownames("product") %>% 
    PCA(quanti.sup = meta_hedonic, graph = FALSE)
  
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
    "Method" = "PCA",
    "Active individual" = paste(attr(.data, "n_product"), "products as active individuals"),
    "Active variable" = paste(attr(.data, "n_attribute"), "sensory attributes as active variables"),
    "Suplementary variable" = paste(ifelse(attr(.data, "hedonic") == "NULL", "None", attr(.data, "hedonic")), "as supplementary quantitative variable")
  )
  
  tbl_product <- glance_product(res_global)
  
  tbl_attribute <- glance_attribute(res_global)
  
  res <- list(
    eigenvalue = tbl_eig,
    product = tbl_product,
    attribute = tbl_attribute,
    res_global = res_global
  )
  
  class(res) <- append(class(res), "tbl_sensory_global")
  
  attr(res, "method") <- attr(.data, "method")
  attr(res, "method_local") <- "PCA"
  attr(res, "hedonic") <- attr(.data, "hedonic")
  
  return(res)
}
