#' Global analysis of sensory data
#' 
#' Perform global analysis on sensory table.
#' @param .data a sensory table
#' @param ... other arguments to pass on specific method
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
#' 
#' @export
analyse_global <- function(.data, ...) {
  UseMethod("analyse_global")
}

#' @export
analyse_global.default <- function(.data, ...) {
  stop("`.data` should be a sensory table.", call. = FALSE)
}

#' @importFrom dplyr select group_by summarise_all left_join
#' @importFrom tibble column_to_rownames
#' @importFrom FactoMineR PCA
#' 
#' @export
analyse_global.tbl_sensory_qda <- function(.data, ...) {
  meta_product <- attr(.data, "product")
  meta_attribute <- attr(.data, "attribute")
  
  if (attr(.data, "hedonic") == "NULL") {
    meta_hedonic <- NULL
    res_global <- .data %>% 
      select(product = meta_product,
             meta_attribute,
             meta_hedonic) %>% 
      group_by(product) %>% 
      summarise_at(., vars(meta_attribute), ~mean(.x, na.rm = TRUE)) %>% 
      as.data.frame() %>% 
      column_to_rownames("product") %>% 
      PCA(quanti.sup = NULL, graph = FALSE)
  } else {
    meta_hedonic <- attr(.data, "hedonic")
    res_global <- .data %>% 
      select(product = meta_product,
             meta_attribute,
             meta_hedonic) %>% 
      group_by(product) %>% {
        left_join(
          summarise_at(., vars(meta_attribute), ~mean(.x, na.rm = TRUE)),
          summarise_at(., vars(meta_hedonic), ~mean(.x, na.rm = TRUE)),
          by = "product"
        )
      } %>% 
      as.data.frame() %>% 
      column_to_rownames("product") %>% 
      PCA(quanti.sup = NCOL(.), graph = FALSE)
  }
  
  tbl_eig <- glance_eigenvalue(res_global)
  
  tbl_product <- glance_product(res_global)
  
  tbl_attribute <- glance_attribute(res_global)
  
  res <- list(
    eigenvalue = tbl_eig,
    product = tbl_product,
    attribute = tbl_attribute,
    res_global = res_global
  )
  
  attr(res, "method") <- attr(.data, "method")
  attr(res, "method_global") <- "PCA"
  attr(res, "n_product") <- attr(.data, "n_product")
  attr(res, "n_attribute") <- attr(.data, "n_attribute")
  attr(res, "hedonic") <- attr(.data, "hedonic")
  
  class(res) <- append(class(res), "tbl_sensory_global")
  
  return(res)
}

#' @importFrom dplyr select group_by summarise_all left_join
#' @importFrom tibble column_to_rownames
#' @importFrom FactoMineR CA
#' 
#' @export
analyse_global.tbl_sensory_cata <- function(.data, ...) {
  meta_product <- attr(.data, "product")
  meta_attribute <- attr(.data, "attribute")
  
  if (attr(.data, "hedonic") == "NULL") {
    meta_hedonic <- NULL
    res_global <- .data %>% 
      select(product = meta_product,
             meta_attribute,
             meta_hedonic) %>% 
      group_by(product) %>% 
      summarise_at(., vars(meta_attribute), ~sum(.x, na.rm = TRUE)) %>% 
      as.data.frame() %>% 
      column_to_rownames("product") %>% 
      CA(quanti.sup = NULL, graph = FALSE)
  } else {
    meta_hedonic <- attr(.data, "hedonic")
    res_global <- .data %>% 
      select(product = meta_product,
             meta_attribute,
             meta_hedonic) %>% 
      group_by(product) %>% {
        left_join(
          summarise_at(., vars(meta_attribute), ~sum(.x, na.rm = TRUE)),
          summarise_at(., vars(meta_hedonic), ~mean(.x, na.rm = TRUE)),
          by = "product"
        )
      } %>% 
      as.data.frame() %>% 
      column_to_rownames("product") %>% 
      CA(quanti.sup = NCOL(.), graph = FALSE)
  }
  
  tbl_eig <- glance_eigenvalue(res_global)
  
  tbl_product <- glance_product(res_global)
  
  tbl_attribute <- glance_attribute(res_global)
  
  res <- list(
    eigenvalue = tbl_eig,
    product = tbl_product,
    attribute = tbl_attribute,
    res_global = res_global
  )
  
  attr(res, "method") <- attr(.data, "method")
  attr(res, "method_global") <- "CA"
  attr(res, "n_product") <- attr(.data, "n_product")
  attr(res, "n_attribute") <- attr(.data, "n_attribute")
  attr(res, "hedonic") <- attr(.data, "hedonic")
  
  class(res) <- append(class(res), "tbl_sensory_global")
  
  return(res)
}
