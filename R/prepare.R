#' Prepare sensory table
#'
#' @param .data a dataframe with minimal sensory informations: panelist, product, and attribute
#' @param panelist panelist column
#' @param product product column
#' @param pres_order presentation order column
#' @param attribute sensory attribute columns
#' @param hedonic hedonic column
#' 
#' @import dplyr
#' @importFrom rlang as_label
#' @importFrom tidyselect vars_select
#' @importFrom tibble new_tibble
#'
#' @return a tibble with class of `tbl_sensory`
#' @export
#'
#' @examples
#' data(perfume_qda_consumer)
#' prepare(.data = perfume_qda_consumers, panelist = consumer, product = product, attribute = intensity:green, hedonic = NULL)

prepare <- function(.data, panelist = NULL, product = NULL, pres_order = NULL, attribute = NULL, hedonic = NULL) {
  tbl <- .data %>% 
    select(!!enquo(panelist),
           !!enquo(product),
           !!enquo(pres_order),
           !!enquo(attribute),
           !!enquo(hedonic)) %>% 
    mutate_at(vars(!!enquo(panelist), !!enquo(product)), ~as.factor(.x))
  meta_panelist <- rlang::as_label(enquo(panelist))
  meta_n_panelist <- length(unique(pull(tbl, !!enquo(panelist))))
  meta_product <- rlang::as_label(enquo(product))
  meta_n_product <- length(unique(pull(tbl, !!enquo(product))))
  meta_pres_order <- rlang::as_label(enquo(pres_order))
  meta_attribute <- unname(tidyselect::vars_select(names(tbl), !!enquo(attribute)))
  meta_n_attribute <- length(meta_attribute)
  meta_hedonic <- rlang::as_label(enquo(hedonic))
  
  res <- tibble::new_tibble(tbl,
                            "panelist" = meta_panelist,
                            "n_panelist" = meta_n_panelist,
                            "product" = meta_product,
                            "n_product" = meta_n_product,
                            "pres_order" = meta_pres_order,
                            "attribute" = meta_attribute,
                            "n_attribute" = meta_n_attribute,
                            "hedonic" = meta_hedonic,
                            nrow = NROW(tbl),
                            class = "tbl_sensory")
  res
}

