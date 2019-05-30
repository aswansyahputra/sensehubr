#' Prepare sensory table
#'
#' Prepare a raw dataframe with minimal sensory informations (panelist, product, and attribute) into a sensory table fur further processing.
#'
#' @param .data a dataframe
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
#' @return a sensory table (dataframe with class of \code{tbl_sensory})
#' @export
#'
#' @examples
#' data(perfume_qda_consumer)
#' (df <- prepare(.data = perfume_qda_consumers, 
#'   panelist = consumer, 
#'   product = product, 
#'   attribute = intensity:green, 
#'   hedonic = NULL))

prepare <- function(.data, panelist = NULL, product = NULL, pres_order = NULL, attribute = NULL, hedonic = NULL) {
  tbl <- .data %>% 
    select(!!enquo(panelist),
           !!enquo(product),
           !!enquo(pres_order),
           !!enquo(attribute),
           !!enquo(hedonic)) %>% 
    mutate_at(vars(!!enquo(panelist), !!enquo(product)), ~as.factor(.x))
  res <- new_tibble(tbl,
                    "panelist" = as_label(enquo(panelist)),
                    "n_panelist" = length(unique(pull(tbl, !!enquo(panelist)))),
                    "product" = as_label(enquo(product)),
                    "n_product" = length(unique(pull(tbl, !!enquo(product)))),
                    "pres_order" = as_label(enquo(pres_order)),
                    "attribute" = unname(vars_select(names(tbl), !!enquo(attribute))),
                    "n_attribute" = length(vars_select(names(tbl), !!enquo(attribute))),
                    "hedonic" = meta_hedonic <- as_label(enquo(hedonic)),
                    nrow = NROW(tbl),
                    class = "tbl_sensory")
  return(res)
}

