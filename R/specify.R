#' Speficy sensory table
#'
#' Specify sensory informations into a raw dataframe. The minimal sensory informations are the panelist, the product, the sensory attributes, and the method in which the evaluation was conducted. Additonally the presentation order and hedonic evaluation can also be specified.
#'
#' @param .data a dataframe
#' @param panelist panelist column
#' @param product product column
#' @param pres_order presentation order column
#' @param attribute sensory attribute columns
#' @param hedonic hedonic column
#' @param method sensory method, i.e. QDA, CATA, RATA, FCP, FP, JAR, IPM
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
#' (df <- specify(.data = perfume_qda_consumers, 
#'   panelist = consumer, 
#'   product = product, 
#'   attribute = intensity:green, 
#'   hedonic = NULL,
#'   method = "QDA"))

specify <- function(.data, panelist = NULL, product = NULL, pres_order = NULL, attribute = NULL, hedonic = NULL, method = c("QDA", "CATA", "RATA", "FCP", "FP", "JAR", "IPM")) {
  tbl <- .data %>% 
    select(!!enquo(panelist),
           !!enquo(product),
           !!enquo(pres_order),
           !!enquo(attribute),
           !!enquo(hedonic)) %>% 
    mutate_at(vars(!!enquo(panelist), !!enquo(product)), ~as.factor(.x))
  
  tbl_class <- switch(method,
                      "QDA" = "tbl_sensory_qda",
                      "CATA" = "tbl_sensory_cata",
                      "RATA" = "tbl_sensory_rata",
                      "FCP" = "tbl_sensory_fcp",
                      "FP" = "tbl_sensory_fp",
                      "JAR" = "tbl_sensory_jar",
                      "IPM" = "tbl_sensory_ipm")
  
  res <- new_tibble(tbl,
                    "method" = method,
                    "panelist" = as_label(enquo(panelist)),
                    "n_panelist" = length(unique(pull(tbl, !!enquo(panelist)))),
                    "product" = as_label(enquo(product)),
                    "n_product" = length(unique(pull(tbl, !!enquo(product)))),
                    "pres_order" = as_label(enquo(pres_order)),
                    "attribute" = unname(vars_select(names(tbl), !!enquo(attribute))),
                    "n_attribute" = length(vars_select(names(tbl), !!enquo(attribute))),
                    "hedonic" = meta_hedonic <- as_label(enquo(hedonic)),
                    nrow = NROW(tbl),
                    class = tbl_class)
  return(res)
}
