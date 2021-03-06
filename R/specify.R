#' Specify sensory table
#'
#' Specify sensory informations into a raw dataframe. The minimal sensory informations are the panelist, the product, the sensory attributes, and the method in which the evaluation was conducted. Additonally the session, the presentation order and hedonic evaluation can also be specified.
#'
#' @param data a dataframe
#' @param sensory_method method of sensory evaluation, available methods are QDA, CATA, RATA, FCP, FP, JAR, IPM
#' @param panelist panelist column
#' @param product product column
#' @param session session column
#' @param pres_order presentation order column
#' @param attribute sensory attribute columns
#' @param hedonic hedonic column
#'
#' @importFrom dplyr select mutate_at pull
#' @importFrom rlang arg_match enquo as_label
#' @importFrom tidyselect vars_select
#' @importFrom tibble new_tibble
#'
#' @return a sensory table (dataframe with class of \code{tbl_sensory})
#'
#' @export
#'
#' @examples
#' (df <- specify(
#'   data = perfume_qda_consumers,
#'   sensory_method = "QDA",
#'   panelist = consumer,
#'   product = product,
#'   attribute = intensity:green,
#'   hedonic = NULL
#' ))
#'
#' perfume_qda_experts %>%
#'   specify(
#'     sensory_method = "QDA",
#'     panelist = panelist,
#'     product = product,
#'     session = session,
#'     pres_order = rank,
#'     attribute = spicy:wrapping
#'   )
specify <- function(data, sensory_method = c("QDA", "CATA", "RATA", "FCP", "FP", "JAR", "IPM"), panelist, product, session = NULL, pres_order = NULL, attribute, hedonic = NULL) {
  method <- arg_match(sensory_method)

  tbl <- data %>%
    select(
      !!enquo(panelist),
      !!enquo(product),
      !!enquo(session),
      !!enquo(pres_order),
      !!enquo(attribute),
      !!enquo(hedonic)
    ) %>%
    mutate_at(
      vars(
        !!enquo(panelist),
        !!enquo(product),
        !!enquo(session),
        !!enquo(pres_order)
      ),
      ~ as.factor(.x)
    )

  tbl_class <- switch(sensory_method[[1]],
    "QDA" = "tbl_sensory_qda",
    "CATA" = "tbl_sensory_cata",
    "RATA" = "tbl_sensory_rata",
    "FCP" = "tbl_sensory_fcp",
    "FP" = "tbl_sensory_fp",
    "JAR" = "tbl_sensory_jar",
    "IPM" = "tbl_sensory_ipm"
  )

  res <- new_tibble(tbl,
    "sensory_method" = sensory_method[[1]],
    "panelist" = as_label(enquo(panelist)),
    "n_panelist" = length(unique(pull(tbl, !!enquo(panelist)))),
    "product" = as_label(enquo(product)),
    "n_product" = length(unique(pull(tbl, !!enquo(product)))),
    "session" = as_label(enquo(session)),
    "pres_order" = as_label(enquo(pres_order)),
    "attribute" = unname(vars_select(names(tbl), !!enquo(attribute))),
    "n_attribute" = length(vars_select(names(tbl), !!enquo(attribute))),
    "hedonic" = meta_hedonic <- as_label(enquo(hedonic)),
    nrow = NROW(tbl),
    class = tbl_class
  )
  return(res)
}
