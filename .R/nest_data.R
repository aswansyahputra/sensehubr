#' Nest sensory dataset
#'
#' Function to nest dataset to its sensory attribute
#'
#' @param .data a dataframe
#' @param panelist column contaning panelist information
#' @param product column contaning product information
# @param pres_order column contaning presentation order information
# @param session column contaning replicate/session information
#'
#' @import dplyr
#' @return a tibble
#' @export

nest_data <-
  function(.data,
           panelist,
           product) {
    panelist_quo <- enquo(panelist)
    product_quo <- enquo(product)
    res <-
      .data %>%
      as_tibble() %>%
      select(!!panelist_quo,
             !!product_quo,
             everything()) %>%
      gather("attribute", "value",-c(!!panelist_quo:!!product_quo)) %>%
      rename(
        panelist = !!panelist_quo,
        product = !!product_quo
      ) %>%
      mutate(
        panelist = as.factor(panelist),
        product = as.factor(product)
      ) %>%
      nest(-!!"attribute")
    return(res)
  }

# nest_data <-
#   function(.data,
#            panelist,
#            product,
#            pres_order,
#            session = NULL) {
#     panelist_quo <- enquo(panelist)
#     product_quo <- enquo(product)
#     pres_order_quo <- enquo(pres_order)
#     if (missing(session)) {
#       res <-
#         .data %>%
#         as_tibble() %>%
#         select(!!panelist_quo,
#                !!product_quo,
#                !!pres_order_quo,
#                everything()) %>%
#         gather("attribute", "value",-c(!!panelist_quo:!!pres_order_quo)) %>%
#         rename(
#           panelist = !!panelist_quo,
#           product = !!product_quo,
#           pres_order = !!pres_order_quo
#         ) %>%
#         nest(-!!"attribute")
#     } else if (!missing(session)) {
#       session_quo <- enquo(session)
#       res <-
#       .data %>%
#         as_tibble() %>%
#         select(!!panelist_quo,
#                !!product_quo,
#                !!pres_order_quo,
#                !!session_quo,
#                everything()) %>%
#         gather("attribute", "value", -c(!!panelist_quo:!!session_quo)) %>%
#         rename(
#           panelist = !!panelist_quo,
#           product = !!product_quo,
#           pres_order = !!pres_order_quo,
#           session = !!session_quo
#         ) %>%
#         nest(-!!"attribute")
#     }
#     return(res)
#   }
