#' Nest sensory dataset
#' 
#' Function to nest dataset to its sensory attribute
#' 
#' @param .data a dataframe
#' @param panelist column contaning panelist information
#' @param product column contaning product information
#' @param pres_order column contaning presentation order information
#' @param replicate column contaning replicate/session information
#' 
#' @import dplyr
#' @return a tibble
#' @export

nest_data <-
  function(.data,
           panelist,
           product,
           pres_order,
           replicate = NULL) {
    panelist_quo <- enquo(panelist)
    product_quo <- enquo(product)
    pres_order_quo <- enquo(pres_order)
    if (is.null(replicate)) {
      res <- 
        .data %>%
        as_tibble() %>%
        select(!!panelist_quo,
               !!product_quo,
               !!pres_order_quo,
               everything()) %>%
        gather("attribute", "value",-c(!!panelist_quo:!!pres_order_quo)) %>%
        rename(
          panelist = !!panelist_quo,
          product = !!product_quo,
          pres_order = !!pres_order_quo
        ) %>%
        nest(-!!"attribute")
    } else if (!is.null(replicate)){
      replicate_quo <- enquo(replicate)
      res <- 
      .data %>%
        as_tibble() %>%
        select(!!panelist_quo,
               !!product_quo,
               !!pres_order_quo,
               !!replicate_quo,
               everything()) %>%
        gather("attribute", "value", -c(!!panelist_quo:!!replicate_quo)) %>%
        rename(
          panelist = !!panelist_quo,
          product = !!product_quo,
          pres_order = !!pres_order_quo,
          session = !!replicate_quo
        ) %>%
        nest(-!!"attribute")
    }
    return(res)
  }
