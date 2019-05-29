#' Create metadata for sensory table
#'
#' @param x object of interest
#'
#' @return a dataframe with class of `tbl_sensory`

tbl_sum.tbl_sensory <- function(x){
  c(
    "Panelist" = meta_info(x, "panelist"),
    "Product" = meta_info(x, "product"),
    "Attribute" = meta_info(x, "attribute"),
    "Hedonic" = meta_info(x, "hedonic")
  )
}
