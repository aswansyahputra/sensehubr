#' Create metadata for sensory table
#'
#' @param x object of interest
#'
#' @return a dataframe with class of `tbl_sensory`

tbl_sum.tbl_sensory <- function(x){
  c(
    "A sensory table" = meta_info(x, "dimension"),
    "Panelist" = meta_info(x, "panelist"),
    "Product" = meta_info(x, "product"),
    "Attribute" = meta_info(x, "attribute"),
    "Hedonic" = meta_info(x, "hedonic")
  )
}

tbl_sum.tbl_sensory_qda <- function(x) {
  c(
    "A sensory table" = meta_info(x, "dimension"),
    "Type" = "Local analysis",
    "Method" = attr(x, "method_local")
  )
}
