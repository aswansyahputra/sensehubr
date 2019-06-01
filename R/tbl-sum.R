#' @export

tbl_sum.tbl_sensory_qda <- function(x){
  c(
    "A sensory table" = meta_info(x, "sensory_table"),
    "Panelist" = meta_info(x, "panelist"),
    "Product" = meta_info(x, "product"),
    "Attribute" = meta_info(x, "attribute"),
    "Hedonic" = meta_info(x, "hedonic")
  )
}

#' @export

tbl_sum.tbl_sensory_local <- function(x) {
  c(
    "A sensory table" = meta_info(x, "method"),
    "Type" = "Local analysis",
    "Method" = attr(x, "method_local")
  )
}


print.tbl_sensory_global <- function(x){
  print.tbl(x$eigenvalue)
  cat_subtle("#\n")
  print.tbl(x$product)
  cat_subtle("#\n")
  print.tbl(x$attribute)
  invisible(x)
}
