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

tbl_sum.tbl_sensory_qda_local <- function(x) {
  c(
    "A sensory table" = meta_info(x, "method"),
    "Type" = "Local analysis",
    "Method" = attr(x, "method_local")
  )
}

#' @export

print <- function(x) {
  UseMethod("print")
}

#' @import tibble
#' @export

print.tbl_sensory_global <- function(x){
  cat_subtle("# Sensory table\n")
  cat_subtle("#\n")
  tibble:::print.tbl(x$eigenvalue)
  cat_subtle("#\n")
  tibble:::print.tbl(x$product)
  cat_subtle("#\n")
  tibble:::print.tbl(x$attribute)
  invisible(x)
}
