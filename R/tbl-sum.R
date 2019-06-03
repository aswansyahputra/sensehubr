#' @export
tbl_sum.tbl_sensory_design <- function(x){
  c(
    "A sensory table" = "Design of Experiment",
    "Panelist" = paste(attr(x, "n_panelist"), "subjects"),
    "Product" = paste(attr(x, "n_product"), "items")
  )
}

#' @export
tbl_sum.tbl_sensory_template <- function(x){
  c(
    "A sensory table" = "Design of Experiment",
    "Panelist" = paste(attr(x, "n_panelist"), "subjects"),
    "Product" = paste(attr(x, "n_product"), "items"),
    "Attribute" = paste(attr(x, "n_attribute"), "lexicons") 
  )
}

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
    "Method" = attr(x, "method_local"),
    "Model" = attr(x, "model")
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
